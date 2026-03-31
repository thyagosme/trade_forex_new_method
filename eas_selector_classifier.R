library(XML)
library(dplyr)
library(ggplot2)
library(xgboost)

rm(list = ls())
graphics.off()

# =========================================================
# Parâmetros
# =========================================================
f1_val_lim <- 0.5
n_best_test <- 10
folder_data <- "data_2"
prob_cutoff <- 0.7
B <- 500

cov_cols <- c(
  "Profit",
  "Expected Payoff",
  "Profit Factor",
  "Recovery Factor",
  "Sharpe Ratio",
  "Equity DD %",
  "Trades",
  "Stop_Loss",
  "Take_Profit",
  "Ind1Param0",
  "Ind3Param0"
)


# cov_cols <- c(
#   "Stop_Loss",
#   "Take_Profit",
#   "Ind0Param0",
#   "Ind2Param0"
# )

pred_col <- "Profit"

# =========================================================
# Função para leitura do XML
# =========================================================
read_xml_file <- function(file_name) {
  doc <- xmlParse(file_name)
  
  rows <- getNodeSet(doc, "//*[local-name()='Row']")
  
  dados <- lapply(rows, function(r) {
    cells <- getNodeSet(r, ".//*[local-name()='Cell']")
    
    sapply(cells, function(c) {
      data_node <- getNodeSet(c, ".//*[local-name()='Data']")
      
      if (length(data_node) == 0) {
        NA
      } else {
        xmlValue(data_node[[1]])
      }
    })
  })
  
  max_cols <- max(sapply(dados, length))
  
  dados_pad <- lapply(dados, function(x) {
    length(x) <- max_cols
    x
  })
  
  mat <- do.call(rbind, dados_pad)
  df <- as.data.frame(mat, stringsAsFactors = FALSE)
  
  colnames(df) <- df[1, ]
  df <- df[-1, ]
  rownames(df) <- NULL
  
  return(df)
}

# =========================================================
# Ler todos os arquivos
# =========================================================
paths <- list.files(folder_data, pattern = "\\.xml$", full.names = TRUE)
print(paths)

df <- data.frame()

for (path in paths) {
  file_name <- tools::file_path_sans_ext(basename(path))
  
  new_xml <- read_xml_file(path) %>%
    arrange(as.numeric(Pass))
  
  df <- rbind(
    df,
    new_xml %>%
      mutate(Period = file_name)
  )
}

df <- df %>%
  mutate(across(everything(), ~ type.convert(.x, as.is = TRUE)))

colnames(df)

# =========================================================
# Função para calcular F1-score
# =========================================================
calc_f1 <- function(y_true, y_pred) {
  tp <- sum(y_true == 1 & y_pred == 1, na.rm = TRUE)
  fp <- sum(y_true == 0 & y_pred == 1, na.rm = TRUE)
  fn <- sum(y_true == 1 & y_pred == 0, na.rm = TRUE)
  
  precision <- if ((tp + fp) == 0) 0 else tp / (tp + fp)
  recall    <- if ((tp + fn) == 0) 0 else tp / (tp + fn)
  
  if ((precision + recall) == 0) {
    return(0)
  } else {
    return(2 * precision * recall / (precision + recall))
  }
}

# =========================================================
# Função auxiliar para registrar mês sem operação
# =========================================================
append_zero_result <- function(summary_results, results_list, i, periods, motivo) {
  
  cat("\n---------------------------------\n")
  cat("Iteração pulada:", i, "\n")
  cat("Motivo:", motivo, "\n")
  cat("Train X:", periods[i - 2], "\n")
  cat("Train Y / Val X:", periods[i - 1], "\n")
  cat("Val Y / Test X:", periods[i], "\n")
  cat("Test Y:", periods[i + 1], "\n")
  cat("Lucro do mês de previsão = 0\n")
  cat("---------------------------------\n")
  
  df_test_rank <- data.frame()
  df_val <- data.frame()
  df_test <- data.frame()
  
  results_list[[length(results_list) + 1]] <- list(
    iteration = i,
    train_period_x = periods[i - 2],
    train_period_y = periods[i - 1],
    val_period_x = periods[i - 1],
    val_period_y = periods[i],
    test_period_x = periods[i],
    test_period_y = periods[i + 1],
    model = NULL,
    f1_train = NA_real_,
    f1_val = NA_real_,
    f1_test = NA_real_,
    passed_val_filter = FALSE,
    n_selected_test = 0,
    sum_profit_best_test = 0,
    skip_reason = motivo,
    df_val = df_val,
    df_test = df_test,
    df_test_rank = df_test_rank
  )
  
  summary_results <- rbind(
    summary_results,
    data.frame(
      iteration = i,
      train_period_x = periods[i - 2],
      train_period_y = periods[i - 1],
      val_period_x = periods[i - 1],
      val_period_y = periods[i],
      test_period_x = periods[i],
      test_period_y = periods[i + 1],
      f1_train = NA_real_,
      f1_val = NA_real_,
      f1_test = NA_real_,
      passed_val_filter = FALSE,
      n_selected_test = 0,
      sum_profit_best_test = 0,
      skip_reason = motivo,
      stringsAsFactors = FALSE
    )
  )
  
  return(list(
    summary_results = summary_results,
    results_list = results_list
  ))
}

# =========================================================
# Função principal
# =========================================================
run_model <- function(df,
                      seed = NULL,
                      cov_cols,
                      pred_col = "Profit",
                      f1_val_lim = 0.7,
                      n_best_test = 10,
                      make_plots = TRUE) {
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  periods <- sort(unique(df$Period))
  print(periods)
  
  results_list <- list()
  
  summary_results <- data.frame(
    iteration = integer(),
    train_period_x = character(),
    train_period_y = character(),
    val_period_x = character(),
    val_period_y = character(),
    test_period_x = character(),
    test_period_y = character(),
    f1_train = numeric(),
    f1_val = numeric(),
    f1_test = numeric(),
    passed_val_filter = logical(),
    n_selected_test = integer(),
    sum_profit_best_test = numeric(),
    skip_reason = character(),
    stringsAsFactors = FALSE
  )
  
  if (length(periods) < 4) {
    warning("É necessário ter pelo menos 4 períodos para rodar o esquema train/val/test.")
    return(list(
      summary_results = summary_results,
      results_list = results_list
    ))
  }
  
  for (i in 3:(length(periods) - 1)) {
    
    cat("\n============================\n")
    cat("Iteração:", i, "\n")
    cat("Train X:", periods[i - 2], "\n")
    cat("Train Y / Val X:", periods[i - 1], "\n")
    cat("Val Y / Test X:", periods[i], "\n")
    cat("Test Y:", periods[i + 1], "\n")
    cat("============================\n")
    
    # -------------------------------------------------------
    # Montagem dos conjuntos
    # -------------------------------------------------------
    X_train_raw <- df %>%
      filter(Period == periods[i - 2]) %>%
      select(all_of(cov_cols)) %>%
      as.matrix()
    
    Y_train <- df %>%
      filter(Period == periods[i - 1]) %>%
      mutate(Target = as.integer(.data[[pred_col]] > 0)) %>%
      pull(Target)
    
    X_val_raw <- df %>%
      filter(Period == periods[i - 1]) %>%
      select(all_of(cov_cols)) %>%
      as.matrix()
    
    Y_val <- df %>%
      filter(Period == periods[i]) %>%
      mutate(Target = as.integer(.data[[pred_col]] > 0)) %>%
      pull(Target)
    
    X_test_raw <- df %>%
      filter(Period == periods[i]) %>%
      select(all_of(cov_cols)) %>%
      as.matrix()
    
    Y_test_df <- df %>%
      filter(Period == periods[i + 1]) %>%
      mutate(Target = as.integer(.data[[pred_col]] > 0)) %>%
      select(all_of(pred_col), Target)
    
    Y_test <- Y_test_df$Target
    Profit_test_real <- Y_test_df[[pred_col]]
    
    # -------------------------------------------------------
    # Checagens de integridade
    # -------------------------------------------------------
    if (nrow(X_train_raw) == 0 || nrow(X_val_raw) == 0 || nrow(X_test_raw) == 0 ||
        length(Y_train) == 0 || length(Y_val) == 0 || length(Y_test) == 0) {
      
      tmp <- append_zero_result(summary_results, results_list, i, periods,
                                "dataset vazio em treino/validação/teste")
      summary_results <- tmp$summary_results
      results_list <- tmp$results_list
      next
    }
    
    if (nrow(X_train_raw) != length(Y_train) ||
        nrow(X_val_raw) != length(Y_val) ||
        nrow(X_test_raw) != length(Y_test)) {
      
      tmp <- append_zero_result(summary_results, results_list, i, periods,
                                "X e Y com tamanhos diferentes")
      summary_results <- tmp$summary_results
      results_list <- tmp$results_list
      next
    }
    
    if (any(!is.finite(X_train_raw)) ||
        any(!is.finite(X_val_raw)) ||
        any(!is.finite(X_test_raw)) ||
        any(!is.finite(Y_train)) ||
        any(!is.finite(Y_val)) ||
        any(!is.finite(Y_test)) ||
        any(!is.finite(Profit_test_real))) {
      
      tmp <- append_zero_result(summary_results, results_list, i, periods,
                                "NA/NaN/Inf encontrado")
      summary_results <- tmp$summary_results
      results_list <- tmp$results_list
      next
    }
    
    if (length(unique(Y_train)) < 2) {
      tmp <- append_zero_result(summary_results, results_list, i, periods,
                                "Y_train com apenas uma classe")
      summary_results <- tmp$summary_results
      results_list <- tmp$results_list
      next
    }
    
    if (length(unique(Y_val)) < 2) {
      tmp <- append_zero_result(summary_results, results_list, i, periods,
                                "Y_val com apenas uma classe")
      summary_results <- tmp$summary_results
      results_list <- tmp$results_list
      next
    }
    
    if (length(unique(Y_test)) < 2) {
      tmp <- append_zero_result(summary_results, results_list, i, periods,
                                "Y_test com apenas uma classe")
      summary_results <- tmp$summary_results
      results_list <- tmp$results_list
      next
    }
    
    # -------------------------------------------------------
    # Escalonamento
    # -------------------------------------------------------
    X_train <- scale(X_train_raw)
    center_train <- attr(X_train, "scaled:center")
    scale_train  <- attr(X_train, "scaled:scale")
    
    scale_train[is.na(scale_train) | scale_train == 0] <- 1
    
    X_val  <- scale(X_val_raw, center = center_train, scale = scale_train)
    X_test <- scale(X_test_raw, center = center_train, scale = scale_train)
    
    X_train <- as.matrix(X_train)
    X_val   <- as.matrix(X_val)
    X_test  <- as.matrix(X_test)
    
    if (any(!is.finite(X_train)) ||
        any(!is.finite(X_val)) ||
        any(!is.finite(X_test))) {
      
      tmp <- append_zero_result(summary_results, results_list, i, periods,
                                "dados inválidos após escalonamento")
      summary_results <- tmp$summary_results
      results_list <- tmp$results_list
      next
    }
    
    # -------------------------------------------------------
    # DMatrix
    # -------------------------------------------------------
    dtrain <- xgb.DMatrix(data = X_train, label = Y_train)
    dval   <- xgb.DMatrix(data = X_val, label = Y_val)
    dtest  <- xgb.DMatrix(data = X_test, label = Y_test)
    
    watchlist <- list(train = dtrain, val = dval)
    
    # -------------------------------------------------------
    # Parâmetros do modelo
    # -------------------------------------------------------
    params <- list(
      objective = "binary:logistic",
      eval_metric = "logloss",
      max_depth = 3,
      eta = 1.5,
      subsample = 0.8,
      colsample_bytree = 0.8
    )
    
    # -------------------------------------------------------
    # Treinamento protegido
    # -------------------------------------------------------
    modelo <- tryCatch(
      {
        xgb.train(
          params = params,
          data = dtrain,
          nrounds = 100,
          evals = watchlist,
          early_stopping_rounds = 20,
          verbose = 0
        )
      },
      error = function(e) {
        cat("Erro no xgb.train():", conditionMessage(e), "\n")
        return(NULL)
      }
    )
    
    if (is.null(modelo)) {
      tmp <- append_zero_result(summary_results, results_list, i, periods,
                                "falha no treinamento do xgboost")
      summary_results <- tmp$summary_results
      results_list <- tmp$results_list
      next
    }
    
    # -------------------------------------------------------
    # Predições protegidas
    # -------------------------------------------------------
    pred_ok <- TRUE
    Pred_train_prob <- Pred_val_prob <- Pred_test_prob <- NULL
    
    tryCatch(
      {
        Pred_train_prob <- predict(modelo, dtrain)
        Pred_val_prob   <- predict(modelo, dval)
        Pred_test_prob  <- predict(modelo, dtest)
      },
      error = function(e) {
        cat("Erro na predição:", conditionMessage(e), "\n")
        pred_ok <<- FALSE
      }
    )
    
    if (!pred_ok ||
        is.null(Pred_train_prob) ||
        is.null(Pred_val_prob) ||
        is.null(Pred_test_prob) ||
        any(!is.finite(Pred_train_prob)) ||
        any(!is.finite(Pred_val_prob)) ||
        any(!is.finite(Pred_test_prob))) {
      
      tmp <- append_zero_result(summary_results, results_list, i, periods,
                                "falha nas previsões")
      summary_results <- tmp$summary_results
      results_list <- tmp$results_list
      next
    }
    
    # -------------------------------------------------------
    # Classes previstas
    # -------------------------------------------------------
    Pred_train_class <- as.integer(Pred_train_prob >= prob_cutoff )
    Pred_val_class   <- as.integer(Pred_val_prob >= prob_cutoff )
    Pred_test_class  <- as.integer(Pred_test_prob >= prob_cutoff )
    
    # -------------------------------------------------------
    # Métricas
    # -------------------------------------------------------
    f1_train <- calc_f1(Y_train, Pred_train_class)
    f1_val   <- calc_f1(Y_val, Pred_val_class)
    f1_test  <- calc_f1(Y_test, Pred_test_class)
    
    cat("F1 treino:", round(f1_train, 4), "\n")
    cat("F1 validação:", round(f1_val, 4), "\n")
    cat("F1 teste:", round(f1_test, 4), "\n")
    
    # -------------------------------------------------------
    # Filtro de validação
    # -------------------------------------------------------
    passed_val_filter <- (f1_val >= f1_val_lim) & (f1_train >= f1_val_lim)
    
    # -------------------------------------------------------
    # Seleção das melhores oportunidades do teste
    # -------------------------------------------------------
    if (passed_val_filter) {
      
      n_select <- min(n_best_test, length(Pred_test_prob))
      
      df_test_rank <- data.frame(
        Pass = c(0:(nrow(X_test) - 1)),
        Profit_test_real = Profit_test_real,
        Y_test = Y_test,
        Pred_test_prob = Pred_test_prob,
        Pred_test_class = Pred_test_class
      ) %>%
        arrange(desc(Pred_test_prob)) %>%
        slice_head(n = n_select)
      
      sum_profit_best_test <- sum(df_test_rank$Profit_test_real, na.rm = TRUE)
      
    } else {
      
      n_select <- 0
      df_test_rank <- data.frame()
      sum_profit_best_test <- 0
    }
    
    cat("Passou filtro de validação? ", passed_val_filter, "\n")
    cat("Soma lucro top teste: ", sum_profit_best_test, "\n")
    cat("df_test_rank:\n")
    print(df_test_rank)
    
    # -------------------------------------------------------
    # Dataframes auxiliares
    # -------------------------------------------------------
    df_val <- data.frame(
      Y_val = Y_val,
      Pred_val_prob = Pred_val_prob,
      Pred_val_class = Pred_val_class
    )
    
    df_test <- data.frame(
      Profit_test_real = Profit_test_real,
      Y_test = Y_test,
      Pred_test_prob = Pred_test_prob,
      Pred_test_class = Pred_test_class
    )
    
    # -------------------------------------------------------
    # Gráficos
    # -------------------------------------------------------
    if (make_plots) {
      plot_val <- df_val %>%
        ggplot(aes(x = factor(Y_val), y = Pred_val_prob)) +
        geom_boxplot() +
        labs(
          x = "Classe real (0 = Profit <= 0, 1 = Profit > 0)",
          y = "Probabilidade prevista da classe 1",
          title = paste("Validação -", periods[i])
        ) +
        theme_minimal()
      
      print(plot_val)
      
      plot_test <- df_test %>%
        ggplot(aes(x = factor(Y_test), y = Pred_test_prob)) +
        geom_boxplot() +
        labs(
          x = "Classe real (0 = Profit <= 0, 1 = Profit > 0)",
          y = "Probabilidade prevista da classe 1",
          title = paste("Teste -", periods[i + 1])
        ) +
        theme_minimal()
      
      print(plot_test)
    }
    
    # -------------------------------------------------------
    # Guardar resultados
    # -------------------------------------------------------
    results_list[[length(results_list) + 1]] <- list(
      iteration = i,
      train_period_x = periods[i - 2],
      train_period_y = periods[i - 1],
      val_period_x = periods[i - 1],
      val_period_y = periods[i],
      test_period_x = periods[i],
      test_period_y = periods[i + 1],
      model = modelo,
      f1_train = f1_train,
      f1_val = f1_val,
      f1_test = f1_test,
      passed_val_filter = passed_val_filter,
      n_selected_test = n_select,
      sum_profit_best_test = sum_profit_best_test,
      skip_reason = NA_character_,
      df_val = df_val,
      df_test = df_test,
      df_test_rank = df_test_rank
    )
    
    summary_results <- rbind(
      summary_results,
      data.frame(
        iteration = i,
        train_period_x = periods[i - 2],
        train_period_y = periods[i - 1],
        val_period_x = periods[i - 1],
        val_period_y = periods[i],
        test_period_x = periods[i],
        test_period_y = periods[i + 1],
        f1_train = f1_train,
        f1_val = f1_val,
        f1_test = f1_test,
        passed_val_filter = passed_val_filter,
        n_selected_test = n_select,
        sum_profit_best_test = sum_profit_best_test,
        skip_reason = NA_character_,
        stringsAsFactors = FALSE
      )
    )
  }
  
  cat("\nResumo final:\n")
  print(summary_results)
  
  return(list(
    summary_results = summary_results,
    results_list = results_list
  ))
}

# =========================================================
# Simulação / repetição
# =========================================================
# Profit <- data.frame( first_part =  numeric(B), second_part = numeric(B))
Profit<-data.frame()
Profit


all_runs <- vector("list", B)
N <- numeric(B)

for (b in 1:B) {
  cat("\n#################################\n")
  cat("Rodada", b, "\n")
  cat("#################################\n")
  
  out <- run_model(
    df = df,
    seed = b,
    cov_cols = cov_cols,
    pred_col = pred_col,
    f1_val_lim = f1_val_lim,
    n_best_test = n_best_test,
    make_plots = FALSE
  )
  
  # l = length(out$summary_results$sum_profit_best_test)
  # l
  # Profit[b,1] <- sum(out$summary_results$sum_profit_best_test[1:round(l/2)])
  # Profit[b,2] <- sum(out$summary_results$sum_profit_best_test[(round(l/2)+1):l])
  Profit<-rbind(Profit, out$summary_results$sum_profit_best_test)
  
  N[b] <- sum(out$summary_results$passed_val_filter)
  # all_runs[[b]] <- out
}


colnames(Profit)<-sprintf("int_%d", 1:(length(paths)-3))
Profit

Profit %>%
  as.data.frame() %>%
  tidyr::pivot_longer(cols = everything(), names_to = "intervalo", values_to = "valor") %>%
  mutate(intervalo = factor(intervalo, levels = unique(intervalo))) %>%
  ggplot(aes(x = intervalo, y = valor)) +
  geom_col(fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


Profit%>%colSums()%>%as.vector()

# =========================================================
# Estatísticas finais
# =========================================================
#Profit

# m_first <- mean(Profit[,1])
# print(m_first)
# 
# m_second <- mean(Profit[,2])
# print(m_second)
# 
# s_first <- sd(Profit[,1])
# print(s_first)
# 
# s_second <- sd(Profit[,2])
# print(s_second)
# 
# 
# p_pos_first <- sum(Profit[,1] > 0) / sum(Profit[,1] != 0)
# print(p_pos_first)
# 
# p_pos_second <- sum(Profit[,2] > 0) / sum(Profit[,2] != 0)
# print(p_pos_second)
# 
# 
# ic_first <- round(c(m_first - 1.96 * s_first / sqrt(B), m_first + 1.96 * s_first / sqrt(B)), 1)
# print(ic_first)
# 
# ic_second <- round(c(m_second - 1.96 * s_second / sqrt(B), m_second + 1.96 * s_second / sqrt(B)), 1)
# print(ic_second)
# 
# hist(
#   Profit[Profit[,1] != 0,1],
#   breaks = 30,
#   main = "Distribuição dos Lucros Totais",
#   xlab = "Lucro Total",
#   col = "lightblue"
# )
# 
# 
# hist(
#   Profit[Profit[,2] != 0,2],
#   breaks = 30,
#   main = "Distribuição dos Lucros Totais",
#   xlab = "Lucro Total",
#   col = "lightblue"
# )
