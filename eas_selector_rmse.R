library(XML)
library(dplyr)
library(ggplot2)
library(xgboost)
library(tidyr)

rm(list = ls())
graphics.off()

# =========================================================
# Parâmetros
# =========================================================
rmse_val_lim <- 300
n_best_test <- 30
folder_data <- "data_2"
pred_cutoff<-50
B <- 100

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

cov_cols <- c(
  "Stop_Loss",
  "Take_Profit",
  "Ind1Param0",
  "Ind3Param0"
)

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
paths <- list.files(folder_data, pattern = "\\.xml$", full.names = TRUE)[1:24]
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
# Função para calcular RMSE
# =========================================================
calc_rmse <- function(y_true, y_pred) {
  sqrt(mean((y_true - y_pred)^2, na.rm = TRUE))
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
    rmse_train = NA_real_,
    rmse_val = NA_real_,
    rmse_test = NA_real_,
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
      rmse_train = NA_real_,
      rmse_val = NA_real_,
      rmse_test = NA_real_,
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
                      rmse_val_lim = 100,
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
    rmse_train = numeric(),
    rmse_val = numeric(),
    rmse_test = numeric(),
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
      pull(all_of(pred_col))
    
    X_val_raw <- df %>%
      filter(Period == periods[i - 1]) %>%
      select(all_of(cov_cols)) %>%
      as.matrix()
    
    Y_val <- df %>%
      filter(Period == periods[i]) %>%
      pull(all_of(pred_col))
    
    X_test_raw <- df %>%
      filter(Period == periods[i]) %>%
      select(all_of(cov_cols)) %>%
      as.matrix()
    
    Y_test_df <- df %>%
      filter(Period == periods[i + 1]) %>%
      select(all_of(pred_col))
    
    Y_test <- Y_test_df[[pred_col]]
    Profit_test_real <- Y_test
    
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
    # Parâmetros do modelo preditivo
    # -------------------------------------------------------
    params <- list(
      objective = "reg:squarederror",
      eval_metric = "rmse",
      max_depth = 3,
      eta = 0.005,
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
          nrounds = 200,
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
    Pred_train <- Pred_val <- Pred_test <- NULL
    
    tryCatch(
      {
        Pred_train <- predict(modelo, dtrain)
        Pred_val   <- predict(modelo, dval)
        Pred_test  <- predict(modelo, dtest)
      },
      error = function(e) {
        cat("Erro na predição:", conditionMessage(e), "\n")
        pred_ok <<- FALSE
      }
    )
    
    if (!pred_ok ||
        is.null(Pred_train) ||
        is.null(Pred_val) ||
        is.null(Pred_test) ||
        any(!is.finite(Pred_train)) ||
        any(!is.finite(Pred_val)) ||
        any(!is.finite(Pred_test))) {
      
      tmp <- append_zero_result(summary_results, results_list, i, periods,
                                "falha nas previsões")
      summary_results <- tmp$summary_results
      results_list <- tmp$results_list
      next
    }
    
    # -------------------------------------------------------
    # Métricas
    # -------------------------------------------------------
    rmse_train <- calc_rmse(Y_train, Pred_train)
    rmse_val   <- calc_rmse(Y_val, Pred_val)
    rmse_test  <- calc_rmse(Y_test, Pred_test)
    
    cat("RMSE treino:", round(rmse_train, 4), "\n")
    cat("RMSE validação:", round(rmse_val, 4), "\n")
    cat("RMSE teste:", round(rmse_test, 4), "\n")
    
    # -------------------------------------------------------
    # Filtro de validação
    # -------------------------------------------------------
    passed_val_filter <- (rmse_val <= rmse_val_lim) & (rmse_train <= rmse_val_lim)
    
    # -------------------------------------------------------
    # Seleção das melhores oportunidades do teste
    # -------------------------------------------------------
    if (passed_val_filter) {
      
      n_select <- min(n_best_test, length(Pred_test))
      
      df_test_rank <- data.frame(
        Pass = c(0:(nrow(X_test) - 1)),
        Profit_test_real = Profit_test_real,
        Pred_test = Pred_test
      ) %>%
        arrange(desc(Pred_test)) %>%
        slice_head(n = n_select)%>%
        filter(Pred_test > pred_cutoff)
      
      cat("n_select (df_test_rank): ", n_select, "\n")
      print(df_test_rank)
      
      
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
      Pred_val = Pred_val
    )
    
    df_test <- data.frame(
      Profit_test_real = Profit_test_real,
      Pred_test = Pred_test
    )
    
    # -------------------------------------------------------
    # Gráficos
    # -------------------------------------------------------
    if (make_plots) {
      plot_val <- df_val %>%
        ggplot(aes(x = Y_val, y = Pred_val)) +
        geom_point(alpha = 0.6) +
        labs(
          x = "Lucro real",
          y = "Lucro previsto",
          title = paste("Validação -", periods[i])
        ) +
        theme_minimal()
      
      print(plot_val)
      
      plot_test <- df_test %>%
        ggplot(aes(x = Profit_test_real, y = Pred_test)) +
        geom_point(alpha = 0.6) +
        labs(
          x = "Lucro real",
          y = "Lucro previsto",
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
      rmse_train = rmse_train,
      rmse_val = rmse_val,
      rmse_test = rmse_test,
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
        rmse_train = rmse_train,
        rmse_val = rmse_val,
        rmse_test = rmse_test,
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
Profit <- data.frame()
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
    rmse_val_lim = rmse_val_lim,
    n_best_test = n_best_test,
    make_plots = FALSE
  )
  
  Profit <- rbind(Profit, out$summary_results$sum_profit_best_test)
  N[b] <- sum(out$summary_results$passed_val_filter)
}



# Criar um dataframe com os dados e os rótulos
R = Profit%>%colSums()%>%as.vector()
dados <- data.frame(
  indice = paste0("int_", 1:length(R)),  # Cria "int_1", "int_2", etc.
  valor =  R
)

# Criar o gráfico de barras



# CONVERTER PARA FATOR COM NÍVEIS NA ORDEM CORRETA
dados$indice <- factor(dados$indice, levels = paste0("int_", 1:length(R)))

# Criar o gráfico - a ordem será mantida!
ggplot(dados, aes(x = indice, y = valor, fill = valor > 0)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "coral")) +
  labs(title = "Ordem Correta no Eixo X",
       x = "Índice",
       y = "Valor") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


mean(R[R!=0]>0)

sum(R)
