import subprocess
import configparser
from pathlib import Path
import time
from datetime import datetime
from dateutil.relativedelta import relativedelta




ini_path = Path(r"C:\Users\thyagomendes\AppData\Roaming\MetaQuotes\Terminal\4C230EB692C96360065CCBB721258414\config\wfrm.ini")
mt5_path = r"C:\Program Files\Capital Point Trading MT5 Terminal\terminal64.exe"

# dates = [
        
#          ('2023.01.01','2023.02.01'),
#          ('2023.02.01','2023.03.01'),
#          ('2023.03.01','2023.04.01'),
#          ('2023.04.01','2023.05.01'),
#          ('2023.05.01','2023.06.01'),
#          ('2023.06.01','2023.07.01'),
#          ('2023.07.01','2023.08.01'),
#          ('2023.08.01','2023.09.01'),
#          ('2023.09.01','2023.10.01'),
#          ('2023.10.01','2023.11.01'),
#          ('2023.11.01','2023.12.01'),
#          ('2023.12.01','2024.01.01'),
#          ('2024.01.01','2024.02.01'),
#          ('2024.02.01','2024.03.01'),
#          ('2024.03.01','2024.04.01'),
#          ('2024.04.01','2024.05.01'),
#          ('2024.05.01','2024.06.01'),
#          ('2024.06.01','2024.07.01'),
#          ('2024.07.01','2024.08.01'),
#          ('2024.08.01','2024.09.01'),
#          ('2024.09.01','2024.10.01'),
#          ('2024.10.01','2024.11.01'),
#          ('2024.11.01','2024.12.01'),
#          ('2024.12.01','2025.01.01'),
#          ('2025.01.01','2025.02.01'),
#          ('2025.02.01','2025.03.01'),
#          ('2025.03.01','2025.04.01'),
#          ('2025.04.01','2025.05.01'),
#          ('2025.05.01','2025.06.01'),
#          ('2025.06.01','2025.07.01'),
#          ('2025.07.01','2025.08.01'),
#          ('2025.08.01','2025.09.01'),
#          ('2025.09.01','2025.10.01'),
#          ('2025.10.01','2025.11.01'),
#          ('2025.11.01','2025.12.01'),
#          ('2025.12.01','2026.01.01')
         
         
         
#          ]




# dates = [
        
#          ('2023.01.01','2023.03.01'),
#          ('2023.03.01','2023.05.01'),
#          ('2023.05.01','2023.07.01'),
#          ('2023.07.01','2023.09.01'),
#          ('2023.09.01','2023.11.01'),
#          ('2023.11.01','2024.01.01'),
#          ('2024.01.01','2024.03.01'),
#          ('2024.03.01','2024.05.01'),
#          ('2024.05.01','2024.07.01'),
#          ('2024.07.01','2024.09.01'),
#          ('2024.09.01','2024.11.01'),
#          ('2024.11.01','2025.01.01'),
#          ('2025.01.01','2025.03.01'),
#          ('2025.03.01','2025.05.01'),
#          ('2025.05.01','2025.07.01'),
#          ('2025.07.01','2025.09.01'),
#          ('2025.09.01','2025.11.01'),
#          ('2025.11.01','2026.01.01'),
#          ('2026.01.01','2026.03.01'),
         
         
#          ]





def gerar_intervalos_mensais(n_meses):
    hoje = datetime.today()
    
    # vamos alinhar para o primeiro dia do mês atual
    fim = hoje
    print(fim.strftime('%Y.%m.%d'))
    
    datas = []
    
    for i in range(n_meses):
        inicio = fim - relativedelta(months=1)
        
        datas.append((
            inicio.strftime('%Y.%m.%d'),
            fim.strftime('%Y.%m.%d')
        ))
        
        # anda um mês para trás
        fim = inicio
    
    # inverter para ficar crescente (opcional)
    datas.reverse()
    
    return datas

dates = gerar_intervalos_mensais(n_meses=24)
print(dates)



for date in dates:
    print(date)
    from_date = date[0]
    to_date = date[1]

    cfg = configparser.ConfigParser()
    cfg.read(ini_path, encoding="utf-16")

    cfg["Tester"]["FromDate"] = from_date
    cfg["Tester"]["ToDate"] = to_date
  


    base_report_name = Path(f"{ from_date.replace('.', '_') }_to_{ to_date.replace('.', '_') }.xml")

    cfg["Tester"]["Report"] = cfg["Tester"]['Symbol']+'_'+cfg["Tester"]["Report"].replace( 'report',f"{from_date.replace('.', '_') }_to_{ to_date.replace('.', '_')}" )
    


    temp_config_path = ini_path.with_name(ini_path.stem + "_temp.ini")
    with open(temp_config_path, "w", encoding="utf-16") as f:
        cfg.write(f, space_around_delimiters=False)

    subprocess.run([mt5_path, f"/config:{temp_config_path}"])

    time.sleep(2)






