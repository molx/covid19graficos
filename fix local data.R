library(tidyverse)

siglas <- c(AC = "Acre",
            AL = "Alagoas",
            AP = "Amapá",
            AM = "Amazonas",
            BA = "Bahia",
            CE = "Ceará",
            DF = "Distrito Federal",
            ES = "Espírito Santo",
            GO = "Goiás",
            MA = "Maranhão",
            MT = "Mato Grosso",
            MS = "Mato Grosso do Sul",
            MG = "Minas Gerais",
            PA = "Pará",
            PB = "Paraíba",
            PR = "Paraná",
            PE = "Pernambuco",
            PI = "Piauí",
            RJ = "Rio de Janeiro",
            RN = "Rio Grande do Norte",
            RS = "Rio Grande do Sul",
            RN = "Rondônia",
            RR = "Roraima",
            SC = "Santa Catarina",
            SP = "São Paulo",
            SE = "Sergipe",
            TO = "Tocantins") %>% enframe("estado", "location")

convertido <- arquivo_geral %>% select(-regiao) %>% 
  rename(date = data, 
         total_cases = casosAcumulados, new_cases = casosNovos,
         total_deaths = obitosAcumulados, new_deaths = obitosNovos) %>%
  right_join(siglas) %>% 
  select(date, location, total_cases, total_deaths, new_cases, new_deaths) %>%
  arrange(date, location)

write_excel_csv(convertido, path = "data/estados.csv")

tail(convertido)
