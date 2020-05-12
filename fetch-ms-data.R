library(httr)
library(jsonlite)
library(tidyverse)
library(readr)
library(readxl)
library(WriteXLS)
library(jsonlite)

source("siglas.R", encoding = "utf-8")

#url <- "https://xx9p7hp1p7.execute-api.us-east-1.amazonaws.com/prod/PortalMapa"
#url <- "https://xx9p7hp1p7.execute-api.us-east-1.amazonaws.com/prod/PortalGeral"
url <- "https://xx9p7hp1p7.execute-api.us-east-1.amazonaws.com/prod/PortalGeralApi"
res <- GET(url)#, add_headers("x-parse-application-id" = "unAFkcaNDeXajurGB7LChj8SgQYS2ptm"))

tmp <- parse_json(content(res, "text"), simplifyVector = TRUE)
GET(tmp$planilha$arquivo$url, write_disk(ms_data_full <- "data/msdata.xlsx", overwrite = TRUE))

calc_new <- function(x) {
  new <- x - lag(x)
  new[is.na(new)] <- 0
  new
}

ms_data <- read_excel(ms_data_full) %>%
  mutate(data = as.Date(data), casosAcumulado = as.numeric(casosAcumulado),
         obitosAcumulado = as.numeric(obitosAcumulado)) %>%
  filter(is.na(municipio), is.na(codmun), !is.na(estado), data == last(sort(data))) %>% 
  select(estado, data, casosAcumulado, obitosAcumulado) %>%
  rename(total_cases = casosAcumulado, total_deaths = obitosAcumulado, date = data) %>%
  right_join(siglas)


# cols <- c("nome", "qtd_confirmado", "qtd_obito")
# last_ms_data <- parse_json(content(res, "text"), simplifyVector = TRUE)[[1]][cols] %>%
#   rename(total_cases = qtd_confirmado, total_deaths = qtd_obito, location = nome) %>%
#   mutate(date = Sys.Date())

olddata <- read_csv("data/estados.csv")

pop <- read_excel("data/Brasil.xlsx", sheet = "Populacao")

dbl <- read_csv("data/dbl_time.csv")
dbl_br <- read_csv("data/dbl_time_br.csv")

popJSON = paste("var populacao =",
                toJSON(pop %>% pivot_wider(names_from = UF, values_from = População),
                       pretty = FALSE))
dblJSON = paste("var dbl_time =",
                toJSON(dbl, pretty = FALSE))
dblJSONbr = paste("var dbl_time_br =",
                  toJSON(dbl_br, pretty = FALSE))

if (max(olddata$date) == max(ms_data$date)) {
  cat("\nData already up to date.\n\n") 
  
  dataJSON = paste("var estados =",
                   toJSON(olddata, pretty = FALSE))
  
} else {
  cat("\nUpdating data...\n\n")
  newdata <- full_join(olddata, ms_data) %>% group_by(location) %>%
    mutate(new_cases = calc_new(total_cases),
           new_deaths = calc_new(total_deaths))
  cat("Saving CSV file...\n\n")
  write_excel_csv(newdata, path = "data/estados.csv")
  
  exceldata_cases <- newdata %>% select(date, location, total_cases) %>%
    pivot_wider(names_from = location, values_from = total_cases) %>% 
    rename(Data = date)
  
  exceldata_deaths <- newdata %>% select(date, location, total_deaths) %>%
    pivot_wider(names_from = location, values_from = total_deaths) %>% 
    rename(Data = date)
  
 
  
  cat("Saving Excel file...\n\n")
  WriteXLS(x = list(exceldata_cases, exceldata_deaths, pop),
           ExcelFileName = "data/Brasil.xlsx",
           SheetNames = c("Confirmados", "Mortes", "Populacao"))
  
  cat("Saving JS file...\n\n")
 
  dataJSON = paste("var estados =",
                   toJSON(newdata, pretty = FALSE))
}

writeLines(paste(popJSON, dblJSON, dblJSONbr, dataJSON, sep = ";"),
           con = "js/data.js", useBytes = TRUE)
