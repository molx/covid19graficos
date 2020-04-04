library(httr)
library(jsonlite)
library(tidyverse)
library(readr)
library(readxl)
library(WriteXLS)
library(jsonlite)

url <- "https://xx9p7hp1p7.execute-api.us-east-1.amazonaws.com/prod/PortalMapa"
res <- GET(url, add_headers("x-parse-application-id" = "unAFkcaNDeXajurGB7LChj8SgQYS2ptm"))

calc_new <- function(x) {
  new <- x - lag(x)
  new[is.na(new)] <- 0
  new
}

cols <- c("nome", "qtd_confirmado", "qtd_obito")
last_ms_data <- parse_json(content(res, "text"), simplifyVector = TRUE)[[1]][cols] %>%
  rename(total_cases = qtd_confirmado, total_deaths = qtd_obito, location = nome) %>%
  mutate(date = Sys.Date())

olddata <- read_csv("data/estados.csv")

#Last day added to old data
last_saved_day <- filter(olddata, date == max(olddata$date))

if (all(sort(last_ms_data$total_cases) == sort(last_saved_day$total_cases))) {
  cat("\nData already up to date.\n\n") 
} else {
  cat("\nUpdating data...\n\n")
  newdata <- full_join(olddata, last_ms_data) %>% group_by(location) %>%
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
  
  pop <- read_excel("data/Brasil.xlsx", sheet = "Populacao")
  
  cat("Saving Excel file...\n\n")
  WriteXLS(x = list(exceldata_cases, exceldata_deaths, pop),
           ExcelFileName = "data/Brasil.xlsx",
           SheetNames = c("Confirmados", "Mortes", "Populacao"))
  
  cat("Saving JS file...\n\n")
  
  writeLines(paste("var estados =",
                   toJSON(newdata, pretty = FALSE)),
             con = "js/estados.js", useBytes = TRUE)
  
}

writeLines(paste("var populacao =",
                 toJSON(pop %>% pivot_wider(names_from = UF, values_from = População),
                        pretty = FALSE)),
           con = "js/populacao.js", useBytes = TRUE)
