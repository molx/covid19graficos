library(WriteXLS)
library(jsonlite)

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

cat("Saving CSV file...\n\n")
write_excel_csv(estados, path = "data/estados.csv")

exceldata_cases <- estados %>% select(date, location, total_cases) %>%
  pivot_wider(names_from = location, values_from = total_cases) %>% 
  rename(Data = date)

exceldata_deaths <- estados %>% select(date, location, total_deaths) %>%
  pivot_wider(names_from = location, values_from = total_deaths) %>% 
  rename(Data = date)

cat("Saving Excel file...\n\n")
WriteXLS(x = list(exceldata_cases, exceldata_deaths, pop),
         ExcelFileName = "data/Brasil.xlsx",
         SheetNames = c("Confirmados", "Mortes", "Populacao"))

cat("Saving JS file...\n\n")

dataJSON = paste("var estados =",
                 toJSON(estados, pretty = FALSE))

writeLines(paste(popJSON, dblJSON, dblJSONbr, dataJSON, sep = ";"),
           con = "js/data.js", useBytes = TRUE)
