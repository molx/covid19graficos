source("siglas.R", encoding = "utf-8")

url <- "https://data.brasil.io/dataset/covid19/caso_full.csv.gz"
download.file(url, destfile="data/caso_full.csv.gz", quiet = TRUE)
brio <- read_csv("data/caso_full.csv.gz") %>% filter(place_type == "state") %>%
  select(date, state, new_confirmed, new_deaths) %>%
  arrange(date) %>% 
  rename(estado = state, new_cases = new_confirmed) %>%
  full_join(siglas) %>%
  group_by(location) %>%
  mutate(total_cases = cumsum(new_cases),
         total_deaths = cumsum(new_deaths)) %>%
  select(date, location, new_cases, total_cases, new_deaths, total_deaths)

