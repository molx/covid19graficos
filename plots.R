library(readr)
library(readxl)
library(tidyverse)
library(grid)
library(ggrepel)
library(gganimate)

source("funcs.R", encoding= "utf8")

get_full_data_style <- function(mindeaths, group = "País") {
  list(theme_light(),
       geom_line(aes(group = location, colour = location), size = 1),
       geom_label_repel(aes(label = label), nudge_x = 1, na.rm = TRUE),
       labs(x = "Dia", y = "Número de Casos", colour = group),
       ggtitle(paste("Óbitos após o", mindeaths, "º óbito")),
       theme(plot.title = element_text(hjust = 0.5),
             plot.margin = margin(0.2, 0.5, 0.2, 0.5, "cm")),
       scale_x_continuous(breaks = 1:100, minor_breaks = 1:100))
}

# source <- "Fonte: 2019 Novel Coronavirus COVID-19 (2019-nCoV)\nData Repository by Johns Hopkins CSSE\nhttps://github.com/CSSEGISandData/COVID-19"

# conf_raw <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
# deaths_raw <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
# confirmed <- read_csv(conf_raw)
# countries <- confirmed$`Country/Region`
# unq_countries <- confirmed$`Country/Region`[is.na(confirmed$`Province/State`)]
# 
# br <- confirmed[confirmed$`Country/Region` == 'Brazil',] %>%
#   select(matches("\\d{1,2}/\\d{1,2}/\\d{2}")) %>%
#   pivot_longer(everything(), names_to = "Date", values_to = "Cases") %>%
#   mutate(Date = as.Date(Date, format = "%m/%d/%y"), New = Cases - lag(Cases)) %>%
#   tail(-29)

casos <- read_excel("data/Brasil.xlsx", sheet = "Confirmados") %>%
  mutate(Data = as.Date(Data)) %>% rename(date = Data) %>%
  pivot_longer(-date, names_to = "location", values_to = "total_cases")

write_excel_csv(read_excel("data/Brasil.xlsx", sheet = "Confirmados"), path = "data/estados_wide.csv")

mortes <- read_excel("data/Brasil.xlsx", sheet = "Mortes") %>%
  mutate(Data = as.Date(Data)) %>% rename(date = Data) %>%
  pivot_longer(-date, names_to = "location", values_to = "total_deaths")

estados <- full_join(casos, mortes) %>% group_by(location) %>%
  mutate(new_cases = calc_new(total_cases),
         new_deaths = calc_new(total_deaths)) %>%
  mutate_if(is.numeric, na_to_zero)

estados %>% filter(location == "São Paulo") %>% tail

#write_excel_csv(estados, path = "data/estados.csv")

brasil <- estados %>% group_by(date) %>%
  summarise_if(is.numeric, sum) %>% 
  mutate(location = "Brasil") %>%
  full_join(estados, .) %>% arrange(date, location)

##Brasil (log)

brasil_log_data <- brasil %>% filter(location == "Brasil", total_cases >= 100) %>% 
  mutate(total_deaths = log10(total_deaths)) %>% 
  mutate(total_deaths = ifelse(is.infinite(total_deaths), 0, total_deaths)) %>% 
  mutate(time = as.numeric(date - date[1]) + 1)

tot_range <- seq(min(brasil_log_data$total_deaths), max(brasil_log_data$total_deaths))  

brasil_log_plot <- ggplot(brasil_log_data, aes(x = time, y = total_deaths)) + #datastyle +
  geom_line(size = 1) +
  ggtitle(paste("Óbitos - Brasil")) 

fit0 <- lm(total_deaths ~ time, data = tail(brasil_log_data, 7))
intcpt <- fit0$coefficients[1]

slopes <- sapply(1:7, function(dbl_time) {
  growth <- growth_line_log(2, dbl_time, brasil_log_data$time)
  fit <- lm(y ~ x, data = growth)
  fit$coefficients[2]
})

ablines <- lapply(1:7, function(dbl_time) {
  growth <- growth_line_log(2, dbl_time, brasil_log_data$time)
  fit <- lm(y ~ x, data = growth)
  data.frame(a = intcpt, b = fit$coefficients[2], style = dbl_time)
}) %>% do.call(rbind, .)

# ablines <- lapply(seq_along(slopes), function(i) {
#   geom_abline(linetype = i, slope = slopes[i], intercept = intcpt)
# })

br_log_brks <- unlist(lapply(0:5, function(i) log10(c(1*10^i, 2*10^i, 5*10^i))))

brasil_log_plot + theme_light() + #datastyle + # ablines + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5)) +
  geom_abline(aes(intercept = a, slope = b, linetype = factor(style)), data = ablines) +
  scale_x_continuous(limits = c(0, nrow(brasil_log_data) + 2), breaks = seq_along(brasil_log_data$time), 
                     minor_breaks = NULL,
                     labels = format(brasil_log_data$date, format = "%d/%m")) +
  scale_y_continuous(limits = c(NA, 4), breaks = br_log_brks, labels = pot10,
                     minor_breaks = NULL) +
  labs(x = "Data", y = "Óbitos (log)", linetype = "Dias para\ndobrar")

next_day <- nrow(brasil_log_data) + 1
est_ref_interval <- 7
est_pred_interval <- 5
estimates <- data.frame(x = next_day:(next_day + est_pred_interval - 1),
  y = predict(lm(total_deaths ~ time, data = tail(brasil_log_data, est_ref_interval),
                 weights = sqrt(1:est_ref_interval)), 
              data.frame(time = next_day:(next_day + est_pred_interval - 1)),
              interval = 'conf')) %>%
  head(est_pred_interval)

brasil_log_plot + theme_light() + #datastyle + # ablines + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(0, nrow(brasil_log_data) + est_pred_interval),
                     breaks = seq(1, nrow(brasil_log_data) + est_pred_interval), 
                     minor_breaks = NULL,
                     labels = format(seq(brasil_log_data$date[1], by = "days", 
                                         length.out = nrow(brasil_log_data) + est_pred_interval),
                                     format = "%d/%m")) +
  scale_y_continuous(limits = c(NA, max(estimates$y.upr) * 1.1), breaks = br_log_brks, labels = pot10l,
                     minor_breaks = NULL) +
  labs(x = "Data", y = "Casos (log)") +
  ggtitle(paste("Previsão de casos de acordo com últimos", est_ref_interval, "dias")) +
  geom_smooth(data = tail(brasil_log_data, est_ref_interval), method = "lm",
              fullrange = TRUE, level = 0.95, formula = y ~ x, size = 0.5, linetype = "f4") +
  geom_text(aes(x = time, y = total_cases, label = round(pot10(total_cases))),
            hjust = 1.1, vjust = -0.1, size = 3) +
  geom_point(data = brasil_log_data, aes(x = time, y = total_cases)) +
  geom_text(data = estimates, aes(x = x, y = y.upr, label = round(pot10(y.upr))),
            hjust = 1.1, vjust = -1.5, size = 3) +
  geom_text(data = estimates, aes(x = x, y = y.lwr, label = round(pot10(y.lwr))),
            hjust = 1.1, vjust = 1.5, size = 3) +
  geom_text(data = estimates, aes(x = x, y = y.fit, label = round(pot10(y.fit))),
            hjust = 1.1, vjust = -0.1, size = 3) 

### Comparação inclinações vs isolamento

brasil_log_plot + theme_light() + #datastyle + # ablines + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(0, nrow(brasil_log_data) + est_pred_interval),
                     breaks = seq(1, nrow(brasil_log_data) + est_pred_interval), 
                     minor_breaks = NULL,
                     labels = format(seq(brasil_log_data$date[1], by = "days", 
                                         length.out = nrow(brasil_log_data) + est_pred_interval),
                                     format = "%d/%m")) +
  scale_y_continuous(limits = c(0, NA), breaks = br_log_brks, labels = pot10l,
                     minor_breaks = NULL) +
  coord_cartesian(ylim = c(0, 4)) +
  labs(x = "Data", y = "Óbitos (log)") +
  ggtitle("Comparação da taxa de crescimento em função das medidas de isolamento") +
  # geom_smooth(data = tail(brasil_log_data, nrow(brasil_log_data) - 8), method = "lm",
  #             fullrange = TRUE, level = 0.95, formula = y ~ x, size = 0.5, linetype = "f4") +
  geom_vline(xintercept = 2, linetype = "6a") + 
  geom_smooth(data = brasil_log_data[5:9,], method = "lm", se = FALSE,
              fullrange = TRUE, level = 0.95, formula = y ~ x, size = 0.5, linetype = "f4") +
  geom_vline(xintercept = 9, linetype = "6a") + 
  geom_smooth(data = brasil_log_data[10:16,], method = "lm", se = FALSE,
              fullrange = TRUE, level = 0.95, formula = y ~ x, size = 0.5, linetype = "f4") +
  geom_vline(xintercept = 16, linetype = "6a") + 
  geom_smooth(data = brasil_log_data[17:23,], method = "lm", se = FALSE,
              fullrange = TRUE, level = 0.95, formula = y ~ x, size = 0.5, linetype = "f4") +
  geom_vline(xintercept = 23, linetype = "6a") + 
  geom_smooth(data = brasil_log_data[24:30,], method = "lm", se = FALSE,
              fullrange = TRUE, level = 0.95, formula = y ~ x, size = 0.5, linetype = "f4") +
  geom_vline(xintercept = 30, linetype = "6a") + 
  geom_smooth(data = brasil_log_data[31:37,], method = "lm", se = FALSE,
              fullrange = TRUE, level = 0.95, formula = y ~ x, size = 0.5, linetype = "f4") +
  geom_vline(xintercept = 37, linetype = "6a") + 
  geom_smooth(data = brasil_log_data[38:44,], method = "lm", se = FALSE,
              fullrange = TRUE, level = 0.95, formula = y ~ x, size = 0.5, linetype = "f4") +
  geom_vline(xintercept = 44, linetype = "6a") + 
  # geom_smooth(data = tail(brasil_log_data, 7), method = "lm",
  #             fullrange = TRUE, level = 0.95, formula = y ~ x, size = 0.5, linetype = "f4") +
  geom_point(data = brasil_log_data, aes(x = time, y = total_deaths)) +
  annotate("text", x = 2, y = 3, label = "Início das medidas\nde isolamento (SP)",
           hjust = -0.05) +
  annotate("text", x = 9, y = 1, label = "Medidas de isolamento\n+7 dias (SP)",
           hjust = -0.05) +
  annotate("text", x = 16, y = 1, label = "+14 dias",
          hjust = -0.05) +
  annotate("text", x = 23, y = 1, label = "+21 dias",
           hjust = -0.05) +
  annotate("text", x = 30, y = 1, label = "+28 dias",
           hjust = -0.05) +
  annotate("text", x = 37, y = 1, label = "+35 dias",
           hjust = -0.05) +
  annotate("text", x = 44, y = 1, label = "+42 dias",
           hjust = -0.05)
  # geom_vline(xintercept = 10, linetype = "6a") +
  # annotate("text", x = 10, y = 2.3, label = "Pronunciamento Bolsonaro",
  #          hjust = -0.05) 
  # geom_vline(xintercept = 17, linetype = "6a") + 
  # annotate("text", x = 17, y = 2, label = "Pronunciamento Bolsonaro\n+7 dias",
  #          hjust = -0.05) 

dbl_time_ref <- 7
dbl_time_br <- brasil %>% filter(total_deaths >= 100) %>%
  group_by(location) %>% filter(n() > 10) %>%
  mutate(dbl_time = round(doubling_time_lm(total_deaths, dbl_time_ref), 2),
         dbl_time_lwr = round(doubling_time_lm(total_deaths, dbl_time_ref, TRUE)$lwr, 2),
         dbl_time_upr = round(doubling_time_lm(total_deaths, dbl_time_ref, TRUE)$upr, 2),
         day_deaths = 1:n()) %>%
  filter(dbl_time > 0) %>% select(-new_cases, -new_deaths, -total_cases, -total_deaths)

write_excel_csv(dbl_time_br, "data/dbl_time_br.csv")


dobling_time_lookup <- function(cases) {
  # Dobling time calculated by looking up the nearest day when # of cases was half of that day,
  # then calculating how far off the actual half that day was and using that as a factor
  # to adjust the actual amount of days it took to double
  halves <- cases/2
  ids <- sapply(cases, function(i) tail(which(abs(i/2 - cases) == min(abs(i/2 - cases))), 1))
  (halves/(cases - cases[ids])) * (seq_along(cases) - ids)
}

dbl_time_intervals <- doubling_time_lm(filter(brasil, location == "Brasil", total_deaths > 0)$total_deaths,
                                       dbl_time_ref, TRUE)

brasil_dbl_time <- brasil %>% filter(location == "Brasil", total_deaths > 0) %>%
  mutate(dbl_time = doubling_time_lm(total_deaths, dbl_time_ref),
         dbl_time_lwr = dbl_time_intervals$lwr,
         dbl_time_upr = dbl_time_intervals$upr) %>%
  filter(total_deaths > 100)


brasil_dbl_plot <- brasil_dbl_time %>% 
  ggplot(aes(x = date, y = dbl_time)) + theme_light() + datastyle +
  geom_ribbon(aes(ymin = dbl_time_lwr, ymax = dbl_time_upr), fill = "grey70", alpha = 0.5) +
  geom_line() + geom_point() + ggtitle("Tempo de duplicação") +
  scale_y_continuous(breaks = 1:15, labels = 1:15) +
  labs(x = "Data", y = "Tempo de duplicação (dias)")
  
brasil_dbl_plot

nma <- 7
brasil_ma <- brasil %>% filter(location == "Brasil") %>% 
  mutate(new_deaths_ma = round(ma(new_deaths, nma)))

brasil_ma_plot <- ggplot(brasil_ma, aes(x = date, y = new_deaths_ma)) + theme_light() +
  geom_bar(stat = "identity", aes(x = date, y = new_deaths)) +
  geom_step(size = 1, direction = "mid" ) +
  scale_x_date(date_breaks = "2 days", date_minor_breaks = "1 day",
             date_labels = "%d/%m", limits = c(brasil_ma$date[which(!is.na(brasil_ma$new_deaths_ma))[1]], NA)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(0.2, 0.5, 0.2, 0.5, "cm")) +
  labs(x = "Data", y = "Média móvel de novos óbitos") +
  ggtitle(paste("Novos Óbitos - Brasil (média móvel,", nma, "dias)")) + 
  annotate("text", x = brasil_ma$date[1], y = max(brasil_ma$new_deaths_ma, na.rm = TRUE), 
           label = "Fonte: Ministério da Saúde", hjust = 0, vjust = 0) 
  # annotate("text", x = max(brasil_ma$date), y = brasil_ma$new_deaths_ma[nrow(brasil_ma) - (nma - 1)/2], 
  #          label = brasil_ma$new_deaths_ma[nrow(brasil_ma) - (nma - 1)/2], hjust = 1.1)

brasil_ma_plot

ggsave(paste0("data/Brasil - Média Móvel Óbitos.png"), plot = brasil_ma_plot,
       device = png(),
       width = 20,
       height = 10,
       units = "cm",
       dpi = 100)
dev.off()


# Gráficos por estado

ufs <- unique(brasil$location)

for (uf in ufs) {
  ts <- brasil %>% filter(location == uf)
  if (nrow(ts) <= 1) next
  tot_range <- seq(min(ts$total_cases), max(ts$total_cases))  
  p1 <- ggplot(ts, aes(x = date, y = total_cases)) + datastyle +
    scale_y_continuous(breaks = pretty(tot_range), 
                       limits = c(min(tot_range), max(tot_range) * 1.05)) + 
    ggtitle(paste("Casos Confirmados -", uf)) + 
    annotate("text", x = ts$date[1], y = max(ts$total_cases) * 1.01, 
             label = "Fonte: Ministério da Saúde", hjust = 0, vjust = 0) +
    annotate("text", x = max(ts$date), y = max(ts$total_cases), 
             label = max(ts$total_cases), hjust = 1.1)
  
  p1
  tot_range_new <- seq(min(ts$new_deaths), max(ts$new_deaths))  
  p2 <- ggplot(ts, aes(x = date, y = new_deaths)) + datastyle +
    scale_y_continuous(breaks = pretty(tot_range_new), 
                       limits = c(min(tot_range_new), max(tot_range_new) * 1.05)) + 
    ggtitle(paste("Novos Óbitos -", uf)) + 
    annotate("text", x = ts$date[1], y = max(ts$new_deaths) * 1.01, 
             label = "Fonte: Ministério da Saúde", hjust = 0, vjust = 0) +
    annotate("text", x = max(ts$date), y = last(ts$new_deaths), 
             label = last(ts$new_deaths), hjust = 1.1)
  p2
  
  ggsave(paste0("data/", uf, "-Total.png"), plot = p1,
         device = png(),
         width = 20,
         height = 10,
         units = "cm",
         dpi = 100)
  dev.off()
  ggsave(paste0("data/", uf, "-Novos.png"), plot = p2,
         device = png(),
         width = 20,
         height = 10,
         units = "cm",
         dpi = 100)
  dev.off()
}

distplot <- estados %>% group_by(location) %>% filter(row_number() == n()) %>%
  ggplot(aes(x = reorder(location, -total_deaths), y = total_deaths)) + geom_bar(stat = "identity") +
  geom_text(aes(label = total_deaths), position = position_dodge(width = 0.9), 
            vjust = -0.2, size = 3) +
  theme_light() + labs(x = "UF", y = "Óbitos") + ggtitle("Distribuição dos óbitos") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(0.2, 0.5, 0.2, 0.5, "cm")) +
  annotate("text", x = 27, y = max(estados$total_deaths), 
           label = "Fonte: Ministério da Saúde", hjust = 1, vjust = 1) 

distplot

ggsave(paste0("data/Distribuição.png"), plot = distplot,
       device = png(),
       width = 20,
       height = 10,
       units = "cm",
       dpi = 100)
dev.off()

pop <- read_excel("data/Brasil.xlsx", sheet = "Populacao")

pop_vec <- pop$População %>% setNames(pop$UF)
pop_vec["Brasil"] <- sum(pop_vec)
pop_vec <- pop_vec[order(names(pop_vec))]

obitos_confirmados <- brasil %>% group_by(location) %>% filter(row_number() == n()) %>%
  arrange(location) %>% `[[`("total_deaths")

obitos_confirmados <- tibble(Estado = names(pop_vec),
                        Obitos = obitos_confirmados / (pop_vec / 1000000))

casos_por_hab_br <- obitos_confirmados$Obitos[obitos_confirmados$Estado == "Brasil"]

cph_plot <- obitos_confirmados %>% filter(Estado != "Brasil") %>%
  ggplot(aes(x = reorder(Estado, -Obitos), y = Obitos)) + geom_bar(stat = "identity") +
  geom_text(aes(label = round(Obitos, 2)), position = position_dodge(width = 0.9),
            vjust = -0.2, size = 3) +
  theme_light() + labs(x = "UF", y = "Óbitos/(habitantes/1,000,000)") + ggtitle("Óbitos por 1 milhão de habitantes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(0.2, 0.5, 0.2, 0.5, "cm")) +
  annotate("text", x = 25, y = max(obitos_confirmados$Obitos),
           label = "Fontes: Ministério da Saúde\nIBGE", hjust = 1, vjust = 1) +
  geom_hline(yintercept = casos_por_hab_br, linetype = "dashed") +
  annotate("text", x = 27, y = casos_por_hab_br,
           label = paste("Brasil =", round(casos_por_hab_br, 2)),
           hjust = 1, vjust = -0.25)

cph_plot

ggsave(paste0("data/Óbitos por Habitantes.png"), plot = cph_plot,
       device = png(),
       width = 20,
       height = 10,
       units = "cm",
       dpi = 100)
dev.off()

##### Comparações

mindeaths <- 100
full_data_style <- get_full_data_style(mindeaths, "Estado")

estado_comp <- estados %>%
  filter(total_deaths >= mindeaths) %>% group_by(location) %>%
  filter(n() > 1) %>% mutate(day = 1:n()) %>%
  mutate(label = if_else(day == max(day), location, NA_character_),
         total_deaths = log10(total_deaths))

estado_log_brks <- log10(c(100, seq(250, 1000, 250), seq(1500, 2500, 500)))
estado_comp_plot <- ggplot(estado_comp, aes(day, total_deaths)) + 
  full_data_style +
  labs(y = "Número de óbitos (log10)") +
  scale_y_continuous(breaks = estado_log_brks, minor_breaks = NULL, labels = pot10) +
  annotate("text", x = 1, y = max(estado_comp$total_deaths), 
         label = "Fonte: Ministério da Saúde ",
         hjust = 0, vjust = 0.5)

estado_comp_plot

ggsave(paste0("data/Comparação Estados.png"), plot = estado_comp_plot,
       device = png(),
       width = 20,
       height = 10,
       units = "cm",
       dpi = 100)
dev.off()

#########

# Our World in Data
# https://ourworldindata.org/coronavirus-source-data

full_data <- read_csv("https://covid.ourworldindata.org/data/ecdc/full_data.csv") %>% 
  mutate(date = as.Date(date))

dbl_time <- full_data %>% filter(total_deaths >= 100) %>%
  group_by(location) %>% filter(n() > 10) %>%
  mutate(dbl_time = round(doubling_time_lm(total_deaths, dbl_time_ref), 2),
         dbl_time_lwr = round(doubling_time_lm(total_deaths, dbl_time_ref, TRUE)$lwr, 2),
         dbl_time_upr = round(doubling_time_lm(total_deaths, dbl_time_ref, TRUE)$upr, 2),
         day_deaths = 1:n()) %>%
  filter(dbl_time > 0) %>% select(-new_cases, -new_deaths, -total_cases, -total_deaths)

write_excel_csv(dbl_time, "data/dbl_time.csv")

source("fetch-ms-data.R", encoding = "utf8")

full_data %>% filter(location == "Brazil") %>% tail

full_data <- full_join(full_data, filter(brasil, location == "Brasil"))

compare <- c("Brasil", "Italy", "United States", "Spain", "France",
             "South Korea", "Germany", "United Kingdom")



maxday <- 25
mindeaths <- 100
full_data_style <- get_full_data_style(mindeaths)

lin_data <- full_data %>% filter(location %in% compare) %>%
  filter(total_deaths >= mindeaths) %>% group_by(location) %>%
  mutate(day = 1:n()) %>% filter(day <= maxday) %>%
  mutate(label = if_else(day == max(day), location, NA_character_),
         wd = if_else(location == "Brasil", "A", "B"))

lin_plot <- ggplot(lin_data, aes(day, total_deaths)) + 
  full_data_style +
  annotate("text", x = 1, y = max(lin_data$total_deaths), 
           label = "Fontes: https://ourworldindata.org/coronavirus\nMinistério da Saúde do",
           hjust = 0, vjust = 0.5)

lin_plot

ggsave(paste0("data/Comparação - Linear.png"), plot = lin_plot,
       device = png(),
       width = 20,
       height = 10,
       units = "cm",
       dpi = 100)
dev.off()

log_data <- lin_data %>%
  mutate(total_deaths = log10(total_deaths)) 

world_log_brks <- log10(c(seq(2.5e2, 1e3, 2.5e2), seq(2.5e3, 1e4, 2.5e3), seq(2.5e4, 1e5, 2.5e4)))

log_plot <- ggplot(log_data, aes(day, total_deaths)) + 
  full_data_style +
  labs(y = "Número de Casos (log10)") +
  scale_y_continuous(breaks = world_log_brks, minor_breaks = NULL, labels = pot10) +
  annotate("text", x = 1, y = max(log_data$total_deaths), 
           label = "Fontes: https://ourworldindata.org/coronavirus\nMinistério da Saúde",
           hjust = 0, vjust = 0.5)
  
  
log_plot

ggsave(paste0("data/Comparação - Log.png"), plot = log_plot,
       device = png(),
       width = 20,
       height = 10,
       units = "cm",
       dpi = 100)
dev.off()

comp_data <- full_data %>% filter(location %in% compare) %>%
  filter(total_deaths >= mindeaths ) %>% group_by(location) %>%
  mutate(day = 1:n()) %>% 
  mutate(label = if_else(day == max(day), location, NA_character_)) %>%
  mutate(total_deaths = log10(total_deaths)) 

comp_plot <- ggplot(comp_data, aes(day, total_deaths)) + full_data_style +
  labs(y = "Número de Casos (log10)") +
  scale_y_continuous(breaks = world_log_brks, minor_breaks = NULL, labels = pot10) +
  scale_x_continuous(breaks = seq(0, max(comp_data$day) + 1, by = 2),
                     minor_breaks = seq(0, max(comp_data$day) + 1, by = 1),
                     expand = c(0, 1)) +
  annotate("text", x = 2, y = max(comp_data$total_deaths), 
           label = "Fontes: https://ourworldindata.org/coronavirus\nMinistério da Saúde",
           hjust = 0, vjust = 0.5)

comp_plot

ggsave(paste0("data/Comparação - Log Full.png"), plot = comp_plot,
       device = png(),
       width = 20,
       height = 10,
       units = "cm",
       dpi = 100)
dev.off()


### Óbitos

death_dist_plot <- estados %>% group_by(location) %>% filter(row_number() == n()) %>%
  ggplot(aes(x = reorder(location, -total_deaths), y = total_deaths)) + geom_bar(stat = "identity") +
  geom_text(aes(label = total_deaths), position = position_dodge(width = 0.9), 
            vjust = -0.2, size = 3) +
  theme_light() + labs(x = "UF", y = "Óbitos confirmados") + ggtitle("Distribuição dos óbitos") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(0.2, 0.5, 0.2, 0.5, "cm")) +
  annotate("text", x = 27, y = max(estados$total_deaths), 
           label = "Fonte: Ministério da Saúde", hjust = 1, vjust = 1)

death_dist_plot

death_time_data <- brasil %>% filter(location == "Brasil" & total_deaths > 0)

death_time_plot <- death_time_data %>%
  ggplot(aes(x = date, y = total_deaths)) + datastyle +
  ggtitle("Óbitos - Brasil") + 
  labs(y = "Óbitos") +
  annotate("text", x = death_time_data$date[1], y = max(death_time_data$total_deaths) * 1.01, 
           label = "Fonte: Ministério da Saúde", hjust = 0, vjust = 0) +
  annotate("text", x = max(death_time_data$date), y = last(death_time_data$total_deaths), 
           label = max(death_time_data$total_deaths), hjust = 1.1)

death_time_plot

ggsave(paste0("data/Brasil - Óbitos.png"), plot = death_time_plot,
       device = png(),
       width = 20,
       height = 10,
       units = "cm",
       dpi = 100)
dev.off()

avg_death <- full_data %>% filter(total_deaths >= 20) %>%
  mutate(death_ratio = total_deaths / total_cases) %>%
  group_by(location) %>% slice(n()) %>%
  `$`("death_ratio") %>% mean %>% round(4)

compare2 <- c("Brasil", "Italy", "United States", "Spain", "France",
              "South Korea", "Germany", "United Kingdom", "China")

death_comp <- full_data %>%
  filter(location %in% compare2) %>%
  mutate(death_ratio = total_deaths / total_cases) %>%
  group_by(location) %>% slice(n())

death_comp_plot <- death_comp %>%
  ggplot(aes(x = reorder(location, -death_ratio), y = death_ratio)) + geom_bar(stat = "identity") +
  geom_hline(yintercept = avg_death, linetype = "dashed") +
  geom_text(aes(label = paste0(round(death_ratio * 100, 2), "%")) , position = position_dodge(width = 0.9), 
            vjust = -0.2, size = 3) +
  scale_y_continuous(breaks = seq(0, 0.2, 0.025), labels = paste0(seq(0, 20, 2.5), "%")) +
  theme_light() + labs(x = "País", y = "Letalidade (CFR)") + ggtitle("Letalidade") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(0.2, 0.5, 0.2, 0.5, "cm")) +
  annotate("text", x = nrow(death_comp) + 0.5, y = avg_death,
           label = paste("Média (n >= 20):", avg_death * 100, "%"), 
           hjust = 1, vjust = -0.25) +
  annotate("text", x = nrow(death_comp) + 0.5, y = max(death_comp$death_ratio), 
           label = "Fontes: https://ourworldindata.org/coronavirus\nMinistério da Saúde", 
           hjust = 1, vjust = 1) 

death_comp_plot 

ggsave(paste0("data/Letalidade.png"), plot = death_comp_plot,
       device = png(),
       width = 20,
       height = 10,
       units = "cm",
       dpi = 100)
dev.off()

today <- format(Sys.Date(), format = "%d de %B")
rng_dat <- 10 * seq(0, 1, length.out = 11) ^ 10
png("data/Instagram.png", 
    width = 20,
    height = 10,
    units = "cm",
    res = 100)
par(mar = c(3, 4, 3, 4), xpd = TRUE)
plot(0:10, rng_dat, xaxt = "n", yaxt = "n", frame.plot = TRUE,
     type = "b", xlab = NA, ylab = NA, 
     xlim = c(0, 10), ylim = c(0, 10))
text(x = 5, y = 5, labels = paste("Gráficos do dia", today),
     cex = 2.8)
rect(-1, -1, 11, 11, lwd = 3)
dev.off()

#####

#Bar Chart Race

estados_hora <- estados %>% mutate(date = as.POSIXct(date)) %>% group_by(date, location) %>% 
  do({
    dia <- .
    ref_row <- dia %>% slice(n())
    hours_rows <- tibble(date = seq(ref_row$date, by = "hour", length.out = 24),
                         location = ref_row$location,
                         total_cases = round(seq(from = ref_row$total_cases - ref_row$new_cases,
                                           to = ref_row$total_cases,
                                           length.out = 24), 0))
    estados <- hours_rows
  })
  
estados_hora %>% filter(location == "Distrito Federal") %>% tail(50)
  

p <- estados_hora %>% group_by(date) %>%
  arrange(total_cases) %>% mutate(rank = 1:n()) %>%
  ggplot(aes(x = rank, y = total_cases, group = location)) +
  geom_tile(aes(y = total_cases / 2, height = total_cases, fill = location), width = 0.9) +
  geom_text(aes(label = location, y = -10), hjust = "right", colour = "black", fontface = "bold") +
  geom_text(aes(label = scales::comma(total_cases)), hjust = "left", nudge_y = 10, colour = "grey30") +
  coord_flip(clip = "off") +
  #scale_x_discrete("") +
  scale_y_continuous("",labels = scales::comma, 
                     expand = c(0, 0)) +
                     #limits = c(0, max(estados_hora$total_cases) * 1.05)) +
  #hrbrthemes::theme_ipsum(plot_title_size = 32, subtitle_size = 24, caption_size = 20, base_size = 20) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none",
        plot.margin = margin(1,1,1,3.5,"cm"),
        axis.text.y = element_blank()) +
  # gganimate code to transition by year:
  transition_time(date) +
  labs(x = "",
       title = "Casos confirmados por UF",
       subtitle = "Casos confirmados em {format(frame_time, format = '%d/%m/%Y')}",
       caption = "Fonte: Ministério da Saúde") +
  scale_fill_manual(name = 'UF', values = rainbow(27)) +
  #ease_aes('cubic-in-out') +
  ease_aes('linear') 

anim <- animate(p, duration = 30, fps = 25, end_pause = 100, 
                width = 787, height = 500)
anim_save("data/EstadosAnim.gif")

cfrdeaths <- full_data %>% group_by(location) %>% filter(total_deaths >= 100, row_number() == n(),
                                            !location %in% c("World", "Brasil")) %>%
  mutate(cfr = total_deaths/total_cases, total_deaths_log = log10(total_deaths))

cfrdeaths %>%
  ggplot(aes(x = total_deaths_log, y = cfr)) + geom_point() + 
  geom_text(aes(label = location), vjust = 1.1) +
  scale_y_continuous(breaks = seq(0, 1, 0.025), labels = function(x) paste0(100 * x, "%"),
                     minor_breaks = NULL) +
  scale_x_continuous(breaks = world_log_brks, labels = function(x) 10^x,
                     minor_breaks = NULL, limits = c(3, NA)) +
  labs(x = "Total Deaths", y = "Case Fatality Rate", title = "CFR vs Total Deaths") + 
  geom_smooth(method = "lm")

cor(cfrdeaths$total_deaths_log, cfrdeaths$cfr)
