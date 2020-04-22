datastyle <- list(theme_light(), geom_line(size = 1), geom_point(),
                  scale_x_date(date_breaks = "2 days", date_minor_breaks = "1 day",
                               date_labels = "%d/%m"),
                  theme(axis.text.x = element_text(angle = 45, hjust = 1),
                        plot.title = element_text(hjust = 0.5),
                        plot.margin = margin(0.2, 0.5, 0.2, 0.5, "cm")),
                  labs(x = "Data", y = "Número de Casos"))




################################
# Ministério da saúde
# http://plataforma.saude.gov.br/novocoronavirus/
# https://covid.saude.gov.br/

#Calculating new cases by the difference between day and day - 1
calc_new <- function(x) {
  new <- x - lag(x)
  new[is.na(new)] <- 0
  new
}

ma <- function(x, n = 7) {
  stats::filter(x, rep(1 / n, n), sides = 1)
}

ma2 <- function(x, n = 5) {
  d <- (n - 1)/2
  sapply(seq_along(x), function(i) {
    mean(x[max(1, i - d):min(i + d, length(x))])
  })
} 

na_to_zero <- function(x) {
  ifelse(is.na(x), 0, x)
}

# Function to calculate theoretical exponetial growth
# factor = the amount which grows
# time = the time it takes to grow by factor
# x = the time period to simulate
# e.g.: factor = 2, time = 3, means doubles every 3 units of time
growth_line <- function(factor, time, duration) {
  x <- duration
  y <- factor ^ (x / time)
  data.frame(x = x, y = y)
}

# Calculate 10 power to use in log scale labels
pot10 <- function(x) {10 ^ x}

# Use pot10 to create strings formatted for the axis label
pot10l <- function(x) {format(pot10(x), scientific = FALSE, big.mark = ",")}

growth_line_log <- function(factor, time, duration) {
  x <- duration
  y <- duration * log10(factor) / time
  data.frame(x = x, y = y)
}


growth_rate <- function(x) {
  rate <- (x - lag(x)) / lag(x)
  rate[is.na(rate)] <- 0
  rate
}

doubling_time <- function(x) {
  log(2) / log(1 + growth_rate(x))
}

doubling_time_lm <- function(x, days = 5, intervals = FALSE) {
  d <- sapply((days):length(x), function(i) {
    start <- i - days + 1
    df <- data.frame(y = log10(x[start:i]), x = 1:days)
    fit <- lm(y ~ x, data = df, weights = sqrt(seq_along(x)))
    intervals <- log10(2)/confint(fit, 2)
    c(intervals[2], log10(2)/fit$coef[2], intervals[1])
  })
  if (intervals) {
    out <- as.data.frame(rbind(matrix(rep(0, 3 * (days - 1)), ncol = 3), t(d)))
    colnames(out) <- c("lwr", "dbl", "upr")
  } else {
    out <- c(rep(0, days - 1), d[2,])
  }
  out
}
