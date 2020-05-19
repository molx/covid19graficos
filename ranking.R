
x <- 1:10
y <- jitter(2^x, 10)

plot(x, y, type = "b")

df <- data.frame(x = x, 
                 y = log10(y))
summary(lm(y ~ x, df))

days <- 15
nms <- unique(estados$location)
rsqs <- sapply(setNames(nms, nms), function (loc) {
  sp <- estados %>% filter(location == loc) %>% tail(days) %>%
    select(date, new_cases)
  
  # sp
  # 
  # plot(sp$date, sp$new_cases, type = "h")
  # 
  
  fit <- lm(new_cases ~ date, sp)
  summary(fit)$r.squared
})

sort(rsqs, decreasing = TRUE)
