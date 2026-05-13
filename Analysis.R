# This'll be the starting analysis
library(tidyverse)

day <- seq(1, 365, 1)

pm25 <- rnorm(365, mean = 20, sd = 10) #create normal distribution of pm2.5 values
pm25 <- abs(pm25) #convert negative values to positive

seasonal_mean <- ifelse(day <= 90, 35,        # winter: elevated
                        ifelse(day <= 180, 12,        # spring: cleaner
                               ifelse(day <= 270, 5,         # summer: cleanest
                                      15)))       # fall: rising again

season <- ifelse(day <= 90,  "winter",
                 ifelse(day <= 180, "spring",
                        ifelse(day <= 270, "summer",
                               "fall")))
season <- factor(season, levels = c("winter", "spring", "summer", "fall"))

Air_Quality <- data.frame(
  day = day,
  season = season,
  pm25 = pm25,
  risk = NA
)

#Find risk
for (i in 1:nrow(Air_Quality)){
  Air_Quality$risk[i] <- ifelse(Air_Quality$pm25[i] <= 12, "good",
         ifelse(Air_Quality$pm25[i] <= 35.4, "moderate",
               "unhealthy"))
}



