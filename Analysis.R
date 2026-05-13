# This'll be the starting analysis

day <- 1:365

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
  seasonal_mean = seasonal_mean,
  risk = NA
)