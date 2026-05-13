# This'll be the starting analysis
library(tidyverse)

day <- seq(1, 365, 1)

#Create normal distribution of pm2.5 values
pm25 <- rnorm(365, mean = 20, sd = 15) #we can change the mean and sd if we want
pm25 <- abs(pm25) #convert negative values to positive

#Not sure if we need this so I left it out but this was a good idea
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

#Create counter variables for unhealthy streak
unhealthy_streak = 0
max_unhealthy_streak = 0

#Find risk
for (i in 1:nrow(Air_Quality)){
  Air_Quality$risk[i] <- ifelse(Air_Quality$pm25[i] <= 12, "good",
         ifelse(Air_Quality$pm25[i] <= 35.4, "moderate",
               "unhealthy"))
  
  #Add 1 to the current streak if air quality is unhealthy, otherwise reset to 0
  unhealthy_streak <- ifelse(Air_Quality$risk[i] == "unhealthy",
                             unhealthy_streak + 1,
                             0)
  
  #Reset max streak if the current streak is longer
  max_unhealthy_streak <- ifelse(unhealthy_streak > max_unhealthy_streak,
                                 unhealthy_streak,
                                 max_unhealthy_streak) #sorry this is so cursed
}
#maximum unhealthy streak
max_unhealthy_streak




