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

# Seasonal unhealthy with 'for' loop

seasons <- levels(Air_Quality$season)
unhealthy_by_season <- setNames(integer(length(seasons)), seasons)

for (s in seasons) {
  season_subset <- Air_Quality[Air_Quality$season == s, ]
  unhealthy_by_season[s] <- sum(season_subset$risk == "unhealthy")
}

unhealthy_by_season

# Plot 1: PM2.5 time series colored by season
plot1_title <- sprintf(
  "Daily PM2.5 Concentrations Over One Year (Max Unhealthy Streak: %d days)",
  max_unhealthy_streak
)

ggplot(Air_Quality, aes(x = day, y = pm25, color = season)) +
  geom_line(linewidth = 0.6, alpha = 0.8) +
  geom_hline(yintercept = 35.4, linetype = "dashed",
             color = "red", linewidth = 0.7) +
  geom_hline(yintercept = 12, linetype = "dashed",
             color = "yellow", linewidth = 0.7) +
  annotate("text", x = 355, y = 37.2, label = "Unhealthy (35.4)",
           color = "red", size = 3, hjust = 1) +
  annotate("text", x = 355, y = 13.5, label = "Moderate (12.0)",
           color = "yellow", size = 3, hjust = 1) +
  scale_color_manual(values = c(
    winter = "#5b9bd5",
    spring = "#70ad47",
    summer = "red",
    fall   = "brown"
  )) +
  labs(title = plot1_title, x = "Day of Year",
       y = "PM2.5 (µg/m³)", color = "Season") +
  theme_minimal(base_size = 12)


# Plot 2: Bar chart of days per risk category
plot2_title <- sprintf(
  "Days per Air Quality Risk Category (Max Unhealthy Streak: %d days)",
  max_unhealthy_streak
)

Air_Quality$risk <- factor(Air_Quality$risk,
                           levels = c("good", "moderate", "unhealthy"))

ggplot(Air_Quality, aes(x = risk, fill = risk)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)),
            vjust = -0.5, size = 4.5, fontface = "bold") +
  scale_fill_manual(values = c(
    good      = "#4dac26",
    moderate  = "#f9c84a",
    unhealthy = "#d01c1f"
  )) +
  labs(title = plot2_title, x = "Risk Category", y = "Number of Days") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")




