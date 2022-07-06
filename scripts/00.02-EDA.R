## ---------------------------
## Script name: 
##
## Purpose of script:
##
## Author: Dr. Meridith L. Bartley
## Date Created: 2022-07-05
## ---------------------------
## Notes:
##   
##
## ---------------------------


# load packages -----------------------------------------------------------
library(dplyr) #lots of great data manipulation functions
library(ggplot2) #needed for plots
library(magrittr) #allows for use of %>% pipe operator 


# plot Strava timeseries --------------------------------------------------

#daily aggregated
strava_day_TSplot_trips <- strava_day %>% 
  dplyr::group_by(timeframe, trailname) %>% 
  dplyr::summarise(total_count = sum(totaltrips)) %>%
  ggplot(aes(x = timeframe, y = total_count)) +
  geom_area(aes(fill = factor(trailname)), alpha = .5, position = "stack") +
  facet_wrap(~trailname, ncol = 3) +
  guides(fill = 'none') +
  ylim(0, 1000) +
  # scale_x_date(labels = date_format("%b %y"),
  #              name = "Month",
  #              date_breaks = "Daily") +
  labs(y = "Count ",
       title = "Strava Daily Trail Traffic",
       subtitle = "2021",
       fill = "Trail Name")

#monthly aggregated
strava_month_TSplot_trips <- strava_month %>% 
  dplyr::group_by(timeframe, trailname) %>% 
  dplyr::summarise(total_count = sum(totaltrips)) %>%
  ggplot(aes(x = timeframe, y = total_count)) +
  geom_area(aes(fill = factor(trailname)), alpha = .5, position = "stack") +
  facet_wrap(~trailname, ncol = 3) +
  guides(fill = 'none') +
  # ylim(0, 1000) +
  # scale_x_date(labels = date_format("%b %y"),
  #              name = "Month",
  #              date_breaks = "Daily") +
  labs(y = "Count ",
       title = "Strava Monthly Trail Traffic",
       subtitle = "2021",
       fill = "Trail Name")



# plot Counter timeseries -------------------------------------------------

#daily aggregated
count_day_TSplot <- trail_count %>% 
  dplyr::group_by(date, trailname) %>% 
  dplyr::summarise(total_count = sum(count)) %>%
  ggplot(aes(x = date, y = total_count)) +
  geom_area(aes(fill = factor(trailname)), alpha = .5, position = "stack") +
  facet_wrap(~trailname, ncol = 3) +
  guides(fill = 'none') +
  # ylim(0, 1000) +
  # scale_x_date(labels = date_format("%b %y"),
  #              name = "Month",
  #              date_breaks = "Daily") +
  labs(y = "Count ",
       title = "Counter Daily Trail Traffic",
       subtitle = "2021",
       fill = "Trail Name")


# plot Strava/Counter overlap  --------------------------------------------


# plot Counter by hour/day of week ----------------------------------------

trail_count %>% 
  group_by(hour, wday, crossingWrap) %>%
  summarise(total_count = sum(count, na.rm = TRUE)) %>%
  group_by(crossingWrap) %>%
  mutate(pct_bike = total_count/sum(total_count)) %>%
  ggplot(aes(x = hour, y = pct_bike, color = crossingWrap, fill = crossingWrap)) +
  geom_line() +
  geom_area(alpha = 0.25) +
  facet_grid(wday ~ crossingWrap) +
  scale_x_continuous(name = "Hour of Day", breaks = c(0, 6, 12, 18)) +
  scale_y_continuous(name = "Percentage of Bike Crossings", labels = scales::percent_format(accuracy=0.1)) + 
  scale_fill_manual(values=seattlePalette) +
  scale_color_manual(values=seattlePalette) +
  guides(color=FALSE, fill = FALSE) + 
  labs(title = "Seattle Bike Traffic Patterns by Hour and Weekday",
       subtitle = "Data: 2014-2019")

# plot Counters against covariates ------------------------------------------

## weather - all trails combined

trail_count_weather <- trail_count %>%
  left_join(weather, by = c("date" = "date")) %>% 
  group_by(date) %>%
  summarise(count = sum(count), 
            AQI = mean(daily_aqi_value), 
            PRCP = sum(precipitation_in),
            TEMP_MAX = mean(temp_max_f),
            TEMP_MIN = mean(temp_min_f))

trail_count_weather$wet = "No"
trail_count_weather <- trail_count_weather[!is.na(trail_count_weather$PRCP),]
trail_count_weather[trail_count_weather$PRCP >= 0.2, "wet"] = "Yes"

trail_count_weather %>%
  group_by(date, wet) %>%
  ggplot(aes(x = TEMP_MAX, y = count, color = wet)) +
  # scale_color_manual(values=seattlePalette[c(6, 2)]) +
  scale_x_continuous(name = "Average Max Temperature (Degrees Fahrenheit)") +
  scale_y_continuous(name = "Count") + 
  geom_jitter(alpha = 0.5) + 
  labs(title = "Comparison of Daily Bridger Trail Traffic to Weather",
       color = "Daily Precipitation \n over 0.2 Inches") 

trail_count_weather %>%
  group_by(date, wet) %>%
  ggplot(aes(x = AQI, y = count, color = wet)) +
  # scale_color_manual(values=seattlePalette[c(6, 2)]) +
  scale_x_continuous(name = "Average Air Quality Index (AQI)") +
  scale_y_continuous(name = "Count") + 
  geom_jitter(alpha = 0.5) + 
  labs(title = "Comparison of Daily Bridger Trail Traffic to Air Quality",
       color = "Daily Precipitation \n over 0.2 Inches") 
