## ---------------------------
## Script name: 00.02-EDA.R
##
## Purpose of script: Exploratory data analysis of provided data. 
##        Looking for missing data. Checking understanding of all data and
##        all column names. Lots of plots!
##
## Author: Dr. Meridith L. Bartley
## Date Created: 2022-07-05
## ---------------------------
## Notes:
##   - when plotting Strava counts, need to use max(count) of multiple segments
##     instead of sum(count) because presumably one segment is the trail head and 
##      each subsequent segment count is included in previous ones. 
## ---------------------------

source("scripts/01-LoadData.R") #loads and cleans data

# load packages -----------------------------------------------------------
library(devtools) #useful for installing packages from github
library(dplyr) #lots of great data manipulation functions
library(ggplot2) #needed for plots
library(ggspatial) #spatial plots
library(gridExtra) #arrange multiple plots in single panel
library(magrittr) #allows for use of %>% pipe operator 
library(scales)   # to access breaks/formatting functions
library(tabplot) # super cool table visualization

## needed to install tabplot from Github (workaround)
# devtools::install_github("edwindj/ffbase", subdir="pkg")
# devtools::install_github("mtennekes/tabplot")


# plot tabplot for each datasource ----------------------------------------

tabplot::tableplot(strava_day[, -1]) #doesn't work for date so removing that column
tabplot::tableplot(strava_month[, -1])
tabplot::tableplot(strava_year[, -1])

tabplot::tableplot(trail_count[, -3])
tabplot::tableplot(trail_char[, 1:10])
tabplot::tableplot(trail_char[, 11:20])

tabplot::tableplot(weather[, -1])

# count histograms --------------------------------------------------------

#strava daily aggregated data
hist(strava_day$totaltrips)
hist(strava_day$totalpeople)

#strava monthly
hist(strava_month$totaltrips)
hist(strava_month$totalpeople)

#strava yearly
hist(strava_year$totaltrips)
hist(strava_year$totalpeople)

#counter data
hist(trail_count$count)


# covariate plots ---------------------------------------------------------

MaxTempDaily <- ggplot(weather, aes(date, temp_max_f)) +
  geom_point() +
  ggtitle("Daily Max Air Temperature\n Bridger Mountains \n 2021") +
  xlab("Date") + ylab("Temperature (F)") +
  ylim(-25, 100) +
  scale_x_date(labels=scales::date_format ("%m-%y"))+
  theme(plot.title = element_text(lineheight=.8, face="bold",
                                  size = 20)) +
  theme(text = element_text(size=18))

MinTempDaily <- ggplot(weather, aes(date, temp_min_f)) +
  geom_point() +
  ggtitle("Daily Min Air Temperature\n Bridger Mountains \n 2021") +
  xlab("Date") + ylab("Temperature (F)") +
  ylim(-25, 100) +
  scale_x_date(labels=scales::date_format ("%m-%y"))+
  theme(plot.title = element_text(lineheight=.8, face="bold",
                                  size = 20)) +
  theme(text = element_text(size=18))

AQIDaily <- ggplot(weather, aes(date, daily_aqi_value)) +
  geom_point() +
  ggtitle("Daily Air Quality \n Bridger Mountains \n 2021") +
  xlab("Date") + ylab("Air Quality Index") +
  scale_x_date(labels=scales::date_format ("%m-%y"))+
  theme(plot.title = element_text(lineheight=.8, face="bold",
                                  size = 20)) +
  theme(text = element_text(size=18))

PrecipDaily <- ggplot(weather, aes(date, precipitation_in)) +
  geom_point() +
  ggtitle("Daily Precipitation \n Bridger Mountains \n 2021") +
  xlab("Date") + ylab("Precipitation (in)") +
  scale_x_date(labels=scales::date_format ("%m-%y"))+
  theme(plot.title = element_text(lineheight=.8, face="bold",
                                  size = 20)) +
  theme(text = element_text(size=18))

PM25Daily <- ggplot(weather, aes(date, dailymeanpm25concentration)) +
  geom_point() +
  ggtitle("Daily Particulate Matter (PM_2.5) \n Bridger Mountains \n 2021") +
  xlab("Date") + ylab("Particulate Matter ") +
  scale_x_date(labels=scales::date_format ("%m-%y"))+
  theme(plot.title = element_text(lineheight=.8, face="bold",
                                  size = 20)) +
  theme(text = element_text(size=18))


MaxTempDaily
MinTempDaily
PM25Daily
PrecipDaily
AQIDaily

#save plots
ggsave(filename = here::here("output/figures/MaxTempDaily.pdf"), 
       plot = MaxTempDaily, 
       height = 8.5, 
       width = 13,
       dpi=700)

ggsave(filename = here::here("output/figures/MinTempDaily.pdf"), 
       plot = MinTempDaily, 
       height = 8.5, 
       width = 13,
       dpi=700)

ggsave(filename = here::here("output/figures/PM25Daily.pdf"), 
       plot = PM25Daily, 
       height = 8.5, 
       width = 13,
       dpi=700)

ggsave(filename = here::here("output/figures/PrecipDaily.pdf"), 
       plot = PrecipDaily, 
       height = 8.5, 
       width = 13,
       dpi=700)

ggsave(filename = here::here("output/figures/AQIDaily.pdf"), 
       plot = AQIDaily, 
       height = 8.5, 
       width = 13,
       dpi=700)

##ggarrange

weather.plots <- gridExtra::grid.arrange(MaxTempDaily,
                                         MinTempDaily,
                                         PM25Daily,
                                         PrecipDaily,
                                         AQIDaily, ncol =2)
ggsave(filename = here::here("output/figures/weatherplots.pdf"), 
       plot = weather.plots, 
       height = 8.5, 
       width = 13,
       dpi=700)
# strava & counter data summaries ----------------------------------------

summary(strava_day$totaltrips)
summary(trail_count$count)


# camera locations --------------------------------------------------------
cameras_sf <- st_as_sf(cameras, coords = c("counter_long", "counter_lat"), 
                   crs = 4326, agr = "constant")

trail_spatial %>% 
  dplyr::filter(NAME %notin% c("NEW WORLD GULCH")) %>%
  st_zm() %>% 
  ggplot() + 
  geom_sf() +
  geom_sf(data = cameras_sf,
          aes(
            # x = counter_long,
            # y = counter_lat, 
            color = factor(trailname)), size = 2) +
  annotation_scale(style = 'ticks', pad_x = unit(4.35, 'cm'), 
                   pad_y = unit(.5, 'cm')) +
  labs(title='Bridger Mountains Trail Cameras') +
  labs(x = "Longitude", y = "Latitude", size = 20) +
   labs(color='Trails') +
theme(axis.text = element_text(size = rel(1.25)),
      axis.title = element_text(size = rel(1.25)),
      plot.title = element_text(size = rel(2))) #+  

# theme_void() #+
  # theme(legend.position = 'bottom')

ggsave(filename = here::here("output/figures/camera_locations.pdf"), 
       height = 8.5, 
       width = 13,
       dpi=700)

# plot Strava timeseries --------------------------------------------------

#daily aggregated
strava_day_TSplot_trips <- strava_day %>% 
  dplyr::group_by(timeframe, trailname) %>% 
  dplyr::summarise(total_count = max(totaltrips)) %>%
  ggplot(aes(x = timeframe, y = total_count)) +
  geom_area(aes(fill = factor(trailname)), alpha = .5, position = "stack") +
  facet_wrap(~trailname, ncol = 3) +
  guides(fill = 'none') +
  # ylim(0, 1000) +
  labs(y = "Count ",
       title = "Strava Daily Trail Traffic (by trip)",
       subtitle = "2021",
       fill = "Trail Name")

strava_day_TSplot_trips

ggsave(filename = here::here("output/figures/Strava_day_TS_bytrip.pdf"), 
       plot = strava_day_TSplot_trips, 
       height = 8.5, 
       width = 13,
       dpi=700
)

strava_day_TSplot_people <- strava_day %>% 
  dplyr::group_by(timeframe, trailname) %>% 
  dplyr::summarise(total_count = max(totalpeople)) %>%
  ggplot(aes(x = timeframe, y = total_count)) +
  geom_area(aes(fill = factor(trailname)), alpha = .5, position = "stack") +
  facet_wrap(~trailname, ncol = 3) +
  guides(fill = 'none') +
  # ylim(0, 1000) +
  labs(y = "Count ",
       title = "Strava Daily Trail Traffic (by people)",
       subtitle = "2021",
       fill = "Trail Name")
strava_day_TSplot_people

#monthly aggregated
strava_month_TSplot_trips <- strava_month %>% 
  dplyr::group_by(timeframe, trailname) %>% 
  dplyr::summarise(total_count = max(totaltrips)) %>%
  ggplot(aes(x = timeframe, y = total_count)) +
  geom_area(aes(fill = factor(trailname)), alpha = .5, position = "stack") +
  facet_wrap(~trailname, ncol = 3) +
  guides(fill = 'none') +
  # ylim(0, 1000) +
  # scale_x_date(labels = date_format("%b %y"),
  #              name = "Month",
  #              date_breaks = "Daily") +
  labs(y = "Count ",
       title = "Strava Monthly Trail Traffic (by trips)",
       subtitle = "2021",
       fill = "Trail Name")

strava_month_TSplot_people <- strava_month %>% 
  dplyr::group_by(timeframe, trailname) %>% 
  dplyr::summarise(total_count = max(totalpeople)) %>%
  ggplot(aes(x = timeframe, y = total_count)) +
  geom_area(aes(fill = factor(trailname)), alpha = .5, position = "stack") +
  facet_wrap(~trailname, ncol = 3) +
  guides(fill = 'none') +
  # ylim(0, 1000) +
  # scale_x_date(labels = date_format("%b %y"),
  #              name = "Month",
  #              date_breaks = "Daily") +
  labs(y = "Count ",
       title = "Strava Monthly Trail Traffic (by people)",
       subtitle = "2021",
       fill = "Trail Name")


# Counter TSplots by counterid --------------------------------------------

not_included <- c(1, 16, 24, 25)

# trail_count_plotting <- trail_count
# 
# ##weird workaround to fix facet labelling when we've got 
# # multiple cameras on a single subsection
# trail_count_plotting[which(trail_count_plotting$counterid == 5),
#                      "subsectionname"] <- "Baldy to Bridger 2"
# 
# trail_count_plotting[which(trail_count_plotting$counterid == 10),
#                      "subsectionname"] <- "Ross Pass to Sacagawea Peak 2"
# 
# trail_count_plotting[which(trail_count_plotting$counterid == 13),
#                      "subsectionname"] <- "Sacagawea Pass 2"
# 
# trail_count_plotting[which(trail_count_plotting$counterid == 27),
#                      "subsectionname"] <- "Corbly Gulch 2"


#daily aggregated
count_ID_TSplot_high <- trail_count %>% 
  dplyr::filter(subsectionname %in% c(high_use, 
                                      "Fairy Lakeshore"#, 
                                      # "Sacagawea Pass 2", 
                                      # "Baldy to Bridger 2"
                                      )) %>% 
  dplyr::group_by(subsectionname, date, trailname) %>% 
  dplyr::summarise(total_count = max(count)) %>%
  ggplot(aes(x = date, y = total_count)) +
  geom_area(aes(fill = factor(trailname)),
            alpha = .5, position = "stack") +
  facet_wrap(~subsectionname, ncol = 3) +
  # guides(fill = 'none') +
  # ylim(0, 1000) +
  # scale_x_date(labels = date_format("%b %y"),
  #              name = "Month",
  #              date_breaks = "Daily") +
  labs(y = "Count ",
       title = "Counter Daily Trail Traffic",
       subtitle = "2021: High Use Trails",
       fill = "Trail Name")

count_ID_TSplot_high

ggsave(filename = here("output/figures/Counter_byID_TS_highuse.pdf"), 
       height = 8.5, 
       width = 13,
       dpi=700)

#daily aggregated
count_ID_TSplot_low <- trail_count %>% 
  dplyr::filter(subsectionname %notin% c(high_use,
                                         "Fairy Lakeshore")) %>% 
  dplyr::group_by(subsectionname, date, trailname) %>% 
  dplyr::summarise(total_count = max(count)) %>%
  ggplot(aes(x = date, y = total_count)) +
  geom_area(aes(fill = factor(trailname)),
            alpha = .5, position = "stack") +
  facet_wrap(~subsectionname, ncol = 3) +
  # guides(fill = 'none') +
  # ylim(0, 1000) +
  # scale_x_date(labels = date_format("%b %y"),
  #              name = "Month",
  #              date_breaks = "Daily") +
  labs(y = "Count ",
       title = "Counter Daily Trail Traffic",
       subtitle = "2021: Low Use Trails",
       fill = "Trail Name")

count_ID_TSplot_low

ggsave(filename = here("output/figures/Counter_byID_TS_lowuse.pdf"), 
       height = 8.5, 
       width = 13,
       dpi=700)

# plot Counter timeseries by trail -------------------------------------------------

#daily aggregated
count_day_TSplot <- trail_count %>% 
  dplyr::group_by(date, trailname) %>% 
  dplyr::summarise(total_count = sum(count)) %>%
  ggplot(aes(x = date, y = total_count)) +
  geom_area(aes(fill = factor(trailname)),
            alpha = .5, position = "stack") +
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

# separate out bridger ridge and Sac Pass (high traffic #s)
count_day_TSplot_BRSP <- trail_count %>% 
  dplyr::filter(trailname %in% c("Bridger Ridge", 
                                "Sacagawea Pass")) %>% 
  dplyr::group_by(date, trailname) %>% 
  dplyr::summarise(total_count = sum(count)) %>%
  ggplot(aes(x = date, y = total_count)) +
  geom_area(aes(fill = factor(trailname)),
            alpha = .5, position = "stack") +
  facet_wrap(~trailname) +
  guides(fill = 'none') +
  # ylim(0, 1000) +
  # scale_x_date(labels = date_format("%b %y"),
  #              name = "Month",
  #              date_breaks = "Daily") +
  labs(y = "Count ",
       title = "Counter Daily Trail Traffic",
       subtitle = "2021",
       fill = "Trail Name")

count_day_TSplot_subset <- trail_count %>% 
  dplyr::filter(trailname %notin% c("Bridger Ridge", 
                                 "Sacagawea Pass")) %>% 
  dplyr::group_by(date, trailname) %>% 
  dplyr::summarise(total_count = sum(count)) %>%
  ggplot(aes(x = date, y = total_count)) +
  geom_area(aes(fill = factor(trailname)),
            alpha = .5, position = "stack") +
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

#monthly aggregated
count_month_TSplot <- trail_count %>% 
  dplyr::mutate(month = lubridate::month(date)) %>% 
  dplyr::group_by(month, trailname) %>% 
  dplyr::summarise(total_count = sum(count)) %>%
  ggplot(aes(x = month, y = (total_count))) +
  geom_point(aes(x = month, y = total_count, 
             color = factor(trailname)), size = 1) +
  geom_area(aes(fill = factor(trailname)),
            alpha = .5, position = "stack") +
  facet_wrap(~trailname, ncol = 3) +
  guides(fill = 'none') +
  # ylim(0, 1000) +
  # scale_x_date(labels = date_format("%b %y"),
  #              name = "Month",
  #              date_breaks = "Daily") +
  labs(y = "Count ",
       title = " Monthly Bridger Trail Traffic",
       subtitle = "2021 Counter Data",
       fill = "Trail Name")




# plot Strava/Counter overlap  --------------------------------------------

strava_count_join <-  trail_count %>%
  left_join(strava_day, by = c("trailnumber" = "trailnumber",
                               "date" = "timeframe")) 

strava_count_join %>% 
  dplyr::group_by(date, trailname.x) %>% 
  dplyr::summarise(total_count = sum(count), 
                   total_trips = sum(totaltrips)) %>%
  ggplot(aes(x = date, y = total_count)) +
  # geom_point(data = trail_count, aes(x = date, y = count), 
  #            alpha = 0.5) + 
  facet_wrap(~trailname.x, ncol = 3) +
  geom_area(aes(fill = factor(trailname.x)),
            alpha = .5, position = "stack") +
  geom_line(aes(x = date, y = total_trips), color = 'red', size = 1) +

  scale_y_continuous(name = "Counter Count", 
                     labels=function(x)  format(x, big.mark = ",", 
                                                scientific = FALSE),
                     sec.axis = sec_axis(trans = ~ . * 1, 
                                         name = "Strava Count", 
                                         labels=function(x)  format(x, big.mark = ",", scientific = FALSE))) +
  labs(x = "Date",
       title = "Bridger Trail Traffic and Strava Use",
       subtitle = "Summer 2021",
       fill = "none") #+ 
  # labs(
    # title = "<span style='font-size:14pt'> Seattle <span style='color:#00688b;'>Bike Traffic</span> and
    # <span style='color:#8b3e2f;'>COVID 19 Cases</span> 
    # </span>")+
  # theme(
  #   plot.title = element_markdown(lineheight = 1.1)
  # )

# Counter day of week ----------------------------------------

trail_hourly %>% 
  group_by(hour, wday, trailname) %>%
  summarise(total_count = sum(count, na.rm = TRUE)) %>%
  group_by(trailname) %>%
  mutate(pct = total_count/sum(total_count)) %>%
  ggplot(aes(x = hour, y = pct, 
             color = trailname,
             fill = trailname)) +
  geom_line() +
  geom_area(alpha = 0.25) +
  facet_grid(wday ~ trailname) +
  scale_x_continuous(name = "Hour of Day", 
                     breaks = c(0, 6, 12, 18)) +
  scale_y_continuous(name = "Percentage of Trail Use
                     ", labels = scales::percent_format(accuracy=0.1)) + 
  # scale_fill_manual(values=seattlePalette) +
  # scale_color_manual(values=seattlePalette) +
  guides(color='none', fill = 'none') + 
  labs(title = "Bridgers Trail Traffic Patterns by Hour and Weekday",
       subtitle = "Data: 2021") 

ggsave(filename = here("output/figures/hourly_bydayofweek.pdf"), 
       height = 8.5, 
       width = 13,
       dpi=700)

# Counters vs covariates ------------------------------------------

## weather - all trails combined

trail_count_weather <- trail_count %>%
  left_join(weather, by = c("date" = "date")) %>% 
  group_by(date, trailname) %>%
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

## weekends
trail_count_weather$wday <- lubridate::wday(trail_count_weather$date, label = T)
trail_count_weather$weekend = "No"
trail_count_weather[trail_count_weather$wday %in% c("Sun", "Sat"),
                    "weekend"] = "Yes"

trail_count_weather %>%
  group_by(date, weekend, trailname) %>%
  ggplot(aes(x = TEMP_MAX, y = count, color = weekend)) +
  facet_wrap(~trailname) +
  # scale_color_manual(values=seattlePalette[c(6, 2)]) +
  scale_x_continuous(name = "Max Temperature (Degrees Fahrenheit)") +
  scale_y_continuous(name = "Count") + 
  geom_jitter(alpha = 0.5) + 
  labs(title = "Comparison of Daily Bridger Trail Traffic to Max Temperature",
       color = "Day of Week is Weekend") 

## trail use
trail_count_char <- trail_count %>%
  left_join(trail_char, by = c("trailname" = "trailname")) %>% 
  group_by(date, trailname) %>%
  summarise(count = sum(count), 
            parking = parkinglot, 
            miles = total_miles,
            traveltime = total_traveltime) 

trail_count_char %>% 
  ggplot(aes(x = miles, y = count, color = factor(parking))) +
  # scale_color_manual(values=seattlePalette[c(6, 2)]) +
  scale_x_continuous(name = "Miles to Trailhead") +
  scale_y_continuous(name = "Count") + 
  geom_jitter(alpha = 0.5) + 
  labs(title = "Comparison of Daily Bridger Trail Traffic to Travel Distance",
       color = "Parking Lot Size") 
  
trail_count_char %>% 
  ggplot(aes(x = traveltime, y = count, color = factor(parking))) +
  # scale_color_manual(values=seattlePalette[c(6, 2)]) +
  scale_x_continuous(name = "Travel Time to Trailhead") +
  scale_y_continuous(name = "Count") + 
  geom_jitter(alpha = 0.5) + 
  labs(title = "Comparison of Daily Bridger Trail Traffic to Travel Time",
       color = "Parking Lot Size") 



# counter vs strava correlation -------------------------------------------



# trail correlations ------------------------------------------------------


