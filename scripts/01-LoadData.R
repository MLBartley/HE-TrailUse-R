## header ---------------------------
## Script name: 00.01-LoadData.R
##
## Purpose of script: To load and process raw data files obtained from Megan Lawson.
##
## Author: Dr. Meridith L. Bartley
## Date Created: 2022-07-05
## notes ---------------------------
## 
##   - Data received on July 5, 2022
##   - Data updated (trail_count and weather) on July 11, 2022
##   - Data updated (trailheads/edgeID file) on July 14, 2022
##   - Data updated (weather - temp min/max) on August 1, 2022 
##
## ---------------------------


# load packages -----------------------------------------------------------
library(data.table) #let's us use %like% operator 
library(dplyr) #lots of data handling tools
library(here) #helps to use relative file paths within the R project
library(readxl) #useful for loading excel files into R
library(lubridate) #helps with date (day/month/year) data formating
library(magrittr) #let's us use pipe function '%>% 
library(sf) #useful for spatial data


`%notin%` <- Negate(`%in%`) #useful for exploring data

# load data ---------------------------------------------------------------

# load strava data
strava_day <- readxl::read_excel(here("data/raw/strava_day.xlsx"))
strava_month <- readxl::read_excel(here("data/raw/strava_month.xlsx"))
strava_year <- readxl::read_excel(here("data/raw/strava_year.xlsx"))

#load trail data
trail_char <- readxl::read_excel(here("data/raw/trailcharacteristics.xlsx"))
trail_count <- readxl::read_excel(here("data/raw/trailcounterdata.xlsx"))
trail_hourly <- readxl::read_excel(here("data/raw/AllCounts_Long_Hourly_20220713.xlsx"))

#load relational data table joining strava and trail count IDs - multiple sheets

## previous file name - replaced by ML on 14 July 2022
# join_IDs <- readxl::read_excel(here("data/raw/Trailhead_Strava_Crosswalk.xlsx"), sheet = 1)
# join_IDs_allEdges <- readxl::read_excel(here("data/raw/Trailhead_Strava_Crosswalk.xlsx"), 
#                                             sheet = 3)

join_IDs <- readxl::read_excel(here("data/raw/Trailheads.xlsx"), sheet = 2)
#only need joins included in study
join_IDs <- join_IDs[which(join_IDs$`Include in Bridgers study` == 1),]
#removing columns (Include in study, Notes)
join_IDs <- join_IDs[, 1:5]


#load weather data
weather <- readxl::read_excel(here("data/raw/weather_airquality.xlsx"))
weather_updateTemps <- read.csv(here("data/raw/NOAA_Weather_MSU_20210101_20211231.csv"))

#load spatial shapefile data
trail_spatial <- sf::st_read(dsn = here("data/raw/Bridgers_Trails_Shp_Strava_Crosswalk/"))

# process data ------------------------------------------------------------

### fix inconsistancies in trailnaming  ####

## join_ID fixes

# College M is subsection but all other data files have this as the trailname
join_IDs[which(join_IDs$TrailNumber == 511), 
         c("TrailName", "SubsectionName")] <- matrix(rep(c("College M", NA) , 6), 
                                                     nrow = 6, byrow = T)

##spelling issues 
join_IDs[which(join_IDs$TrailName == "Carrol Creek"), "TrailName"] <- "Carroll Creek"
join_IDs[which(join_IDs$TrailName == "E Bridger North"), "TrailName"] <- "East Bridger North"
join_IDs[which(join_IDs$TrailName == "E Bridger South"), "TrailName"] <- "East Bridger South"
join_IDs[which(join_IDs$TrailName == "S Fork Flathead Creek"), "TrailName"] <- "South Fork Flathead Creek"

##strava_day fixes

##spelling issues - similar to aboave
strava_day[which(strava_day$trailname == "E Bridger North"), "trailname"] <- "East Bridger North"
strava_day[which(strava_day$trailname == "E Bridger South"), "trailname"] <- "East Bridger South"

# College M is listed inconsistantly with "Bridger Foothills" as the trail name
# for trail number 511

strava_day[which(strava_day$trailnumber == 511), "trailname"] <- "College M"

## Some spelling inconsistancies fixed with trail_char names
strava_day1 <- strava_day %>% 
  dplyr::left_join(trail_char[, 1:2], by = c("trailnumber" = "trailnumber")) 

## trailnames that are included in strava but not in trail_char
add_to_trail_char <- unique(strava_day1[is.na(strava_day1$trailname.y), "trailname.x"])

#use strava trail names for NA trail_char names
strava_day1[is.na(strava_day1$trailname.y), "trailname.y"] <- strava_day1[is.na(strava_day1$trailname.y), "trailname.x"]

strava_day <- strava_day1 %>% 
  dplyr::select(-trailname.x) %>% #remove trailnames from strava_day
  dplyr::rename(trailname = trailname.y) %>% #replayce with names from trail_char
  dplyr::distinct() #removes duplicate rows that pop in for some reason. but this makes the row nuumbers match original strava_day

## check for discrepancies - should be empty df
## uncomment to check
# View(strava_day[which(strava_day$trailname.x != strava_day$trailname.y), ])

### check data types; change to date (day/month/year) ####

strava_day$timeframe <- lubridate::as_date(strava_day$timeframe)
trail_count$date <- lubridate::as_date(trail_count$date)
weather$date <- lubridate::as_date(weather$date)
trail_count$wday <- lubridate::wday(trail_count$date, label=T)
strava_day$wday <- lubridate::wday(strava_day$timeframe, label=T)
trail_hourly$date <- lubridate::as_date(trail_hourly$datetime)
trail_hourly$wday <- lubridate::wday(trail_hourly$date, label = T)

# add fake day to make it easier to change to date format
strava_month$timeframe <- paste(strava_month$timeframe, "-01", sep="")
strava_month$timeframe <- lubridate::as_date(strava_month$timeframe)
strava_month$month <- lubridate::month(strava_month$timeframe)

#add fake month/day to make it easier to change date format
strava_year$timeframe <- paste(strava_year$timeframe, "-01-01", sep="")
strava_year$timeframe <- lubridate::as_date(strava_year$timeframe)
strava_year$year <- lubridate::year(strava_year$timeframe)

## weather_update make date a date class
weather_updateTemps$DATE <- lubridate::mdy(weather_updateTemps$DATE)

### fix class of trail_spatial$ID to be numeric ####
trail_spatial$ID <- as.numeric(trail_spatial$ID)

### remove trails not to be included in analysis ####

trail_count <- trail_count %>% 
  dplyr::filter(counterid %notin% c(1, 16, 24, 25))

# strava_day <- strava_day %>% 
#   dplyr::filter(trailname %notin% c("Fairy Lakeshore", "Felix Canyon Rd"))



### add subsection names to  Strava data with join_IDs ####

strava_day <- strava_day %>% 
  dplyr::left_join(join_IDs, by = c("trailnumber" = "TrailNumber", 
                             "edge_uid" = "edge_uid", 
                             "trailname" = "TrailName")) %>% 
  dplyr::rename(subsectionname = SubsectionName)

### update temp min and max values ####
weather_updateTemps <- weather_updateTemps %>% 
  dplyr:: select(-c("STATION",
                    "NAME", 
                    "PRCP", 
                    "SNOW", 
                    "SNWD",
                    "TOBS"))

weather <- weather %>% 
  dplyr::full_join(weather_updateTemps, by = c("date" = "DATE") ) %>% 
  dplyr::select(-c("temp_max_f", "temp_min_f")) %>% 
  dplyr::rename(c("temp_max_f" = "TMAX", 
                  "temp_min_f" = "TMIN"))
  


# combine for GAMM analysis -----------------------------------------------

## some subections (e.g. Baldy to Bridger and Ross Pass to Sac Peak)
allCount <- trail_count %>% 
  dplyr::group_by(date, trailnumber) %>% 
  dplyr::mutate(max.camera = max(count)) %>% 
  dplyr::select(-c(count, counterid, counter_lat,
                   # subsectionname, # removing to start simple with all Trails WILL NEED TO REASSESS
                   counter_long, stravaedgeid2022)) %>% 
  dplyr::distinct()

allStrava <- strava_day %>%
  dplyr::group_by(timeframe, trailname, subsectionname) %>% 
  dplyr::mutate(max.count = max(totaltrips)) %>% 
  dplyr::select(-c(totaltrips, totalpeople, edge_uid, 'Segment with trail counter')) %>% 
  dplyr::distinct()

#want total miles of each trail (summing multiple subsections)
trail_spatial_summary <- trail_spatial %>% 
  dplyr::group_by(ID) %>% 
  dplyr::mutate(totallength_miles = sum(length_mi)) %>% 
  dplyr::ungroup()


#remove the "geometry" spatial information (causes issues later on)
trail_spatial_summary <- trail_spatial_summary %>% sf::st_drop_geometry()

allTrailChar <- trail_char %>% 
  dplyr::left_join(trail_spatial_summary[, c("ID", 
                                             # "length_mi", #removing this so we don't have repeat entries, but might be useful later
                                             "totallength_miles")], 
                   by = c("trailnumber" = "ID" ))





## Combine Camera Counts, Strava Counts, Weather, Trail Characteristics

allTrail <- allCount %>%
  dplyr::left_join(
    allStrava,
    by = c(
      "date" = "timeframe",
      "trailnumber" = "trailnumber",
      "trailname" = "trailname",
      "wday" = "wday", 
      "subsectionname" = "subsectionname"
      )) %>%
  dplyr::left_join(weather, by = c("date" = "date")) %>% 
  dplyr::left_join(allTrailChar, by = c("trailnumber" = "trailnumber", 
                                        "trailname" = "trailname", 
                                        "subsectionname" = "subsectionname"
                                        )) %>% 
  # dplyr::select(-c(subsectionname, th_lat, th_long, latitude, longitude)) %>% 
  dplyr::distinct()

## if trail doesnt have subsections, let's use the trailname as the subseciton name
## this allows us to analyse based on subsectionname - but we need to still address how to nest within trail number
allTrail[is.na(allTrail$subsectionname), 'subsectionname' ] <- allTrail[is.na(allTrail$subsectionname), 'trailname' ] 

## CHECK: does allTrail have the same number of rows as trail_count? 
## If not something is off and we might have duplicate rows.
nrow(allCount) == nrow(allTrail)

## Add in a Time variable
allTrail$yday <- lubridate::yday(allTrail$date)

# CHECK which spatial data missing
# View(allTrail[is.na(allTrail$length_mi),]) #result: no spatial info for Johnson Canyon Jeep trail nor Benchmark Rd
# View(allTrail[is.na(allTrail$totallength_miles),]) #result: no spatial info for Johnson Canyon Jeep trail nor Benchmark Rd

spatialmissing.subsections <- pull(unique(allTrail[is.na(allTrail$totallength_miles),
                                                   'subsectionname']))

## remove trails without Strava data/spatial data 
# allTrail <- allTrail %>% 
#   dplyr::filter(subsectionname %notin% spatialmissing.subsections) #Johnson Canyon and Benchmark Rd

# CHECK which trail/dates missing Strava data
# View(allTrail[is.na(allTrail$max.count), ])

checkStravaCount <- allTrail[is.na(allTrail$max.count), ]

nrow(allTrail[is.na(allTrail$max.count) & allTrail$max.camera <= 20, ])/ nrow(allTrail[is.na(allTrail$max.count), ])

## Assume missing Strava data reflect a 0 observation - BIG ASSUMPTION
## however ~75% of these missing (NA) Strava counts correspond to a camera count
## of 20 or less. 

allTrail$max.count[which(is.na(allTrail$max.count))] <- 0

## CHECK do we have any strava counts that are greater than camera counts?

# View(allTrail[swhich(allTrail$max.camera < allTrail$max.count),])

checkCountvsStrava <- allTrail[which(allTrail$max.camera < allTrail$max.count),]

## remove unecessary columns
allTrail <- allTrail %>% 
  dplyr::select(-c(
    # "counterid",
                   # "trailnumber",
                   # "subsectionname", 
                   "counterowner",
                   "th_lat",
                   "th_long"#,
                   # "counter_lat", 
                   # "counter_long",
                   # "stravaedgeid2022"
  )) %>% 
  dplyr::distinct()

#convert week and wday to integers now
allTrail$wday <- as.integer(allTrail$wday)
allTrail$week <- as.integer(allTrail$week)


## gather strava+covariate data w/out camera counter data for predicting
# 
# predict.data <- allStrava %>% 
#   dplyr::left_join(weather, by = c("timeframe" = "date")) %>% 
#   dplyr::left_join(allTrailChar, by = c("trailnumber" = "trailnumber", 
#                                         "trailname" = "trailname", 
#                                         "subsectionname" = "subsectionname")) %>% 
#   dplyr::filter(timeframe %notin% allCount$date) %>% 
#   dplyr::mutate(yday = lubridate::yday(timeframe)) %>% 
#   dplyr::ungroup() %>% 
#   dplyr::select(c("yday", 
#                   "wday", 
#                   "max.count", 
#                   "month", 
#                   "precipitation_in", 
#                   "temp_min_f", 
#                   "daily_aqi_value"))
# 
# #convert week and wday to integers now
# predict.data$wday <- as.integer(predict.data$wday)
# # predict.data$week <- as.integer(predict.data$week)



# save updated data files -------------------------------------------------

#save subset of data where StavaCount (column = max.count) is NA
write.csv(checkStravaCount, 
          file = here("data/processed/checkStravaCount.csv"), 
          row.names = FALSE)

#save subset of data where Strava count > camera count
write.csv(checkCountvsStrava, 
          file = here("data/processed/checkCountvsStrava.csv"), 
          row.names = FALSE)


