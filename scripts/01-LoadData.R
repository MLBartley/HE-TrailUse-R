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
##   - Data updated (AllTrails search data) on August 8, 2022
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

#load allTrails search views
allTrails_search <- readxl::read_excel(here("data/raw/alltrails_pageviews.xlsx"), 
                                       col_types = c("numeric", "text", "date", 
                                                     "numeric", "numeric", "numeric", "numeric"))

# process data ------------------------------------------------------------

##useful when need to plot high/low use trail subsections separately
high_use <- c("Baldy to Bridger", "Bridger", 
              "Bridger to Ross Pass", 
              "College M", "M to Baldy",
              "Middle Cottonwood", "Sypes Canyon",
              "Ross Pass to Sacagawea Peak",
              "Sacagawea Pass", "Steep Way")

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

# ### remove trails not to be included in analysis ####
# 
# trail_count <- trail_count %>% 
#   dplyr::filter(counterid %notin% c(1, 16, 24, 25))

# strava_day <- strava_day %>% 
#   dplyr::filter(trailname %notin% c("Fairy Lakeshore", "Felix Canyon Rd"))



### add subsection names to  Strava data with join_IDs ####

strava_day <- strava_day %>% 
  dplyr::left_join(join_IDs, by = c("trailnumber" = "TrailNumber", 
                             "edge_uid" = "edge_uid", 
                             "trailname" = "TrailName")) %>% 
  dplyr::rename(subsectionname = SubsectionName)

## add subsection name from trail name if NA
trail_count[is.na(trail_count$subsectionname),
         'subsectionname' ] <- trail_count[is.na(trail_count$subsectionname), 
                                        'trailname' ] 

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
  
### camera data details ####
cameras <- trail_count %>% 
  dplyr::select(c(counterid,
                  trailnumber, 
                  trailname, 
                  counterowner,
                  counter_lat, 
                  counter_long)) %>% 
  dplyr::distinct()

# combine for GAMM analysis -----------------------------------------------

### All Data ####

### remove trails not to be included in analysis ####

trail_count_analysis <- trail_count %>% 
  dplyr::filter(counterid %notin% c(1,
                                    # 14, 33, #Horsetrail and Benchroad don't have ANY Strava data
                                    16, 24, 25))


## some subections (e.g. Baldy to Bridger and Ross Pass to Sac Peak)
allCount <- trail_count_analysis %>% 
  dplyr::group_by(date, trailnumber) %>% 
  dplyr::mutate(max.camera = max(count)) %>% 
  dplyr::select(-c(count, counterid, counter_lat,
                   counterowner,
                   # subsectionname, # removing to start simple with all Trails WILL NEED TO REASSESS
                   counter_long, stravaedgeid2022)) %>% 
  dplyr::distinct()

allCount[is.na(allCount$subsectionname),
          'subsectionname' ] <- allCount[is.na(allCount$subsectionname), 
                                          'trailname' ] 

allStrava <- strava_day %>%
  dplyr::group_by(timeframe, trailname, subsectionname) %>% 
  dplyr::mutate(max.count = max(totaltrips)) %>% 
  dplyr::select(-c(totaltrips, totalpeople, edge_uid, 'Segment with trail counter')) %>% 
  dplyr::distinct()

allStrava[is.na(allStrava$subsectionname),
          'subsectionname' ] <- allStrava[is.na(allStrava$subsectionname), 
                                          'trailname' ] 
## Add in a Time variable
allStrava$yday <- lubridate::yday(allStrava$timeframe)

#### add zero entries back into Strava Data (any trail/day without an entry) ####
tmp <- allStrava[, c( "subsectionname", "trailname", "trailnumber")] %>% distinct()
nrows <- dim(tmp)[1]

names_repeated <- tmp[rep(1:nrows, 365), ]

allStrava_fixzeros <- as.data.frame(cbind(rep(1:365, nrows), 
                                      names_repeated))
colnames(allStrava_fixzeros) <- c("yday", "subsectionname", 
                                  "trailname", "trailnumber")

allStrava_fixzeros$yday <- as.numeric(allStrava_fixzeros$yday)


allStrava_fixzeros <- allStrava_fixzeros %>% 
  dplyr::full_join(allStrava, by = c("yday" = "yday", 
                                     "subsectionname" = "subsectionname",
                                     "trailname" = "trailname", 
                                     "trailnumber" = "trailnumber")) %>% 
  dplyr::mutate(subsectionF = as.factor(subsectionname),
                timeframe = lubridate::as_date(yday, origin = "2020-12-31"), 
                wday = lubridate::wday(timeframe, label = T)) %>% 
  dplyr::distinct()

# CHECK which trail/dates missing Strava data
# View(allTrail[is.na(allTrail$max.count), ])

checkStravaCount <- allStrava_fixzeros[is.na(allStrava_fixzeros$max.count), ]

nrow(allStrava_fixzeros[is.na(allStrava_fixzeros$max.count), ])/nrow(allStrava_fixzeros)


allStrava_fixzeros[is.na(allStrava_fixzeros$max.count), "max.count"] <- 0
  
allStrava <- allStrava_fixzeros

#want total miles of each trail (summing multiple subsections)
trail_spatial_summary <- trail_spatial %>% 
  dplyr::group_by(ID) %>% 
  dplyr::mutate(totallength_miles = sum(length_mi)) %>% 
  dplyr::ungroup()


#remove the "geometry" spatial information (causes issues later on)
trail_spatial_summary_keep <- trail_spatial_summary
trail_spatial_summary <- trail_spatial_summary %>% sf::st_drop_geometry()

allTrailChar <- trail_char %>% 
  dplyr::left_join(trail_spatial_summary[, c("ID", 
                                             # "length_mi", #removing this so we don't have repeat entries, but might be useful later
                                             "totallength_miles")], 
                   by = c("trailnumber" = "ID" ))

allTrailChar[is.na(allTrailChar$subsectionname),
          'subsectionname' ] <- allTrailChar[is.na(allTrailChar$subsectionname), 
                                          'trailname' ] 

#### Combine Camera Counts, Strava Counts, Weather, Trail Characteristics ####

allData <- allCount %>% 
  dplyr::full_join(
    allStrava,
    by = c(
      "date" = "timeframe",
      "trailnumber" = "trailnumber",
      "trailname" = "trailname",
      "wday" = "wday", 
      "subsectionname" = "subsectionname"
    )) %>%
  dplyr::full_join(weather, by = c("date" = "date")) %>% 
  dplyr::full_join(allTrailChar, by = c("trailnumber" = "trailnumber", 
                                        "trailname" = "trailname", 
                                        "subsectionname" = "subsectionname"
  )) %>% 
  dplyr::distinct()

## if trail doesnt have subsections, let's use the trailname as the subseciton name
## this allows us to analyse based on subsectionname - but we need to still address how to nest within trail number
allData[is.na(allData$subsectionname), 'subsectionname' ] <- allData[is.na(allData$subsectionname), 'trailname' ] 

## Add in a Time variable
allData$yday <- lubridate::yday(allData$date)

# CHECK which spatial data missing
# View(allTrail[is.na(allTrail$length_mi),]) #result: no spatial info for Johnson Canyon Jeep trail nor Benchmark Rd
# View(allTrail[is.na(allTrail$totallength_miles),]) #result: no spatial info for Johnson Canyon Jeep trail nor Benchmark Rd

spatialmissing.subsections <- pull(unique(allData[is.na(allData$totallength_miles),
                                                   'subsectionname']))

## remove trails without Strava data/spatial data 
# allTrail <- allTrail %>% 
#   dplyr::filter(subsectionname %notin% spatialmissing.subsections) #Johnson Canyon and Benchmark Rd

# CHECK which trail/dates missing Strava data
# View(allTrail[is.na(allTrail$max.count), ])

# checkStravaCount <- allTrail[is.na(allTrail$max.count), ]
# 
# nrow(allTrail[is.na(allTrail$max.count) & allTrail$max.camera <= 20, ])/ nrow(allTrail[is.na(allTrail$max.count), ])

## Assume missing Strava data reflect a 0 observation - BIG ASSUMPTION
## however ~75% of these missing (NA) Strava counts correspond to a camera count
## of 20 or less. 

# allTrail$max.count[which(is.na(allTrail$max.count))] <- 0

## CHECK do we have any strava counts that are greater than camera counts?

# View(allTrail[swhich(allTrail$max.camera < allTrail$max.count),])

# checkCountvsStrava <- allData[which(allData$max.camera < allData$max.count),]

## remove unecessary columns
allData <- allData %>% 
  dplyr::select(-c(
    # "counterid",
    # "trailnumber",
    # "subsectionname", 
    # "counterowner",
    "th_lat",
    "th_long"#,
    # "counter_lat", 
    # "counter_long",
    # "stravaedgeid2022"
  )) %>% 
  dplyr::distinct()

#convert week and wday to integers now
allData$wday <- as.integer(allData$wday)
allData$week <- as.integer(allData$week)
  

##All Trails - Model Fit Data ####

## new, easier way to subset data for model (and later, data for prediciton)
## I moved the addition of zeros to the Strava data earlier in the process so 
    #it only has to be done once!
allTrail <- allData %>% 
  tidyr::drop_na(max.camera) %>% 
  ## remove trails that are ENTIRELY NA for strava data 
  tidyr::drop_na(max.count) %>% 
  ## remove non-independent ("duplicate') sections of Bridger Ridge
  dplyr::filter(subsectionname %notin% c("Baldy to Bridger",
                                         "Bridger to Ross Pass",
                                         "M to Baldy", 
                                         "Ross Pass to Sacagawea Peak"))

subsection.excluded.num <- which(unique(allData$subsectionname) %notin% unique(allTrail$subsectionname))
subsection.excluded.name <- unique(allData$subsectionname)[subsection.excluded.num]


## older, still valid way to combine data. 
## Unsure if this captures all Strava zeros added, especially when switching to 
## gathering data for prediciton

# allTrail <- allCount %>%
#   dplyr::left_join(
#     allStrava,
#     by = c(
#       "date" = "timeframe",
#       "trailnumber" = "trailnumber",
#       "trailname" = "trailname",
#       "wday" = "wday", 
#       "subsectionname" = "subsectionname"
#     )) %>%
#   dplyr::left_join(weather, by = c("date" = "date")) %>% 
#   dplyr::left_join(allTrailChar, by = c("trailnumber" = "trailnumber", 
#                                         "trailname" = "trailname", 
#                                         "subsectionname" = "subsectionname"
#   )) %>% 
#   # dplyr::select(-c(subsectionname, th_lat, th_long, latitude, longitude)) %>% 
#   dplyr::distinct()

## if trail doesnt have subsections, let's use the trailname as the subseciton name
## this allows us to analyse based on subsectionname - but we need to still address how to nest within trail number
# allTrail[is.na(allTrail$subsectionname), 'subsectionname' ] <- allTrail[is.na(allTrail$subsectionname), 'trailname' ] 

## CHECK: does allTrail have the same number of rows as trail_count? 
## If not something is off and we might have duplicate rows.
nrow(allCount %>% filter(subsectionname %notin% c("Baldy to Bridger",
                                                  "Bridger to Ross Pass",
                                                  "M to Baldy"))) == nrow(allTrail)

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

# checkStravaCount <- allTrail[is.na(allTrail$max.count), ]
## Horsetheif Moutain and Benchmark Trail both have NO STRAVA DATA AT ALL
## Only remaining NA = zeros, but seems less reliable since all data are missing

# nrow(allTrail[is.na(allTrail$max.count) & allTrail$max.camera <= 20, ])/ nrow(allTrail[is.na(allTrail$max.count), ])

## Assume missing Strava data reflect a 0 observation - BIG ASSUMPTION
## however ~75% of these missing (NA) Strava counts correspond to a camera count
## of 20 or less. 

# allTrail$max.count[which(is.na(allTrail$max.count))] <- 0

## CHECK do we have any strava counts that are greater than camera counts?

# View(allTrail[swhich(allTrail$max.camera < allTrail$max.count),])

checkCountvsStrava <- allTrail[which(allTrail$max.camera < allTrail$max.count),]

## remove unecessary columns
# allTrail <- allTrail %>% 
#   dplyr::select(-c(
#     # "counterid",
#     # "trailnumber",
#     # "subsectionname", 
#     "counterowner",
#     "th_lat",
#     "th_long"#,
#     # "counter_lat", 
#     # "counter_long",
#     # "stravaedgeid2022"
#   )) %>% 
#   dplyr::distinct()

#convert week and wday to integers now
# allTrail$wday <- as.integer(allTrail$wday)
# allTrail$week <- as.integer(allTrail$week)


### Middle Cottonwood Only ####
## Focusing on Middle Cottenwood Trail (Trail # 586, Counter ID # 31)

## new, more efficient way to subset Middle Cottonwood data
singleTrail <- allData %>% 
  dplyr::filter(trailnumber == 586) %>% 
  tidyr::drop_na(max.camera) 

## older way to subset Middle Cottonwood data

# singleCount <- trail_count %>%
#   dplyr::filter(counterid == 31)
# 
# singleStrava <- strava_day %>%
#   dplyr::filter(trailnumber == 586) %>%
#   # we want a single representation count value for multiple edges per day
#   dplyr::group_by(timeframe) %>%
#   dplyr::mutate(max.count = max(totaltrips)) %>%
#   dplyr::select(-c(totaltrips, totalpeople, edge_uid, "Segment with trail counter" )) %>%
#   dplyr::distinct()
# trail characteristics not pertinent for single trail analysis
# no variation within trail information

# no variation within trail information

## Combine Camera Counts, Strava Counts, Weather, Trail Characteristics

# singleTrail <- singleCount %>%
#   dplyr::left_join(
#     singleStrava,
#     by = c(
#       "date" = "timeframe",
#       "trailnumber" = "trailnumber",
#       "trailname" = "trailname",
#       "wday" = "wday"
#     )
#   ) %>%
#   dplyr::left_join(weather, by = c("date" = "date"))
# 
# ## Add in a Time variable
# singleTrail$yday <- lubridate::yday(singleTrail$date)
# 
# ## Assume missing Strava data reflect a 0 observation
# 
# singleTrail$max.count[which(is.na(singleTrail$max.count))] <- 0
# 
# ## remove unecessary columns
# singleTrail <- singleTrail %>% 
#   dplyr::select(-c("counterid",
#                    "trailnumber",
#                    # "subsectionname", 
#                    "counterowner",
#                    "th_lat",
#                    "th_long",
#                    "counter_lat", 
#                    "counter_long",
#                    "stravaedgeid2022" 
#                    # "Segment with trail counter"                   
#   )) %>% 
#   dplyr::distinct()

#convert week and wday to integers now
singleTrail$wday <- as.integer(singleTrail$wday)
singleTrail$week <- as.integer(singleTrail$week)


### gather spatial data and count data for spatial network gamm ####

#which trails do we have reliable spatial information for?
# (some subsections differ in spatial vs characteristics)
spatial_subset <- c(500, #Fairy Creek
                    511, #College M - excludes shortcut spatial section
                    518, #Sacagawea Pass
                    523, # Horsetheif Moutain
                    527, #Carroll/Carrol Creek
                    530, #Raptor View
                    531, #Sypes Canyon - excludes Bridger Foothills to Ridge spatial section
                    535, #Truman Gulch
                    538, #East Bridger South
                    539, #East Bridger North
                    540, #Shafthouse Hill
                    544, #Corbly Gulch
                    546, #North cottonwood access
                    551, #Ross Pass
                    586 #middle cottonwood
                   )

## need a df of counts with subsection + to/from locations (get from spatial data)
## also need df of all to/from point locations (get from multipoint line geometry)
# 
# tst <- trail_spatial_summary_keep$geometry %>% 
#   st_cast("LINESTRING") %>% 
#   sf::st_coordinates()
# 
# allTrail_locations <- trail_spatial_summary_keep[7, ] %>% 
#   sf::st_coordinates() %>%
#   
#   dplyr::filter(ID %in% spatial_subset) %>% 
#   tidyr::pivot_longer( c(BEGIN_TERM, END_TERMIN), names_to = "Endpoints") %>% 
#   dplyr::mutate(x = ,
#                 y = )
  


## gather strava+covariate data w/out camera counter data for predicting ####

### Middle Cottonwood ####

predict.MidCot <- allData %>% 
  dplyr::filter(trailnumber == 586)  %>% 
  dplyr::select(c("trailnumber", 
                  "trailname",
                  # "subsectionF", 
                  "date", 
                  "wday",
                  "month",
                  "yday",
                  "max.count", 
                  "precipitation_in", 
                  "daily_aqi_value", 
                  "temp_max_f",
                  "total_traveltime", 
                  "totallength_miles"
                  ## add more if used in model
                  ))
  

# add zeros into missing STRAVA days

# singleStrava_zeros <- singleStrava %>% 
#   dplyr::mutate(yday = lubridate::yday(timeframe))
# 
# zeros_df <- as.data.frame(which(1:365 %notin%  singleStrava_zeros$yday))
# colnames(zeros_df) <- "yday"
# zeros_df <- zeros_df %>% 
#   mutate(timeframe = lubridate::as_date(yday, origin = "2020-12-31"), 
#          trailname = unique(singleStrava$trailname), 
#          trailnumber = unique(singleStrava$trailnumber), 
#          wday = lubridate::wday(timeframe, label = T), 
#          max.count = 0) %>% 
#   dplyr::left_join(weather, by = c("timeframe" = "date"))
# 
# predict.MidCot <- singleStrava %>% 
#   dplyr::left_join(weather, by = c("timeframe" = "date")) %>% 
#   # dplyr::filter(timeframe %notin% singleCount$date) %>% 
#   dplyr::mutate(yday = lubridate::yday(timeframe)) %>% 
#   dplyr::ungroup() %>% 
#   dplyr::bind_rows(zeros_df) %>% 
#   dplyr::select(c("yday", 
#                   "wday", 
#                   "max.count", 
#                   "month", 
#                   "precipitation_in", 
#                   "temp_max_f", 
#                   "daily_aqi_value"))
# 
# #convert week and wday to integers now
# predict.MidCot$wday <- as.integer(predict.MidCot$wday)
# predict.MidCot$week <- as.integer(predict.MidCot$week)

### All Trails####

predict.All <- allData %>% 
  dplyr::filter(subsectionname %notin% subsection.excluded.name)  %>%
  dplyr::select(c("trailnumber", 
                  "trailname",
                  # "subsectionname",
                  "subsectionF",
                  "date", 
                  "wday",
                  "month",
                  "yday",
                  "max.count", 
                  "precipitation_in", 
                  "daily_aqi_value", 
                  "temp_max_f", 
                  "total_traveltime", 
                  "totallength_miles"
                  ## add more if used in model
  )) %>% 
  distinct()

### New Trails (not in model) ####

predict.New <- allData %>% 
  dplyr::filter(subsectionname %in% subsection.excluded.name)  %>%
  dplyr::select(c("trailnumber", 
                  "trailname",
                  "subsectionname",
                  "subsectionF",
                  "date", 
                  "wday",
                  "month",
                  "yday",
                  "max.count", 
                  "precipitation_in", 
                  "daily_aqi_value", 
                  "temp_max_f",
                  "total_traveltime", 
                  "totallength_miles"
                  ## add more if used in model
  ))

# save updated data files -------------------------------------------------

#save subset of data where StavaCount (column = max.count) is NA
write.csv(checkStravaCount, 
          file = here("data/processed/checkStravaCount.csv"), 
          row.names = FALSE)

#save subset of data where Strava count > camera count
write.csv(checkCountvsStrava, 
          file = here("data/processed/checkCountvsStrava.csv"), 
          row.names = FALSE)


