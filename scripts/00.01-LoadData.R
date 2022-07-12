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
##
## ---------------------------


# load packages -----------------------------------------------------------
library(here) #helps to use relative file paths within the R project
library(readxl) #useful for loading excel files into R
library(lubridate) #helps with date (day/month/year) data formating

# load data ---------------------------------------------------------------

# load strava data
strava_day <- readxl::read_excel(here("data/raw/strava_day.xlsx"))
strava_month <- readxl::read_excel(here("data/raw/strava_month.xlsx"))
strava_year <- readxl::read_excel(here("data/raw/strava_year.xlsx"))

#load trail data
trail_char <- readxl::read_excel(here("data/raw/trailcharacteristics.xlsx"))
trail_count <- readxl::read_excel(here("data/raw/trailcounterdata.xlsx"))

#load relational data table joining strava and trail count IDs - multiple sheets
join_IDs <- readxl::read_excel(here("data/raw/Trailhead_Strava_Crosswalk.xlsx"), sheet = 1)
join_IDs_allEdges <- readxl::read_excel(here("data/raw/Trailhead_Strava_Crosswalk.xlsx"), 
                                            sheet = 3)

#load weather data
weather <- readxl::read_excel(here("data/raw/weather_airquality.xlsx"))


# process data ------------------------------------------------------------

# remove extraneous rows from tables (check with tail(<objectname>) to see if NA)
join_IDs <- join_IDs[ -(27:30),]

# check data types; change to date (day/month/year)
strava_day$timeframe <- lubridate::as_date(strava_day$timeframe)
trail_count$date <- lubridate::as_date(trail_count$date)
weather$date <- lubridate::as_date(weather$date)
trail_count$wday <- lubridate::wday(trail_count$date, label=T)
strava_day$wday <- lubridate::wday(strava_day$timeframe, label=T)

# add fake day to make it easier to change to date format
strava_month$timeframe <- paste(strava_month$timeframe, "-01", sep="")
strava_month$timeframe <- lubridate::as_date(strava_month$timeframe)
strava_month$month <- lubridate::month(strava_month$timeframe)

#add fake month/day to make it easier to change date format
strava_year$timeframe <- paste(strava_year$timeframe, "-01-01", sep="")
strava_year$timeframe <- lubridate::as_date(strava_year$timeframe)
strava_year$year <- lubridate::year(strava_year$timeframe)


