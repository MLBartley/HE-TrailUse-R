## ---------------------------
## Script name: Follow Up Work - Predictions at new (out-of-sample) trails
##
## Purpose of script: This script will obtain trail use estimates for all out-of-sample 
## trails, including new trails (7) provided by Headwaters Economics (Megan Lawson). 
##
## Author: Dr. Meridith L. Bartley
## Date Created: 2023-01-04
## ---------------------------
## Notes:
##      - load new data
##      - create out-of-sample trail data frame
##      - check model run for in sample data
##      - run out of sample trails for predictions     
##      - update prediction estimates table and any figures
##      - write up summary and submit to Megan at HE
## ---------------------------
  

# libraries ---------------------------------------------------------------

library(here) #helps to use relative file paths within the R project
library(readxl) #useful for loading excel files into R
library(tidyverse)

`%notin%` <- Negate(`%in%`) #useful for exploring data

# Load --------------------------------------------------------------------

## load "Missing" Trail data

newtrail_names <- readxl::read_excel(here("data/raw/follow-up/Missing_Trail_Segments.xlsx"))
newtrail_spatial <- sf::st_read(dsn = here("data/raw/follow-up/Missing_Trails/"))

##load other out-of-sample data frame

load(here("data/processed/predictNew.Rdata"))

##what do we need to match?
colnames(predict.New)

## load updated Strava crosswalk file (to update Upper Sypes)

# strava_update <- readxl::read_excel(here("data/raw/follow-up/Trailhead_Strava_Crosswalk.xlsx"))

## load original strava data file
strava_day <- readxl::read_excel(here("data/raw/strava_day.xlsx"))

## Load GAMM Model GI (model to use for out of sample predicitons)
load(file=here("output/models/allT_subsection_gamm_GI.rda"))


# Clean -----------------------------------------------------------------

## change column names
colnames(newtrail_spatial) <- c("trailname", "subsectionname", "geometry")

## change "upper" to "upper sypes" to avoid confusion with upper corbly gulch
newtrail_spatial$subsectionname[5] <- "Upper Sypes"

# ## correct North Cottonwood Access nameing
# newtrail_spatial$trailname[4] <- "North Cottonwood Access"
# newtrail_spatial$subsectionname[4] <- "North Cottonwood Access"

## remove North Cottonwood Access - already in original model
newtrail_spatial <- newtrail_spatial[-4, ]

## updated strava_data file to split Sypes Canyon into upper and lower
## edge_ids are copy/pasted from "raw/follow-up/Trailhead_Strava_Crosswalk.xlsx"
upper_edgeID <- c(348369814,
                  348369815,
                  348369826,
                  348369828)

lower_edgeID <- c(348369740,
                   348369742,
                   348369743,
                   348369744,
                   348369745,
                   348369749,
                   348369752)

strava_day_splitSypes <- strava_day

strava_day_splitSypes[which(strava_day_splitSypes$edge_uid %in% upper_edgeID), 
                      "subsectionname"] <- "Upper Sypes"

strava_day_splitSypes[which(strava_day_splitSypes$edge_uid %in% lower_edgeID), 
                      "subsectionname"] <- "Lower Sypes"

strava_day <- strava_day_splitSypes

save(strava_day, 
     file = here("data/processed/strava_day_updated.Rdata"))

### Once strava_day is updated I then reran "output/scripts/01-LoadData.R" with the 
###. updated strava_day file to create an updated allData file

## load allData (combines trail info, strava, trail cameras, etc)
load(here("data/processed/allData_updated.Rdata"))

## Connect Middle Cottonwood to Truman subsections in AllData

allData.update <- allData

allData.update[which(allData.update$subsectionname %in% c("Middle Cottonwood to Bostwick",
                                                          "Bostwick to Truman")), "subsectionname"] <- "Middle Cottonwood to Truman"

## Connect Truman to Ross Pass
allData.update[which(allData.update$subsectionname %in% c("Truman to Jones",
                                                          "Jones to Ross Pass")), "subsectionname"] <- "Truman to Ross Pass"

## match subsectionname to allData
subsections.match <- newtrail_spatial$subsectionname[which(newtrail_spatial$subsectionname %in%
                                                       allData.update$subsectionname)]
subsections.nomatch <- newtrail_spatial$subsectionname[which(newtrail_spatial$subsectionname %notin%
                                                      allData.update$subsectionname)]
# Combine -----------------------------------------------------------------

predict.Missing <- allData.update %>% 
  dplyr::filter(subsectionname %in% subsections.match)  %>%
  dplyr::select(c("trailnumber", 
                  "trailname",
                  "subsectionname",
                  "subsectionF",
                  "date", 
                  "wday",
                  "month",
                  "yday",
                  "max.count", 
                  # "max.camera",
                  "precipitation_in", 
                  "daily_aqi_value", 
                  "temp_max_f",
                  "total_traveltime", 
                  "totallength_miles"
                  ## add more if used in model
  )) %>% 
  tidyr::drop_na(subsectionF)

# Estimate ----------------------------------------------------------------

p_GI_missing <- as_tibble(predict(gamm_modGI$gam, 
                              predict.Missing, 
                              exclude = c("s(subsectionF)", 
                                          "s(yday):subsectionFBridger Ridge - All",
                                          # "s(yday):subsectionFBaldy to Bridger",
                                          "s(yday):subsectionFBenchmark Rd",
                                          # "s(yday):subsectionFBridger", 
                                          # "s(yday):subsectionFBridger to Ross Pass",
                                          "s(yday):subsectionFCarroll Creek",
                                          "s(yday):subsectionFCollege M",
                                          "s(yday):subsectionFCollege M to Sypes",
                                          # "s(yday):subsectionFCorbly Gulch",
                                          "s(yday):subsectionFLower",
                                          "s(yday):subsectionFUpper",
                                          "s(yday):subsectionFEast Bridger North",
                                          "s(yday):subsectionFEast Bridger South",
                                          "s(yday):subsectionFFairy Creek",
                                          "s(yday):subsectionFHorsethief Mountain",
                                          "s(yday):subsectionFJohnson Canyon Jeep Trail",
                                          "s(yday):subsectionFLower Shafthouse",
                                          "s(yday):subsectionFM to Baldy",
                                          "s(yday):subsectionFMiddle Cottonwood",
                                          "s(yday):subsectionFNorth Cottonwood Access",
                                          "s(yday):subsectionFNorth Cottonwood to Johnson Canyon",
                                          "s(yday):subsectionFRaptor View",
                                          "s(yday):subsectionFRoss Pass",
                                          # "s(yday):subsectionFRoss Pass to Sacagawea Peak",
                                          "s(yday):subsectionFSacagawea Pass",
                                          "s(yday):subsectionFSteep Way",
                                          "s(yday):subsectionFSypes Canyon",
                                          "s(yday):subsectionFTruman Gulch" ),
                              se.fit = TRUE, 
                              type = "response")) %>%
  rename(fit_GI = fit, se_GI = se.fit)

# Table -------------------------------------------------------------------

predict.Missing_df <- bind_cols(predict.Missing, p_GI_missing) %>%
  tidyr::pivot_longer(fit_GI:se_GI, names_sep = '_',
                      names_to = c('variable', 'model')) %>%
  tidyr::pivot_wider(names_from = variable, values_from = value) %>%
  mutate(upr_ci = fit + (2 * se), lwr_ci = fit - (2 * se)) 
# %>%
  # tidyr::drop_na(total_traveltime)

# Save --------------------------------------------------------------------

write.csv(predict.Missing_df, 
          file = here("output/predictions/predict_ModelGI_outofsample.csv"), 
          row.names = FALSE)
