## ---------------------------
## Script name: 03-GAM-Univariate.R
##
## Purpose of script: Starting simple exploring the utility of a
##                    Generalized Additive Model (GAM) that incorperates
##                    seasonality. We apply this model to Middle Cottonwood trail
##                    (counter ID 31).
##
## Author: Dr. Meridith L. Bartley
## Date Created: 2022-07-13
## ---------------------------
## Notes:
##
##
## ---------------------------


# load packages -----------------------------------------------------------
library(dplyr)
library(mgcv)


# Load Data ---------------------------------------------------------------
source("scripts/01-LoadData.R")

## Focusing on Middle Cottenwood Trail (Trail # 586, Counter ID # 31)

singleTrail <- trail_count %>%
  dplyr::filter(counterid == 31)

singleStrava <- strava_day %>%
  dplyr::filter(trailnumber == 586) %>% 
  # we want a single representation count value for multiple edges per day
  dplyr::group_by(timeframe) %>% 
  dplyr::mutate(max.count = max(totaltrips)) %>% 
  dplyr::select(-c(totaltrips, totalpeople, edge_uid)) %>% 
  dplyr::distinct()
# trail characteristics not pertinent for single trail analysis
# no variation within trail information

# singleChar <- trail_char %>%
#   dplyr::filter(trailnumber == 586)

## Combine Camera Counts, Strava Counts, Weather, Trail Characteristics

singleTrail_CountsCovariates <- singleTrail %>%
  dplyr::left_join(
    singleStrava,
    by = c(
      "date" = "timeframe",
      "trailnumber" = "trailnumber",
      "trailname" = "trailname",
      "wday" = "wday"
    )
  ) %>%
  dplyr::left_join(weather, by = c("date" = "date"))

## Add in a Time variable
# singleTrail_CountsCovariates <- transform(singleTrail_CountsCovariates, 
#                                           Time = as.numeric(date) / 1000)

singleTrail_CountsCovariates$yday <- lubridate::yday(singleTrail_CountsCovariates$date)

# Note: Strava is missing several days of data. What does this mean?

# visualize single trail counts -------------------------------------------

MidCotton_TS <- singleTrail_CountsCovariates %>% 
  dplyr::group_by(date, trailname) %>%
  dplyr::summarise(total_count = max(count), 
                   total_trips = max.count) %>%
  ggplot(aes(x = date, y = total_count)) +
  geom_area(aes(fill = factor(trailname)), 
            alpha = .5, position = "stack") +
  geom_line(aes(x = date, y = total_trips)) +
  guides(fill = 'none') +
  # ylim(0, 1000) +
  labs(
    y = "Count ",
    title = "Middle Cottonwood Daily Trail Traffic (by camera counts and Strava trips)",
    subtitle = "2021",
    fill = "Trail Name"
  )

MidCotton_TS

ggsave(
  filename = here::here("output/figures/MiddleCottonwood_TS.pdf"),
  plot = MidCotton_TS
)

# gamm with uncorrelated errors -------------------------------------------

gamm_uncorr <- gamm(count ~ s(yday) + 
                      s(as.integer(week), bs = 'cc', k = 8) +
                      s(as.integer(wday), bs = "cc", k = 7) +
                      s(daily_aqi_value) + 
                      max.count
                    ,
                data = singleTrail_CountsCovariates, 
                family = poisson)

summary(gamm_uncorr$gam)

layout(matrix(1:6, ncol = 2))
plot(gamm_uncorr$gam, scale = 0)
layout(1)

layout(matrix(1:2, ncol = 2))
acf(resid(gamm_uncorr$lme), lag.max = 36, main = "ACF")
pacf(resid(gamm_uncorr$lme), lag.max = 36, main = "pACF")
layout(1)


# gamm with correlated errors ---------------------------------------------
 
 ctrl <- list(niterEM = 0, 
              msVerbose = TRUE, 
              optimMethod="L-BFGS-B")
 
 ## AR(1)
 m.AR1 <- gamm(count ~ 
              # s(yday) +
              s(as.integer(week), bs = 'cc', k = 8) +
              s(as.integer(wday), bs = "cc", k = 7) +
              s(daily_aqi_value) + 
              max.count,
            data = singleTrail_CountsCovariates, 
            family = poisson,
            # correlation = corARMA(p = 1, q = 0),
            correlation = corAR1(form = ~ yday),
            control = ctrl)
 
 ## AR(1)
 m1 <- gamm(count ~
              # s(yday) + 
              s(as.integer(week), bs = 'cc', k = 8) +
              s(as.integer(wday), bs = "cc", k = 7) +
              s(daily_aqi_value) + 
              max.count,
            data = singleTrail_CountsCovariates, 
            family = poisson,
            correlation = corARMA(form = ~ 1|yday, p = 1, q = 0),
            # correlation = corAR1(),
            control = ctrl)
 
 ## AR(2)
 m2 <- gamm(count ~
              # s(yday) + 
              s(as.integer(week), bs = 'cc', k = 8) +
              s(as.integer(wday), bs = "cc", k = 7) +
              s(daily_aqi_value) + 
              max.count,
            data = singleTrail_CountsCovariates, 
            family = poisson,
            correlation = corARMA(form = ~ 1|yday, p = 2, q = 0),
            # correlation = corAR1(),
            control = ctrl)
 
## AR(3)
m3 <- gamm(count ~ 
             # s(yday) + 
             s(as.integer(week), bs = 'cc', k = 8) +
             s(as.integer(wday), bs = "cc", k = 7) +
             s(daily_aqi_value) + 
             max.count,
           data = singleTrail_CountsCovariates, 
           family = poisson,
           correlation = corARMA(form = ~ 1|yday, p = 3, q = 0),
           control = ctrl)

# compare models ----------------------------------------------------------

summary.gam(gamm_uncorr$gam)
summary.gam(m.AR1$gam)
summary.gam(m1$gam)
summary.gam(m2$gam)
summary.gam(m3$gam)

layout(matrix(1:2, ncol = 2))
res <- resid(m1$lme, type = "normalized")
acf(res, lag.max = 36, main = "ACF - AR(3) errors")
pacf(res, lag.max = 36, main = "pACF- AR(3) errors")
layout(1)


# predict new trail counts ------------------------------------------------
