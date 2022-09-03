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
## Notes: - This document is defunct. Please refer to 
##  "output/report/04-Analyze_MidCottonwood_GAMM.Rmd for analysis
## and "scripts/02-EDA.R" for plotting. 
##
##
## ---------------------------


# load packages -----------------------------------------------------------
library(dplyr)
library(gamm4)
library(here)
library(itsadug)
library(lubridate)
library(ggplot2)
library(magrittr)
library(mgcv)



# Load Data ---------------------------------------------------------------
source(here("scripts/01-LoadData.R"))

## Focusing on Middle Cottenwood Trail (Trail # 586, Counter ID # 31)

singleCount <- trail_count %>%
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

singleTrail <- singleCount %>%
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
# singleTrail <- transform(singleTrail, 
#                                           Time = as.numeric(date) / 1000)

singleTrail$yday <- lubridate::yday(singleTrail$date)

# Note: Strava is missing several days of data. What does this mean?

# visualize single trail counts -------------------------------------------

MidCotton_TS <- singleTrail %>% 
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
                      s(as.integer(wday), bs = "ps", k = 7) +
                      s(daily_aqi_value) + 
                      s(temp_max_f) +
                      max.count
                    ,
                data = singleTrail, 
                family = poisson)

summary(gamm_uncorr$gam)

layout(matrix(1:4, ncol = 2))
plot(gamm_uncorr$gam, scale = 0, shade = T)
layout(1)

# check for autocorrelation
layout(matrix(1:2, ncol = 2))
acf(resid(gamm_uncorr$lme, type = "normalized"), lag.max = 36, main = "ACF")
pacf(resid(gamm_uncorr$lme, type = "normalized"), lag.max = 36, main = "pACF")
layout(1)

#check distribution and autocorrelation structure of the residuals

gam.check(gamm_uncorr$gam)

# acf(resid(gamm_uncorr$gam), plot = F)

# plot fitted values

  ## note there are no fitted values where strava max.count = NA
real.vs.fitted <- data.frame(matrix(ncol = 3, 
                                    nrow = (length(singleTrail$count) + 
                                      length(gamm_uncorr$gam$fitted.values))))
colnames(real.vs.fitted) <- c("count", "date", "type")
real.vs.fitted$count <- c(singleTrail$count, 
                              gamm_uncorr$gam$fitted.values)
real.vs.fitted$date <- c(singleTrail$date,
                         singleTrail[-(c(which(is.na(singleTrail$max.count)), 
                                       which(is.na(singleTrail$temp_max_f)))), ]$date)
  
real.vs.fitted$type <- c(rep("Real", nrow(singleTrail)),
                         rep("Fitted", length(gamm_uncorr$gam$fitted.values)))
  
  ggplot(data = real.vs.fitted, aes(date, 
                           count, group = type, 
                           colour = type)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Time", y = "Trail Use",
       title = "Fit from GAM n.1")
  

# gamm - remove week variable ---------------------------------------------

  gamm_uncorr2 <- gamm(count ~ s(yday) + 
                        # s(as.integer(week), bs = 'cc', k = 8) +
                        s(as.integer(wday), bs = "ps", k = 7) +
                        s(daily_aqi_value) + 
                        s(temp_max_f) +
                        max.count,
                      data = singleTrail, 
                      family = poisson)
  
  summary(gamm_uncorr2$gam)
  
  layout(matrix(1:4, ncol = 2))
  plot(gamm_uncorr2$gam, scale = 0, shade = T)
  layout(1)
  
  # check for autocorrelation
  layout(matrix(1:2, ncol = 2))
  acf(resid(gamm_uncorr2$lme, type = "normalized"), lag.max = 36, main = "ACF")
  pacf(resid(gamm_uncorr$lme, type = "normalized"), lag.max = 36, main = "pACF")
  layout(1)
  
  #check distribution and autocorrelation structure of the residuals
  
  gam.check(gamm_uncorr2$gam)
  
  # acf(resid(gamm_uncorr$gam), plot = F)
  
  # plot fitted values
  
  ## note there are no fitted values where strava max.count = NA
  real.vs.fitted <- data.frame(matrix(ncol = 3, 
                                      nrow = (length(singleTrail$count) + 
                                                length(gamm_uncorr$gam$fitted.values))))
  colnames(real.vs.fitted) <- c("count", "date", "type")
  real.vs.fitted$count <- c(singleTrail$count, 
                            gamm_uncorr$gam$fitted.values)
  real.vs.fitted$date <- c(singleTrail$date,
                           singleTrail[-(c(which(is.na(singleTrail$max.count)), 
                                           which(is.na(singleTrail$temp_max_f)))), ]$date)
  
  real.vs.fitted$type <- c(rep("Real", nrow(singleTrail)),
                           rep("Fitted", length(gamm_uncorr$gam$fitted.values)))
  
  ggplot(data = real.vs.fitted, aes(date, 
                                    count, group = type, 
                                    colour = type)) +
    geom_line(size = 0.8) +
    theme_bw() +
    labs(x = "Time", y = "Trail Use",
         title = "Fit from GAM n.1")
  
  
# gamm4 with uncorrelated errors (check for same results) -----------------
  
  singleTrail.rmna <- singleTrail[-(c(which(is.na(singleTrail$max.count)), 
                                      which(is.na(singleTrail$temp_max_f)))), ]
  
  gamm4_uncorr <- gamm4:::gamm4(count ~ s(yday) + 
                                  s(as.integer(week), bs = 'cc', k = 8) +
                                  s(as.integer(wday), bs = "ps", k = 7) +
                                  s(daily_aqi_value) + 
                                  s(temp_max_f) +
                                  max.count,
                                data = singleTrail.rmna, 
                                family = poisson)
  
  summary(gamm4_uncorr$gam)
  
  layout(matrix(1:6, ncol = 2))
  plot(gamm4_uncorr$gam, scale = 0, shade = T)
  layout(1)
  
  # check for autocorrelation
  layout(matrix(1:2, ncol = 2))
  acf(resid(gamm4_uncorr$mer), lag.max = 36, main = "ACF")
  pacf(resid(gamm4_uncorr$mer), lag.max = 36, main = "pACF")
  layout(1)
  
  #check distribution and autocorrelation structure of the residuals
  
  gam.check(gamm4_uncorr$gam)
  

  # plot fitted values
  
  ## note there are no fitted values where strava max.count = NA
  real.vs.fitted4 <- data.frame(matrix(ncol = 3, 
                                      nrow = (length(singleTrail.rmna$count) + 
                                                length(gamm4_uncorr$gam$fitted.values))))
  colnames(real.vs.fitted4) <- c("count", "date", "type")
  real.vs.fitted4$count <- c(singleTrail.rmna$count, 
                            gamm4_uncorr$gam$fitted.values)
  real.vs.fitted4$date <- c(singleTrail.rmna$date,
                           singleTrail.rmna$date)
  
  real.vs.fitted4$type <- c(rep("Real", nrow(singleTrail.rmna)),
                           rep("Fitted", length(gamm4_uncorr$gam$fitted.values)))
  
  ggplot(data = real.vs.fitted4, aes(date, 
                                    count, group = type, 
                                    colour = type)) +
    geom_line(size = 0.8) +
    theme_bw() +
    labs(x = "Time", y = "Trail Use",
         title = "Fit from GAMM4 n.1")
  
  
  


# gamm with correlated errors ---------------------------------------------
 
 ctrl <- list(niterEM = 0, 
              msVerbose = TRUE, 
              optimMethod="L-BFGS-B")
 
 ## AR(1)
  m.AR1 <- gamm(count ~ 
                  s(yday) +
                  # s(as.integer(week), bs = 'cc', k = 8) +
                  s(as.integer(wday), bs = "ps", k = 7) +
                  s(daily_aqi_value) + 
                  s(temp_max_f) +
                  max.count,
                data = singleTrail, 
                family = poisson,
                # correlation = corARMA(p = 1, q = 0),
                correlation = corAR1(),
                control = ctrl)
  
  summary(m.AR1$gam)
  
  # check for autocorrelation
  layout(matrix(1:2, ncol = 2))
  acf(resid(m.AR1$lme, type = "normalized" ), lag.max = 36, main = "ACF")
  pacf(resid(m.AR1$lme, type = "normalized"), lag.max = 36, main = "pACF")
  layout(1)
  # r1 <- start_value_rho(m.AR1$gam, plot=TRUE)
  
  real.vs.fitted.mAR1 <- data.frame(matrix(ncol = 3, 
                                      nrow = (length(singleTrail$count) + 
                                                length(m.AR1$gam$fitted.values))))
  colnames(real.vs.fitted.mAR1) <- c("count", "date", "type")
  real.vs.fitted.mAR1$count <- c(singleTrail$count, 
                                 m.AR1$gam$fitted.values)
  real.vs.fitted.mAR1$date <- c(singleTrail$date,
                           singleTrail[-(c(which(is.na(singleTrail$max.count)), 
                                           which(is.na(singleTrail$temp_max_f)))), ]$date)
  
  real.vs.fitted.mAR1$type <- c(rep("Real", nrow(singleTrail)),
                           rep("Fitted", length(m.AR1$gam$fitted.values)))
  
  ggplot(data = real.vs.fitted.mAR1, aes(date, 
                                    count, group = type, 
                                    colour = type)) +
    geom_line(size = 0.8) +
    theme_bw() +
    labs(x = "Time", y = "Trail Use",
         title = "Fit from GAM AR(1)")
  
  
 ## ARMA(1)
  m1 <- gamm(count ~
               s(yday) +
               # s(as.integer(week), bs = 'cc', k = 8) +
               s(as.integer(wday), bs = "ps", k = 7) +
               s(daily_aqi_value) +
               s(temp_max_f) +
               max.count,
             data = singleTrail,
             family = poisson,
             correlation = corARMA( p = 1, q = 1),
             # correlation = corAR1(),
             control = ctrl)
  summary(m1$gam)
  
  # check for autocorrelation
  layout(matrix(1:2, ncol = 2))
  acf(resid(m1$lme, type = "normalized"), lag.max = 36, main = "ACF")
  pacf(resid(m1$lme, type = "normalized"), lag.max = 36, main = "pACF")
  layout(1)
#   r1 <- start_value_rho(m1$gam, plot=TRUE)
#   
#   ## ARMA(1) - updated rho
#   m1.rho <- bam(count ~
#                # s(yday) + 
#                s(as.integer(week), bs = 'cc', k = 8) +
#                s(as.integer(wday), bs = "ps", k = 7) +
#                s(daily_aqi_value) + 
#                s(temp_max_f) +
#                max.count,
#              data = singleTrail, 
#              family = poisson,
#              # correlation = corARMA(form = ~ 1|yday, p = 1, q = 0),
#              rho = r1, discrete = T)
#   # check for autocorrelation
#   layout(matrix(1:2, ncol = 2))
#   acf(resid(m1.rho), lag.max = 36, main = "ACF")
#   pacf(resid(m1.rho), lag.max = 36, main = "pACF")
#   layout(1)
#   
#  
 ## ARMA(2)
 m2 <- gamm(count ~
              s(yday) +
              # s(as.integer(week), bs = 'cc', k = 8) +
              s(as.integer(wday), bs = "ps", k = 7) +
              s(daily_aqi_value) +
              s(temp_max_f) +
              max.count,
            data = singleTrail,
            family = poisson,
            correlation = corARMA(p = 2),
            # correlation = corAR1(),
            control = ctrl)
 
 summary(m2$gam)
 
   # check for autocorrelation
   layout(matrix(1:2, ncol = 2))
   acf(resid(m2$gam), lag.max = 36, main = "ACF")
   pacf(resid(m2$gam), lag.max = 36, main = "pACF")
   layout(1)
   
   gam.check(m2$gam)
 
#  
# ## AR(3)
# m3 <- gamm(count ~ 
#              # s(yday) + 
#              s(as.integer(week), bs = 'cc', k = 8) +
#              s(as.integer(wday), bs = "cc", k = 7) +
#              s(daily_aqi_value) + 
#              s(temp_max_f) +
#              max.count,
#            data = singleTrail, 
#            family = poisson,
#            correlation = corARMA(form = ~ 1|yday, p = 3, q = 0),
#            control = ctrl)

# compare models ----------------------------------------------------------

summary.gam(gamm_uncorr$gam)
summary.gam(gamm_uncorr2$gam) #removed week covariate
summary.gam(m.AR1$gam)
summary.gam(m1$gam)
summary.gam(m2$gam)
summary.gam(m3$gam)

layout(matrix(1:2, ncol = 2))
res <- resid(m.AR1$lme, type = "normalized")
acf(res, lag.max = 36, main = "ACF - AR(1) errors")
pacf(res, lag.max = 36, main = "pACF- AR(1) errors")
layout(1)


# predict new trail counts ------------------------------------------------
