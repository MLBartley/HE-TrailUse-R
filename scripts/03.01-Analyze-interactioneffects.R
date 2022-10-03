## ---------------------------
## Script name: 
##
## Purpose of script: Explore interaction effect with yday/precipitation_in
##
## Author: Dr. Meridith L. Bartley
## Date Created: 2022-09-01
## ---------------------------
## Notes: Not included in final report. Keeping file for
## potential future applications.
##   
##
## ---------------------------
library("here")
library("mgcv")

source(here("scripts/01-LoadData.R"))

## Model Checking function
tsDiagGamm <- function(x, timevar, observed, f = 0.3, type = "normalized") {
  resi <- resid(x$lme, type = type)
  fits <- fitted(x$lme)
  on.exit(layout(1))
  layout(matrix(1:6, ncol = 3, byrow = TRUE))
  plot(resi ~ fits, ylab = "Normalized Residuals",
       xlab = "Fitted Values", main = "Fitted vs. Residuals")
  lines(lowess(x = fits, y = resi, f = f), col = "blue",
        lwd = 2)
  plot(resi ~ timevar, ylab = "Normalized Residuals",
       xlab = "Time", main = "Time series of residuals")
  lines(lowess(x = timevar, y = resi, f = f), col = "blue", lwd = 2)
  plot(observed ~ fits, ylab = "Observed",
       xlab = "Fitted Values", main = "Fitted vs. Observed",
       type = "n")
  abline(a = 0, b = 1, col = "red")
  points(observed ~ fits)
  lines(lowess(x = fits, y = observed, f = f), col = "blue",
        lwd = 2)
  hist(resi, freq = FALSE, xlab = "Normalized Residuals")
  qqnorm(resi)
  qqline(resi)
  acf(resi, main = "ACF of Residuals")
}

sub.number <- length(unique(allTrail$subsectionF))

#uncomment depending on whether you want all models to run (will take several hours)
runall = T
# runall = F


# Model G - Global Smoother -----------------------------------------------
if(runall){
  ## we are fitting several versions of each Model (here, model G) with increasing temporal structure on the errors. We will report the best one in this report, but provide the code for multiple specification in case a different model is needed in the future. 
  
  
  ## explore gam() approach - OK if other parameters take care of
  ## temporal dependence (don't seem to)
  

  ## gamm model - check if this takes care of temporal dependence as is (w/o temporal autocorr sturcture on errors)
  gamm_modG_te <- gamm(max.camera ~
                          # s(yday, bs="cc", k = 30) +
                          # trailnameF + 
                          te(yday, precipitation_in, 
                             bs = c("cc", "tp"), k = c(10, 10), 
                             np = FALSE) +
                          s(subsectionF,
                            bs="re",
                            # by = trailnameF,
                            k=sub.number) +
                          s(month, k = 7) +
                          s(wday,
                            bs = "cc", k = 7) +
                          s(daily_aqi_value) +
                          s(temp_max_f) +
                          # s(precipitation_in) +
                          s(totallength_miles) + 
                          total_traveltime +
                          max.count,
                        knots = list(yday = c(0,365)),
                        method = 'REML',
                        # correlation = corARMA(form = ~yday|subsectionF, p = 5), 
                        data = allTrail, 
                        family = poisson#, 
                        # niterPQL = 20
  )
  
  arma_res_G <- forecast::auto.arima(resid(gamm_modG_te$lme, 
                                           type = "normalized"), 
                                     stationary = TRUE,
                                     seasonal = T)
  
  arma_res_G$coef
  
  gamm_modGAR1_te <- gamm(max.camera ~
                              # s(yday, bs="cc") +
                              te(yday, precipitation_in, 
                                 bs = c("cc", "tp"), k = c(10, 10), 
                                 np = FALSE) +
                              # trailnameF + 
                              s(subsectionF,
                                bs="re",
                                # by = trailnameF,
                                k=sub.number) +
                              s(month, k = 7) +
                              s(wday,
                                bs = "cc", k = 7) +
                              s(daily_aqi_value) +
                              s(temp_max_f) +
                              # s(precipitation_in) +
                              s(totallength_miles) + 
                              total_traveltime +
                              max.count,
                            knots = list(yday = c(0,365)),
                            method = 'REML',
                            correlation = corAR1(form = ~yday|subsectionF),
                            data = allTrail, 
                            family = poisson#, 
                            # niterPQL = 20
  )
  
  gamm_modGARMA_te <- gamm(max.camera ~
                              # s(yday, bs="cc") +
                             te(yday, precipitation_in, 
                                bs = c("cc", "tp"), k = c(10, 5), 
                                np = FALSE) + 
                              # trailnameF + 
                              s(subsectionF,
                                bs="re",
                                # by = trailnameF,
                                k=sub.number) +
                              s(month, k = 7) +
                              s(wday,
                                bs = "cc", k = 7) +
                              s(daily_aqi_value) +
                              s(temp_max_f) +
                              # s(precipitation_in) +
                              s(totallength_miles) + 
                              total_traveltime +
                              max.count,
                            knots = list(yday = c(0,365)),
                            method = 'REML',
                            correlation = corARMA(form = ~yday|subsectionF,
                                                  p = 1, q = 2),
                            data = allTrail, 
                            family = poisson, 
                            niterPQL = 30
  )
  
  # save(gam_modG_, file=here("output/models/allT_subsection_gam_G_te.rda"),
  #      compress='xz')  
  
  save(gamm_modG_te, file=here("output/models/allT_subsection_gamm_G_te.rda"),
       compress='xz') 
  
  save(gamm_modGAR1_te, file=here("output/models/allT_subsection_gamm_G-AR1_te.rda"),
       compress='xz')
  
  save(gamm_modGARMA_te, file=here("output/models/allT_subsection_gamm_G-ARMA_te.rda"),
       compress='xz')
}

# load(here("output/models/allT_subsection_gam_G_te.rda"))
load(here("output/models/allT_subsection_gamm_G_te.rda"))
load(here("output/models/allT_subsection_gamm_G-AR1_te.rda"))
load(here("output/models/allT_subsection_gamm_G-ARMA_te.rda"))


summary(gamm_modG_te$gam)
summary(gamm_modGAR1_te$gam)
summary(gamm_modGARMA_te$gam)

allTrail_G <- allTrail %>% 
  tidyr::drop_na(c('daily_aqi_value', "totallength_miles"))


with(allTrail_G, tsDiagGamm(gamm_modGARMA_te, timevar = yday,
                            observed = max.camera))

layout(matrix(1:2, ncol = 2))
acf(resid(gamm_modGARMA_te$lme, type = "normalized"), 
    lag.max = 36, main = "ACF")
pacf(resid(gamm_modGARMA_te$lme, type = "normalized"),
     lag.max = 36, main = "pACF")
layout(1)

gratia::draw(gamm_modGARMA_te$gam)

rg <- gratia::rootogram(gamm_modGARMA_te$gam)
draw(rg)

vis.gam(gamm_modGARMA_te$gam, 
        view = c('yday', 'precipitation_in'),
        # theta = 45, phi = 15, 
        type = 'response',
        # se = -1, 
        # color = 'bw'
)

pvisgam(gamm_modGARMA_te$gam,
        view = c('yday', 'precipitation_in'),
        # select=6,
        # zlim=c(-20,10),
        main="pvisgam plot")

p_G_none <- as_tibble(predict(gamm_modG_te$gam, 
                              predict.All, 
                              se.fit = TRUE, 
                              type = "response")) %>%
  rename(fit_none = fit, se_none = se.fit)
p_G_AR1 <- as_tibble(predict(gamm_modG_AR1_te$gam, 
                             predict.All, 
                             se.fit = TRUE, 
                             type = "response")) %>%
  rename(fit_AR1 = fit, se_AR1 = se.fit)
p_G_ARMA <- as_tibble(predict(gamm_modG_ARMA_te$gam, 
                              predict.All, 
                              se.fit = TRUE, 
                              type = "response")) %>%
  rename(fit_ARMA = fit, se_ARMA = se.fit)


pred_all_G_te <- bind_cols(predict.All, p_G_none, p_G_AR1, p_G_ARMA) %>%
  tidyr::pivot_longer(fit_none:se_ARMA, names_sep = '_',
                      names_to = c('variable', 'correlation')) %>%
  tidyr::pivot_wider(names_from = variable, values_from = value) %>%
  mutate(upr_ci = fit + (2 * se), lwr_ci = fit - (2 * se))

compare_modG <- ggplot(mapping = aes(x = yday, y = max.camera)) +
  geom_ribbon(data = pred_all_G,
              mapping = aes(ymin = lwr_ci, ymax = upr_ci, x = yday, fill = factor(correlation)),
              inherit.aes = FALSE, alpha = 0.2) +
  geom_point(data = allTrail, aes(colour = as.factor(wday))) +
  geom_line(data = pred_all_G, aes(y = fit, x = yday, colour2 = factor(correlation)),
            size = 1) %>%
  relayer::rename_geom_aes(new_aes = c("colour" = "colour2")) +
  scale_colour_brewer(palette = "Set1", aesthetics = "colour", name = "Day of Week") +
  scale_colour_OkabeIto(aesthetics = "colour2", name = "Model") +
  scale_fill_OkabeIto(name = "Model") +
  coord_cartesian(xlim = c(0, 365), ylim = c(0, 1000)) +
  facet_wrap(~subsectionF, drop = T, ncol = 3) +
  labs(title = "Model G",
       subtitle = "How prediction varies with different temporal correlation structures")

compare_high_G <-  ggplot(mapping = aes(x = yday, y = max.camera)) +
  geom_ribbon(data = pred_all_G %>% filter(subsectionF %in% high_use),
              mapping = aes(ymin = lwr_ci, ymax = upr_ci, x = yday, fill = factor(correlation)),
              inherit.aes = FALSE, alpha = 0.2) +
  geom_point(data = allTrail %>% filter(subsectionF %in% high_use), aes(colour = as.factor(wday))) +
  geom_line(data = pred_all_G  %>% filter(subsectionF %in% high_use),
            aes(y = fit, x = yday, colour2 = factor(correlation)),
            alpha= 0.7,
            size = 1) %>%
  relayer::rename_geom_aes(new_aes = c("colour" = "colour2")) +
  scale_colour_brewer(palette = "Set1", aesthetics = "colour", name = "Day of Week") +
  scale_colour_OkabeIto(aesthetics = "colour2", name = "Model") +
  scale_fill_OkabeIto(name = "Model") +
  coord_cartesian(xlim = c(0, 365), ylim = c(0, 1000)) +
  facet_wrap(~subsectionF, drop = T, ncol = 3) +
  labs(title = "Model G",
       subtitle = "How prediction varies with different temporal correlation structures")

# ggsave(filename = here::here("output/figures/high_pred_compareG.pdf"),
#        plot = compare_high_G,
#        height = 8.5, 
#        width = 13,
#        dpi=700)

compare_low_G <-  ggplot(mapping = aes(x = yday, y = max.camera)) +
  geom_ribbon(data = new_data_bases %>% filter(subsectionF %notin% high_use),
              mapping = aes(ymin = lwr_ci, ymax = upr_ci, x = yday, fill = factor(model)),
              inherit.aes = FALSE, alpha = 0.2) +
  geom_point(data = allTrail %>% filter(subsectionF %notin% high_use), aes(colour = as.factor(wday))) +
  geom_line(data = new_data_bases  %>% filter(subsectionF %notin% high_use),
            aes(y = fit, x = yday, colour2 = factor(model)),
            alpha= 0.7,
            size = 1) %>%
  relayer::rename_geom_aes(new_aes = c("colour" = "colour2")) +
  scale_colour_brewer(palette = "Set1", aesthetics = "colour", name = "Day of Week") +
  scale_colour_OkabeIto(aesthetics = "colour2", name = "Model") +
  scale_fill_OkabeIto(name = "Model") +
  coord_cartesian(xlim = c(0, 365), ylim = c(0, 300)) +
  facet_wrap(~subsectionF, drop = T, ncol =3) +
  labs(title = "Extrapolation",
       subtitle = "How prediction varies with different model specifications")

# ggsave(filename = here::here("output/figures/low_pred_compareG.pdf"),
#        plot = compare_low_G,
#        height = 8.5, 
#        width = 13,
#        dpi=700)


# Model GS ----------------------------------------------------------------
if(runall){
  gamm_modGS_te <- gamm(max.camera ~ 
                       # s(yday, bs="cc", k = 10) +
                         te(yday, precipitation_in, 
                            bs = c("cc", "tp"), k = c(10, 10), 
                            np = FALSE) +
                       s(yday, subsectionF,
                         bs="fs", k = 25, xt=list(bs="cc")) +
                       s(subsectionF, bs = "re") +
                       # trailnameF +
                       s(month, k = 7) +
                       s(wday, bs = "cc", k = 7) +
                       s(daily_aqi_value) +
                       s(temp_max_f) +
                       # s(precipitation_in) +
                       s(totallength_miles) + 
                       total_traveltime +
                       max.count,
                     knots = list(yday = c(0,365)),
                     data = allTrail, 
                     method = "REML",
                     family = poisson#, 
                     # niterPQL = 20
  )
  
  arma_res_GS <- forecast::auto.arima(resid(gamm_modGS_te$lme, type = "normalized"),
                                      stationary = TRUE,
                                      seasonal = T)
  
  arma_res_GS$coef
  
  gamm_modGS_AR1_te <- gamm(max.camera ~ 
                           # s(yday, bs="cc", k = 10) +
                             te(yday, precipitation_in, 
                                bs = c("cc", "tp"), k = c(10, 10), 
                                np = FALSE) +
                           s(yday, subsectionF,
                             bs="fs", k = 25, xt=list(bs="cc")) +
                           s(subsectionF, bs = "re") +
                           # trailnameF +
                           s(month, k = 7) +
                           s(wday,
                             bs = "cc", k = 7) +
                           s(daily_aqi_value) +
                           s(temp_max_f) +
                           # s(precipitation_in) +
                           s(totallength_miles) + 
                           total_traveltime +
                           max.count,
                         knots = list(yday = c(0,365)),
                         data = allTrail, 
                         method = "REML", 
                         correlation = corAR1(form = ~yday|subsectionF),
                         family = poisson#, 
                         # niterPQL = 20
  )
  
  gamm_modGS_ARMA_te <- gamm(max.camera ~ 
                            # s(yday, bs="cc", k = 10) +
                              te(yday, precipitation_in, 
                                 bs = c("cc", "tp"), k = c(10, 10), 
                                 np = FALSE) +
                            s(yday, subsectionF,
                              bs="fs", k = 25, xt=list(bs="cc")) +
                            s(subsectionF, bs = "re") +
                            # trailnameF +
                            s(month, k = 7) +
                            s(wday,
                              bs = "cc", k = 7) +
                            s(daily_aqi_value) +
                            s(temp_max_f) +
                            # s(precipitation_in) +
                            s(totallength_miles) + 
                            # total_traveltime +
                            max.count,
                          knots = list(yday = c(0,365)),
                          data = allTrail, 
                          method = "REML", 
                          correlation = corARMA(form = ~yday|subsectionF, 
                                                p = 2, q = 2),
                          family = poisson #, 
                          # niterPQL = 20
  )
  
  save(gamm_modGS_te, file=here("output/models/allT_subsection_gamm_GS_te.rda"),
       compress='xz')
  
  save(gamm_modGS_AR1_te, file=here("output/models/allT_subsection_gamm_GS_AR1_te.rda"),
       compress='xz')
  
  save(gamm_modGS_ARMA_te, file=here("output/models/allT_subsection_gamm_GS_ARMA_te.rda"),
       compress='xz')
}

load(here("output/models/allT_subsection_gamm_GS_te.rda"))
load(here("output/models/allT_subsection_gamm_GS_AR1_te.rda"))
# load(here("output/models/allT_subsection_gamm_GS_ARMA.rda"))


summary(gamm_modGS_te$gam)
summary(gamm_modGS_AR1_te$gam)
summary(gamm_modGS_ARMA_te$gam)


with(allTrail_G, tsDiagGamm(gamm_modGS_ARMA_te, 
                            timevar = yday,
                            observed = max.camera))

layout(matrix(1:2, ncol = 2))
acf(resid(gamm_modGS_ARMA_te$lme, type = "normalized"), 
    lag.max = 36, main = "ACF")
pacf(resid(gamm_modGS_ARMA_te$lme, type = "normalized"),
     lag.max = 36, main = "pACF")
layout(1)

gratia::draw(gamm_modGS_ARMA_te$gam)

rg <- gratia::rootogram(gamm_modGS_ARMA_te$gam)
draw(rg)

vis.gam(gamm_modGS_ARMA_te$gam, 
        view = c('yday', 'precipitation_in'),
        # theta = 45, phi = 15, 
        type = 'response',
        # se = -1, 
        # color = 'bw'
)

pvisgam(gamm_modGS_ARMA_te$gam,
        view = c('yday', 'precipitation_in'),
        # select=6,
        # zlim=c(-20,10),
        main="pvisgam plot")

p_GS_none <- as_tibble(predict(gamm_modGS_te$gam, 
                              predict.All, 
                              se.fit = TRUE, 
                              type = "response")) %>%
  rename(fit_none = fit, se_none = se.fit)
p_GS_AR1 <- as_tibble(predict(gamm_modGS_AR1_te$gam, 
                             predict.All, 
                             se.fit = TRUE, 
                             type = "response")) %>%
  rename(fit_AR1 = fit, se_AR1 = se.fit)
p_GS_ARMA <- as_tibble(predict(gamm_modGS_ARMA_te$gam, 
                              predict.All, 
                              se.fit = TRUE, 
                              type = "response")) %>%
  rename(fit_ARMA = fit, se_ARMA = se.fit)


pred_all_GS_te <- bind_cols(predict.All, p_GS_none,
                            p_GS_AR1, p_GS_ARMA) %>%
  tidyr::pivot_longer(fit_none:se_ARMA, names_sep = '_',
                      names_to = c('variable', 'correlation')) %>%
  tidyr::pivot_wider(names_from = variable, values_from = value) %>%
  mutate(upr_ci = fit + (2 * se), lwr_ci = fit - (2 * se))

compare_modGS <- ggplot(mapping = aes(x = yday, y = max.camera)) +
  geom_ribbon(data = pred_all_GS,
              mapping = aes(ymin = lwr_ci, ymax = upr_ci, x = yday, fill = factor(correlation)),
              inherit.aes = FALSE, alpha = 0.2) +
  geom_point(data = allTrail, aes(colour = as.factor(wday))) +
  geom_line(data = pred_all_GS, aes(y = fit, x = yday, colour2 = factor(correlation)),
            size = 1) %>%
  relayer::rename_geom_aes(new_aes = c("colour" = "colour2")) +
  scale_colour_brewer(palette = "Set1", aesthetics = "colour", name = "Day of Week") +
  scale_colour_OkabeIto(aesthetics = "colour2", name = "Model") +
  scale_fill_OkabeIto(name = "Model") +
  coord_cartesian(xlim = c(0, 365), ylim = c(0, 1000)) +
  facet_wrap(~subsectionF, drop = T, ncol = 3) +
  labs(title = "Model GS",
       subtitle = "How prediction varies with different temporal correlation structures")

compare_high_GS <-  ggplot(mapping = aes(x = yday, y = max.camera)) +
  geom_ribbon(data = pred_all_GS %>% filter(subsectionF %in% high_use),
              mapping = aes(ymin = lwr_ci, ymax = upr_ci, x = yday, fill = factor(correlation)),
              inherit.aes = FALSE, alpha = 0.2) +
  geom_point(data = allTrail %>% filter(subsectionF %in% high_use), aes(colour = as.factor(wday))) +
  geom_line(data = pred_all_GS  %>% filter(subsectionF %in% high_use),
            aes(y = fit, x = yday, colour2 = factor(correlation)),
            alpha= 0.7,
            size = 1) %>%
  relayer::rename_geom_aes(new_aes = c("colour" = "colour2")) +
  scale_colour_brewer(palette = "Set1", aesthetics = "colour", name = "Day of Week") +
  scale_colour_OkabeIto(aesthetics = "colour2", name = "Model") +
  scale_fill_OkabeIto(name = "Model") +
  coord_cartesian(xlim = c(0, 365), ylim = c(0, 1000)) +
  facet_wrap(~subsectionF, drop = T, ncol = 3) +
  labs(title = "Model GS",
       subtitle = "How prediction varies with different temporal correlation structures")

# ggsave(filename = here::here("output/figures/high_pred_compareG.pdf"),
#        plot = compare_high_G,
#        height = 8.5, 
#        width = 13,
#        dpi=700)

compare_low_GS <-  ggplot(mapping = aes(x = yday, y = max.camera)) +
  geom_ribbon(data = pred_all_GS %>% filter(subsectionF %notin% high_use),
              mapping = aes(ymin = lwr_ci, ymax = upr_ci,
                            x = yday, fill = factor(correlation)),
              inherit.aes = FALSE, alpha = 0.2) +
  geom_point(data = allTrail %>% filter(subsectionF %notin% high_use), aes(colour = as.factor(wday))) +
  geom_line(data = pred_all_GS  %>% filter(subsectionF %notin% high_use),
            aes(y = fit, x = yday, colour2 = factor(correlation)),
            alpha= 0.7,
            size = 1) %>%
  relayer::rename_geom_aes(new_aes = c("colour" = "colour2")) +
  scale_colour_brewer(palette = "Set1", aesthetics = "colour", name = "Day of Week") +
  scale_colour_OkabeIto(aesthetics = "colour2", name = "Model") +
  scale_fill_OkabeIto(name = "Model") +
  coord_cartesian(xlim = c(0, 365), ylim = c(0, 300)) +
  facet_wrap(~subsectionF, drop = T, ncol =3) +
  labs(title = "Extrapolation",
       subtitle = "How prediction varies with different model specifications")

# ggsave(filename = here::here("output/figures/low_pred_compareG.pdf"),
#        plot = compare_low_G,
#        height = 8.5, 
#        width = 13,
#        dpi=700)


# Model GI ----------------------------------------------------------------



if(runall){
  gamm_modGI_te <- gamm(max.camera ~
                       # s(yday, m=2, bs="cc") +
                       te(yday, precipitation_in, 
                          bs = c("cc", "tp"), k = c(10, 10), 
                          np = FALSE) +
                       s(yday, by = subsectionF,
                         m=1, bs="cc") +
                       s(subsectionF,
                         bs="re",
                         # by = trailnameF,
                         k=sub.number) +
                       # trailnameF +
                       s(month, k = 7) +
                       s(wday,
                         bs = "cc", k = 7) +
                       s(daily_aqi_value) +
                       s(temp_max_f) +
                       # s(precipitation_in) +
                       s(totallength_miles) + 
                       total_traveltime +
                       max.count,
                     knots = list(yday = c(0,365)),
                     data = allTrail,
                     family = poisson)
  
  arma_res_GI <- forecast::auto.arima(resid(gamm_modGI_te$lme, 
                                            type = "normalized"),
                                      stationary = TRUE,
                                      seasonal = TRUE)
  
  arma_res_GI$coef
  
  gamm_modGI_AR1_te3 <- gamm(max.camera ~
                           # s(yday, m=2, bs="cc") +
                             te(yday, precipitation_in, 
                                # daily_aqi_value,
                                bs = c("cc", "tp"
                                       # , "tp"
                                       ), k = c(10, 10
                                                # , 10
                                                ), 
                                np = FALSE) +
                           s(yday, by = subsectionF,
                             m=1, bs="cc") +
                           s(subsectionF,
                             bs="re",
                             # by = trailnameF,
                             k=sub.number) +
                           # trailnameF +
                           s(month, k = 7) +
                           s(wday,
                             bs = "cc", k = 7) +
                           s(daily_aqi_value) +
                           s(temp_max_f) +
                           # s(precipitation_in) +
                           s(totallength_miles) + 
                           total_traveltime +
                           max.count,
                         knots = list(yday = c(0,365)),
                         correlation = corAR1(form = ~yday|subsectionF),
                         data = allTrail,
                         family = poisson)
  
  # gamm_modGI_ARMA_te <- gamm(max.camera ~
  #                          s(yday, m=2, bs="cc") +
  #                          s(yday, by = subsectionF,
  #                            m=1, bs="cc") +
  #                          s(subsectionF,
  #                            bs="re",
  #                            # by = trailnameF,
  #                            k=25) +
  #                          # trailnameF +
  #                          s(month, k = 7) +
  #                          s(wday,
  #                            bs = "cc", k = 7) +
  #                          s(daily_aqi_value) +
  #                          s(temp_max_f) +
  #                          s(precipitation_in) +
  #                          s(totallength_miles) + 
  #                       total_traveltime +
  #                           max.count,
  #                         knots = list(yday = c(0,365)),
  #                          correlation = corARMA(form = ~yday|subsectionF, 
  #                                                p = 1, q = 1),
  #                        data = allTrail,
  #                        family = poisson)
  
  save(gamm_modGI_te, 
       file=here("output/models/allT_subsection_gamm_GI_te.rda"),
       compress='xz')
  
  save(gamm_modGI_AR1_te, 
       file=here("output/models/allT_subsection_gamm_GI-AR1_te.rda"),
       compress='xz')
  
  # save(gamm_modGI_ARMA_sub, 
  #      file=here("output/models/allT_subsection_gamm_GI-ARMA.rda"),
  #      compress='xz')
}

load(file=here("output/models/allT_subsection_gamm_GI_te.rda"))

load(file=here("output/models/allT_subsection_gamm_GI-AR1_te.rda"))


summary(gamm_modGI_AR1_te$gam)


gratia::draw(gamm_modGI_AR1_te)
# gratia::appraise(bam_modGI_sub)

allTrail_G <- allTrail %>% 
  tidyr::drop_na(c('daily_aqi_value', "totallength_miles"))

with(allTrail_G, tsDiagGamm(gamm_modGI_AR1_te, timevar = yday,
                            observed = max.camera))


layout(matrix(1:2, ncol = 2))
acf(resid(gamm_modGI_AR1_te$lme, type = "normalized"),
    lag.max = 36, main = "ACF")
pacf(resid(gamm_modGI_AR1_te$lme, type = "normalized"),
     lag.max = 36, main = "pACF")
layout(1)

rg <- gratia::rootogram(gamm_modGI_AR1_te$gam)
draw(rg)



# 3-way interaction -------------------------------------------------------

gamm_modGI_AR1_te3 <- gamm(max.camera ~
                             # s(yday, m=2, bs="cc") +
                             te(yday, precipitation_in, daily_aqi_value,
                                bs = c("cc", "tp", "tp"), k = c(10, 10, 10), 
                                np = FALSE) +
                             s(yday, by = subsectionF,
                               m=1, bs="cc") +
                             s(subsectionF,
                               bs="re",
                               # by = trailnameF,
                               k=sub.number) +
                             # trailnameF +
                             s(month, k = 7) +
                             s(wday,
                               bs = "cc", k = 7) +
                             # s(daily_aqi_value) +
                             s(temp_max_f) +
                             # s(precipitation_in) +
                             s(totallength_miles) + 
                             total_traveltime +
                             max.count,
                           knots = list(yday = c(0,365)),
                           correlation = corAR1(form = ~yday|subsectionF),
                           data = allTrail,
                           family = poisson)

save(gamm_modGI_AR1_te3, 
     file=here("output/models/allT_subsection_gamm_GI-AR1_te3.rda"),
     compress='xz')
# Compare te() to s()  ----------------------------------------------------

# load(here("output/models/allT_subsection_gamm_G-AR1.rda"))
# load(here("output/models/allT_subsection_gamm_GS-AR1.rda"))
load(here("output/models/allT_subsection_gamm_GI-AR1.rda"))

p_GI_te <- as_tibble(predict(gamm_modGI_AR1_te$gam, 
                             predict.All, 
                             se.fit = TRUE, 
                             type = "response")) %>%
  rename(fit_te = fit, se_te = se.fit)

p_GI_te3 <- as_tibble(predict(gamm_modGI_AR1_te3$gam, 
                             predict.All, 
                             se.fit = TRUE, 
                             type = "response")) %>%
  rename(fit_te3 = fit, se_te3 = se.fit)

p_GI_s <- as_tibble(predict(gamm_modGI_AR1$gam, 
                              predict.All, 
                              se.fit = TRUE, 
                              type = "response")) %>%
  rename(fit_s = fit, se_s = se.fit)


pred_all_te <- bind_cols(predict.All, p_GI_te, p_GI_te3, p_GI_s) %>%
  tidyr::pivot_longer(fit_te:se_s, names_sep = '_',
                      names_to = c('variable', 'interaction')) %>%
  tidyr::pivot_wider(names_from = variable, values_from = value) %>%
  mutate(upr_ci = fit + (2 * se), lwr_ci = fit - (2 * se))

compare_modG <- ggplot(mapping = aes(x = yday, y = max.camera)) +
  geom_ribbon(data = pred_all_te,
              mapping = aes(ymin = lwr_ci, ymax = upr_ci,
                            x = yday, fill = factor(interaction)),
              inherit.aes = FALSE, alpha = 0.2) +
  geom_point(data = allTrail, aes(colour = as.factor(wday))) +
  geom_line(data = pred_all_te, aes(y = fit, x = yday, 
                                    colour2 = factor(interaction)),
            size = 1) %>%
  relayer::rename_geom_aes(new_aes = c("colour" = "colour2")) +
  scale_colour_brewer(palette = "Set1", aesthetics = "colour", name = "Day of Week") +
  scale_colour_OkabeIto(aesthetics = "colour2", name = "Model") +
  scale_fill_OkabeIto(name = "Model") +
  coord_cartesian(xlim = c(0, 365), ylim = c(0, 1000)) +
  facet_wrap(~subsectionF, drop = T, ncol = 3) +
  labs(title = "Model GI",
       subtitle = "How prediction varies with different interaction/smoothing term use.")

compare_high_te <-  ggplot(mapping = aes(x = yday, y = max.camera)) +
  geom_ribbon(data = pred_all_te %>% filter(subsectionF %in% high_use),
              mapping = aes(ymin = lwr_ci, ymax = upr_ci, 
                            x = yday, fill = factor(interaction)),
              inherit.aes = FALSE, alpha = 0.2) +
  geom_point(data = allTrail %>% filter(subsectionF %in% high_use),
             aes(colour = as.factor(wday))) +
  geom_line(data = pred_all_te  %>% filter(subsectionF %in% high_use),
            aes(y = fit, x = yday, colour2 = factor(interaction)),
            alpha= 0.7,
            size = 1) %>%
  relayer::rename_geom_aes(new_aes = c("colour" = "colour2")) +
  scale_colour_brewer(palette = "Set1", aesthetics = "colour", name = "Day of Week") +
  scale_colour_OkabeIto(aesthetics = "colour2", name = "Model") +
  scale_fill_OkabeIto(name = "Model") +
  coord_cartesian(xlim = c(0, 365), ylim = c(0, 1000)) +
  facet_wrap(~subsectionF, drop = T, ncol = 3) +
  labs(title = "Model GI",
       subtitle = "How prediction varies with different interaction/smoothing term use.")
# ggsave(filename = here::here("output/figures/high_pred_compareG.pdf"),
#        plot = compare_high_G,
#        height = 8.5, 
#        width = 13,
#        dpi=700)

compare_low_te <-  ggplot(mapping = aes(x = yday, y = max.camera)) +
  geom_ribbon(data = pred_all_te %>% filter(subsectionF %notin% high_use),
              mapping = aes(ymin = lwr_ci, ymax = upr_ci,
                            x = yday, fill = factor(interaction)),
              inherit.aes = FALSE, alpha = 0.2) +
  geom_point(data = allTrail %>% filter(subsectionF %notin% high_use), 
             aes(colour = as.factor(wday))) +
  geom_line(data = pred_all_te  %>% filter(subsectionF %notin% high_use),
            aes(y = fit, x = yday, colour2 = factor(interaction)),
            alpha= 0.7,
            size = 1) %>%
  relayer::rename_geom_aes(new_aes = c("colour" = "colour2")) +
  scale_colour_brewer(palette = "Set1", aesthetics = "colour", name = "Day of Week") +
  scale_colour_OkabeIto(aesthetics = "colour2", name = "Model") +
  scale_fill_OkabeIto(name = "Model") +
  coord_cartesian(xlim = c(0, 365), ylim = c(0, 300)) +
  facet_wrap(~subsectionF, drop = T, ncol =3) +
  labs(title = "Model GI",
       subtitle = "How prediction varies with different interaction/smoothing term use.")


# anova compare -----------------------------------------------------------
load(file=here("output/models/allT_subsection_gamm_GS_AR1.rda"))
load(file=here("output/models/allT_subsection_gamm_GI-AR1.rda"))


anova(gamm_modGS_AR1$lme, 
      gamm_modGS_AR1_te$lme)


anova(gamm_modGI_AR1$lme, 
      gamm_modGI_AR1_te$lme)

anova(gamm_modG_ARMA$lme, 
      gamm_modGARMA_te$lme)

