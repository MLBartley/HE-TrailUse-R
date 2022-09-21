## ---------------------------
## Script name: AllTrails Exploration
##
## Purpose of script: Examing predictive capability of A
## llTrails moving average data. Compare to Strava and
## check collinearity/concavity.
##
## Author: Dr. Meridith L. Bartley
## Date Created: 2022-09-01
## ---------------------------
## Notes:
##   
##
## ---------------------------

library(colorblindr)
library(here)
library(ggplot2)
library(mgcv)

source(here("scripts/01-LoadData.R"))

# Combine AllTrails search data to analysis data --------------------------

#add subsection (trailname only ) to AT search data

allTrail_search <- allTrail %>% 
  left_join(allTrails_search %>% select(-c("trailnum", 
                                           # "subsectionname", 
                                           "month",
                                           "day",
                                           "year")),
            by = c("subsectionname" = "subsectionname",
                   "trailname" = "trailname",
                   "date" = "date")) %>% 
  tidyr::drop_na("moving_avg")

unique(allTrail_search$trailname)

sub.number <- length(unique(allTrail_search$subsectionname))


# visualize relationship with trail use -----------------------------------

ggplot(allTrail_search, aes(num_views,
                        max.camera)) +
  geom_point(aes(color = trailname)) +
  facet_wrap(~subsectionF, ncol = 3) +
  scale_color_manual(values = colors)

# Run GS/GI-AR1 model with Search Only ---------------------------------------

gamm_modGS_AR1_searchOnly <- gamm(max.camera ~ 
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
                             s(totallength_miles, k = 9) +  
                             total_traveltime +
                             moving_avg,
                           knots = list(yday = c(0,365)),
                           data = allTrail_search, 
                           method = "REML", 
                           correlation = corAR1(form = ~yday|subsectionF),
                           family = poisson#, 
                           # niterPQL = 20
)

save(gamm_modGS_AR1_searchOnly, 
     file=here("output/models/allT_subsection_gamm_GS-AR1_ATSearch.rda"),
     compress='xz')

gamm_modGS_AR1_searchViews <- gamm(max.camera ~ 
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
                                    s(totallength_miles, k = 9) +  
                                    total_traveltime +
                                    num_views,
                                  knots = list(yday = c(0,365)),
                                  data = allTrail_search, 
                                  method = "REML", 
                                  correlation = corAR1(form = ~yday|subsectionF),
                                  family = poisson#, 
                                  # niterPQL = 20
)

save(gamm_modGS_AR1_searchViews, 
     file=here("output/models/allT_subsection_gamm_GS-AR1_ATSearchViews.rda"),
     compress='xz')

gamm_modGI_AR1_searchOnly <- gamm(max.camera ~
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
                            s(totallength_miles, k = 9) + 
                            total_traveltime +
                            moving_avg,
                          knots = list(yday = c(0,365)),
                          correlation = corAR1(form = ~yday|subsectionF),
                          data = allTrail_search,
                          family = poisson)

save(gamm_modGI_AR1_searchOnly, 
     file=here("output/models/allT_subsection_gamm_GI-AR1_ATSearch.rda"),
     compress='xz')


# Run GI-AR1 Model with Strava AND AllTrails ------------------------------


gamm_modGS_AR1_Both <- gamm(max.camera ~ 
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
                                    s(totallength_miles, k = 9) +  
                                    total_traveltime +
                                    moving_avg +
                                    max.count,
                                  knots = list(yday = c(0,365)),
                                  data = allTrail_search, 
                                  method = "REML", 
                                  correlation = corAR1(form = ~yday|subsectionF),
                                  family = poisson#, 
                                  # niterPQL = 20
)

save(gamm_modGS_AR1_Both, 
     file=here("output/models/allT_subsection_gamm_GS-AR1_Both.rda"),
     compress='xz')

gamm_modGI_AR1_Both <- gamm(max.camera ~
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
                              s(totallength_miles, k = 9) + 
                              total_traveltime +
                              moving_avg +
                              max.count,
                            knots = list(yday = c(0,365)),
                            correlation = corAR1(form = ~yday|subsectionF),
                            data = allTrail_search,
                            family = poisson)

save(gamm_modGI_AR1_Both, 
     file=here("output/models/allT_subsection_gamm_GI-AR1_Both.rda"),
     compress='xz')

load(file=here("output/models/allT_subsection_gamm_GI-AR1_ATSearch.rda"))
load(file=here("output/models/allT_subsection_gamm_GI-AR1_Both.rda"))

# Plot Compare GI Predictions ------------------------------------------------

# might need to subset predict.All data 
predict.search <- predict.All %>% 
  filter(subsectionF %in% unique(allTrail_search$subsectionname)) %>% 
  # left_join(allTrails_search %>% select(c("subsectionname", "trailname", "date", "moving_avg")), 
  #           by = c("subsectionF" = "subsectionname",
  #                  "trailname" = "trailname",
  #                  "date" = "date", 
  #                  "trailnumber" = "trailnumber")) %>% 
  left_join(allTrails_search %>% select(-c("trailnum", 
                                           # "subsectionname", 
                                           "month",
                                           "day",
                                           "year")),
            by = c("subsectionF" = "subsectionname",
                   # "trailname" = "trailname",
                   "date" = "date")) %>% 
  tidyr::drop_na("moving_avg")


load(file=here("output/models/allT_subsection_gamm_GI-AR1_te.rda"))

p_GI_Strava <- as_tibble(predict(gamm_modGI_AR1_te$gam, 
                             predict.search, 
                             se.fit = TRUE, 
                             type = "response")) %>%
  rename(fit_Strava = fit, se_Strava = se.fit)

p_GI_AllTrails <- as_tibble(predict(gamm_modGI_AR1_searchOnly$gam, 
                                    predict.search, 
                            se.fit = TRUE, 
                            type = "response")) %>%
  rename(fit_AT = fit, se_AT = se.fit)

p_GI_Both <- as_tibble(predict(gamm_modGI_AR1_Both$gam, 
                               predict.search, 
                                    se.fit = TRUE, 
                                    type = "response")) %>%
  rename(fit_Both = fit, se_Both = se.fit)


pred_all_ATStrava <- bind_cols(predict.search,
                               p_GI_Strava,
                               p_GI_AllTrails,
                               p_GI_Both) %>%
  tidyr::pivot_longer(fit_Strava:se_Both, names_sep = '_',
                      names_to = c('variable', 'auxData')) %>%
  tidyr::pivot_wider(names_from = variable, values_from = value) %>%
  mutate(upr_ci = fit + (2 * se), lwr_ci = fit - (2 * se))

compare_ATStrava <- ggplot(mapping = aes(x = yday, y = max.camera)) +
  geom_ribbon(data = pred_all_ATStrava,
              mapping = aes(ymin = lwr_ci, ymax = upr_ci,
                            x = yday, fill = factor(auxData)),
              inherit.aes = FALSE, alpha = 0.2) +
  geom_point(data = allTrail_search, aes(colour = as.factor(wday)) 
            ) +
  geom_line(data = pred_all_ATStrava, aes(y = fit, x = yday, 
                                    colour2 = factor(auxData)),
            alpha = 0.5,
            size = 1) %>%
  relayer::rename_geom_aes(new_aes = c("colour" = "colour2")) +
  scale_colour_brewer(palette = "Set1", aesthetics = "colour", name = "Day of Week") +
  scale_colour_OkabeIto(aesthetics = "colour2", name = "Model") +
  scale_fill_OkabeIto(name = "Model") +
  coord_cartesian(xlim = c(0, 365), ylim = c(0, 1000)) +
  facet_wrap(~subsectionF, drop = T) +
  labs(title = "Model GI",
       subtitle = "How prediction varies with different auxillary data inclusion.")
compare_ATStrava

# compare_high_ATStrava <-  ggplot(mapping = aes(x = yday, y = max.camera)) +
#   geom_ribbon(data = pred_all_ATStrava %>% filter(subsectionF %in% high_use),
#               mapping = aes(ymin = lwr_ci, ymax = upr_ci, 
#                             x = yday, fill = factor(interaction)),
#               inherit.aes = FALSE, alpha = 0.2) +
#   geom_point(data = allTrail_search %>% filter(subsectionF %in% high_use),
#              aes(colour = as.factor(wday))) +
#   geom_line(data = pred_all_ATStrava  %>% filter(subsectionF %in% high_use),
#             aes(y = fit, x = yday, colour2 = factor(interaction)),
#             alpha= 0.7,
#             size = 1) %>%
#   relayer::rename_geom_aes(new_aes = c("colour" = "colour2")) +
#   scale_colour_brewer(palette = "Set1", aesthetics = "colour", name = "Day of Week") +
#   scale_colour_OkabeIto(aesthetics = "colour2", name = "Model") +
#   scale_fill_OkabeIto(name = "Model") +
#   coord_cartesian(xlim = c(0, 365), ylim = c(0, 1000)) +
#   facet_wrap(~subsectionF, drop = T, ncol = 3) +
#   labs(title = "Model GI",
#        subtitle = "How prediction varies with different interaction/smoothing term use.")
# # ggsave(filename = here::here("output/figures/high_pred_compareG.pdf"),
# #        plot = compare_high_G,
# #        height = 8.5, 
# #        width = 13,
# #        dpi=700)
# 
# compare_low_ATStrava <-  ggplot(mapping = aes(x = yday, y = max.camera)) +
#   geom_ribbon(data = pred_all_te %>% filter(subsectionF %notin% high_use),
#               mapping = aes(ymin = lwr_ci, ymax = upr_ci,
#                             x = yday, fill = factor(interaction)),
#               inherit.aes = FALSE, alpha = 0.2) +
#   geom_point(data = allTrail %>% filter(subsectionF %notin% high_use), 
#              aes(colour = as.factor(wday))) +
#   geom_line(data = pred_all_te  %>% filter(subsectionF %notin% high_use),
#             aes(y = fit, x = yday, colour2 = factor(interaction)),
#             alpha= 0.7,
#             size = 1) %>%
#   relayer::rename_geom_aes(new_aes = c("colour" = "colour2")) +
#   scale_colour_brewer(palette = "Set1", aesthetics = "colour", name = "Day of Week") +
#   scale_colour_OkabeIto(aesthetics = "colour2", name = "Model") +
#   scale_fill_OkabeIto(name = "Model") +
#   coord_cartesian(xlim = c(0, 365), ylim = c(0, 300)) +
#   facet_wrap(~subsectionF, drop = T, ncol =3) +
#   labs(title = "Model GI",
#        subtitle = "How prediction varies with different interaction/smoothing term use.")
# 
# 





# Plot Compare GS Predictions ------------------------------------------------

# might need to subset predict.All data 



load(file=here("output/models/allT_subsection_gamm_GS-AR1_te.rda"))

p_GS_Strava <- as_tibble(predict(gamm_modGS_AR1_te$gam, 
                                 predict.search, 
                                 se.fit = TRUE, 
                                 type = "response")) %>%
  rename(fit_Strava = fit, se_Strava = se.fit)

p_GS_AllTrails <- as_tibble(predict(gamm_modGS_AR1_searchOnly$gam, 
                                    predict.search, 
                                    se.fit = TRUE, 
                                    type = "response")) %>%
  rename(fit_AT = fit, se_AT = se.fit)

p_GS_Both <- as_tibble(predict(gamm_modGS_AR1_Both$gam, 
                               predict.search, 
                               se.fit = TRUE, 
                               type = "response")) %>%
  rename(fit_Both = fit, se_Both = se.fit)


pred_all_ATStrava_GS <- bind_cols(predict.search,
                               p_GS_Strava,
                               p_GS_AllTrails,
                               p_GS_Both) %>%
  tidyr::pivot_longer(fit_Strava:se_Both, names_sep = '_',
                      names_to = c('variable', 'auxData')) %>%
  tidyr::pivot_wider(names_from = variable, values_from = value) %>%
  mutate(upr_ci = fit + (2 * se), lwr_ci = fit - (2 * se))

compare_ATStrava_GS <- ggplot(mapping = aes(x = yday, y = max.camera)) +
  geom_ribbon(data = pred_all_ATStrava_GS,
              mapping = aes(ymin = lwr_ci, ymax = upr_ci,
                            x = yday, fill = factor(auxData)),
              inherit.aes = FALSE, alpha = 0.2) +
  geom_point(data = allTrail_search, aes(colour = as.factor(wday)) 
  ) +
  geom_line(data = pred_all_ATStrava_GS, aes(y = fit, x = yday, 
                                          colour2 = factor(auxData)),
            alpha = 0.5,
            size = 1) %>%
  relayer::rename_geom_aes(new_aes = c("colour" = "colour2")) +
  scale_colour_brewer(palette = "Set1", aesthetics = "colour", name = "Day of Week") +
  scale_colour_OkabeIto(aesthetics = "colour2", name = "Model") +
  scale_fill_OkabeIto(name = "Model") +
  coord_cartesian(xlim = c(0, 365), ylim = c(0, 1000)) +
  facet_wrap(~subsectionF, drop = T) +
  labs(title = "Model GS",
       subtitle = "How prediction varies with different auxillary data inclusion.")
compare_ATStrava_GS

# compare_high_ATStrava <-  ggplot(mapping = aes(x = yday, y = max.camera)) +
#   geom_ribbon(data = pred_all_ATStrava %>% filter(subsectionF %in% high_use),
#               mapping = aes(ymin = lwr_ci, ymax = upr_ci, 
#                             x = yday, fill = factor(interaction)),
#               inherit.aes = FALSE, alpha = 0.2) +
#   geom_point(data = allTrail_search %>% filter(subsectionF %in% high_use),
#              aes(colour = as.factor(wday))) +
#   geom_line(data = pred_all_ATStrava  %>% filter(subsectionF %in% high_use),
#             aes(y = fit, x = yday, colour2 = factor(interaction)),
#             alpha= 0.7,
#             size = 1) %>%
#   relayer::rename_geom_aes(new_aes = c("colour" = "colour2")) +
#   scale_colour_brewer(palette = "Set1", aesthetics = "colour", name = "Day of Week") +
#   scale_colour_OkabeIto(aesthetics = "colour2", name = "Model") +
#   scale_fill_OkabeIto(name = "Model") +
#   coord_cartesian(xlim = c(0, 365), ylim = c(0, 1000)) +
#   facet_wrap(~subsectionF, drop = T, ncol = 3) +
#   labs(title = "Model GI",
#        subtitle = "How prediction varies with different interaction/smoothing term use.")
# # ggsave(filename = here::here("output/figures/high_pred_compareG.pdf"),
# #        plot = compare_high_G,
# #        height = 8.5, 
# #        width = 13,
# #        dpi=700)
# 
# compare_low_ATStrava <-  ggplot(mapping = aes(x = yday, y = max.camera)) +
#   geom_ribbon(data = pred_all_te %>% filter(subsectionF %notin% high_use),
#               mapping = aes(ymin = lwr_ci, ymax = upr_ci,
#                             x = yday, fill = factor(interaction)),
#               inherit.aes = FALSE, alpha = 0.2) +
#   geom_point(data = allTrail %>% filter(subsectionF %notin% high_use), 
#              aes(colour = as.factor(wday))) +
#   geom_line(data = pred_all_te  %>% filter(subsectionF %notin% high_use),
#             aes(y = fit, x = yday, colour2 = factor(interaction)),
#             alpha= 0.7,
#             size = 1) %>%
#   relayer::rename_geom_aes(new_aes = c("colour" = "colour2")) +
#   scale_colour_brewer(palette = "Set1", aesthetics = "colour", name = "Day of Week") +
#   scale_colour_OkabeIto(aesthetics = "colour2", name = "Model") +
#   scale_fill_OkabeIto(name = "Model") +
#   coord_cartesian(xlim = c(0, 365), ylim = c(0, 300)) +
#   facet_wrap(~subsectionF, drop = T, ncol =3) +
#   labs(title = "Model GI",
#        subtitle = "How prediction varies with different interaction/smoothing term use.")
# 
# 





# compare models ----------------------------------------------------------

anova(
  # gamm_modGI_AR1_te$lme, 
      gamm_modGI_AR1_searchOnly$lme,
      gamm_modGI_AR1_Both$lme
     )

## looks like both GI works! But also seems very little difference in model fit than just using Strava Data. 

anova(
  gamm_modGI_AR1_searchOnly$lme,
  gamm_modGI_AR1_Both$lme,
  # gamm_modGI_AR1_te$lme,
  gamm_modGS_AR1_searchOnly$lme,
  gamm_modGS_AR1_searchViews$lme,
  gamm_modGS_AR1_Both$lme#,
  # gamm_modGS_AR1_te$lme
  )



# Compare with Middle Cottonwood Only -------------------------------------

## AT Only - views

gamm_mod2_ATViews <- gamm(max.camera ~ s(yday, bs = "cc") + 
                    s(month, k = 3) +
                    s(wday, 
                      bs = "cc", k = 7) +
                    s(daily_aqi_value) +
                    s(temp_max_f) +
                    s(precipitation_in, k = 9) +
                    # max.count,
                      num_views,
                  data = singleTrail,    
                  knots = list(yday = c(0,365)),
                  method = "REML", 
                  family = poisson)

save(gamm_mod2_ATViews,
     file = here("output/models/singleT_gamm2_ATViews.rda"),
     compress='xz')

# AT Only - moving average

gamm_mod2_ATavg <- gamm(max.camera ~ s(yday, bs = "cc") + 
                            s(month, k = 3) +
                            s(wday, 
                              bs = "cc", k = 7) +
                            s(daily_aqi_value) +
                            s(temp_max_f) +
                            s(precipitation_in, k = 9) +
                            # max.count,
                            moving_avg,
                          data = singleTrail,    
                          knots = list(yday = c(0,365)),
                          method = "REML", 
                          family = poisson)

save(gamm_mod2_ATavg,
     file = here("output/models/singleT_gamm2_ATavg.rda"),
     compress='xz')

## Both = AT Views and Strava

gamm_mod2_Both <- gamm(max.camera ~ s(yday, bs = "cc") + 
                          s(month, k = 3) +
                          s(wday, 
                            bs = "cc", k = 7) +
                          s(daily_aqi_value) +
                          s(temp_max_f) +
                          s(precipitation_in, k = 9) +
                          max.count+
                          num_views,
                        data = singleTrail,    
                        knots = list(yday = c(0,365)),
                        method = "REML", 
                        family = poisson)

save(gamm_mod2_Both,
     file = here("output/models/singleT_gamm2_Both.rda"),
     compress='xz')

## ts gamm diagnostics ####

with(singleTrail, 
     tsDiagGamm(gamm_mod2, 
                timevar = yday,
                observed = max.camera))
with(singleTrail, 
     tsDiagGamm(gamm_mod2_Both, 
                timevar = yday,
                observed = max.camera))


## compare anova ####
anova(gamm_mod2$lme, 
      gamm_mod2_ATViews$lme, 
      gamm_mod2_ATavg$lme, 
      gamm_mod2_Both$lme)

## plot predictions


p_gamm2 <- as_tibble(predict(gamm_mod2$gam, predict.MidCot, 
                             se.fit = TRUE, type = "response")) %>%
  rename(fit_Strava = fit, se_Strava = se.fit)
p_gamm2_Both <- as_tibble(predict(gamm_mod2_AR1$gam, predict.MidCot, 
                                se.fit = TRUE, type = "response")) %>%
  rename(fit_Both = fit, se_Both = se.fit)

new_data_bases <- bind_cols(predict.MidCot, p_gamm2, p_gamm2_Both) %>%
  tidyr::pivot_longer(fit_Strava:se_Both, names_sep = '_',
                      names_to = c('variable', 'model')) %>%
  tidyr::pivot_wider(names_from = variable, values_from = value) %>%
  mutate(upr_ci = fit + (2 * se), lwr_ci = fit - (2 * se))



ggplot(mapping = aes(x = yday, y = max.camera)) +
  geom_ribbon(data = new_data_bases,
              mapping = aes(ymin = lwr_ci, ymax = upr_ci, x = yday, fill = factor(model)),
              inherit.aes = FALSE, alpha = 0.2) +
  geom_point(data = singleTrail, aes(colour = wday)) +
  geom_line(data = new_data_bases, aes(y = fit, x = yday, colour2 = factor(model)),
            size = 1) %>%
  relayer::rename_geom_aes(new_aes = c("colour" = "colour2")) +
  scale_colour_distiller(palette = "Set1", aesthetics = "colour", name = "Day of Week") +
  scale_colour_OkabeIto(aesthetics = "colour2", name = "Model") +
  scale_fill_OkabeIto(name = "Model") +
  coord_cartesian(xlim = c(min(singleTrail$yday) - 30, max(singleTrail$yday) + 30), ylim = c(0, 1000)) +
  labs(title = "Extrapolation",
       subtitle = "How prediction varies with different model specifications")



# Compare best wrt interaction (yday/precip) ------------------------------


gamm_mod2_Both_te <- gamm(max.camera ~
                         # s(yday, bs = "cc") + 
                         te(yday, precipitation_in, 
                            bs = c("cc", "tp"), k = c(10, 9), 
                            np = FALSE) +
                         s(month, k = 3) +
                         s(wday, 
                           bs = "cc", k = 7) +
                         s(daily_aqi_value) +
                         s(temp_max_f) +
                         # s(precipitation_in, k = 9) +
                         max.count+
                         num_views,
                       data = singleTrail,    
                       knots = list(yday = c(0,365)),
                       method = "REML", 
                       family = poisson)

save(gamm_mod2_Both_te,
     file = here("output/models/singleT_gamm2_Both_te.rda"),
     compress='xz')

with(singleTrail, 
     tsDiagGamm(gamm_mod2_Both, 
                timevar = yday,
                observed = max.camera))


## compare anova ####
anova(gamm_mod2_Both$lme,
      gamm_mod2_Both_te$lme)

## te does worse (see AIC and QQ plot)

  

# What if we use fewer knots (k) for precip? ------------------------------
gamm_mod2_Both_precip <- gamm(max.camera ~
                            s(yday, bs = "cc") +
                            # te(yday, precipitation_in,
                            #    bs = c("cc", "tp"), k = c(10, 9),
                            #    np = FALSE) +
                            s(month, k = 3) +
                            s(wday, 
                              bs = "cc", k = 7) +
                            s(daily_aqi_value) +
                            s(temp_max_f) +
                            s(precipitation_in, k = 5) +
                            max.count+
                            num_views,
                          data = singleTrail,    
                          knots = list(yday = c(0,365)),
                          method = "REML", 
                          family = poisson)

anova(gamm_mod2_Both_precip$lme, 
      gamm_mod2_Both$lme)


with(singleTrail, 
     tsDiagGamm(gamm_mod2_Both, 
                timevar = yday,
                observed = max.camera))
with(singleTrail, 
     tsDiagGamm(gamm_mod2_Both_precip, 
                timevar = yday,
                observed = max.camera))

