## ---------------------------
## Script name: 
##
## Purpose of script:
##
## Author: Dr. Meridith L. Bartley
## Date Created: 2022-08-03
## ---------------------------
## Notes:
##   
##
## ---------------------------

library(ggspatial)

allTrailChar_full <- trail_char %>% 
  dplyr::left_join(trail_spatial_summary, 
                   by = c("trailnumber" = "ID" ))



trail_spatial %>%  
  st_zm() %>% 
  ggplot() + 
  geom_sf(aes(color = factor(OBJECTID)), size = 2) +
  annotation_scale(style = 'ticks', pad_x = unit(4.35, 'cm'), 
                   pad_y = unit(.5, 'cm')) +
  theme_void() +
  theme(legend.position = 'bottom')

  
