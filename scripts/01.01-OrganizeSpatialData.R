library(here)
library(sfnetworks)
library(sf)
library(tidygraph)
library(tidyverse)
library(igraph)

source(here("scripts/01-LoadData.R")) #loads and cleans data

# library(rgrass)
# 
# # gisBase = ifelse(Sys.info()[["sysname"]] == "Linux", "/usr/lib/grass78/", "C://Program Files//GRASS GIS 7.6")
# location <- basename(tempfile())
# gisdbase <- tempdir()
# corner <- st_bbox(trail_spatial)
# xmax <- corner[3]
# xmin <- corner[1]
# ymax <- corner[4]
# ymin <- corner[2]
# proj4 <- unlist(sf::st_crs(trail_spatial)[2])
# resolution <- "1"
# gisBase = "/Applications/GRASS-8.2.app/Contents/Resources/"
# rgrass::initGRASS(
#   gisBase = gisBase,
#   home = tempdir(), 
#   gisDbase = gisdbase, 
#   mapset = "PERMANENT", 
#   location = location,
#   override = TRUE,
# )
# 
# 
# 
# # Add data to GRASS spatial database  
# rgrass::write_VECT(
#   x = trail_spatial_summary_keep, 
#   vname = 'trail_spatial_summary_keep'#, 
#   # v.in.ogr_flags = 'overwrite'
# )
# 
# # Execute the v.clean tool
# execGRASS("g.proj", flags = c("c", "quiet"), proj4 = proj4)
# execGRASS(
#   cmd = 'v.clean', 
#   input = 'trail_spatial_summary_keep', 
#   output = 'trail_spatial_cleaned',        
#   tool = 'break', 
#   flags = c('overwrite', 'c')
# )
# 
# # Read back into R
# use_sf()
# trail_spatial_summary_keep <- readVECT('trail_spatial_cleaned') %>%
#   rename(geometry = geom) %>%
#   select(-cat)
# 
# 
# edges <- trail_spatial_summary_keep %>%
#   mutate(edgeID = c(1:n()))
# 
# edges
# 
# nodes <- edges %>%
#   st_coordinates() %>%
#   as_tibble() %>%
#   rename(edgeID = L1) %>%
#   group_by(edgeID) %>%
#   slice(c(1, n())) %>%
#   ungroup() %>%
#   mutate(start_end = rep(c('start', 'end'), times = n()/2))
# 
# nodes
# 
# nodes <- nodes %>%
#   mutate(xy = paste(.$X, .$Y)) %>% 
#   mutate(nodeID = group_indices(., factor(xy, levels = unique(xy)))) %>%
#   select(-xy)
# 
# nodes
# 
# 
# source_nodes <- nodes %>%
#   filter(start_end == 'start') %>%
#   pull(nodeID)
# 
# target_nodes <- nodes %>%
#   filter(start_end == 'end') %>%
#   pull(nodeID)
# 
# edges = edges %>%
#   mutate(from = source_nodes, to = target_nodes)
# 
# edges
# 
# nodes <- nodes %>%
#   distinct(nodeID, .keep_all = TRUE) %>%
#   select(-c(edgeID, start_end)) %>%
#   st_as_sf(coords = c('X', 'Y')) %>%
#   st_set_crs(st_crs(edges))
# 
# nodes
# 
# graph = tbl_graph(nodes = nodes, edges = as_tibble(edges), directed = FALSE)
# 
# graph
# 
# 
# sf_to_tidygraph = function(x, directed = TRUE) {
#   
#   edges <- x %>%
#     mutate(edgeID = c(1:n()))
#   
#   nodes <- edges %>%
#     st_coordinates() %>%
#     as_tibble() %>%
#     rename(edgeID = L1) %>%
#     group_by(edgeID) %>%
#     slice(c(1, n())) %>%
#     ungroup() %>%
#     mutate(start_end = rep(c('start', 'end'), times = n()/2)) %>%
#     mutate(xy = paste(.$X, .$Y)) %>% 
#     mutate(nodeID = group_indices(., factor(xy, levels = unique(xy)))) %>%
#     select(-xy)
#   
#   source_nodes <- nodes %>%
#     filter(start_end == 'start') %>%
#     pull(nodeID)
#   
#   target_nodes <- nodes %>%
#     filter(start_end == 'end') %>%
#     pull(nodeID)
#   
#   edges = edges %>%
#     mutate(from = source_nodes, to = target_nodes)
#   
#   nodes <- nodes %>%
#     distinct(nodeID, .keep_all = TRUE) %>%
#     select(-c(edgeID, start_end)) %>%
#     st_as_sf(coords = c('X', 'Y')) %>%
#     st_set_crs(st_crs(edges))
#   
#   tbl_graph(nodes = nodes, edges = as_tibble(edges), directed = directed)
#   
# }
# 
# sf_to_tidygraph(trail_spatial_summary_keep, directed = FALSE)
# 
# graph <- graph %>%
#   activate(edges) %>%
#   mutate(length = st_length(geometry))
# 
# graph


## plot spatial + cameras 
cameras_sf <- st_as_sf(cameras, coords = c("counter_long", "counter_lat"), 
                       crs = 4326, agr = "constant")

# library(Polychrome)
# P20 = createPalette(20,  c("#ff0000", "#00ff00", "#0000ff"))
# swatch(P20)
# 
# trail_spatial %>% 
#   dplyr::filter(NAME %notin% c("NEW WORLD GULCH")#, 
#                 # OBJECTID %in% c("1273")
#                 # BEGIN_TERM %in% c("Rapter View")
#                 ) %>% 
#   # tidyr::drop_na(subsectionname) %>% 
#   # st_zm() %>% 
#   ggplot() + 
#   geom_sf() +
#   geom_sf(data = cameras_sf #%>% filter(counterid %in% c(3))
#           ,
#           aes(
#             # x = counter_long,
#             # y = counter_lat, 
#             shape = factor(subsectionname)), size = 2) #+ 
#   geom_sf_label(data = cameras_sf,
#                 aes(
#                   # x = counter_long,
#                   # y = counter_lat, 
#                   label = factor(subsectionname)))
  # scale_color_manual(values = P20)
  # annotation_scale(style = 'ticks', pad_x = unit(4.35, 'cm'),
  #                  pad_y = unit(.5, 'cm')) +
  # labs(title='Bridger Mountains Trail Cameras') +
  # labs(x = "Longitude", y = "Latitude", size = 20) +
  # labs(color='Trails') +
  # theme(axis.text = element_text(size = rel(1.25)),
  #       axis.title = element_text(size = rel(1.25)),
  #       plot.title = element_text(size = rel(2))) #+



# combine trail_count and trail_spatial

##hardcode subsectionnames in trail_spatial
trail_spatial_summary_keep$subsectionname = NA

trail_spatial_summary_keep[which(trail_spatial_summary_keep$NAME == "BENCHMARK ROAD"), "subsectionname"] <- "Benchmark Rd"
trail_spatial_summary_keep[which(trail_spatial_summary_keep$NAME == "BRIDGER BOWL"), "subsectionname"] <- "Bridger Bowl"
trail_spatial_summary_keep[which(trail_spatial_summary_keep$OBJECTID == "853"), "subsectionname"] <- "Sypes Canyon"
trail_spatial_summary_keep[which(trail_spatial_summary_keep$NAME == "CARROL CREEK"), "subsectionname"] <- "Carroll Creek"
trail_spatial_summary_keep[which(trail_spatial_summary_keep$NAME == "COLLEGE M"), "subsectionname"] <- "College M"
trail_spatial_summary_keep[which(trail_spatial_summary_keep$NAME == "CORBLY GULCH"), "subsectionname"] <- "Corbly Gulch"
trail_spatial_summary_keep[which(trail_spatial_summary_keep$NAME == "EAST BRIDGER NORTH"), "subsectionname"] <- "East Bridger North"
trail_spatial_summary_keep[which(trail_spatial_summary_keep$NAME == "EAST BRIDGER SOUTH"), "subsectionname"] <- "East Bridger South"
trail_spatial_summary_keep[which(trail_spatial_summary_keep$NAME == "FAIRY CREEK"), "subsectionname"] <- "Fairy Creek"
trail_spatial_summary_keep[which(trail_spatial_summary_keep$NAME == "FAIRY LAKESHORE"), "subsectionname"] <- "Fairy Lakeshore"
trail_spatial_summary_keep[which(trail_spatial_summary_keep$NAME == "JOHNSON CANYON JEEP TRAIL"), "subsectionname"] <- "Johnson Canyon Jeep Trail"
trail_spatial_summary_keep[which(trail_spatial_summary_keep$NAME == "HORSETHIEF MOUNTAIN"), "subsectionname"] <- "Horsethief Mountain"
trail_spatial_summary_keep[which(trail_spatial_summary_keep$NAME == "MIDDLE COTTONWOOD"), "subsectionname"] <- "Middle Cottonwood"
trail_spatial_summary_keep[which(trail_spatial_summary_keep$NAME == "NORTH COTTONWOOD ACCESS"), "subsectionname"] <- "North Cottonwood Access"
trail_spatial_summary_keep[which(trail_spatial_summary_keep$OBJECTID == "1305"), "subsectionname"] <- "North Cottonwood to Johnson Canyon"
trail_spatial_summary_keep[which(trail_spatial_summary_keep$NAME == "ROSS PASS"), "subsectionname"] <- "Ross Pass"
trail_spatial_summary_keep[which(trail_spatial_summary_keep$NAME == "SHAFTHOUSE HILL Lower"), "subsectionname"] <- "Lower Shafthouse"
trail_spatial_summary_keep[which(trail_spatial_summary_keep$NAME == "SACAGAWEA PASS"), "subsectionname"] <- "Sacagawea Pass"
trail_spatial_summary_keep[which(trail_spatial_summary_keep$NAME == "SHAFTHOUSE HILL Lower"), "subsectionname"] <- "Lower Shafthouse"
trail_spatial_summary_keep[which(trail_spatial_summary_keep$NAME == "SHAFTHOUSE HILL Upper"), "subsectionname"] <- "Upper Shafthouse"
trail_spatial_summary_keep[which(trail_spatial_summary_keep$NAME == "SOUTH FORK FLATHEAD CREEK"), "subsectionname"] <- "South Fork Flathead Creek"
trail_spatial_summary_keep[which(trail_spatial_summary_keep$NAME == "TRUMAN GULCH"), "subsectionname"] <- "Truman Gulch"
trail_spatial_summary_keep[which(trail_spatial_summary_keep$OBJECTID == "1282"), "subsectionname"] <- "Felix Canyon"
trail_spatial_summary_keep[which(trail_spatial_summary_keep$OBJECTID == "848"), "subsectionname"] <- "Steep Way"
trail_spatial_summary_keep[which(trail_spatial_summary_keep$OBJECTID == "1286"), "subsectionname"] <- "College M to Sypes"
trail_spatial_summary_keep[which(trail_spatial_summary_keep$OBJECTID == "850"), "subsectionname"] <- "Ross Pass to Sacagawea Peak"
trail_spatial_summary_keep[which(trail_spatial_summary_keep$OBJECTID == "852"), "subsectionname"] <- "Raptor View"
trail_spatial_summary_keep[which(trail_spatial_summary_keep$BEGIN_TERM == "Baldy"), "subsectionname"] <- "Baldy to Bridger"
trail_spatial_summary_keep[which(trail_spatial_summary_keep$END_TERMIN == "Baldy"), "subsectionname"] <- "M to Baldy"
trail_spatial_summary_keep[which(trail_spatial_summary_keep$BEGIN_TERM == "Rapter View"), "subsectionname"] <- "Bridger to Ross Pass"

# which <- which(unique(cameras_sf$subsectionname) %notin% unique(trail_spatial_summary_keep$subsectionname))
# 
# unique(cameras_sf$subsectionname)[which]

trail_char[is.na(trail_char$subsectionname),
                     'subsectionname' ] <- trail_char[is.na(trail_char$subsectionname), 
                                                                'trailname' ] 

allTrailChar_spatial <- trail_char %>% 
  dplyr::left_join(trail_spatial_summary_keep %>% tidyr::drop_na(subsectionname), 
                   by = c("subsectionname" = "subsectionname"))

allTrailChar_spatial[is.na(allTrailChar_spatial$subsectionname),
             'subsectionname' ] <- allTrailChar_spatial[is.na(allTrailChar_spatial$subsectionname), 
                                                'trailname' ] 


#### Combine Camera Counts, Strava Counts, Weather, Trail Characteristics ####

allData_spatial <- allCount %>% 
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
  dplyr::full_join(allTrailChar_spatial, by = c("trailnumber" = "trailnumber", 
                                        "trailname" = "trailname", 
                                        "subsectionname" = "subsectionname"
  )) %>% 
  
  dplyr::distinct()

allTrail_spatial <- allData_spatial %>% 
  tidyr::drop_na(max.camera)

#fix missing yday and max.count (set = zero for HTM and Benchmark RD)
allTrail_spatial$yday = lubridate::yday(allTrail_spatial$date)
allTrail_spatial[is.na(allTrail_spatial$max.count), "max.count"] <- 0


ls = st_cast(st_as_sf(allTrail_spatial), "LINESTRING")

net = sfnetworks::as_sfnetwork(ls) 

net = net %>%
  mutate(x = node_X(), y = node_Y(), z = node_Z())

# net %>%
#   activate("edges") %>%
#   mutate(weight = edge_length()) %>%
#   activate("nodes") %>%
#   mutate(bc = centrality_betweenness(weights = weight, directed = FALSE))
# 
# net %>%
#   activate("nodes")%>% 
#   st_as_sf()
# 
# st_as_sf(net, "edges")
# st_as_sf(net, "nodes")


# autoplot(net %>% st_zm) + ggtitle("Trail network of Bridger Mountains")


# ggplot() +
#   geom_sf(data = st_as_sf(net %>% st_zm, "edges"),  
#           aes(col = trailname#, 
#               # size = length_mi/2
#               )) +
#   geom_sf(data = st_as_sf(net %>% st_zm, "nodes"), 
#          color = "grey50"
#           ) +
#   geom_sf(data = cameras_sf,
#           aes(
#             color = factor(subsectionname)), shape = 2) +
#   ggtitle("Trail Name and Camera Locations in Bridger Mountains")
