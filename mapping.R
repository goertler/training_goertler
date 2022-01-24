library(sf)
library(ggplot2)
library(leaflet)
library(scales)
library(ggmap)
library(dplyr)

## Read in shapefile using sf
ak_regions <- read_sf("shapefiles/ak_regions_simp.shp")

plot(ak_regions)  

class(ak_regions)

head(ak_regions)

st_crs(ak_regions)

ak_regions_3338 <- ak_regions %>%
  st_transform(crs = 3338)

plot(ak_regions_3338) 

st_crs(ak_regions_3338)

ak_regions_3338 %>%
  select(region == "Kuskokwim")

pop <- read.csv("shapefiles/alaska_population.csv")

pop_4326 <- st_as_sf(pop, 
                     coords = c('lng', 'lat'),
                     crs = 4326,
                     remove = F)

class(pop_4326)

st_crs(pop_4326)

head(pop_4326)

pop_3338 <- st_transform(pop_4326, crs = 3338)

#pop_joined <- st_join(pop_4326, ak_regions_3338, join = st_within)
pop_joined <- st_join(pop_3338, ak_regions_3338, join = st_within)

head(pop_joined)

?sf::tidyverse

pop_region <- pop_joined %>% 
  as.data.frame() %>% 
  group_by(region) %>% 
  summarise(total_pop = sum(population))

head(pop_region)

pop_region_3338 <- left_join(ak_regions_3338, pop_region)

plot(pop_region_3338)

#plot to check
plot(pop_region_3338["total_pop"])

pop_mgmt_338 <- pop_region_3338 %>% 
  group_by(mgmt_area) %>% 
  summarize(total_pop = sum(total_pop))

plot(pop_mgmt_338["total_pop"])

pop_mgmt_3338 <- pop_region_3338 %>% 
  group_by(mgmt_area) %>% 
  summarize(total_pop = sum(total_pop), do_union = F)

plot(pop_mgmt_3338["total_pop"])

write_sf(pop_region_3338, "shapefiles/ak_regions_population.shp", delete_layer = TRUE)

# make maps
ggplot(pop_region_3338) +
  geom_sf(aes(fill = total_pop)) +
  theme_bw() +
  labs(fill = "Total Population") +
  scale_fill_continuous(low = "khaki", high =  "firebrick", labels = comma)

rivers_3338 <- read_sf("shapefiles/ak_rivers_simp.shp")
st_crs(rivers_3338)

ggplot() +
  geom_sf(data = pop_region_3338, aes(fill = total_pop)) +
  geom_sf(data = rivers_3338, aes(size = StrOrder), color = "black") +
  geom_sf(data = pop_3338, aes(), size = 0.5) +
  scale_size(range = c(0.01, 0.2), guide = "none") +
  theme_bw() +
  labs(fill = "Total Population") +
  scale_fill_continuous(low = "khaki", high =  "firebrick", labels = comma)

# add basemaps
pop_3857 <- pop_3338 %>%
  st_transform(crs = 3857) # because this is what google maps needs

# Define a function to fix the bbox to be in EPSG:3857
# See https://github.com/dkahle/ggmap/issues/160#issuecomment-397055208
ggmap_bbox_to_3857 <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  
  # Coonvert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}

bbox <- c(-170, 52, -130, 64)   # This is roughly southern Alaska
ak_map <- get_stamenmap(bbox, zoom = 4)
ak_map_3857 <- ggmap_bbox_to_3857(ak_map)

ggmap(ak_map_3857) + 
  geom_sf(data = pop_3857, aes(color = population), inherit.aes = FALSE) +
  scale_color_continuous(low = "khaki", high = "firebrick", labels = comma)


