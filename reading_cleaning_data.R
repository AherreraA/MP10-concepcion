######################## Libraries ########################
############ Loading ############
library(sp)  
library(sf)
library(rgdal)
library(lwgeom)
library(tidyverse)

######################## Reading data ########################
############ Spatial data ############
###### Geographical layer ######
# We read the shapefile, that contains the geographical layer for the Region del Bio Bio
#unzip(zipfile = "input/Límites-shp.zip")
biobio <- st_read(dsn = "input/Limites-shp/L%C3%ADmites.shp")

# We filter the boundaries for the Concepcion province.
prov_conce <- biobio[biobio$NOM_PROVIN == "CONCEPCIÓN", ]

# We'll keep only the POLYGON that has the largest area, as we are interested only in the continental territory
prov_conce <- prov_conce %>%
  st_cast("POLYGON") %>%
  mutate(area = st_area(.)) %>%
  arrange(desc(area)) %>%
  slice(1)

# We can make a plot of the geographical zone
ggplot(prov_conce) +
  geom_sf() +
  theme_bw()

# We'll project the data using UTM projection, to EPSG code 9154 
prov_conce <- prov_conce %>%
  st_transform(9154)

# Again, we make a map, but now from the projected data
ggplot(prov_conce) +
  geom_sf() +
  theme_bw() +
  coord_sf(datum = st_crs(prov_conce))

###### Monitoring stations ######
file_path <- "input/Estaciones-de-calidad-de-aire-MMA.csv"
estaciones_biobio <- st_read(file_path, options=c("X_POSSIBLE_NAMES=longitud","Y_POSSIBLE_NAMES=latitud"))

# We'll set the CRS to the monitoring station coordinates, and project the data as we made with the geographical layers
estaciones_biobio <- st_set_crs(estaciones_biobio, 4326)
estaciones_biobio <- estaciones_biobio %>% 
  st_transform(9154)

# Also, we'll sustitute the geographical coordinates by the UTM coordinates
estaciones_biobio[, c("x", "y")] <- st_coordinates(estaciones_biobio)

# Finally, we'll filter the monitoring stations that we'll consider as data sources. Then, we select only the fields that we can use in the future
estaciones_prov_conce <- estaciones_biobio[prov_conce, ] %>% 
  mutate(id = as.integer(as.vector(OBJECTID))) %>%
  filter(!(OBJECTID %in% c(74, 76, 81, 92))) %>%
  select(id, nombre, latitud, longitud, comuna, x, y)


# Now, we can visualize the following plot, that map the projected area,and the monitoring locations as a layer
ggplot(prov_conce) +
  geom_sf() +
  coord_sf(datum = st_crs(prov_conce)) +
  geom_point(data = estaciones_prov_conce,
             mapping = aes(x = x, y = y),
             color = "blue") +
  theme_bw() +
  xlab("") + ylab("")


############ Attribute data (MP10) ############
# We'll read the attribute data, that corresponds to the mp10's dairy average concentration by monitoring station. The data is in a long format, that is, one measure per row
df_long_form <- read_csv("input/data_mp10")

# Now, we have to combine the attribute data that we just read with the data of the monitoring stations. For that, we use the function left_join(), in order to keep the geometry format sf in the dataframe
mp10_long_loc <- left_join(estaciones_prov_conce, df_long_form, by = c("id" = "id"))

#  (space wide format)
# mp10_space_wide <- df_long_form %>%
#   pivot_wider(names_from = id, values_from = MP_10) %>%
#   as.matrix()
# View(mp10_space_wide)
