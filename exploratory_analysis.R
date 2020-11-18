######################## Libraries ########################
############ Loading ############
library(tidyverse)
library(sf)
library(sp)
library(viridis)

######################## Visualizacion ########################
spat_av <- mp10_long_loc %>%
  group_by(latitud, longitud) %>%
  summarize(mu_coord = mean(MP_10))
# Por latitud
ggplot(spat_av) +
  geom_point(mapping = aes(x = latitud, mu_coord)) +
  xlab("Latitud (°)") +
  ylab("Concentración de MP10 promedio (ug/m³)")
# Por longitud
ggplot(spat_av) +
  geom_point(mapping = aes(x = longitud, mu_coord)) +
  xlab("Longitud (°)") +
  ylab("Concentración de MP10 promedio (ug/m³)")
# Por estacion
ggplot(data = mp10_long_loc, 
       mapping = aes(x = reorder(nombre, MP_10, FUN = median), 
                     y = MP_10)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom="point", color="red") +
  coord_flip() +
  labs(x = "", y = "Concentración promedio diaria de MP10 (ug/m³)") +
  theme_bw()

# Por fecha
mp10_por_fecha_av <- mp10_long_loc %>%
  group_by(fecha) %>%
  summarize(avg_fecha = mean(MP_10))

mp10_long_loc %>%
  group_by(fecha) %>%
  ggplot() +
  geom_line(mapping = aes(x = fecha, y = MP_10),
            colour = "blue", alpha = 0.2) +
  geom_line(data =  mp10_por_fecha_av,
            mapping = aes(x = fecha, y = avg_fecha)) +  
  xlab("Día del año") + 
  ylab("Concentracion MP10 promedio diaria (ug/m³)") +
  theme_classic()

# Por dia del año
mp10_long_loc %>%
  group_by(dia_año) %>%
  summarize(av_dia = mean(MP_10)) %>%
  ggplot(aes(x = dia_año, y = av_dia)) +
  geom_line() +  
  xlab("Día del año") + 
  ylab("Concentracion MP10 promedio diaria (ug/m^3)") +
  theme_classic()

# Por estacion y por fecha
mp10_long_loc %>%
  group_by(fecha, nombre) %>%
  ggplot(mp10_dia, mapping = aes(x = fecha, y = MP_10)) +
  geom_line() +
  geom_abline(intercept = 195, slope = 0, color = "red") +
  xlab("Día del año") + 
  ylab("Concentracion MP10 promedio diaria (ug/m³)") +
  theme_classic() +
  facet_wrap(~ nombre, ncol = 3)

# Por estacion y por dia del año
mp10_long_loc %>%
  group_by(dia_año, nombre) %>%
  summarize(av_dia_año = mean(MP_10)) %>%
  ggplot(mp10_dia, mapping = aes(x = dia_año, y = av_dia_año)) +
  geom_line() +  
  xlab("Día del año") + 
  ylab("Concentracion MP10 promedio diaria (ug/m³)\nAgregada por estacion y dia del año") +
  theme_classic() +
  facet_wrap(~ nombre, ncol = 3)

# Por estacion el 2018
mp10_long_loc %>%
  filter(año == 2018) %>%
  group_by(dia_año, nombre) %>%
  summarize(av_dia_año = mean(MP_10)) %>%
  ggplot(mp10_dia, mapping = aes(x = dia_año, y = av_dia_año)) +
  geom_line() +  
  xlab("Día del año") + 
  ylab("Concentracion MP10 promedio diaria (ug/m³)") +
  theme_classic() +
  facet_wrap(~ nombre, ncol = 3)

# Por estacion entre 01/04 y 30/09
mp10_long_loc %>%
  filter(mes %in% 4:9) %>%
  group_by(dia_año, nombre) %>%
  summarize(av_dia_año = mean(MP_10)) %>%
  ggplot(mp10_dia, mapping = aes(x = dia_año, y = av_dia_año)) +
  geom_line() +  
  xlab("Día del año") + 
  ylab("Concentracion MP10 promedio diaria (ug/m³)") +
  theme_classic() +
  facet_wrap(~ nombre, ncol = 3)

# Por estacion y por año
mp10_por_estacion_anual <- mp10_long_loc %>%
  group_by(nombre, año) %>%
  summarize(mean_mp10 = mean(MP_10),
            median_mp10 = median(MP_10),
            sd_mp10 = sd(MP_10)) 

mp10_por_estacion_anual[, c("x", "y")] <- st_coordinates(mp10_por_estacion_anual)

ggplot(prov_conce) +
  geom_sf() +
  coord_sf(datum = NA) +
  geom_point(data = mp10_por_estacion_anual,
             mapping = aes(x = x, y = y, color = mean_mp10),
             size = 2) +
  labs(x = "", y = "") +
  scale_color_viridis(name = "Media anual MP10\n(μg/ m³)") +
  facet_wrap(~ año) +
  theme_bw()
