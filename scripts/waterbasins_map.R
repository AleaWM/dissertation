library(tidyverse)
#library(rayshader)
library(sf)
#library(geodata)



border <- st_read("inputs/Mapping_FIRMs/Cook_County_Border/Cook_County_Border.shp") |> 
  st_transform("EPSG:6454")
 
county_rivers <- read_sf("inputs/Mapping_Firms/17031C_20240319/S_WTR_LN.shp")

# makes black county shape with white rivers
ggplot() +
  geom_sf(data = border, fill = "gray20", color = "black") +
  geom_sf(data = county_rivers, color = "white", linewidth = 0.3)+
  theme_void()


# basins : idea from rayshader blog post thing -----------------------------
sf_use_s2(F)   # 

basins <- st_read("inputs/hybas_na_lev01-12_v1c/hybas_na_lev12_v1c.shp") |> 
  st_transform("EPSG:6454") |>   
  st_intersection(st_as_sf(border)) |>
  mutate(HYBAS_ID = as.character(HYBAS_ID),
         MAIN_BAS = as.character(MAIN_BAS))



cook_basin <- basins |> 
  st_transform("EPSG:6454") |> 
  select(HYBAS_ID)  

ggplot() + 
  geom_sf(data = basins, aes(geometry = geometry,
                             fill = (HYBAS_ID),
  )) +
  theme_void() + 
  theme(legend.position = "none") 


rivers <- st_read("inputs/HydroRIVERS_v10_na_shp/HydroRIVERS_v10_na.shp") |> 
  st_transform("EPSG:6454") |>  
  st_intersection(st_as_sf(border))

cook_rivers <- rivers |> 
  select(ORD_STRA)


ggplot() + 
  #geom_sf(data = cook_basin, aes(geometry = geometry, fill = as.character(MAIN_BAS))) +
  geom_sf(data = border, aes(geometry = geometry), fill = "black") +
  geom_sf(data = rivers, aes(geometry = geometry,
                            linewidth = ORD_STRA/4,
                          # linewidth = ORD_FLOW/10,
                             ), color = "white"
          ) + 
 scale_linewidth_identity() +
  theme_void()

# blog things : idea from rayshader blog post thing -----------------------------


# Get elevation data:
# install.packages("elevatr")
# install.packages("terra")
library(rayshader)
library(elevatr)
library(terra)

elevation <- elevatr::get_elev_raster(border,  z = 9, 
                                      clip =  "location") |>
  terra::rast()

elevation1 <- terra::project(elevation, "EPSG:6454")

mat <- rayshader::raster_to_matrix(elevation1)



river_overlay <- generate_line_overlay(cook_rivers, 
                                       extent = terra::ext(elevation1),
                                       heightmap = mat,
                                       linewidth = cook_rivers$ORD_STRA,
                      #linewidth = rivers$ORD_FLOW,
                     color = "white")

 river_overlay |> plot_map() # can't see because of white rivers


basin_overlay <- generate_polygon_overlay(cook_basin,
                                          extent = terra::ext(elevation1), 
                                          heightmap = mat,
                     # data_column_fill = "HYBAS_ID", 
                      linecolor = NA,
                     palette = hcl.colors(n=33, palette = "purples")
                     )

## If you want each basin to be its own specific color, filter out each basin
# b1 <- cook_basin <- filter(HYBAS_ID == "   ")
basin_overlay |> plot_map()


 # 2D map
mat |> sphere_shade() |> plot_map()

mat |> #sphere_shade() |>
  constant_shade(color = "black") |>
  add_overlay(basin_overlay, alphalayer = 0.8)|>
  add_overlay(river_overlay, alphalayer = 1) |>
  # plot_map()
  plot_3d(mat, 
          #zscale = 40,
       #   theta = 0, 
       #  phi = 80, 
         shadow = FALSE,
         solid = F
          )

render_highquality(
  samples = 400, sample_method = "sobol", 
  preview = T,
  interactive = F                   )

render_snapshot()



# References
# Lehner, B., Grill G. (2013). Global river hydrography and network routing: baseline data and new approaches to study the world’s large river systems. Hydrological Processes, 27(15): 2171–2186. https://doi.org/10.1002/hyp.9740

  