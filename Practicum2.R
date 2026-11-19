library(tidyverse)
library(sf)

library(tigris)    # for shift_geometry()
library(ggthemes)  # for theme_map()

counties <- st_read("/Users/edwardblaha/DataWrangling/Practicum 2/OneDrive_1_11-15-2026/cb_2024_us_county_500k/cb_2024_us_county_500k.shp")
high_points <- read.csv("/Users/edwardblaha/DataWrangling/Practicum 2/OneDrive_1_11-15-2026/nyshighpoints.csv")

#1
counties_ny <- counties |>  
  filter(STATEFP == 36)


NY_map <- ggplot(counties_ny) +
  geom_sf()+
  labs(title = "Counties of New York", fill = NULL) +
  theme_map()+ 
  theme(plot.title = element_text(hjust = 0.5))

NY_map

#2
counties_ny_sorted <- counties_ny[order(counties_ny$ALAND), ]
tail(counties_ny_sorted, 1)    

#St. Lawrence County is the largest in NY

#3- High Points Map

NY_Map_High_Points <- ggplot(counties_ny) +
  geom_sf()+
  geom_point(data = high_points, aes(Longitude, Latitude))+
  labs(title = "High Point of Each County in New York", fill = NULL) +
  theme_map()+ 
  theme(plot.title = element_text(hjust = 0.5))

NY_Map_High_Points

#4


HighPoint_cols <- c("darkgreen", "gold", "chocolate4", "white", "red")

NY_Map_High_Points_Color <- ggplot(counties_ny) +
  geom_sf()+
  geom_point(data = high_points, aes(Longitude, Latitude, color = Elevation_Feet), size = 2)+
  scale_color_gradientn(colors = HighPoint_cols, name = "Elevation (Feet)")+
  labs(title = "High Point of Each County in New York") +
  theme_map()+ 
  theme(plot.title = element_text(hjust = 0.5))

NY_Map_High_Points_Color 

#5

highpoints_sorted <- high_points[order(high_points$Elevation_Feet), ]
tail(highpoints_sorted, 1)
head(highpoints_sorted, 1)


#Battle Hill in Kings County Ny (Brooklyn) is the lowest high point of any county
#in NY, while Mount Marcy in Essex County is the highest point in NY


#6


NY_Map_High_Points_Color_Lab <- ggplot(counties_ny) +
  geom_sf()+
  geom_point(data = high_points, aes(Longitude, Latitude, color = Elevation_Feet), size = 2)+
  scale_color_gradientn(colors = HighPoint_cols, name = "Elevation (Feet)")+
  labs(title = "High Point of Each County in New York") +
  geom_text(data = tail(highpoints_sorted, 1),aes(Longitude, Latitude, label = High_Point_Name), nudge_y = 0.15, size = 3)+
  theme_map()+ 
  theme(plot.title = element_text(hjust = 0.5))

NY_Map_High_Points_Color_Lab 


#7
library(tidycensus)
pop <- get_decennial(geography = "county",
  state = "NY",
  variables = c(total = "P1_001N"),   
  year = 2020,
  output = "wide"
) |>
  select(GEOID, total)


counties_ny <- counties_ny|> full_join(pop, by = ("GEOID"))


scale_population <- c("lightyellow", "#CD3700")

NY_map_population <- ggplot(counties_ny) +
  geom_sf(aes(fill = total), color = "white") +
  scale_fill_gradientn(colors = scale_population, name = "Population", labels = scales::label_comma()) +
  labs(title = "Counties of New York by Population") +
  theme_map()+ 
  theme(plot.title = element_text(hjust = 0.5))

NY_map_population


#8
counties_ny <- counties_ny|> mutate(pop_density = (total/ALAND))




scale_density <- c("lavender","mediumpurple1", "purple2", "purple4")

NY_map_population_density <- ggplot(counties_ny) +
  geom_sf(aes(fill = pop_density), color = "white") +
  scale_fill_gradientn(colors = scale_density, name = "Population Density", labels = scales::label_comma(),  trans = "sqrt") +
  labs(title = "Counties of New York by Population Density (Square Root)") +
  theme_map()+ 
  theme(plot.title = element_text(hjust = 0.5))

NY_map_population_density



#9
counties_ny_sorted_pop <- counties_ny[order(counties_ny$total), ]
tail(counties_ny_sorted_pop$NAME, 1)  
head(counties_ny_sorted_pop$NAME, 1)  

#Kings County is the most populous in New York
#Hamilton County is the least populous in New York


counties_ny_sorted_density <- counties_ny[order(counties_ny$pop_density), ]
tail(counties_ny_sorted_density$NAME, 1) 
head(counties_ny_sorted_density$NAME, 1) 

#New York County is the densest in NY
#Hamilton County is the least dense in NY


