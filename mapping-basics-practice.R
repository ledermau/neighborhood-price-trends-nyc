if(F){
  install.packages("tigris")
  install.packages("dplyr")
  install.packages("leaflet")
  install.packages("sp")
  install.packages("ggmap")
  install.packages("maptools")
  install.packages("broom")
  install.packages("httr")
  install.packages("rgdal")
  
  install.packages("rgeos")
}

library(tigris)
library(dplyr)
library(leaflet)
library(sp)
library(ggmap)
library(maptools)
library(broom)
library(httr)
library(rgdal)
library(rgeos) #
#Google Maps API Key
#reminder for how to register the API key
#register_google("AIzaSyDiBJV_q9-RBTvuHTFQVI5YMYRDZHfzUk4")

dev.new()
nyc_map <- get_map(location = c(lon = -74.00, lat = 40.71), maptype = "terrain", zoom = 11)

ggmap(nyc_map)

lookup_code("New York", "New York")
#lookup_code("New York", "Kings")
#lookup_code("New York", "Queens")
#lookup_code("New York", "Bronx")
#lookup_code("New York", "Richmond?")

nyc_tracts <- tracts(state = '36', county = c('061','047','081','005','085'))

summary(nyc_tracts)
plot(nyc_tracts)

r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

summary(nyc_neighborhoods)

nyc_neighborhoods_df <- tidy(nyc_neighborhoods)

ggplot() + 
  geom_polygon(data=nyc_neighborhoods_df, aes(x=long, y=lat, group=group))
  
ggmap(nyc_map) + 
  geom_polygon(data=nyc_neighborhoods_df, aes(x=long, y=lat, group=group), color="blue", fill=NA)
  
leaflet(nyc_neighborhoods) %>%
  addTiles() %>% 
  addPolygons(popup = ~neighborhood) %>%
  addProviderTiles("CartoDB.Positron")
  
#generate random lat/longs in the city
set.seed(43)
lats <- 40.7544882 + rnorm(10)/100
lngs <- -73.9879923 + rnorm(10)/200
points <- data.frame(lat=lats, lng=lngs)
points

#get the neighborhood info for the random points
points_spdf <- points
coordinates(points_spdf) <- ~lng + lat
proj4string(points_spdf) <- proj4string(nyc_neighborhoods)
matches <- over(points_spdf, nyc_neighborhoods)
points <- cbind(points, matches)
points

#Interactive view with markers at the points
leaflet(nyc_neighborhoods) %>%
  addTiles() %>% 
  addPolygons(popup = ~neighborhood) %>% 
  addMarkers(~lng, ~lat, popup = ~neighborhood, data = points) %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-73.98, 40.75, zoom = 13)
  
##color the polygons
points_by_neighborhood <- points %>%
  group_by(neighborhood) %>%
  summarize(num_points=n())

map_data <- geo_join(nyc_neighborhoods, points_by_neighborhood, "neighborhood", "neighborhood")

pal <- colorNumeric(
  palette = "RdBu",
  domain = range(map_data@data$num_points, na.rm=T)
)

leaflet(map_data) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~pal(num_points), popup = ~neighborhood) %>% 
  addMarkers(~lng, ~lat, popup = ~neighborhood, data = points) %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-73.98, 40.75, zoom = 13)

##use ggplot to do the same thing
plot_data <- tidy(nyc_neighborhoods, region="neighborhood") %>%
  left_join(., points_by_neighborhood, by=c("id"="neighborhood")) %>%
  filter(!is.na(num_points))

manhattan_map <- get_map(location = c(lon = -74.00, lat = 40.77), maptype = "terrain", zoom = 12)

ggmap(manhattan_map) + 
  geom_polygon(data=plot_data, aes(x=long, y=lat, group=group, fill=num_points), alpha=0.75)

collision_data <- read.csv



