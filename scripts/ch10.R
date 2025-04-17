## Code for Chapter 10 of An Introduction to R for Spatial Analysis and Mapping
# Chris Brunsdon & Lex Comber 

library(tidyverse)
library(sf)
library(ggspatial)
library(jsonlite)
library(httr)
library(tidyr)


aq <- fromJSON("https://api.openaq.org/v2/locations?limit=1000&country=TH")


names(aq)

aq_tib <- as_tibble(aq$results)
aq_tib

aq_tib |> select(lastUpdated)

aq_tib |> mutate(lastUpdated=ymd_hms(lastUpdated)) |> select(lastUpdated)

aq_tib |> select(coordinates)

aq_tib |> select(coordinates) |> unnest_wider(coordinates)

aq_sf <- aq_tib |> 
  unnest_wider(coordinates) |> 
  st_as_sf(coords=c("longitude","latitude"),crs=4326)
aq_sf

ggplot(aq_sf) + 
  annotation_map_tile(type='cartolight') + 
  geom_sf() +  
  theme(text = element_text(size = 8))

library(Intro2R4SpatAnal)
data(bk_prov)

bk_sf <- aq_sf |> st_join(bk_prov) |> filter(ADM1_EN=='Bangkok')
bk_sf

ggplot(bk_sf) + annotation_map_tile(type='cartolight',zoomin=-1) + geom_sf() +
  geom_sf(data=bk_prov,fill=NA,col='Darkred', lwd=1)

bk_sf |> select(parameters)

bk_sf_unnest <- bk_sf |> select(parameters) |> unnest(parameters)
bk_sf_unnest

bk_pm10_ave <- bk_sf_unnest |> filter(parameter == 'pm10') |> 
  filter(lastUpdated == max(lastUpdated),.by=geometry) |> 
  select(ave_pm10=average)
bk_pm10_ave 

# together in a RData file
save(bk_pm10_ave,bk_prov,file='bangkok.RData')
# as separate GeoPackage files
st_write(bk_pm10_ave, "bk_pm10_ave.gpkg")
st_write(bk_prov, "bk_prov.gpkg")
# in a single GeoPackage file
st_write(bk_pm10_ave, "bk_pm10.gpkg", layer = "bk_pm10_ave")
st_write(bk_prov,"bk_pm10.gpkg", layer = "bk_prov" )


library(tidyverse)   
library(sf)
library(eurostat)   # The 'eurostat' package as mentioned in the text
library(giscoR)     # Similarly, the 'giscoR' package
library(sfdep)      # Analyse spatial dependencies for `sf` objects 
library(spdep)      # Ditto

nuts2 <- get_eurostat_geospatial(year='2021',  # <1>
        resolution='10',                       # <2>
        nuts_level = '2',                      # <3>
        crs='3857')                            # <4>

var <- 'lfst_r_lfu3rt'
unemp <- get_eurostat(var) 
head(unemp)                

# migration
search.tmp <- search_eurostat("migrat")
search.tmp |> select(title,code)
# migration and age
index <- grep("age", search.tmp$title, ignore.case = T)
search.tmp |> select(title,code) |> slice(index)

nuts2_unemp <- 
  nuts2 |> # The base polygon object for the new data
  left_join(
    unemp |> #  A subclause to filter out the data of interest 
      filter(isced11=='TOTAL',           
            age=='Y20-64',               
            sex=='T',                    
            TIME_PERIOD==ymd(20180101)), 
  by='geo')                              

nuts2_unemp <- nuts2_unemp |> rename(Unemployment=values)
ggplot(nuts2_unemp,aes(fill=Unemployment)) + 
  geom_sf() + 
  scale_fill_viridis_c(direction=-1) 

nuts2_unemp_eu <- nuts2_unemp |> 
  filter(CNTR_CODE %in% eu_countries$code) # <1>
ggplot(nuts2_unemp_eu) + 
  geom_sf(map=aes(fill=Unemployment)) + 
  scale_fill_viridis_c(direction=-1) +     # <2>
  geom_sf(data=nuts2_unemp,fill=NA)        # <3>

library(ggspatial)
ggplot(nuts2_unemp_eu) + 
  annotation_map_tile(type='cartolight',zoomin=-1) + 
  geom_sf(map=aes(fill=Unemployment)) + 
  scale_fill_viridis_c(direction=-1) 

# create a clip polygon (see Chapter 3)
df <- data.frame(ID = rep(1, 5),
                 X = c(35, 35, -12, -12, 25),
                 Y = c(30, 73, 73, 30, 30))
clip_sf <- 
  df |> st_as_sf(coords = c("X", "Y")) |>
  group_by(ID) |>
  summarise(geometry = st_combine(geometry)) |>
  st_cast("POLYGON") |>
  st_set_crs(4326) |> 
  st_transform(3857)
# clip eurostat data
nuts2_unemp_clip <- nuts2_unemp_eu[clip_sf,]
ggplot(nuts2_unemp_clip) + 
  annotation_map_tile(type='cartolight',zoomin=-1) + # <1>
  geom_sf(map=aes(fill=Unemployment)) + 
  scale_fill_viridis_c(direction=-1) 


nuts2_unemp_de <- nuts2_unemp |> filter(CNTR_CODE == 'DE') 

ggplot(nuts2_unemp_de) + 
  annotation_map_tile(type='cartolight',zoomin=-1) +
  geom_sf(map=aes(fill=Unemployment)) +
  scale_fill_viridis_c(direction=-1, alpha=0.5)

library(tidyverse)
library(osmdata)
library(sf)

vignette(topic = "osmdata", package = "osmdata")

curl::has_internet()
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

osm_q <- opq("Harrogate, UK") |>
  add_osm_feature(key = "amenity", value = "restaurant") |>
  osmdata_sf()

curl::has_internet()
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

osm_q

points_sf <- osm_q$osm_points

points_sf

names(points_sf)
# have at the amenity
unique(points_sf$amenity)

points_sf <- points_sf %>% filter(amenity == "restaurant" )

points_sf %>% dplyr::select(name, cuisine, `addr:city`: `addr:street`) -> points_sf

points_sf |>
  ggplot() +
  annotation_map_tile() +
  geom_sf(col = "red")

# write to shapefile
st_write(points_sf, dsn = ".", layer = "OSMshops.shp", 
         driver = "ESRI Shapefile", delete_layer = T)
# write to geopackage
st_write(points_sf, "OSMshops.gpkg", delete_layer = T)

# OSM street data
osm_q <- opq ("Harrogate, UK") |>
  add_osm_feature("highway") |>
  osmdata_sf()
# extract the line objects
streets <- osm_q$osm_lines[, c("name", "highway", "ref")] 

# examine types
unique(streets$highway)
# this can be filtered
# define a list
road_list <- c("primary", "primary_link" , "secondary", "secondary_link", 
               "tertiary", "tertiary_link", "trunk", "trunk_link", 
               "motorway", "motorway_link")
streets <- 
  streets |> 
	filter(highway %in% road_list)

streets

streets |>
  ggplot() +
  annotation_map_tile(zoom = 12) +
  geom_sf(col = "red")

# OSM building  data
osm_q <- opq("Harrogate, UK") |>
  add_osm_feature("building") |>
  osmdata_sf()

osm_shops <- osm_q$osm_polygons[!is.na(osm_q$osm_polygons$shop), ] 

names(osm_shops)

osm_shops |> select(name, amenity, `addr:city`: `addr:suburb`) -> osm_shops

osm_shops |>
  ggplot() +
  annotation_map_tile(zoom =12) +
  geom_sf(fill = "cyan")

opq("Harrogate, UK") |> add_osm_feature(key = "amenity", value = "restaurant") 

library(httr)
library(jsonlite)
url = paste0("http://data.police.uk/api/crimes-street/all-crime",
                      "?lat=53.7997", 
                      "&lng=-1.5492",
                      "&date=2023-03")
x = GET(url)
crimes <- as_tibble(fromJSON(content(x, as = "text", encoding = "utf8"), flatten = T))
crimes
