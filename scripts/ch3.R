## Code for Chapter 3 of An Introduction to R for Spatial Analysis and Mapping
# Lex Comber & Chris Bunsdon

if (!is.element("sf", installed.packages()))
    install.packages("sf", dep = T)
library(sf)

is.element('sf', installed.packages())

install.packages("sf", dep = TRUE)

library(sf)

# all functions
help(package = "sf")

help(st_read)
# or
?st_read

library(sf)                 # for spatial data handling
library(tidyverse)          # for dplyr operations and ggplot  
library(Intro2R4SpatAnal)   # for example data
library(cols4all)           # for colour palettes
library(tmap)               # for mapping
library(cowplot)            # for combining maps
library(ggspatial)          # for OSM context and backdrops 
library(raster)             # for handling raster objects

if (!is.element("devtools", installed.packages()))
    install.packages("devtools", dep = T)
devtools::install_github("lexcomber/Intro2R4SpatAnal") 

library(Intro2R4SpatAnal)

library(Intro2R4SpatAnal)
library(cols4all)
library(tidyverse)
library(sf)
library(cowplot)
data(leeds_msoa)
data(roads)
data(places)
places <- st_as_sf(places, coords = c("lon", "lat"), crs = 4326)
places <- places |> st_transform(27700)
out <- st_union(leeds_msoa)
out <- st_buffer(out, 5000)
places <- places[out, ]
p1 <- 
  ggplot() + 
  geom_sf(data = roads |> st_transform(27700),
          col = "lightblue") +
  geom_sf(data = leeds_msoa, fill = NA, col = "black") + 
  geom_sf(data = places, col = "red") + 
  geom_sf_label(data = places, aes(label = town), 
                nudge_y = 1000, size = 3) + 
  theme_void() 

p2 <- 
  ggplot() + 
  geom_sf(data = leeds_msoa, aes(fill = unemp), col = "lightgrey") + 
  scale_fill_continuous_c4a_seq(palette = "brewer.oranges", 
                                name = "% Unemp") +
  theme_void() 
plot_grid(p1, p2, ncol = 2, rel_widths = c(0.8, 1))
rm(list = ls())

help(package = "Intro2R4SpatAnal")

# packages
library(Intro2R4SpatAnal)
library(sf)
# load data
data(leeds_msoa)
data(crimes)
data(places)
# list what is present in your workspace
ls()
# examine the data
class(leeds_msoa)
class(crimes)
head(places)

data(leeds_lsoa)      
# standard histogram
ggplot(leeds_lsoa, aes(carework)) +
  geom_histogram(col = "salmon", fill = "lightblue", bins = 20) + 
  xlab("percentage vacant") + 
  labs(title = "The distribution of % in service occupations")
# density histogram
ggplot(tb, aes(x=carework)) + 
  geom_histogram(aes(y=after_stat(density)), col = "salmon", fill = "lightblue", bins = 20) +
  geom_density(alpha=.4, fill="darksalmon") +

p1 <- leeds_lsoa |> st_drop_geometry() |>
  # create the categorical variable
  mutate(MigClass = ordered(ifelse(incomer1yr > quantile(incomer1yr, 0.75), "High", 
                                  ifelse(incomer1yr < quantile(incomer1yr, 0.25), "Low", "Average")),
                           levels = c("Low", "Average", "High"))) |>
  # extract the variables 
  dplyr::select(MigClass, overcrowd, u20, degree) |>
  # make longer
  pivot_longer(-MigClass) |>
  # create the plot 
  ggplot(aes(name, value)) + geom_boxplot() + facet_wrap(~MigClass)
# print the plot
p1 

p1 + geom_boxplot(colour = "yellow", fill = "wheat", alpha = 0.7) + 
  xlab("") + ylab("Percentage") + theme_dark() +
  ggtitle("Boxplot of variables in relation to Neighbourhood churn")

# extract the variables 
leeds_lsoa |> st_drop_geometry() |>
  dplyr::select(incomer1yr, overcrowd, u20, degree) |>
  # make longer
  pivot_longer(-incomer1yr) |>
  # plot 
  ggplot(aes(incomer1yr, value)) + geom_point(alpha = 0.4) + facet_wrap(~name) +
  geom_smooth(method='lm', col = "red") +
  theme_bw() + xlab("LSOA in-migration")

# fit regressions
mod.1 <- lm(incomer1yr ~ overcrowd, data = leeds_lsoa)
mod.2 <- lm(incomer1yr ~ degree, data = leeds_lsoa)
mod.3 <- lm(incomer1yr ~ u20, data = leeds_lsoa)

summary(mod.1)
# not run below
# summary(mod.2)
# summary(mod.3)
# mod.4 = lm(incomer1yr ~ degree + overcrowd + u20, data = leeds_lsoa)
# summary(mod.4)

# extract the geometry to create a sfc object
crimes |> st_geometry()
crimes |> st_geometry() |> class()
# drop the geometry to create a data table & show the first 6 rows
crimes |> st_drop_geometry() |> as_tibble() |> head()

library(sf)
vignette(package = "sf")

library(sf)

vignette("sf1", package = "sf")

# by attribute name
leeds_lsoa[, c("degree", "unemp")]
# by attribute position
names(leeds_lsoa)
leeds_lsoa[, c(8, 11)]

# by row position
leeds_lsoa[ c(1, 3, 5, 7), c("degree", "unemp")]
leeds_lsoa[c(1, 3, 5, 7) , c(8, 11)]

# # by attribute name
leeds_lsoa |> select(degree, unemp)
# by row position
leeds_lsoa |> 
  select(degree, unemp) |>
  slice(c(1, 3, 5, 7))

crimes |> filter(category == "burglary") |> st_drop_geometry() |> head()
crimes |> filter(category == "burglary") |> st_geometry() |> plot()

vignette("dplyr", package = "dplyr")



leeds_msoa |>
  # wrangle
  mutate(top10pc = quantile(unemp, 0.9)) |>
  filter(unemp >= top10pc) |> 
  # plot
  ggplot() +
  geom_sf(fill = "red") +
  geom_sf(data = leeds_lsoa, fill = NA) +
  # apply a style
  theme_bw()

tmp1 <- mutate(leeds_msoa, top10pc = quantile(unemp, 0.9))
tmp2 <- filter(tmp1, unemp >= top10pc) 
ggplot(tmp2) +
  geom_sf(fill = "red") +
  geom_sf(data = leeds_lsoa, fill = NA) +
  theme_bw()

# load the cols4all package and the data
library(cols4all)
data(leeds_lsoa)
# calculate the deprivation index
leeds_lsoa |>
  # select the variables of interest
  select(unemp, overcrowd, owned, nocar) |> 
  # add 1 to unemployment and overcrowding
  # transform by logging
  mutate(unemp = log(unemp+1), overcrowd = log(overcrowd+1)) |>
  # calculate rented households
  mutate(notowned = 100 - owned) |> 
  # standardized to z-scores using the scale function
  mutate_at(c("nocar", "notowned", "overcrowd", "unemp"), scale) |>
  # sum the 4 four standardized scores 
  mutate(TI = as.vector(nocar + notowned + overcrowd+unemp)) |>
  select(TI) |>
  # initiate the plot
  ggplot() +
  geom_sf(aes(fill = TI)) +
  scale_fill_continuous_c4a_seq(palette = "scico.roma", name = "Deprivation") + 
  # apply some style options
  theme_bw() + theme(legend.position = "bottom")

leeds_msoa |> summarise_if(is_double,mean)
leeds_msoa |> st_drop_geometry() |> summarise_if(is_double,mean)  

PCA <- 
  leeds_lsoa |> 
  st_drop_geometry() |> 
  select_if(is_double) |> 
  princomp(cor = T, scores = T)
# the cumulative amount of variance explained by the components
cumsum(PCA$sdev^2/sum(PCA$sdev^2))

# set seed for reproducibility 
set.seed(1234)
# create classification
clus <- 
  PCA$scores |>
  as_tibble() |>
  select(Comp.1: Comp.5) |> 
  kmeans(centers = 6, iter.max = 500, nstart = 20)
# attach classes
leeds_lsoa$class <- as.factor(clus$cluster)
# examine class counts
table(leeds_lsoa$class)

leeds_lsoa |> 
  ggplot() +
  geom_sf(aes(fill = class)) +
  scale_fill_discrete_c4a_cat(palette = "brewer.accent", name = "Class") + 
  # apply some style options
  theme_bw() + theme(legend.position = "bottom")

# 1. find overall means
mean_by_column <- 
  leeds_lsoa |> st_drop_geometry() |> select_if(is_double) |> summarise_all(mean) 
# 2. find overall standard deviations
sd_by_column <- 
  leeds_lsoa |> st_drop_geometry() |> select_if(is_double) |> summarise_all(sd)
# 3. find group means
mean_by_cluster <- 
  leeds_lsoa |> st_drop_geometry() |>
  # group the data and summarise
  group_by(class) |> select_if(is_double) |> summarise_all(mean) |>
  # get rid of the class variable 
  select(-class) 
# 4. create z-scores 
z_scores <- scale(mean_by_cluster, center = mean_by_column, scale = sd_by_column)  
# 5. plot with heatmap
heatmap(t(z_scores),
        scale = 'none',
        col= c4a("tableau.classic_orange_white_blue",6),
        breaks=c(-1e10,-2,-1,0,1,2,+1e10),
        xlab='Class Number',
        add.expr=abline(h=(0:40)+0.5,v=(0:6)+0.5,col='white'))

z_scores |> 
  # convert to tibble, add class, pivot to long format and arrange
  as_tibble() |> 
  mutate(class = factor(paste("Class", 1:n() ))) |> 
  pivot_longer(-class) |> arrange(name) |>
  # plot
  ggplot(aes(x=factor(name), y=value,
             group= class, colour=class, fill=class)) +
  # specify the points and areas for shading
  geom_point(size=2) + 
  geom_polygon(size = 1, alpha= 0.2) +
  # specify the shading
  scale_color_discrete_c4a_div(palette = "brewer.set2", 6) +
  scale_fill_discrete_c4a_div(palette = "brewer.set2", 6) +
  # specify the faceting and the polar plots
  facet_wrap(~class, ncol = 3) +
  coord_polar() +
  # apply some style options
  theme_bw() + 
  theme(legend.position = "none", axis.title = element_blank(),
        axis.text = element_text(size = 6))

crimes |> st_geometry()
leeds_msoa|> st_geometry()

crimes |> st_transform(27700) |> st_geometry()
leeds_msoa|> st_transform(4326) |> st_geometry()

crimes |> st_transform(st_crs(leeds_msoa)) 
leeds_msoa|> st_transform(st_crs(crimes)) 

# clear the workspace
rm(list = ls())

# load the package and data
library(Intro2R4SpatAnal)
data(lads.polys)
data(leeds_msoa)

getwd()

st_write(obj = leeds_msoa, dsn = "msoa", layer = "msoa", driver = "GPKG")

st_delete("msoa", driver = "GPKG")
st_write(obj = leeds_msoa, dsn = "msoa.gpkg", layer = "msoa", driver = "GPKG")

st_write(leeds_msoa, "msoa.gpkg", delete_layer = TRUE)

st_write(leeds_msoa, "msoa.gpkg", delete_layer = TRUE, quiet = T)

# ESRI shapefile 
st_write(leeds_msoa, "msoa.shp", delete_layer = TRUE, quiet = T)
# GeoJSON
st_write(leeds_msoa, "msoa.GeoJSON", delete_layer = TRUE, quiet = T)
# KML 
st_write(leeds_msoa, "msoa.kml", delete_layer = TRUE, quiet = T)

# ESRI Shapefile
msoa <- st_read("msoa.shp", quiet = T)
# GeoJSON
msoa <- st_read("msoa.GeoJSON", quiet = T)
# KML
msoa <- st_read("msoa.kml", quiet = T)

# split the URL so it fits on the page
url = paste0("https://api.os.uk/downloads/v1/products/OpenGreenspace/",
             "downloads?area=GB&format=GeoPackage&redirect")
# download the zip file to a local zip file
download.file(url, "os_gs.zip", mode = "wb")
# unzip the file to a folder
unzip("os_gs.zip", exdir="os_gs")
# load the data from the folder
os_gs <- st_read("os_gs/Data/opgrsp_gb.gpkg", layer = "access_point", quiet = T)

# 1 create the file with msoa
st_write(leeds_msoa, "leeds_data.gpkg", layer = "msoa")
# 2 add the re-projected crimes layer 
st_write(crimes |> st_transform(st_crs(leeds_msoa)), 
         "leeds_data.gpkg", layer = "crimes" )
# 3 read the crimes layer back in 
crimes2 <- st_read("leeds_data.gpkg", layer = "crimes", quiet = T)

# load the data
data(leeds_msoa)
# set a seed for reproducibility
set.seed(123)
# define an output
coords <- 
  leeds_msoa |> 
  # transform the data to WGS84 
  st_transform(4326) |>
  # extract are centroid coordinates
  st_centroid() |> st_coordinates() |>
  # convert to tibble and sample 10 rows
  as_tibble() |> sample_n(10) |>
  # create the output
  mutate(ID = paste("Loc", 1:n()), value = rnorm(n()))
# have a look 
coords

points_sf <- 
  coords |> 
  st_as_sf(coords = c("X", "Y"), crs = 4326)

points_sf
# plot the points
points_sf |> 
  ggplot() + geom_sf() +
  # add some context
  geom_sf(data = leeds_msoa, fill = "tomato", alpha = 0.2) +
  # specify an empty layout: good for maps!
  theme_void()

# load the places data 
data(places)
# create an sf object
places_sf <- 
  places |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(27700)
# inspect 
places_sf

ggplot() + 
  geom_sf(data = leeds_msoa, fill = NA) +
  geom_sf(data = places_sf, col = "red") +
  # add a label
  geom_sf_label(data = places_sf, aes(label = town), nudge_y = 3000, size = 3) + 
  theme_bw() + xlab("") + ylab("") +
  # specify OSGB projection
  coord_sf(datum = st_crs(leeds_msoa)) 

leeds.roads <- roads |> slice(4524,  4174) |> st_coordinates()
head(leeds.roads, n = 4)
tail(leeds.roads, n = 4)

leeds.roads |>
  data.frame() |> 
  st_as_sf(coords = c("X", "Y"), crs = 4326) |>
  summarise(geometry = st_combine(geometry)) |>
  st_cast("LINESTRING")

leeds_roads <- 
  leeds.roads |>
  data.frame() |> 
  st_as_sf(coords = c("X", "Y"), crs = 4326) |>
  group_by(L1) |>
  summarise(geometry = st_combine(geometry)) |>
  st_cast("LINESTRING")
# inspect the output
leeds_roads

# define coordinates
X = c(1, 1, 10, 10, 1)
Y = c(1, 10, 10, 1, 1)
# plot(X, Y, col = "Red", pch = 19)
# create a data frame with a grouping ID
df = data.frame(X = c(X, X +10), 
                Y = c(Y, Y + 10),
                ID = c(rep("Poly1", 5), rep("Poly2", 5)))
# create sf polygon object
df_sf <- 
  df |> st_as_sf(coords = c("X", "Y")) |>
  group_by(ID) |>
  summarise(geometry = st_combine(geometry)) |>
  st_cast("POLYGON")
# inspect and plot
df_sf
ggplot(df_sf) + geom_sf(aes(fill = ID))

# load the data and extract the names of the areas
data(lads.polys)
poly.names = names(lads.polys)
# extract Leeds and examine the coordinates
leeds <- lads.polys[[22]]
head(leeds, n = 2)
tail(leeds, n = 2)

polygon <- 
  leeds |>
  data.frame() |> 
  st_as_sf(coords = c("X", "Y"), crs = 27700) |>
  summarise(geometry = st_combine(geometry)) |>
  st_cast("POLYGON")

# check the plot order
leeds |> data.frame() |> 
  # create an initial sf (point layer) with an ID
  st_as_sf(coords = c("X", "Y"), crs = 27700) |>
  mutate(ID = 1:n()) |>
  ggplot() + geom_sf() +
  geom_sf_label(aes(label = ID), size = 3) + 
  theme_bw()   

# create an empty object
polys = NULL
for(i in 1:length(lads.polys)){
  poly.i <- 
    lads.polys[[i]] |>
    data.frame() |> 
    st_as_sf(coords = c("X", "Y"), crs = 27700) |>
    summarise(geometry = st_combine(geometry)) |>
    st_cast("POLYGON")
  # row bind poly.i to polys
  polys <- rbind(polys, poly.i)
}
polys <- polys |> st_cast("POLYGON")
# add the names
polys <- 
  polys |> mutate(LAD = names(lads.polys)) |> relocate(LAD)

polys |> 
  ggplot() + geom_sf() +
  geom_sf_text(aes(label = LAD), cex = 2.5) +
  theme_bw() + xlab("") + ylab("") 

# check your current working director
getwd()

# write to R binary file
save(polys, file = "polys.RData")

my.polys <- st_coordinates(leeds_msoa |> slice(1:10, )) 

summary(my.polys)
my.sf <- 
  my.polys |> 
  data.frame() |>
  st_as_sf(coords = c("X", "Y")) |>
  group_by(L3) |>
  summarise(geometry = st_combine(geometry)) |>
  st_cast("POLYGON") 
my.sf |>  ggplot() + geom_sf()

# points
points_sf |> ggplot() + geom_sf()
# lines
leeds_roads |> ggplot() + geom_sf() 
# areas
leeds_msoa |> ggplot() + geom_sf() 
# embellishments
leeds_msoa |> ggplot() + geom_sf() + 
  # specify OSGB projection
  coord_sf(datum = st_crs(leeds_msoa)) + 
  theme_bw()  

# points
points_sf |> ggplot() + geom_sf(col = "red")
# lines
leeds_roads |> ggplot() + geom_sf(lwd = 1, lty = 2) 
# areas
leeds_msoa |> 
  ggplot() + geom_sf(fill = "tomato", colour = "dodgerblue") 
# areas with points and transparency
points_sf |> ggplot() + geom_sf(col = "darkgreen") +
  # note the 2nd data later has to be specified
  geom_sf(data = leeds_msoa, fill = "tomato", alpha = 0.1) + 

# points
points_sf |> ggplot(aes(size = value)) + geom_sf()
# areas
leeds_msoa |> ggplot(aes(fill = degree)) + geom_sf() 

library(cols4all)
leeds_msoa |> ggplot(aes(fill = degree)) + geom_sf() + 
  scale_fill_continuous_c4a_seq("scico.batlow")

library(cols4all)0
c4a_gui()

leeds_msoa |> ggplot(aes(fill = degree)) + geom_sf() + 
  scale_fill_continuous_c4a_seq("brewer.yl_gn_bu")

data(leeds_lsoa) 
leeds_lsoa |> ggplot(aes(fill = unemp)) + geom_sf() +
  scale_fill_binned_c4a_seq("kovesi.linear_yl_rd_bk")

data(pm10) 
# by size
pm10 |> ggplot(aes(size = pm102021g)) + geom_sf()  
# by colour
pm10 |> ggplot(aes(col = pm102021g)) + geom_sf(size = 5) +
  scale_colour_continuous_c4a_seq("kovesi.linear_yl_rd_bk")

leeds |> data.frame() |> ggplot(aes(x = X, y = Y)) + geom_polygon() + 
  # ensure the aspect ratio is 1:1
  coord_sf()  

polys |> st_coordinates() |> data.frame() |> mutate(L2 = factor(L2)) |>
  ggplot(aes(x = X, y = Y, group = L2, fill = L2)) + geom_polygon() + 
  coord_sf() +xlab("") + ylab("")

# example using poly glasbey
polys |> st_coordinates() |> data.frame() |> mutate(L2 = factor(L2)) |>
  ggplot(aes(x = X, y = Y, group = L2, fill = L2)) + geom_polygon() + 
  coord_sf() +xlab("") + ylab("") +
  scale_fill_discrete_c4a_div("poly.glasbey")

# PM10 grid 
pm10.grid |>
  ggplot(aes(x = X, y = Y, fill = pm102021g)) +
  geom_raster() + coord_sf() + xlab("") + ylab("") +
	scale_fill_continuous_c4a_seq(palette="kovesi.linear_wh_rd_bk", name = "PM10")
# simulated example	with a spatial trend 
expand.grid(X = 1:25, Y = 25:1) |> 
  mutate(value = 1 + ( (1/12) * (gr$X + gr$Y) )) |>  
  ggplot(aes(x = X, y = Y, fill = value)) + 
  geom_raster() + coord_sf() + theme_void() +
	scale_fill_continuous_c4a_seq(palette="kovesi.linear_wh_rd_bk")

# make p1, a basic plot 
p1 <- pm10.grid |>
  ggplot(aes(x = X, y = Y, fill = pm102021g)) +
  geom_raster() + coord_sf() + xlab("") + ylab("") +
	scale_fill_continuous_c4a_seq(palette="kovesi.linear_wh_rd_bk", name = "PM10")

p1

# legend position 
p1 + theme(legend.position = "bottom")
# legend size
p1 + theme(legend.key.height= unit(1, 'cm'),
           legend.key.width= unit(1, 'cm')) 
# legend text size
p1 + theme(legend.text = element_text(size = 15), 
           legend.title = element_text(size = 25))

# axis text size
p1 + theme(axis.text = element_text(size=12))
p1 + theme(axis.text.y = element_text(size=12))
# axis ticks
p1 + theme(axis.ticks=element_blank())
p1 + theme(axis.ticks.length =unit(0.5, 'cm'))

p1 + theme(panel.background = element_rect(fill = "lightgreen"))
p1 + theme(plot.background = element_rect(fill = "lightgreen"))

library(ggspatial)
p1 <- 
  polys |> 
  ggplot() +
  annotation_map_tile(type = 'osm') + 
  geom_sf(alpha = 0.2,  fill = "tomato") +
  theme_void()

p2 <- 
  # transform from OSGB to WGS84
  leeds_msoa |> st_transform(4326) |>
  ggplot() +
  annotation_map_tile(zoom = 10, type='cartolight') + 
  geom_sf(alpha = 0.2, lwd = 0.5, fill = "darkgreen") +
  theme_void()
plot_grid(p1, p2, ncol = 2)

help(package = 'ggspatial')

p1 <- polys |> ggplot() + geom_sf() + theme_void()
p1 + annotation_scale() 

# corner
p1 + annotation_scale(location = "tl") 
# position
p1 + annotation_scale(pad_x = unit(2.2, "in"), pad_y = unit(0.1, "in"))

# corner
p1 + annotation_north_arrow(location = "tl") 
# style
p1 + annotation_north_arrow(style = north_arrow_fancy_orienteering) 

leeds_lsoa |>
  ggplot(aes(fill = nocar)) +
  annotation_map_tile(zoom = 10, type='opencycle') + 
  geom_sf() + coord_sf(datum = st_crs(leeds_lsoa)) + xlab("") + ylab("") +
	scale_fill_continuous_c4a_seq(palette="kovesi.linear_wh_rd_bk", 
	                              name = "No Car\nOwnership") +
  annotation_north_arrow(style = north_arrow_fancy_orienteering,
                         pad_x = unit(0.3, "in"), pad_y = unit(0.15, "in")) +
  annotation_scale(location = "bl") +
  theme(axis.text = element_text(size=8))

# clear the workspace
rm(list = ls())

# load the packages and data
library(sf)
library(tmap)
library(Intro2R4SpatAnal)
data(leeds_msoa)        # areas
data(leeds_lsoa)        # areas
data(crimes)            # points
data(roads)             # lines
data(pm10)              # points
load("polys.RData")     # LAD areas
# transform to WGS84
leeds_msoa <- leeds_msoa |> st_transform(4326)
leeds_lsoa <- leeds_lsoa |> st_transform(4326)

qtm(leeds_msoa, fill = "red", style = "natural")

qtm(leeds_msoa, fill = "incomer1yr", text.size=0.5, 
    format="World_wide", style="classic",
    fill.title="% moved to \nthe LSOA")

# do a merge
leeds_outline <- st_union(leeds_msoa)

tm_shape(leeds_msoa) + tm_fill("tomato")

tm_shape(leeds_msoa) + tm_fill("tomato") +
  tm_borders(lty = "dashed", col = "gold")

tm_shape(leeds_msoa) + tm_fill("tomato") +
  tm_borders(lty = "dashed", col = "gold") +
  tm_style("natural", bg.color = "grey90")

tm_shape(leeds_msoa) + tm_fill("tomato") +
  tm_borders(lty = "dashed", col = "gold") +
  tm_style("natural", bg.color = "grey90") +
  # add the outline
  tm_shape(leeds_outline) + tm_borders(lwd = 2)

tm_shape(leeds_msoa) + tm_fill("tomato") +
  tm_borders(lty = "dashed", col = "gold") +
  tm_style("natural", bg.color = "grey90") +
  # add the outline
  tm_shape(leeds_outline) + tm_borders(lwd = 2) +
  tm_title(text = "Leeds MSOAs")

# MSOAs with roads
t1 <- 
  tm_shape(leeds_msoa) + tm_polygons("coral") + tm_layout(bg.color = "grey85") +
  # add roads clipped to the outline
  tm_shape(roads[leeds_outline, ]) + tm_lines() 
# MSOAs with crime locations
t2 <- 
  tm_shape(leeds_msoa) + tm_polygons("orange") + tm_layout(bg.color = "grey95") +
  tm_shape(crimes) + tm_dots() 
# plot together
tmap_arrange(t1, t2, ncol = 2)

# points
tm_shape(crimes) + 
  tm_symbols(col = "tomato", fill = NA, size = 0.5, shape = 1, col_alpha = 0.3)
# lines 
tm_shape(roads |> filter(highway == "trunk")) + tm_lines(lwd = 3, col = "darkblue")
# areas outlines
tm_shape(leeds_msoa) + tm_borders(col = "chartreuse") 
# areas fill
tm_shape(leeds_msoa) + tm_fill(col = "cornflowerblue") 
# areas outlines & fill
tm_shape(leeds_msoa) + tm_polygons(col = "cornflowerblue", border.col = "chartreuse") 

tmap_mode("plot")
tm_shape(leeds_lsoa) +
  tm_polygons(fill = "muslim")

tm_shape(leeds_lsoa) + 
  tm_polygons(fill = "muslim",  
              fill.scale = tm_scale_intervals(breaks = seq(0, 100, 20)))

tm_shape(leeds_lsoa) + 
  tm_polygons(fill = "muslim",  
              fill.scale = tm_scale_intervals(breaks = seq(0, 100, 20)), 
              fill.legend = tm_legend(position=c("left", "bottom"), 
                                      bg.color="grey95", frame=TRUE, title = "% Muslim")) 

c4a_gui()

c4a_palettes(type = "seq", series = "brewer")

c4a("brewer.yl_or_rd", n = 9)

tm_shape(leeds_lsoa) + 
  tm_polygons(fill = "nocar",  
              fill.scale = tm_scale_intervals(breaks = seq(0, 90, 10), 
                                              values = "brewer.yl_or_rd"), 
              fill.legend = tm_legend(position=c("left", "bottom"), 
                                      bg.color="grey95", frame=TRUE, title = "% No Car")) 

tm_shape(leeds_lsoa) + 
  tm_polygons(fill = "nocar",  
              fill.scale = tm_scale_intervals(values = "brewer.yl_or_rd",
                                              style = "kmeans"), 
              fill.legend = tm_legend(position=c("left", "bottom"), 
                                      bg.color="grey95", frame=TRUE, title = "% No Car")) 

tm_shape(leeds_lsoa) +
  tm_polygons(rep("nocar", 3), col = NA,
              fill.scale = list(tm_scale_intervals(values = "brewer.yl_or_rd", 
                                                   style = "kmeans"),
                                tm_scale_intervals(values = "brewer.reds"),
                                tm_scale_continuous(values = "brewer.yl_or_br")), 
              fill.legend = tm_legend("", position=c("left", "bottom"), 
                                      bg.color="grey95")) +
  tm_layout(panel.labels = c("kmeans", "equal interval", "continuous"), 
            inner.margins = c(0.05, 0.4, 0.1, 0.05))+
  tm_shape(leeds_outline) + tm_polygons(fill = NA)

tm_shape(leeds_lsoa) +
  tm_polygons("nocar", fill.scale = tm_scale_intervals(n = 7, values = "-brewer.gn_bu"), 
              fill.chart = tm_chart_histogram(position = c("left", "bottom")),
              fill.legend = tm_legend("No Car", position=c("left", "bottom"), 
                                      bg.color="grey85")) +
  tm_layout(inner.margins = c(0.05, 0.4, 0.1, 0.05),
            legend.text.size = 0.7, 
            bg.color = "grey95") +
  tm_compass(position = c("left", "top")) + 
  tm_scalebar(position = c("left", "top"))

# points
p1 <- 
  pm10 |> 
  tm_shape() + 
  tm_symbols(fill = "pm102021g", col = NA, size = 0.5,
             fill.scale = tm_scale_continuous(values = "kovesi.linear_wh_rd_bk"), 
             fill.legend = tm_legend("PM10", position=c("left", "bottom")))
# lines 
p2 <- 
  roads |> filter(highway == "trunk") |>
  tm_shape() + 
  tm_lines(lwd = "length", 
           lwd.scale = tm_scale(values.scale = 4),
           lwd.legend = tm_legend("Road Segment\nlength", position=c("left", "bottom"),
                                  bg.color="white"))
# combine the plots
tmap_arrange(p1, p2, ncol = 1)

library(terra)
# extract the extent of the layer
ex <- as.vector(ext(pm10))
# create a raster object
r <- rast(xmin = ex[1]-500, xmax = ex[2]+500, ymin = ex[3]-500, ymax = ex[4]+500, 
          resolution = 1000, crs = "epsg:27700")
# use rasterize to create the raster
pm10_r <- rasterize(x = vect(pm10), y = r, fun = mean, field = "pm102021g")
names(pm10_r) <- "pm10"

summary(extract(pm10_r, pm10))
summary(pm10_r$pm10)
image(pm10_r, asp = 1)

tm_shape(pm10_r) +
  tm_raster(col = "pm10",
            col.scale = tm_scale_intervals(n = 7, values = "brewer.gn_bu"), 
            col.legend = tm_legend("PM10", position=c("left", "bottom"))) +
  tm_layout(frame = F)

library(png)
library(grid)
img <- readPNG("ch3.tmapraster1.png")
grid.raster(img)

# set the tmap mode to view
tmap_mode('view')
tm_shape(pm10_r) +
  tm_raster(col = "pm10", col_alpha = 0.4, 
            col.scale = tm_scale_intervals(n = 7, style = "kmeans", 
                                           values = "brewer.gn_bu"), 
            col.legend = tm_legend("PM10", position=c("left", "bottom"))) +
  tm_layout(frame = F) +
  tm_basemap("OpenStreetMap")
# reset tmap mode
tmap_mode('plot')

library(png)
library(grid)
img <- readPNG("ch3.tmapraster2.png")
grid.raster(img)

tm_shape(pm10_r) +
  tm_raster(col = "pm10", 
            col.scale = tm_scale_continuous(n = 7, values = "brewer.gn_bu"), 
            col.legend = tm_legend("PM10", position=c("left", "bottom")),
            col.chart = tm_chart_histogram(position = c("left", "bottom"))) +
  tm_layout(inner.margins = c(0.05, 0.4, 0.1, 0.05),
            legend.text.size = 0.7) +
  tm_compass(position = c("left", "bottom")) + 
  tm_scalebar(position = c("left", "bottom"))

polys |> 
  tm_shape() +
  tm_polygons(fill = "white") +
  tm_text("LAD", options = opt_tm_text(remove.overlap = T))

polys |> 
  tm_shape() +
  tm_polygons(fill = "white", alpha = 0.5) +
  tm_text("LAD", options = opt_tm_text(remove.overlap = T)) +
  tm_basemap("OpenStreetMap")

tmap_mode('view') +
polys |> 
  tm_shape() +
  tm_polygons(fill = "white", alpha = 0.3) +
  tm_text("LAD")

library(png)
library(grid)
img <- readPNG("ch3.tmapview1.png")
grid.raster(img)

tmap_mode('view') +
polys |> 
  tm_shape() +
  tm_polygons(fill = NA, col = "red") +
  tm_text("LAD", col = "white") +
  tm_basemap("Esri.WorldImagery")

library(png)
library(grid)
img <- readPNG("ch3.tmapview2.png")
grid.raster(img)

# load packages
library(sf)                 # for spatial data handling
library(Intro2R4SpatAnal)   # for example data
library(cols4all)           # for colour palettes
library(tmap)               # for mapping
library(dplyr)              # for data wrangling
library(terra)              # for raster handling
# load and prep data 
data(leeds_msoa)
data(roads)
data(pm10)
# transformations
roads <- roads |> st_transform(27700)
# create raster as before
ex <- as.vector(ext(pm10))
r <- rast(xmin = ex[1]-500, xmax = ex[2]+500, ymin = ex[3]-500, ymax = ex[4]+500, 
          resolution = 1000, crs = "epsg:27700")
pm10_r <- rasterize(x = vect(pm10), y = r, fun = mean, field = "pm102021g")
names(pm10_r) <- "pm10"
# make the map of the pm10 raster
p1 <- 
  tm_shape(pm10_r) +
  tm_raster(col = "pm10",
            col.scale = tm_scale_intervals(n = 7, values = "brewer.gn_bu"), 
            col.legend = tm_legend("PM10", position=c("left", "bottom"))) +
  # 2. add the MSOA context
  tm_shape(leeds_msoa ) +
  tm_polygons(fill = NA) +
  # 3. add the roads layer
  tm_shape(roads[leeds_msoa,] |> filter(highway == "motorway")) +
  tm_lines(col = "blue", lwd = 2.5) +
  # 4. embellish the map
  tm_compass(position = c("left", "bottom")) + 
  tm_scalebar(position = c("left", "bottom")) + 
  tm_layout(legend.frame = F, 
            legend.outside = T) +
  tm_title("PM10 levels and Motoways in Leeds, UK")
# print the map
print(p1)

source("leeds_map.R")

tm_lines(col = "red") +

tm_borders(col = "red", lwd = 2.5) +

pdf(file='map.pdf')

dev.off()

png(file='map.png')

pdf()
png()
tiff()

pdf(file = "MyPlot.pdf", other setting)
<map code>
dev.off()

# hints 
c4a_gui() # to show the all palettes
fill.scale # parameter in tm_polygons to specify the palette
tm_scale_intervals # function passed fill.scale 
# tools
library(tmap) # for the mapping tools
data(leeds_msoa) # for the MSOA data
tiff() # to open a TIFF file 
dev.off() # to close it

tiff(file = "mymap.tiff")
tm_shape(leeds_msoa) + 
  tm_polygons("manual", fill.scale = 
                tm_scale_intervals(values ="brewer.reds", n = 7, style = "kmeans")) +
  tm_scalebar() +
  tm_layout(frame = T, legend.position = c("left", "bottom"))
dev.off()

# hints
p1 <- tm_shape(...) # assigns the plots to an object 
tmap_arrange(...) # combines multiple plot objects
tm_scale_intervals # function passed fill.scale 
# tools
library(tmap) # for the mapping tools
data(leeds_msoa) # for the MSOA data

p1 <- 
  tm_shape(leeds_msoa) + 
  tm_polygons("muslim", fill.scale = 
                tm_scale_intervals(values ="brewer.yl_or_rd", n = 6, style = "quantile")) +
  tm_title("% Muslim by Quantile") + 
  tm_layout(frame = F , legend.position = c("left", "bottom"))

p2 <- 
  tm_shape(leeds_msoa) + 
  tm_polygons("muslim", fill.scale = 
                tm_scale_intervals(values ="brewer.yl_or_rd", n = 6, style = "equal")) +
  tm_title("% Muslim by Equal Interval") + 
  tm_layout(frame = F, legend.position = c("left", "bottom"))
p3 <- 
  tm_shape(leeds_msoa) + 
  tm_polygons("muslim", fill.scale = 
                tm_scale_intervals(values ="brewer.yl_or_rd", n = 6, style = "sd")) +
  tm_title("% Muslim by Standard Deviation") + 
  tm_layout(frame = F, legend.position = c("left", "bottom"))
tmap_arrange(p1,p2,p3, ncol = 3)

# hints
df$a <- (df$b < 3) # syntax to create new variable with a logical test 
# tools
library(tmap) # for the mapping tools
data(leeds_lsoa) # for the LSOA data

leeds_lsoa$`Old Population` <- (leeds_lsoa$o65 > 20)
tm_shape(leeds_lsoa) + 
  tm_polygons("Old Population", tm_scale_ordinal(values =c("chartreuse4","darkgoldenrod3"))) 

new_sf <- st_transform(old_sf, new.Projection)

# hints 
annotation_map_tile # function for adding context in ggplot / ggspatial;
tm_basemap # function for adding context in tmap 
# tools
library(tmap) # for the mapping with tmap
library(ggplot2) # for mapping with ggplot 
library(ggspatial)  # of OSM with ggplot 
data(lads.polys) # for the LADS data

library(tmap)	      # for the mapping tools
library(ggplot2) 
library(ggspatial)
data(lads.polys)
# create and transform the data 
polygon <- lads.polys[[22]] |>
  data.frame() |> 
  st_as_sf(coords = c("X", "Y"), crs = 27700) |>
  summarise(geometry = st_combine(geometry)) |>
  st_cast("POLYGON") |> 
  st_transform(4326) 
# ggplot
polygon |>
  ggplot() +
  annotation_map_tile(zoom = 11, type='cartolight') + 
  geom_sf(alpha = 0.2, fill = "rosybrown3") +
  theme_void()
# tmap 
polygon |> 
  tm_shape() + 
  tm_polygons(fill = "rosybrown3", fill_alpha = 0.4) +
  tm_basemap(zoom = 11, "OpenStreetMap")


# 1. create the file with msoa
st_write(leeds_msoa, "leeds_data.gpkg", layer = "msoa")
# 2. add the re-projected crimes layer 
st_write(crimes |> st_transform(st_crs(leeds_msoa)), 
         "leeds_data.gpkg", layer = "crimes" )
# 3. read the crimes layer back in 
crimes2 <- st_read("leeds_data.gpkg", layer = "crimes", quiet = T)

my.polys <- st_coordinates(leeds_msoa |> slice(1:10, )) 
summary(my.polys)
my.sf <- 
  my.polys |> 
  data.frame() |>
  st_as_sf(coords = c("X", "Y")) |>
  group_by(L3) |>
  summarise(geometry = st_combine(geometry)) |>
  st_cast("POLYGON") 
my.sf |>  ggplot() + geom_sf()

data(leeds_lsoa) 
leeds_lsoa |> ggplot(aes(fill = unemp)) + geom_sf() +
  scale_fill_binned_c4a_seq("kovesi.linear_yl_rd_bk")

data(pm10) 
# by size
pm10 |> ggplot(aes(size = pm102021g)) + geom_sf()  
# by colour
pm10 |> ggplot(aes(col = pm102021g)) + geom_sf(size = 5) +
  scale_colour_continuous_c4a_seq("kovesi.linear_yl_rd_bk")

# example using poly glasbey
polys |> st_coordinates() |> data.frame() |> mutate(L2 = factor(L2)) |>
  ggplot(aes(x = X, y = Y, group = L2, fill = L2)) + geom_polygon() + 
  coord_sf() +xlab("") + ylab("") +
  scale_fill_discrete_c4a_div("poly.glasbey")

tiff(file = "mymap.tiff")
tm_shape(leeds_msoa) + 
  tm_polygons("manual", fill.scale = 
                tm_scale_intervals(values ="brewer.reds", n = 7, style = "kmeans")) +
  tm_scalebar() +
  tm_layout(frame = T, legend.position = c("left", "bottom"))
dev.off()

p1 <- 
  tm_shape(leeds_msoa) + 
  tm_polygons("muslim", fill.scale = 
                tm_scale_intervals(values ="brewer.yl_or_rd", 
                                   n = 6, style = "quantile")) +
  tm_title("Quantile", position = tm_pos_in("left", "TOP")) 
p2 <- 
  tm_shape(leeds_msoa) + 
  tm_polygons("muslim", fill.scale = 
                tm_scale_intervals(values ="brewer.yl_or_rd", n = 6, style = "equal")) +
  tm_title("Equal Interval",  position = tm_pos_in("left", "TOP"))
p3 <- 
  tm_shape(leeds_msoa) + 
  tm_polygons("muslim", fill.scale = 
                tm_scale_intervals(values ="brewer.yl_or_rd", n = 6, style = "sd")) +
  tm_title("Standard Deviation", position = tm_pos_in("left", "TOP")) 
tmap_arrange(p1,p2,p3, ncol = 3)

leeds_lsoa$`Old Population` <- (leeds_lsoa$o65 > 20)
tm_shape(leeds_lsoa) + 
  tm_polygons("Old Population", tm_scale_ordinal(values =c("chartreuse4","darkgoldenrod3"))) 

library(tmap)	      # for the mapping tools
library(ggplot2) 
library(ggspatial)
data(lads.polys)
# create and transform the data 
polygon <- lads.polys[[22]] |>
  data.frame() |> 
  st_as_sf(coords = c("X", "Y"), crs = 27700) |>
  summarise(geometry = st_combine(geometry)) |>
  st_cast("POLYGON") |> 
  st_transform(4326) 
# ggplot
polygon |>
  ggplot() +
  annotation_map_tile(zoom = 11, type='cartolight') + 
  geom_sf(alpha = 0.2, fill = "rosybrown3") +
  theme_void()

# tmap 
polygon |> 
  tm_shape() + 
  tm_polygons(fill = "rosybrown3", fill_alpha = 0.4) +
  tm_basemap(zoom = 11, "OpenStreetMap")
