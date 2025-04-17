## Code for Chapter 5 of An Introduction to R for Spatial Analysis and Mapping
# Lex Comber & Chris Brunsdon

library(sf)
library(Intro2R4SpatAnal)
library(tmap)
library(tidyverse) 
library(cols4all)
library(units)
library(cowplot)
library(terra)

data(crimes)
data("leeds_msoa")

# plot extent and grey background
ggplot() + 
  geom_sf(data = leeds_msoa , fill = "grey90") +
  # add the crime points 
  geom_sf(data = crimes, col = "#FB6A4A", size = 0.4, shape = 1, alpha = 0.5) +
  theme_bw() + xlab("") + ylab("") 

# coord_sf(datum = st_crs(leeds_msoa))

# set the viewing mode to interactive
tmap_mode("view")
tm_shape(leeds_msoa) + tm_polygons("grey90", fill_alpha = 0.2) + 
  # add the crime points
  tm_shape(crimes) + tm_dots(col = "#FB6A4A", size = 0.1, shape = 1, col_alpha = 0.5) 
# reset the tmap mode
tmap_mode("plot")

summary(crimes)
table(crimes$category)

msoa_names <- 
  paste0("Leeds ", c("042", "038", "034", "032", "048", "037", "020", "024", "023"))
AoI <- 
  leeds_msoa |>
  filter(MSOA21NM %in% msoa_names) 

tmap_mode("view")
tm_shape(leeds_msoa) + tm_polygons(fill = NULL) +
  tm_text("MSOA21NM") + tm_basemap("OpenStreetMap")
tmap_mode("plot")


ggplot() + 
  geom_sf(data = AoI , fill = NA) +
  # add the crime points 
  geom_sf(data = crimes, col = "#FB6A4A", size = 0.4, shape = 1, alpha = 0.5) 

# re-project crimes
crimes_xy <- crimes |> st_transform(27700)
# clip using the extent of AoI
crimes_clip <- crimes_xy[AoI,]

ggplot() + 
  geom_sf(data = AoI) +
  # add the crime points 
  geom_sf(data = crimes_clip, col = "#FB6A4A", size = 2, shape = 1, alpha = 0.5) +
  geom_sf_text(data = AoI, aes(label = MSOA21NM)) + 
  theme_bw() + xlab("") + ylab("") 

# intersection test
int = st_intersects(crimes_xy, AoI, sparse = F)
dim(int)
# logical test for each row
index <- rowSums(int) > 0
# subset crimes_xy
crimes_clip2 = crimes_xy[index, ]
# compare
identical(crimes_clip, crimes_clip2)

# intersection test
int = st_intersects(leeds_msoa, crimes_xy, sparse = F)
# the sum of each matrix row is the polygon count  
crime.count <- rowSums(int)
crime.count

leeds_msoa |> mutate(crimes = crime.count) |> relocate(crimes, .after = incomer1yr)

poly.counts.sf = function(polys, pts) {
  int = st_intersects(polys, pts, sparse = F)
	rowSums(int)
}
poly.counts.sf(leeds_msoa, crimes_xy)

AoI_crimes <- st_intersection(x = AoI, y = crimes_xy)

print(AoI_crimes, n = 4)

# do a merge
aoi_outline <- st_union(AoI)
# plot with MSOA context
ggplot(data = AoI) + geom_sf() +
  geom_sf(data = aoi_outline, lwd = 1, fill = NA)

msoa_names2 <- 
  paste0("Leeds ", c("110", "055", "063", "054"))
AoI.2 <- 
  leeds_msoa |>
  filter(MSOA21NM %in% msoa_names2) 
ggplot() + 
  geom_sf(data = AoI.2 , fill = "tomato") +
  geom_sf(data = AoI, fill = "dodgerblue") 

dim(AoI); dim(AoI.2)
rbind(AoI.2, AoI)

# load data 
data(leeds_lsoa)
data(leeds_msoa)
# 3 large polygons with unemp attribute
polys1 <- 
  leeds_msoa |> filter(str_detect(MSOA21NM, "025|023|034")) |>
  select(unemp) |> mutate(ID1 = 1:n())
# 10 small ones with degree attribute
set.seed(132)
polys2 <- 
  leeds_lsoa |>  filter(str_detect(LSOA21NM, "025|023|034|032")) |>
  select(degree) |>  sample_n(10) |> mutate(ID2 = 1:n()) 
# plot
ggplot() + 
  geom_sf(data = polys1, lwd = 1,fill = "tomato", alpha = 0.3) +
  geom_sf(data = polys2, fill = "dodgerblue", alpha = 0.3) +
  theme_bw() + xlab("") + ylab("") 

p1p2_u <- st_union(polys1, polys2)
p1p2_u

AoI$mergeflag = 1:nrow(AoI)
AoI$mergeflag[8] = 1
AoI$mergeflag[5] = 1

tmap_mode("view")
tm_shape(AoI) + tm_polygons(fill = NULL) + 
tm_text("mergeflag") + 
tm_basemap() 
tmap_mode("plot")

tmap_mode("view")
AoI %>% 
  group_by(mergeflag) %>% 
  summarise() |>  
  tm_shape() + tm_polygons(fill = NULL) + tm_text("mergeflag")
tmap_mode("plot")

AoI %>% 
  group_by(mergeflag) %>% 
  summarise(across(geom, ~ st_union(.)), .groups = "keep") %>%
  summarise(across(geom, ~ st_combine(.)))

data(crimes)
# select single Geographic CRS point
crim_ll <- crimes[1,]
# select single Projected CRS point
crim_xy <- crimes[1,] |> st_transform(27700)
# calculate areas
st_area(st_buffer(crim_ll, 100))
st_area(st_buffer(crim_xy, 100))

AoI_buf <- st_buffer(st_union(AoI), dist = 1000)
ggplot() + 
  geom_sf(data = AoI, lwd = 1) +
  geom_sf(data = AoI_buf, col = "#FB6A4A", alpha = 0.5, lwd = 1) +
  theme_bw() + xlab("") + ylab("") 

buf <- st_buffer(st_centroid(AoI), dist = 500)
ggplot() + 
  geom_sf(data = AoI, lwd = 1) +
  geom_sf(data = buf, col = "#FB6A4A", alpha = 0.5, lwd = 1) +
  geom_sf(data = st_centroid(AoI))

buf <- st_buffer(AoI, dist = 1000)
ggplot() + 
  geom_sf(data = AoI, lwd = 1) +
  geom_sf(data = buf, col = "#FB6A4A", alpha = 0.5, lwd = 1) 

x = 100
units(x) <- as_units("m")
AoI_buf <- st_buffer(st_union(AoI), dist = x)
ggplot() + 
  geom_sf(data = AoI, lwd = 1) +
  geom_sf(data = AoI_buf, col = "#FB6A4A", alpha = 0.5, lwd = 1) 

AoI_buf <- st_buffer(st_union(AoI), dist = 1000)
int <- st_intersects(crimes_xy, AoI_buf, sparse = F) |> as.vector()

# total crimes
tot_buf = sum(table(crimes$category[int]))
tot = sum(table(crimes$category))
# calculate crime rates
tab <- cbind(Buffer = table(crimes$category[int]) / tot_buf, 
             All = table(crimes$category)/tot ) 
# examine to 3 significant figures
round(tab, 3)

st_crs(leeds_msoa)

st_area(leeds_msoa)

# hectares
st_area(leeds_msoa) |> set_units("ha")
# square kilometres
st_area(leeds_msoa) |> set_units("km2")
# and remove the units 
st_area(leeds_msoa) |> set_units("km2") |> as.numeric()

data(crimes)
data(leeds_msoa)

leeds_msoa$asb  <- 
  st_intersects(x = leeds_msoa, 
                y = crimes |> st_transform(27700) |> 
                  filter(category == "anti-social-behaviour"), 
                sparse = F) |> rowSums()
leeds_msoa$areas = st_area(leeds_msoa) |> set_units("km2") |> as.numeric()
leeds_msoa$densities <- leeds_msoa$asb / leeds_msoa$areas
cor(leeds_msoa$owned,leeds_msoa$densities)

ggplot(leeds_msoa, aes(x = owned, y = densities)) + 
  geom_point() + geom_smooth(method = "lm")

# load the data 
data(leeds_msoa) 
# compute counts
leeds_msoa$n.asb <- 
  st_intersects(leeds_msoa, 
                crimes |> st_transform(27700) |> 
                  filter(category == "anti-social-behaviour"),
                sparse = F) |> 
  rowSums(int)
# compute area
leeds_msoa$area = st_area(leeds_msoa) |> set_units("km2") |> as.numeric()
# fit the model
model1 = glm(n.asb ~ owned, offset = log(area), data = leeds_msoa, family = poisson)

model1
summary(model1)

leeds_msoa$s.resids = rstandard(model1)

leeds_msoa |>
  mutate(resid.col = ifelse(s.resids < -2, "red", 
                            ifelse(s.resids > 2, "blue", "grey"))) |> 
  ggplot() + geom_sf(aes(fill = resid.col)) +
  scale_fill_identity()

model2 = glm(n.asb ~ owned + manual, offset = log(area), data = leeds_msoa, 
             family = poisson)
leeds_msoa$s.resids.2 = rstandard(model2)

leeds_msoa |>
  mutate(resid.col = ifelse(s.resids.2 < -2, "red", 
                            ifelse(s.resids.2 > 2, "blue", "grey"))) |> 
  ggplot() + geom_sf(aes(fill = resid.col)) +
  scale_fill_identity()


x <- matrix(rnorm(100), nrow = 5)
colnames(x) <- paste0("Var", 1:20)
dist(x)
as.matrix(dist(x))

as.matrix(dist(st_coordinates(st_centroid(polys1))))

st_distance(crimes[1:2,], crimes[101:103,])

# data and prep
data(leeds_msoa)
data(places)
places_sf <-
  places |> st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(27700)
leeds_cents <- 
    leeds_msoa |> st_centroid() 
distances <- st_distance(places_sf, leeds_cents) 
dim(distances)

distances[1:3, 1:3]
distances <- distances |> set_units("km") 
distances[1:3, 1:3]

d <- st_distance(places_sf, leeds_msoa) |> set_units("km") 
d[1:3, 1:3]

a = 10000
units(a) <- "m"
d <- st_distance(places_sf, leeds_cents) |> set_units("km") < a
d[1:3, 1:3]

d.mat = st_distance(leeds_cents, places_sf) |> set_units("km") 
# make sure to understand what the d.mat contains
dim(d.mat)
nrow(leeds_cents)
nrow(places_sf)
# use apply to extract the Min distance form each row
leeds_msoa$dist = apply(d.mat, 1, function(x) min(x))

# calculate distances
d.mat = st_distance(leeds_cents) |> set_units("km") 
dim(d.mat)
# define a function
nearest_but_1 = function(x){
  sort(x)[2]
}
# apply it
d = apply(d.mat, 1, function(x) nearest_but_1(x))
head(d)


data(docs)

tm_shape(docs)+tm_dots(size = 0.5, fill_alpha = 0.3) +  
	tm_basemap("OpenStreetMap")

thresh = 1.5
units(thresh) <- "km"
d <- st_distance(leeds_cents, docs) |> set_units("km") < thresh
leeds_msoa$doc_lt1.5k <- rowSums(d) > 0
# you could examine this with a quick tmap
# qtm(leeds_msoa, "doc_lt1.5k")

# extract the ethnicity data from the leeds_msoa object
ethnicity <- as.matrix(leeds_msoa |> st_drop_geometry() |> select(white:indian) / 100)
ethnicity <- apply(ethnicity, 2, function(x) (x * leeds_msoa$totpop))
ethnicity <- matrix(as.integer(ethnicity), ncol = 6)
colnames(ethnicity) <- c("White", "Mixed", "Caribbean", "African", "Pakistani", "Indian")

# use xtabs to generate a cross-tabulation
mat.access.tab = xtabs(ethnicity~leeds_msoa$doc_lt1.5k)
# then transposes the data
data.set = as.data.frame(mat.access.tab)
#sets the column names
colnames(data.set) = c("Access","Ethnicity", "Count")

modelethnic = glm(Count~Access*Ethnicity, data=data.set,family=poisson)
# the full model can be printed to the console
# summary(modelethnic)

summary(modelethnic)$coef

mod.coefs = summary(modelethnic)$coef

tab <- 100*(exp(mod.coefs[,1]) - 1)
tab <- tab[8:12]
names(tab) <- colnames(ethnicity)[2:6]
round(tab, 1)

mosaicplot(t(mat.access.tab),
           xlab='',ylab='', 
           main="Mosaic Plot of Doctor Access",
           shade=TRUE,las=3,cex=0.8)

plot(st_geometry(leeds_lsoa),border='red')
plot(st_geometry(leeds_msoa),lwd=2,add=TRUE)

data(leeds_msoa)
msoa_cents = st_centroid(leeds_msoa)
d.mat = st_distance(msoa_cents, docs) |> set_units("km") 
leeds_msoa$d2doc = apply(d.mat, 1, function(x) min(x))

mod_msoa = lm(badhealth~d2doc + o65 + overcrowd + manual, data = leeds_msoa)
summary(mod_msoa)

# make a geometry and subset for the MSOA area
geom = st_make_grid(leeds_msoa, 1000)
geom <- geom[leeds_msoa,]
# make a data frame and assign the geometry to it
grid = data.frame(ID = paste0("gr_",1:length(geom)))
st_geometry(grid) <- geom
plot(geom)

# load data, create ID and a bounding box
data(pm10)
pm10 <- pm10 |> transmute(IDgr = paste0("gr_",1:n()), pm10 = pm102021g)
poly_box <- leeds_msoa |> st_union()   
# create gridded areas
geom <- 
  pm10 |> st_union() |> st_voronoi() |> st_collection_extract() |> 
  st_intersection(poly_box) |>st_as_sf() 
# assign values from points
st_geometry(pm10) <- st_geometry(geom)
pm10$area_gr = st_area(pm10) |> set_units("km2") |> as.numeric()
# plot(geom)

# load and prep the data
data(leeds_msoa)
leeds_msoa <- leeds_msoa |> transmute(IDpl = paste0("pl_",1:n()),
                        unemp = (unemp/100) * totpop)
leeds_msoa$area_pl = st_area(leeds_msoa) |> set_units("km2") |> as.numeric()
# do the intersection 
int_layer = st_intersection(pm10, leeds_msoa) 

# zones and the original area
p1 <- 
  ggplot(pm10) + geom_sf() + theme_bw() +
  geom_sf(data = leeds_msoa, col = "red", fill = NA, lwd = 0.5)
# plot the variable in the intersection
p2 <- 
  ggplot(int_layer, aes(fill = unemp)) + geom_sf(col = "white") + 
  theme_bw() + theme(legend.position = "none")
plot_grid(p1, p2, ncol = 2)

sum(leeds_msoa$unemp)
sum(int_layer$unemp)

int_layer$int.area <- st_area(int_layer) |> set_units("km2") |> as.numeric()
unemp <- 
  int_layer |> st_drop_geometry() |>
  # group by IDgr and IDpl
  group_by(IDgr, IDpl) |>
  # construct weight and apply
  summarise(weight = int.area/area_pl, u = sum(unemp*weight)) |>
  ungroup() |>
  # group by grid IDs and summarise 
  group_by(IDgr) |> summarise(u = sum(u)) 

# check   
unemp |> summarise(sum(u))
sum(leeds_msoa$unemp)

pm10 |>
  left_join(unemp) |>
  ggplot(aes(fill = u)) + geom_sf() +
  scale_fill_binned_c4a_seq("brewer.yl_or_rd", name = "Unemployment") +
  theme_bw()

leeds_msoa |> select(unemp) |> 
  st_interpolate_aw(pm10, extensive = T) |>
  st_drop_geometry() |> unlist() |> summary() 
summary(unemp$u)

library(sf)
library(Intro2R4SpatAnal)
library(terra)
library(tmap)
library(tidyverse)
library(cols4all)
data(pm10)

# define an extent
ex = as.vector(ext(pm10))
# create a raster object
r <- rast(xmin = ex[1]-500, xmax = ex[2]+500, ymin = ex[3]-500, ymax = ex[4]+500, 
          resolution = 1000, crs = "epsg:27700")
# r     # uncomment to examine
# use rasterize to create the raster
pm10_r <- rasterize(x = vect(pm10), y = r, fun = mean, field = "pm102021g")
names(pm10_r) <- "pm10"
# plot(pm10_r)      # uncomment to p[lot]

# change the raster resolution and add CRS
r <- rast(extent = pm10, resolution = 2000, crs = "epsg:27700")
# change the extent object
r <- rast(extent = leeds_msoa, resolution = 1000)
# define different resolutions in different orientations
r <- rast(extent = pm10, resolution = c(1000, 2000))
# or using a buffer
buf = st_as_sf(st_buffer(st_union(leeds_msoa), 1000))
r <- rast(extent = buf, resolution = 1000, crs = "epsg:27700")

# redefine in case you changed it above! 
r <- rast(xmin = ex[1]-500, xmax = ex[2]+500, ymin = ex[3]-500, ymax = ex[4]+500, 
          resolution = 1000, crs = "epsg:27700")
pm10_r <- rasterize(x = vect(pm10), y = r, fun = mean, field = "pm102021g")
names(pm10_r) <- "pm10"
# map
tm_shape(pm10_r) + 
  tm_raster("pm10", 
            col.scale = tm_scale_continuous(values = c4a("brewer.yl_or_rd"))) + 
  tm_layout(legend.position = c("left", "bottom"), frame = F) +
  # add points
  tm_shape(pm10) + tm_symbols(fill = "black", shape = 16, size = 0.4) 

r <- rast(xmin = ex[1]-500, xmax = ex[2]+500, ymin = ex[3]-500, ymax = ex[4]+500, 
          resolution = 1000, crs = "epsg:27700")
pm10_r <- rasterize(x = vect(pm10), y = r, fun = mean, field = "pm102021g")
names(pm10_r) <- "pm10"

# load and transform the data 
data(roads)
roads <- roads|> st_transform(27700)
# create a raster object
r <- rast(extent = roads, resolution= 100, crs = "epsg:27700")
# use rasterize to create the raster
roads_r <- rasterize(x = vect(roads), y = r, fun = max, field = "highway")
names(roads_r) <- "road"
# plot(roads_r)

tm_shape(roads_r) + 
  tm_raster("road", 
            col.scale = tm_scale_categorical(values = c4a("brewer.set1"))) + 
  tm_layout(legend.position = c("left", "bottom"), frame = F, bg.col = "grey92")

# load the data 
data(leeds_msoa)
# create a raster object
r <- rast(extent= leeds_msoa, resolution = 400, crs = "epsg:27700")
# use rasterize to create the raster
msoa_r <- rasterize(x = vect(leeds_msoa), y = r, fun = "mean", field = "u20")
names(msoa_r) <- "under 25"
# plot(msoa_r)
tm_shape(msoa_r) + 
  tm_raster("under 25", 
            col.scale = tm_scale_intervals(values = c4a("brewer.yl_gn_bu"))) + 
  tm_layout(legend.position = c("left", "bottom"), frame = F)


data(leeds_msoa)
r <- rast(extent = leeds_msoa, resolution = 200, crs = "epsg:27700")
id = rasterize(x = vect(leeds_msoa), y = r, fun = "max", field = "code")
u20 = rasterize(x = vect(leeds_msoa), y = r, fun = "mean", field = "u20")
rs = c(id, u20)
rs


data(leeds_lsoa)
# area in m2 and pop density
leeds_lsoa <- leeds_lsoa |>
  mutate(area = st_area(leeds_lsoa) |> set_units("m2") |> as.vector(),
         popd = totpop/area)
r <- rast(extent = leeds_lsoa, resolution = 400, crs = "epsg:27700")
# use rasterize to create the raster
pop <- rasterize(x = vect(leeds_lsoa), y=r, fun="mean", field="popd")
# calculate count from mean population density and grid cell area
pop <- pop * 400 * 400
# check - this is close enough
sum(as.vector(pop), na.rm = T)
sum(leeds_lsoa$totpop)


# convert to vectors
polys1 <- st_as_sf(as.polygons(msoa_r))
points1 <- st_as_sf(as.points(pm10_r))
lines1 <- class(as.lines(roads_r))
summary(as.vector(pm10_r))

tm_shape(leeds_msoa) +tm_polygons(fill = NULL, lwd = 4) +
  tm_shape(polys1) + tm_polygons(fill = NULL, col = "cyan", lwd = 2) +
  tm_layout(legend.position = c("left", "bottom"), frame = F, bg.col = "grey92")

# define raster grid
r <- rast(extent = leeds_lsoa, resolution = 100, crs = "epsg:27700")
# create raster layers
r1 = rasterize(x = vect(roads |> filter(highway == "motorway")), y = r, fun = max)
names(r1) = "mways"
r2 = rasterize(x = vect(leeds_lsoa), y = r, fun = "mean", field = "badhealth")
names(r2) = "badhealth"
# crate a 1 km grid and the pm10 layer
r_1k <- rast(extent = leeds_msoa, resolution = 1000, crs = "epsg:27700")
r3 = rasterize(x = vect(pm10), y = r_1k, fun = "mean", field = "pm102021g")
names(r3) = "pm10"
# resample to 100 m
r3 = resample(r3, r2)
# examine the stack 
c(r1, r2, r3)

# set the plot parameters for 3 columns & 1 row
par(mfrow = c(1,3))
plot(r1, main = "roads")
plot(r2, main = "bad health")
plot(r3, main = "pm10")
# reset par
par(mfrow = c(1,1))

# Result <- Raster_Layer_1 + Raster_Layer_2 # addition
# Result <- Raster_Layer_1 * Raster_Layer_2 # multiplication

summary(as.vector(r1))

res <- r1 * r3 
# plot(res)
summary(as.vector(res))

# convert 1s to 0s
r1_tmp <- r1 - 1
# replace NAs with 1
r1_tmp <- subst(r1_tmp, NA, 1)
# plot(r1_tmp)
# undertake the operation
res <- r1_tmp * r3 
summary(as.vector(res))

# pm10 for motorways
summary(as.vector((r1 == 1) * r3))
# pm10 for other areas
summary(as.vector((is.na(r1)) * r3))

r1_buf = buffer(r1, 500)
plot(r1_buf)

r1_buf = buffer(r1, 500)
pm_tmp <- r1_buf * r3 
bh_tmp = r1_buf * r2
r_cor <- layerCor(c(pm_tmp, bh_tmp), "pearson")
r_cor

layerCor(c(r3, r2), "pearson")

r_cor$correlation[1,2]

# define an empty output
res = NULL
for (i in seq(100, 20000, 100)) {
  r1_buf = buffer(r1, i)
  pm_tmp <- r1_buf * r3 
  bh_tmp = r1_buf * r2
  r_cor <- layerCor(c(pm_tmp, bh_tmp), "pearson")
  res = append(res, r_cor$correlation[1,2])
  # define a progress counter
  if(i %% 2000 == 0) cat(i, "\t")
}

# combine the result to a data frame
df <- data.frame(Correlation = res,
                 Buffer = seq(100, 20000, 100))
# plot with the global value as a horizontal line
ggplot(df, aes(x = Buffer, y = Correlation)) + 
  geom_line(lwd = 1) +
  geom_hline(yintercept = layerCor(c(r3, r2), "pearson")$correlation[1,2],
              col = "red", lty = 2) +
  theme_bw()

res <- sin(r3) + sqrt(r1)
res
res <- ((r1 * 1000 ) / log(r3) ) * r2 
res

my.func <- function(x) {log(x)}
# singe raster
my.func(r3)
# over a stack
my.func(c(r2,r3))

# Self-Test Question 1
# load the data
data(crimes)
data(leeds_msoa)
# Version 1 with minimal pipe
# transform and filter crimes
crimes_tmp <- crimes |> st_transform(27700) |> filter(category == "anti-social-behaviour") 
# intersection and create variable
int  = st_intersects(leeds_msoa, crimes_tmp, sparse = F)
leeds_msoa$asb <- rowSums(int)
# calculate area and asb density
leeds_msoa$area = st_area(leeds_msoa) |> set_units("km2") |> as.numeric()
leeds_msoa <- leeds_msoa |> mutate(asb_km = asb/area) 
# map 
leeds_msoa |> 
  ggplot() + geom_sf(aes(fill = asb_km)) +
  scale_fill_continuous_c4a_seq("viridis", reverse = T)

# Version 2 with max pipe
leeds_msoa |>
  # mutate to create the variables 
  mutate(asb = crimes |> st_transform(27700) |> filter(category == "anti-social-behaviour") |>
         # intersection and create variable with *colSums*
         st_intersects(leeds_msoa, sparse = F) |> colSums(), 
         area = st_area(leeds_msoa) |> set_units("km2") |> as.numeric(), 
         asb_km = asb/area) |> 
  # map 
  ggplot() + geom_sf(aes(fill = asb_km)) +
  scale_fill_continuous_c4a_seq("viridis", reverse = T)

# Self-Test Question 2
leeds_msoa |>
  mutate(resid.col = ifelse(s.resids.2 < -2, "red", 
                            ifelse(s.resids.2 > 2, "blue", "grey"))) |>
tm_shape() +
  tm_polygons(fill = "resid.col")

# Self-Test Question 3
data(leeds_lsoa)
lsoa_cents = st_centroid(leeds_lsoa)
d.mat = st_distance(lsoa_cents, docs) |> set_units("km") 
leeds_lsoa$d2doc = apply(d.mat, 1, function(x) min(x))
mod_lsoa = lm(badhealth~d2doc + o65 + overcrowd + manual, data = leeds_lsoa)
summary(mod_lsoa)

# Self-Test Question 4
# load and prepare the data 
data(leeds_lsoa)
leeds_lsoa <- leeds_lsoa |> 
  transmute(unemp_tot = (unemp/100) * totpop, 
            unemp_pc = unemp)
# make the grid
geom= st_make_grid(leeds_lsoa, 1000, square = FALSE)
geom <- geom[leeds_lsoa,]
# interpolate the count
unemp_tot <- 
  leeds_lsoa |> 
  select(unemp_tot) |> 
  st_interpolate_aw(geom, extensive = T) 
# interpolate the percentage
unemp_pc <- 
  leeds_lsoa |> 
  select(unemp_pc) |> 
  st_interpolate_aw(geom, extensive = F) 
# create the plots 
p1 <- 
  ggplot(unemp_tot, aes(fill = unemp_tot), col = NA) + geom_sf() + theme_bw() +
  scale_fill_continuous_c4a_seq("brewer.yl_or_rd", name = "n") +
  theme(legend.position = "bottom")
p2 <- 
  ggplot(unemp_pc, aes(fill = unemp_pc), col = NA) + geom_sf() + theme_bw() +
  scale_fill_continuous_c4a_seq("brewer.yl_or_rd", name = "%") +
  theme(legend.position = "bottom")
plot_grid(p1, p2, ncol = 2)

x_ll <- st_transform(x, 4326)
x_new <- st_transform(x, st_crs(y))

