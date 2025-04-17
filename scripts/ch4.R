## Code for Chapter 4 of An Introduction to R for Spatial Analysis and Mapping
# Lex Comber & Chris Brunsdon

hospital.distances <- c(4.3, 7.1, 6.3, 5.2, 3.2, 2.1) 

hospital.distances
if (hospital.distances[1] < 6) { 
  cat('Distance is short\n') } else { cat('Distance is long\n')}

# for(variable in sequence) {
#   <do R expression>
# }  

for (i in 1:3) {
  if (hospital.distances[i] < 6) { 
    cat('Distance',i,' is short\n') } else { cat('Distance',i,' is long\n')} 
} 

# define a function
assess.hosp.dist <- function(dist.list, thresh) { 
  for (i in 1:length(dist.list)) { 
    if(dist.list[i] < thresh) {
      cat('Distance',i, ' is short\n')} else { cat('Distance',i,' is long\n')}
  }
}
# apply to data
assess.hosp.dist(hospital.distances, 6)

# if – condition – consequent

x <- -7
if (x < 0) cat("x is negative")
x <- 8
if (x < 0) cat("x is negative")

# if – condition – consequent–  else – alternative

x <- -7
if (x < 0) cat("x is negative") else cat("x is positive")
x <- 8
if (x < 0) cat("x is negative") else cat("x is positive")


??is.

x <- c(1,3,6,8,9,5)
if (all(x > 0)) cat("All numbers are positive")
x <- c(1,3,6,-8,9,5)
if (any(x > 0)) cat("Some numbers are positive")
any(x==0)

x <- c(1,3,6,8,9,5)
if (all(x > 0)) {
  cat("All numbers are positive\n")
  total <- sum(x)
  cat("Their sum is ",total) 
}

# if condition { consequents } else { alternatives }

x <- c(1,3,6,8,9,-5)
if (all(x > 0)) {
  cat("All numbers are positive\n")
  total <- sum(x)
  cat("Their sum is ",total) 
} else {
  cat("Not all numbers are positive\n")
  cat("This is probably an error as numbers are distances") 
  }

# function name <- function(argument list) { R expression }

mean.distance <- function(rf){ 
  if (all(rf> 0))	 {      #open Function 
    mean.value <- mean(rf)	#open Consequent
    cat("The mean is ",mean.value)
  } else	{               #close Consequent and open Alternative
    cat("Warning: Not all values are positive\n")	
    }	#close Alternative
}	#close Function
mean.distance(c(8.5, 9.3, 6.5, 9.3, 9.4))
mean.distance(c(-8.5, 9.3, 6.5, 9.3, 9.4))

mean.distance2 <- function(x) {
  if (all(x> 0)) {
    return( mean(x))
  } else {
      return(NA)
    }
}
mr <- mean.distance2(c(8.5,9.3,6.5,9.3,9.4))
mr

x <- "Tuesday"
mean.distance2(c(8.5,9.3,6.5,9.3,9.4))
x

# for( 'loop variable' in 'list of values' ) do R expression

for (i in 1:5) {
  i.cubed <- i * i * i
  cat("The cube of",i,"is ",i.cubed,"\n")
}

# seq(from, to, by = step value)

# seq(from, to, length = sequence length)

for (val in seq(0,1,by=0.25)) {
  val.squared <- val * val
  cat("The square of",val,"is ",val.squared,"\n")
}

i <- 1; n <- 654
repeat{
  i.squared <- i * i
  if (i.squared > n) break
  i <- i + 1}
cat("The first square number exceeding",n, "is ",i.squared,"\n")

debug(mean.distance2)

mean.distance2(c(8.5,9.3,6.5,9.3,9.4))

undebug(mean.distance2)

ifelse

cube.root <- function(x) {x ^ (1/3)}
cube.root(27)

cube.root <- function(x) {
  result <- x ^ (1/3)
  return(result)
}

source("functions.R")

cube.root <- function(x) {
  result <- x ^ (1/3)
  return(result)
}

source('functions.R')
cube.root(343)
cube.root(99)

circle.area <- function(r) {
  result <- pi * r ^ 2
  return(result)
}

source('functions.R')
cube.root(343)
circle.area(10)

cube.root(-343)

cube.root <- function(x) {
  if (x >= 0) {
    result <- x ^ (1/3) } else {
      result <- -(-x) ^ (1/3) }
  return(result)
}

cube.root(3)
cube.root(-3)

debug(cube.root)

cube.root(-50)

x > 0

undebug(cube.root)

help(debug)

cube.root('Leeds')

is.numeric(77)
is.numeric("Lex")
is.numeric("77")
v <- "Two Sevens Clash"
is.numeric(v)

cube.root <- function(x) {
  if (is.numeric(x)) {
    if (x >= 0) { 
      result <- x^(1/3) } else { 
        result <- -(-x)^(1/3) }
    return(result) } else {
      cat("WARNING: Input must be numerical, not character\n")
      return(NA)
      }
}

result <- sign(x)*abs(x)^(1/3)

# without brackets
cube.root <- \(x) x ^ (1/3)
# with 
circle.area <- \(r) {pi * r ^ 2}
# with multiple lines
cube.root.2 <- \(x) {
  if (is.numeric(x)){ 
    result <- sign(x)*abs(x)^(1/3)
    return(result) } else { 
      cat("WARNING: Input must be numerical, not character\n")
      return(NA) 
    }
}

gcd <- function(a,b) {
  divisor <- min(a,b)
  dividend <- max(a,b)
  repeat{ 
    remainder <- dividend %% divisor
    dividend <- divisor
    divisor <- remainder
    if (remainder == 0) break
  }
  return(dividend)
}

gcd(6,15)
gcd(25,75)
gcd(31,33)

# for (<VAR> in <Item1>:<Item2>) {
#   ... code in loop ...
# }

cube.root.table <- function(n) {
  for (x in 1:n) {
    cat("The cube root of ",x," is", cube.root(x),"\n")
  }
}

library(Intro2R4SpatAnal)
library(sf)
library(dplyr)
data(lads.polys)
# create an empty object
polys = NULL
# start the loop 
for(i in 1:length(lads.polys)){
  # create poly.i from the ith lads.poly - lads.poly[[i]]
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

# number of coordinate pairs
nrow(lads.polys[[22]])
# adding elements to a vector with 
x = c(67, 36)
x
x <- c(x, 47)
x


sf_from_coord_list <- function(x, crs.val = 27700) {
  # get names
  poly.names = names(x)
  # create empty object to be returned
  polys = NULL
  # start the loop 
  for(i in 1:length(x)){
    poly.i <- 
      x[[i]] |>
      data.frame() |> 
      st_as_sf(coords = c("X", "Y"), crs = crs.val) |>
      summarise(geometry = st_combine(geometry)) |>
      st_cast("POLYGON")
    # row bind poly.i to polys
    polys <- rbind(polys, poly.i)
  }
  polys <- polys |> st_cast("POLYGON")
  # add the names
  polys <- polys |> mutate(LAD = poly.names) |> relocate(LAD)
  return(polys)
}

# the first 3 list elements
sf_from_coord_list(lads.polys[1:3], crs.val = 27700) 

sf_from_coord_list(crs.val = 27700, x = lads.polys[1:3]) 
sf_from_coord_list(lads.polys[1:3]) 

library(Intro2R4SpatAnal)
library(sf)
data(leeds_msoa)
# create an empty list for the results
adj.list <- list()
# extract a single area
msoa.i <- leeds_msoa[1,]
# determine the adjacent areas
# the [-1] removes msoa.i from its own list
adj.i <- unlist(st_intersects(msoa.i, leeds_msoa))[-1]
# extract their names
adj.names.i <- leeds_msoa$MSOA21NM[adj.i]
# add to the list
adj.list[[1]] <- adj.i
# name the list elements
names(adj.list[[1]]) <- adj.names.i

adj.list

adj.list[[2]] <- sample(1:100, 3)
# have a look!
adj.list


library(Intro2R4SpatAnal)
data(leeds_lsoa)

# as is
leeds_lsoa
# drop the geometry 
leeds_lsoa |> st_drop_geometry() |> head()
# selecting variables by location, sequence, name
leeds_lsoa |> st_drop_geometry() |> select_at(14:17) |> head()
leeds_lsoa |> st_drop_geometry() |> select(indian:caribean) |> head()
leeds_lsoa |> st_drop_geometry() |> select("indian","pakistani","african","caribean") |> 
  head()

df <- leeds_lsoa |> st_drop_geometry() 

apply(df[, 14:17], 1, max)

apply(df[, 14:17], 2, max)

# set up vector to hold result
result.vector <- vector()
for (i in 1:nrow(df)){
  # for each row determine which column has the max value
  result.i <- which.max(df[i, 14:17])
  # put into the result vector
  result.vector <- append(result.vector, result.i)
}
table(names(result.vector))

res.vec <-apply(df[, 14:17], 1, which.max)
# compare the two results
identical(as.vector(res.vec), as.vector(result.vector))

# Loop
t1 <- Sys.time()
result.vector <- vector()
for (i in 1:nrow(df)){
  result.i <- which.max(df[i, 14:17])
  result.vector <- append(result.vector, result.i)
}
Sys.time() - t1
# Apply
t1 <- Sys.time()
res.vec <-apply(df[, 14:17], 1, which.max)
Sys.time() - t1

plot(x = c(363000, 542300), y = c(378000, 528500), asp = 1, 
     type='n',xlab='',ylab='',xaxt='n',yaxt='n',bty='n')
for (i in 1:length(lads.polys)){
  points(lads.polys[[i]], type='l')
  # small delay so that you can see the plotting
  Sys.sleep(0.25)
}

plot(x = c(363000, 542300), y = c(378000, 528500), asp = 1, 
     type='n',xlab='',ylab='',xaxt='n',yaxt='n',bty='n')
invisible(mapply(polygon,lads.polys))

# create a distance matrix
dMat <- as.matrix(dist(st_coordinates(st_centroid(leeds_msoa))))
dim(dMat)
# create an empty vector
count.vec <- vector()
# create an empty list
names.list <- list()
# for each county...
for( i in 1:nrow(leeds_msoa)) {
  # which areas are within 5km
  vec.i <- which(dMat[i,] <= 5000)
  # add to the vector
  count.vec <- append(count.vec, length(vec.i))
  # find their names
  names.i <- leeds_msoa$MSOA21NM[vec.i]
  # add to the list
  names.list[[i]] <- names.i
}
# have a look! 
count.vec
names.list

lapply(names.list, length)

# count.vec <- apply(dMat,1.my.func1)
# names.list <- apply(dMat,1.my.func2)

library(tidyverse)
library(sf)

data(leeds_lsoa)
data(lsoa_attribs)
head(lsoa_attribs)

vignette("two-table", package = "dplyr")

# xyz_join(x, y)

dim(leeds_lsoa)
dim(lsoa_attribs)
leeds_lsoa <- 
  leeds_lsoa |>
  left_join(lsoa_attribs, by = "code") 
dim(leeds_lsoa)
class(leeds_lsoa)

# 1. keep all lsoa_attribs but remain as sf 
# because leeds_lsoa as x and right_join
leeds_lsoa |>
  right_join(lsoa_attribs, by = "code") |> 
  # class()         # comment / un-comment this line
  dim()             # comment / un-comment this line

# 2. keep all lsoa_attribs but output as data table
# because lsoa_attribs as x and left_join
lsoa_attribs |>
  left_join(leeds_lsoa, by = "code") |> 
  # class()         # comment / un-comment this line
  dim()             # comment / un-comment this line
# etc

library(cols4all)
library(ggspatial)
# do a merge to create an outline
leeds_outline <- st_union(leeds_msoa)
# make the plot
ggplot() + geom_sf(data = leeds_lsoa, aes(fill = bus), col = NA) +
  scale_fill_binned_c4a_seq("brewer.reds", name = "% Commute\nby Bus") +
  geom_sf(data = leeds_outline, fill = NA, lwd = 0.5) +
  theme_bw() +
  annotation_scale(location = "bl")  + 
  annotation_north_arrow(location = "tl") +
  theme(legend.position = "bottom")

my.poly.map.func <- function(poly_layer, var = "bus", tit = "var", pal = "brewer.reds", 
                             binned = TRUE, outline = TRUE,
                             scalebar.pos = "bl", compass.pos = "tl", 
                             legend.pos = "bottom") {
  # make a basic plot
  basic.plot <- 
    ggplot() + geom_sf(data = leeds_lsoa, aes_string(fill = var), col = NA) +
    theme_bw() + 
    annotation_scale(location = scalebar.pos)  + 
    annotation_north_arrow(location = compass.pos) +
    theme(legend.position = legend.pos)  
  # binned or continuous shading
  if(binned) {
    basic.plot <- basic.plot + 
      scale_fill_binned_c4a_seq(pal, name = tit)
  } else  {
    basic.plot <- basic.plot + 
      scale_fill_continuous_c4a_seq(pal, name = tit)
  }
  # if outline 
  if(outline) {
    poly_outline <- st_union(poly_layer)
    basic.plot <- basic.plot + 
      geom_sf(data = leeds_outline, fill = NA, lwd = 0.5)
  }
  return(basic.plot)  
}

my.poly.map.func(leeds_lsoa)

my.poly.map.func(leeds_lsoa, var = "walk", pal = "brewer.blues", 
                 tit = "% Commute\non Foot", binned = F)

my.poly.map.func(leeds_msoa, var = "manual", pal = "brewer.yl_or_rd", 
                 tit = "Manual\noccupation", binned = F)

vars <- 
  data.frame( class = unlist(sapply(leeds_msoa, class))) |> 
  filter(class == "numeric") |> rownames()
pals <- c4a_palettes("seq", series = "brewer")

vars
pals

listo_plots = list()
for (i in 1:length(vars)) {
  listo_plots[[i]] <- my.poly.map.func(leeds_msoa, var = vars[i], pal = sample(pals, 1), 
                 tit = vars[i], binned = T)
}
length(listo_plots)

listo_plots[[3]]
listo_plots[[5]]
listo_plots[[7]]
save(listo_plots, file = "listo_plots.RData")

library(ggspatial) 
leeds_msoa |> 
  filter(MSOA21NM == "Leeds 006") |>
  ggplot() + 
  annotation_map_tile() +
  geom_sf(fill = NA, col = "red", lwd = 1) +
  geom_sf_text(aes(label = MSOA21NM)) + 
  theme_void()

# extract the poly
poly.i <- leeds_msoa |> 
  filter(MSOA21NM == "Leeds 006") 
# extract the bounding box
xy = st_bbox(poly.i)
# add the buffer
x = xy[c(1,3)] + c(-500, 500)
y = xy[c(2,4)] + c(-500, 500)	
# create the polygon
poly_box <- 
  data.frame(x,y) |>
  st_as_sf(coords = c("x", "y"), crs = 27700) |>
  st_bbox() |> st_as_sfc()

leeds_msoa |> 
  filter(MSOA21NM == "Leeds 006") |>
  ggplot() + 
  annotation_map_tile(zoom = 12) +
  geom_sf(fill = NA, col = "red", lwd = 1) +
  geom_sf_text(aes(label = MSOA21NM), size = 10) +
  geom_sf(data = poly_box, fill = NA, lwd = 1) +
  theme_void()

# define the function 
make.poly.box <- function(obj, extend) {
  xy = st_bbox(obj)
  x = xy[c(1,3)] + c(-extend, extend)
  y = xy[c(2,4)] + c(-extend, extend)	
  poly <- data.frame(x,y) |> st_as_sf(coords = c("x", "y")) |>
    st_bbox() |> st_as_sfc()
  st_crs(poly) = st_crs(obj)
  poly 
}

poly_box <- make.poly.box(leeds_msoa |> filter(MSOA21NM == "Leeds 022"), 1000) 
# plot the result
leeds_msoa |> filter(MSOA21NM == "Leeds 022") |>
  make.poly.box(1000) |>
  ggplot() + 
  annotation_map_tile(zoom = 12) +
  geom_sf(fill = NA, lwd = 1) +
  theme_void()

# 1. create a sample of 30 random points with an ID
set.seed(777) # for reproducibility
point_geom <- st_sample(leeds_msoa, 100)
points <- data.frame(ID = 1:length(point_geom))
st_geometry(points) <- point_geom
# 2. create a bounding box for these
poly_box <- points |> st_union() |> st_convex_hull() |> st_buffer(1000)  
poly_geom <- 
  points |> 
  # make into a multipoint object
  st_union() |> 
  # determine Voronoi areas
	st_voronoi() |> 
  # extract the individual geometries
	st_collection_extract() |> 
	# clip to the bounding box
  st_intersection(poly_box) |>
	st_as_sf() 
	# plot
# assign values from points
poly_catchments = points
st_geometry(poly_catchments) <- st_geometry(poly_geom)

ggplot(data = poly_catchments) + 
  annotation_map_tile(zoom = 11, type = 'cartolight') +
  geom_sf(fill = NA, col = "darkred", lwd = 0.5) +
  geom_sf(data = points) + 
  theme_void()

# define the function 
make.point.catchments <- function(point_layer = points, extend) {
  poly_box <- 
    point_layer |> 
    st_union() |> 
    st_convex_hull() |> 
    st_buffer(extend)  
  out_poly_geom <- 
    point_layer |>  
    st_union() |> 
    st_voronoi() |> 
    st_collection_extract() |>  
    st_intersection(poly_box) |> 
    st_as_sf() 
  out_poly <- point_layer
  st_geometry(out_poly) <- st_geometry(out_poly_geom)
  out_poly
}
# apply the function
pm10_polys <- make.point.catchments(pm10, 50)
# and map
pm10_polys |> 
  ggplot() + geom_sf(aes(fill = pm102021g)) +
   scale_fill_continuous_c4a_seq("brewer.yl_gn_bu")

# Self-Test Question 1
cube.root.2 <- function(x){ 
  if (is.numeric(x)){ 
    result <- sign(x)*abs(x)^(1/3)
    return(result) } else { 
      cat("WARNING: Input must be numerical, not character\n")
      return(NA) 
    }
}

# Self-Test Question 2
gcd <- function(a,b) {
  divisor <- min(a,b)                 # line 1 
  dividend <- max(a,b)                # line 1 
  repeat{                             # line 5  
    remainder <- dividend %% divisor  # line 2
    dividend <- divisor               # line 3 
    divisor <- remainder              # line 4
    if (remainder == 0) break         # line 6 
  }
  return(dividend)
}

# Self-Test Question 3
cube.root.table <- function(n){ 
  for (x in seq(0.5, n, by = 0.5)){ 
    cat("The cube root of ",x," is", sign(x)*abs(x)^(1/3),"\n")
  }
}

cube.root.table <- function(n = -4) { 
  if (n > 0 ) by.val = 0.5
  if (n < 0 ) by.val =-0.5
  for (x in seq(0.5, n, by = by.val)){ 
    cat("The cube root of ",x," is", sign(x)*abs(x)^(1/3),"\n") 
  }
}

# Self-Test Question 4
# number of coordinate pairs
nrow(lads.polys[[22]])
# adding elements to a vector with 
x = c(67, 36)
x
x <- c(x, 47)
x

pairs_vec = NULL
# start the loop 
for(i in 1:length(lads.polys)){
  # the ith lads.poly
  val.i <- lads.polys[[i]] |> data.frame() |> nrow()
  # append to pairs_vec
  pairs_vec <- c(pairs_vec, val.i)
}
pairs_vec

# Self-Test Question 5
# create an empty list for the results
adj.list <- list()
for (i in 1:nrow(leeds_msoa)) {
  # extract a single MSOA
  msoa.i <- leeds_msoa[i,]
  # determine the adjacent areas
  # the [-1] removes msoa.i from its own list
  adj.i <- unlist(st_intersects(msoa.i, leeds_msoa))[-1]
  # extract their names
  adj.names.i <- leeds_msoa$MSOA21NM[adj.i]
  # add to the list
  adj.list[[i]] <- adj.i
  # name the list elements
  names(adj.list[[i]]) <- adj.names.i
}


# Self-Test Question 6
# count.vec <- apply(dMat,1.my.func1)
# names.list <- apply(dMat,1.my.func2)

dMat <- as.matrix(dist(st_coordinates(st_centroid(leeds_msoa))))
# number of counties within 50km
my.func1 <- function(x){
  vec.i <- which(x <= 5000)[-i]
  return(length(vec.i))
}
# their names
my.func2 <- function(x){
  vec.i <- which(x <= 5000)
  names.i <- leeds_msoa$MSOA21NM[vec.i]
return(names.i)
}
count.vec <- apply(dMat,1, my.func1)
names.list <- apply(dMat,1, my.func2)
