## Code for Chapter 7 of An Introduction to R for Spatial Analysis and Mapping
# Chris Brunsdon & Lex Comber 

# load the packages and data
library(sf)
library(tidyverse)
library(Intro2R4SpatAnal)
library(ggspatial)
data(bk_pm10_ave)
data(bk_prov)
# create the map
ggplot(bk_pm10_ave, aes(col=ave_pm10)) + 
  annotation_map_tile(type='cartolight', zoomin=-1) +   
  geom_sf() + geom_sf(data=bk_prov, fill=NA, col='Darkred', lwd=1) +
  scale_color_viridis_c(name='PM10 µg/m³')

bk_pm10_utm <- bk_pm10_ave |> st_transform(32647)
bk_prov_utm <- bk_prov |> st_transform(32647)

# Check these with a map...
ggplot(bk_pm10_utm, aes(col=ave_pm10)) + annotation_map_tile(type='cartolight',zoomin=-1) +   
  geom_sf() + geom_sf(data=bk_prov_utm,fill=NA,col='Darkred', lwd=1) +
  scale_color_viridis_c(name='PM10 µg/m³')


bk_pm10_theiss <-
  bk_pm10_utm |> 
  st_union() |>
  st_voronoi() |> 
  st_collection_extract() |> 
  st_intersection(bk_prov_utm) |>
  st_as_sf()
ggplot() + geom_sf(data=bk_pm10_theiss) +
  geom_sf(data=bk_pm10_utm) + theme_bw()

bk_pm10_theiss <- bk_pm10_theiss |> st_join(bk_pm10_utm)

ggplot(bk_pm10_theiss, aes(fill=ave_pm10)) + 
  annotation_map_tile(type='cartolight', zoomin=-1) + 
  geom_sf(alpha=0.5,lwd=0) + 
  geom_sf(data=bk_prov_utm, fill=NA, col='Darkred', lwd=1) +
  scale_fill_viridis_c(name='PM10 µg/m³')

bk_grid_utm <- bk_prov_utm |>
  st_make_grid(cellsize=500 ) |> 
  st_as_sf() |>
  st_intersection(bk_prov_utm) |>
  mutate(grid_ID=row_number())

bk_samp_utm <- bk_grid_utm |> st_centroid()
head(bk_samp_utm)

dmat <- st_distance(bk_samp_utm,bk_pm10_utm) |>
  units::drop_units()
rownorm <- \(x) sweep(x,1,rowSums(x),'/')
W <- rownorm(dmat^-3)

bk_grid_utm <- bk_grid_utm |> 
  mutate(interp=W %*% bk_pm10_utm$ave_pm10)

ggplot(bk_grid_utm, aes(fill=interp)) + 
  annotation_map_tile(type='cartolight', zoomin=-1) + 
  geom_sf(alpha=0.5, col=NA) + 
  geom_sf(data=bk_prov_utm, fill=NA, col='Darkred', lwd=1) +
  scale_fill_viridis_c(name='PM10 µg/m³ (IDW)')

ggplot(bk_grid_utm) + 
  annotation_map_tile(type='cartolight', zoomin=-1) + 
  geom_sf(alpha=0.5,col=NA, mapping=aes(fill=interp)) + 
  geom_sf(data=bk_prov_utm, fill=NA, col='Darkred', lwd=1) +
  scale_fill_viridis_c(name='PM10 µg/m³ (IDW)') +
  geom_sf(data=bk_pm10_utm, aes(size=ave_pm10), shape = 1, col='darkblue')

W <- rownorm(dmat^-2)
bk_grid_utm <- bk_grid_utm |> mutate(interp2=W %*% bk_pm10_utm$ave_pm10)

ggplot(bk_grid_utm) + 
  annotation_map_tile(type='cartolight', zoomin=-1) + 
  geom_sf(alpha=0.5,col=NA, mapping=aes(fill=interp2)) + 
  geom_sf(data=bk_prov_utm, fill=NA, col='Darkred', lwd=1) +
  scale_fill_viridis_c(name='PM10 µg/m³ (IDW)') +
  geom_sf(data=bk_pm10_utm, aes(size=ave_pm10), shape = 1, col='darkblue')


build_loo <- function(z,dmat) {
  loo_core <- function(alpha) {
    wmat <- dmat^(-alpha)
    diag(wmat) <- 0
    wmat <- rownorm(wmat)
    e <- z - wmat %*% z
    return(sum(e*e))}
  return(Vectorize(loo_core))
}

dmat_samp <- st_distance(bk_pm10_utm,bk_pm10_utm) |>
  units::drop_units()
LOO <- build_loo(bk_pm10_utm$ave_pm10,dmat_samp)

curve(LOO,0.3,3,xname = 'alpha')

opt_result <- optimise(LOO,c(0.5,1.5))

opt_result <- optimise(LOO,c(0.5,1.5))
opt_result

W <- rownorm(dmat^-opt_result$minimum)
bk_grid_utm <- bk_grid_utm |> mutate(interp3=W %*% bk_pm10_utm$ave_pm10)

ggplot(bk_grid_utm) + 
  annotation_map_tile(type='cartolight',zoomin=-1) + 
  geom_sf(alpha=0.5, col=NA, mapping=aes(fill=interp3)) + 
  geom_sf(data=bk_prov_utm, fill=NA, col='Darkred', lwd=1) +
  scale_fill_viridis_c(name='PM10 µg/m³ (IDW)') +
  geom_sf(data=bk_pm10_utm, size=0.2, col='darkblue')


W <- rownorm(exp(-0.5*dmat^2/5000^2))
item <- 34
bk_grid_utm <- bk_grid_utm |> mutate(kern=W[,item])

ggplot(bk_grid_utm) + 
  annotation_map_tile(type='cartolight', zoomin=-1) + 
  annotation_scale(location='br') +
  geom_sf(alpha=0.5, col=NA, mapping=aes(fill=kern)) + 
  geom_sf(data=bk_prov_utm, fill=NA, col='Darkred', lwd=1) +
  scale_fill_distiller(name='Kernel', direction = 1) +
  geom_sf(data=bk_pm10_utm, size=0.4, col='darkblue') 


build_loo_kern <- function(z,dmat) {
  loo_core <- function(k) {
    wmat <- exp(-0.5*(dmat^2/k^2))
    diag(wmat) <- 0
    wmat <- rownorm(wmat)
    e <- z - wmat %*% z
    return(sum(e*e))}
  return(Vectorize(loo_core))
}

LOO_kern <- build_loo_kern(bk_pm10_utm$ave_pm10,dmat_samp)

curve(LOO_kern,3000,9000,xname='k')

opt_result_kern <- optimise(LOO_kern,c(3000,9000))

opt_result_kern <- optimise(LOO_kern,c(3000,9000))
opt_result_kern

W <- rownorm(exp(-0.5*dmat^2/opt_result_kern$minimum^2))
bk_grid_utm <- bk_grid_utm |> mutate(ksmooth=W %*% bk_pm10_utm$ave_pm10)

ggplot(bk_grid_utm) + 
  annotation_map_tile(type='cartolight', zoomin=-1) + 
  annotation_scale(location='br') +
  geom_sf(alpha=0.5,col=NA, mapping=aes(fill=ksmooth)) + 
  geom_sf(data=bk_prov_utm,fill=NA, col='Darkred', lwd=1) +
  scale_fill_viridis_c(name='PM10 µg/m³ \n(Kernel Smooth)') +
  geom_sf(data=bk_pm10_utm, size=0.2, col='darkblue')

build_loo_kern2 <- function(z,dmat) {
  loo_core <- function(k) {
    wmat <- exp(-0.5*(dmat^2/k^2))
    diag(wmat) <- 0
    wmat <- rownorm(wmat)
    e <- z - wmat %*% z
    return(sum(abs(e)))}
  return(Vectorize(loo_core))
}

LOO_kern2 <- build_loo_kern2(bk_pm10_utm$ave_pm10,dmat_samp)

curve(LOO_kern2,1000,7000,xname='k')

opt_result_kern2 <- optimise(LOO_kern2,c(1000,7000))
opt_result_kern2

W <- rownorm(exp(-0.5*dmat^2/opt_result_kern2$minimum^2))
bk_grid_utm <- bk_grid_utm |> mutate(ksmooth2=W %*% bk_pm10_utm$ave_pm10)

ggplot(bk_grid_utm) + 
  annotation_map_tile(type='cartolight', zoomin=-1) + 
  annotation_scale(location='br') +
  geom_sf(alpha=0.5, col=NA, mapping=aes(fill=ksmooth2)) + 
  geom_sf(data=bk_prov_utm, fill=NA, col='Darkred', lwd=1) +
  scale_fill_viridis_c(name='PM10 µg/m³ \n(Kernel Smooth - 2)') +
  geom_sf(data=bk_pm10_utm, size=0.2, col='darkblue')

library(mgcv)
# don't run this! 
# model <- gam(z ~ s(x,y, bs='gp', k=m), data=obs)

bk_pm10_tib <- bk_pm10_utm |> 
  st_coordinates() |> 
  as_tibble() |> 
  mutate(pm10=bk_pm10_utm$ave_pm10)
bk_centre <- list(X=mean(bk_pm10_tib$X),Y=mean(bk_pm10_tib$Y))
# mutate the coordinates to km
bk_pm10_tib <- bk_pm10_tib |> mutate(X=(X-bk_centre$X)/1000,Y=(Y-bk_centre$Y)/1000)
head(bk_pm10_tib)

library(mgcv)
m1 <- gam(pm10~s(X,Y,bs='gp',m=c(-2,3.7,2)),data=bk_pm10_tib)

bk_grid_utm <- 
  bk_grid_utm |> 
  mutate(
    gp1=predict(m1,newdata=st_coordinates(bk_samp_utm) |>
                  as_tibble() |> 
                  mutate(X=(X-bk_centre$X)/1000,Y=(Y-bk_centre$Y)/1000)))

ggplot(bk_grid_utm, aes(fill=gp1)) + 
  annotation_map_tile(type='cartolight', zoomin=-1) + 
  geom_sf(alpha=0.5,col=NA) + 
  geom_sf(data=bk_prov_utm, fill=NA, col='Darkred', lwd=1) +
  scale_fill_viridis_c(name='PM10 µg/m³ (GP)')

bk_grid_utm <- 
  bk_grid_utm |> 
  mutate(
    se1=predict(m1,newdata=st_coordinates(bk_samp_utm) |> 
                  as_tibble() |> 
                  mutate(X=(X-bk_centre$X)/1000,Y=(Y-bk_centre$Y)/1000),
                se.fit=TRUE)$se.fit)

ggplot(bk_grid_utm) + 
  annotation_map_tile(type='cartolight', zoomin=-1) + 
  geom_sf(aes(fill=se1), alpha=0.5, col=NA) + 
  scale_fill_viridis_c(name='SE PM10 µg/m³ (GP)') +
  geom_sf(data=bk_prov_utm, fill=NA, col='Darkred', lwd=1) +
  geom_sf(data = bk_pm10_utm, col = "black")

# Self-Test Question 1
# load packages and data
library(tidyverse) 
library(Intro2R4SpatAnal)
library(sf)
library(cols4all)
data(ewhp)
data(ew_out)
ewhp_sf <- 
  ewhp |>
  filter(TypDetch  == 1) |>
  st_as_sf(coords = c("Easting", "Northing"), crs = 27700) |>
  transmute(p2a = (PurPrice/1000)/FlrArea)  
ew_grid_10k <- 
  ew_out |>
  st_make_grid(cellsize = 10000) |> 
  st_as_sf() |>
  st_intersection(ew_out) |>
  mutate(grid_ID=row_number())
# get the grid centroids
ew_samp_10k <- ew_grid_10k |> st_centroid() 
dmat <- st_distance(ew_samp_10k,ewhp_sf) |>
  units::drop_units()
dim(dmat)

# create LOO kernel functions
# squared differences
LOO_kern1 <- build_loo_kern(ewhp_sf$p2a,dmat)
curve(LOO_kern1,1000,max(dmat),xname='k')
# absolute differences
LOO_kern2 <- build_loo_kern2(ewhp_sf$p2a,dmat)
curve(LOO_kern2,1000,max(dmat),xname='k')

LOO_kern2 <- build_loo_kern2(ewhp_sf$p2a,dmat)

opt_result_kern <- optimise(LOO_kern2,c(1000,max(dmat)))
# create the kernel smooth using the LOO-optimal 
W <- rownorm(exp(-0.5*dmat^2/opt_result_kern$minimum^2))
ewhp_sf <- 
  ewhp |>
  filter(TypDetch  == 1) |>
  st_as_sf(coords = c("Easting", "Northing"), crs = 27700) |>
  transmute(p2a = (PurPrice/1000)/FlrArea)
ew_grid_10k <- ew_grid_10k |> mutate(ksmooth=W %*% ewhp_sf$p2a)
# and map the result
ggplot(ew_grid_10k) + 
  geom_sf(alpha=0.5,col=NA, mapping=aes(fill=ksmooth)) + 
  geom_sf(data=ew_out, fill=NA, col='Darkred') +
  scale_fill_viridis_c(name='£1000s/m³ \n(Detached House)') +
  geom_sf(data=ewhp_sf, size=0.2, col='darkblue')

# data of all observations
ewhp_sf <- 
  ewhp |>
  st_as_sf(coords = c("Easting", "Northing"), crs = 27700) |>
  transmute(p2a = (PurPrice/1000)/FlrArea)  
# distance matrix
dmat <- st_distance(ew_samp_10k,ewhp_sf) |>
  units::drop_units()
dim(dmat)
# squared differences
LOO_kern1 <- build_loo_kern(ewhp_sf$p2a,dmat)
curve(LOO_kern1,1000,max(dmat),xname='k')
# absolute differences
LOO_kern2 <- build_loo_kern2(ewhp_sf$p2a,dmat)
curve(LOO_kern2,1000,max(dmat),xname='k')

# Create new distance matrix of observation to observations
dmat2 <- st_distance(ewhp_sf, ewhp_sf) |>
  units::drop_units()
# Create a weight matrix with a value of k (below 5000m)
# assign weights only to all other points for each observation
W.resids <- rownorm(exp(-0.5*dmat2^2/10000^2))
diag(W.resids) <- 0
# and row normalise
W.resids <- rownorm(W.resids)
# Create an identity matrix that gets the local mean of neighbours 
W.resids <- diag(ncol(W.resids))-W.resids
# Use matrix multiplication to multiply by the value being estimated
# this is is a measure of similarity to neighbour based n the value if k above 
resids <- W.resids %*% ewhp_sf$p2a

boxplot(resids)
which(resids > 2)
ewhp_sf[114,]
# examine with others
ewhp[c(100:105, which(resids > 2)), c("PurPrice", "FlrArea")]

# subset the data and recreate the dmat
ewhp_sf_rob <- ewhp_sf[-which(resids > 2), ]
dmat_rob <- st_distance(ew_samp_10k,ewhp_sf_rob) |>
  units::drop_units()
# specify a new optimiser
LOO_kern <- build_loo_kern2(ewhp_sf_rob$p2a, dmat_rob)
curve(LOO_kern,1000,max(dmat_rob),xname='k')

# create the kernel smooth with a user-defined distance  
W <- rownorm(exp(-0.5*dmat^2/50000^2))
ew_grid_10k <- ew_grid_10k |> mutate(ksmooth=W %*% ewhp_sf$p2a)
# and map the result
ggplot(ew_grid_10k) + 
  geom_sf(alpha=0.5,col=NA, mapping=aes(fill=ksmooth)) + 
  geom_sf(data=ew_out, fill=NA, col='Darkred') +
  scale_fill_viridis_c(name='£1000s/m³ \n(All Houses)') +
  geom_sf(data=ewhp_sf, size=0.2, col='darkblue')
