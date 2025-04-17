## Code for Chapter 8 of An Introduction to R for Spatial Analysis and Mapping
# Chris Brunsdon & Lex Comber 

install.packages('SpatialEpi',depend=TRUE)

# Make sure the necessary packages have been loaded
library(tidyverse)
library(sf)
library(SpatialEpi)
# Read in the Pennsylvania lung cancer data
data(pennLC_sf)
# Convert to UTM zone 17N
penn_state_utm <- st_transform(pennLC_sf, 3724)
# Obtain the smoking rates
penn_smk_pc <- penn_state_utm |> 
  summarise(smoking=first(smoking),
            cases=sum(cases),
            population=sum(population),.by=geometry) |> 
  mutate(smk_pc=smoking * 100)
# Draw a choropleth map of the smoking rates
ggplot(penn_smk_pc, aes(fill=smk_pc)) + geom_sf() + theme_bw()

# Set up a set of five 'fake' smoking update rates as well as the real one
# Create a new tibble called 'penn_perms' in *long* format
# containing the true rates (plt=1) and five permutations (plt=2...6)
# Here the seed 19620129 is used - try a different one to get an unknown outcome
set.seed(19620129)
penn_perms_1 <-  penn_smk_pc |> mutate(plt=1)
penn_perms_2 <-  penn_smk_pc |> mutate(plt=2,smk_pc=sample(smk_pc))
penn_perms_3 <-  penn_smk_pc |> mutate(plt=3,smk_pc=sample(smk_pc))
penn_perms_4 <-  penn_smk_pc |> mutate(plt=4,smk_pc=sample(smk_pc))
penn_perms_5 <-  penn_smk_pc |> mutate(plt=5,smk_pc=sample(smk_pc))
penn_perms_6 <-  penn_smk_pc |> mutate(plt=6,smk_pc=sample(smk_pc))
# bind the permutations together
penn_perms <- bind_rows(
  penn_perms_1,
  penn_perms_2,
  penn_perms_3,
  penn_perms_4,
  penn_perms_5,
  penn_perms_6)
# change the plot order: 
# plt is plot order - permuted so you don't know which plot is
# the real data - but noted in 'real_data' as well
# Don't look at 'real_data' before you see the maps!
plt_perms <- sample(6)
penn_perms <- penn_perms |> mutate(plt=plt_perms[plt])
real_data <- plt_perms[1]
ggplot(penn_perms, aes(fill=smk_pc)) + geom_sf() + 
  facet_wrap(~plt,nrow=2) + 
  theme_bw() + theme(axis.text = element_blank(), 
                     axis.ticks = element_blank())

real_data

penn_perms_2 <-  penn_smk_pc |> mutate(plt=2,smk_pc=sample(smk_pc))


require(spdep)
penn_smk_nb <- poly2nb(penn_smk_pc)
penn_smk_nb

length(penn_smk_nb)
nrow(penn_smk_pc)
penn_smk_nb[[4]]

# Create an sf object showing the Queen's case contiguities
penn_smk_net <- 
  penn_smk_nb |>
  nb2lines(coords=penn_smk_pc |> 
              st_centroid() |> 
              st_coordinates(),
           as_sf=TRUE) |>
  st_set_crs(3724)
# Draw the projections
ggplot(penn_smk_pc) + geom_sf(fill = NA, col = "black") + 
  geom_sf(data=penn_smk_net, col='darkred') + 
  theme_bw() 

# Calculate the Rook's case neighbours
penn_smk_nb2 <- poly2nb(penn_smk_pc,queen=FALSE)
# Convert this to a network
penn_smk_net2 <- 
  penn_smk_nb2 |>
  nb2lines(coords=penn_smk_pc |> 
              st_centroid() |> 
              st_coordinates(),
           as_sf=TRUE) |>
  st_set_crs(3724)
# Plot the counties in background,  then the two networks to compare: 
ggplot(penn_smk_pc) + geom_sf(fill = NA, col = "black") + 
  geom_sf(data=penn_smk_net, col='darkred', lwd=2) +
  geom_sf(data=penn_smk_net2, col='wheat', lwd=1) + 
  theme_bw() 

# Convert the neighbour list to a listw object - use Rook's case...
penn_smk_lw <- nb2listw(penn_smk_nb2)
penn_smk_lw

penn_smk_pc <-
  penn_smk_pc |>
  mutate(smk_pc_lagm = lag.listw(penn_smk_lw,smk_pc))
ggplot(penn_smk_pc, aes(fill=smk_pc_lagm)) + geom_sf() + theme_bw() 

ggplot(penn_smk_pc, aes(x=smk_pc, y=smk_pc_lagm))  + 
  geom_point() + 
  geom_abline(slope=1, intercept=0) + 
  geom_hline(yintercept=mean(penn_smk_pc$smk_pc_lagm), lty=2) +
  geom_vline(xintercept=mean(penn_smk_pc$smk_pc), lty=2) +
  coord_equal()

moran.plot(penn_smk_pc$smk_pc, penn_smk_lw)

moran.test(penn_smk_pc$smk_pc,penn_smk_lw)

wm <- listw2mat(penn_smk_lw)
ev <- eigen((wm + t(wm))/2)$values
evh <- max(ev)
evl <- min(ev)

moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}
moran.range(penn_smk_lw)

moran.test(penn_smk_pc$smk_pc,penn_smk_lw,randomisation=FALSE)

moran.mc(penn_smk_pc$smk_pc,penn_smk_lw,10000)

library(spatialreg)
sar.res <- spautolm(smk_pc~1,listw=penn_smk_lw,data=penn_smk_pc)
sar.res

sar.res$lambda.se

sar.res$lambda + c(-2,2)*sar.res$lambda.se

car.res <- spautolm(smk_pc~1,listw=penn_smk_lw,
                    data=penn_smk_pc,
                    family='CAR')
car.res

# create the rate
penn_smk_pc <- 
  penn_smk_pc |> 
  mutate(rate = 10000*cases/population)

ggplot(penn_smk_pc, aes(fill=rate)) + geom_sf() +theme_bw()

# create the square root of the rate
penn_smk_pc <- penn_smk_pc |>
  mutate(z=sqrt(rate)) 
# create the model
sar.mod <- spautolm(z~smk_pc,
                    listw=penn_smk_lw,
                    weight=population,data=penn_smk_pc)
summary(sar.mod)

library(Intro2R4SpatAnal)
data(columbus)
# Create a plot of columbus and add labels for each of the zones
ggplot(columbus) + 
  geom_sf(fill='wheat') + 
  geom_text(map=aes(label=POLYID,x=X,y=Y),size=2) +
  theme_bw()

# Extract a 'queen's case' adjacency object and print it out
col_queen_nb <- poly2nb(columbus,queen=TRUE)
col_queen_nb
# Extract a 'rooks's case' adjacency object and print it out
col_rook_nb <- poly2nb(columbus,queen=FALSE)
col_rook_nb

covmat <- function(lambda,adj) {
  solve(tcrossprod(diag(length(adj)) - lambda* listw2mat(nb2listw(adj))))
}

cormat <- function(lambda,adj) {
  cov2cor(covmat(lambda,adj))
}

# Create a range of valid lambda values
lambda_range <- seq(-1.3,0.99,l=101)
# Create an array to store the corresponding correlations
cor_41_47 <- lambda_range*0
# ... store them
for (i in 1:101) cor_41_47[i] <- cormat(lambda_range[i],col_rook_nb)[41,47]
# ... plot the relationship
plot(lambda_range,cor_41_47,type='l')

# First, add the line from the previous figure for reference
plot(lambda_range,cor_41_47,type='l',xlab=expression(lambda),ylab='Correlation',lty=2)
# Now compute the correlation between zones 40 and 41.
cor_40_41 <- lambda_range*0
for (i in 1:101) cor_40_41[i] <- cormat(lambda_range[i],col_rook_nb)[40,41]
# ... and add these to the plot
lines(lambda_range,cor_40_41)

# First,  plot the empty canvas (type='n')
plot(c(-1,1),c(-1,1),type='n',xlim=c(-1,1),ylim=c(-1,1),xlab='Corr1',ylab='Corr2')
# Then the quadrants
rect(-1.2,-1.2,1.2,1.2,col='pink',border=NA)
rect(-1.2,-1.2,0,0,col='lightyellow',border=NA)
rect(0,0,1.2,1.2,col='lightyellow',border=NA)
# Then the x=y reference line
abline(a=0,b=1,lty=3)
# Then the curve
lines(cor_40_41,cor_41_47)

# First,  plot the empty canvas (type='n)
plot(c(-1,1),c(-1,1),type='n',xlim=c(-1,1),ylim=c(-1,1),
     xlab='Corr1',ylab='Corr2')
# Then the quadrants
rect(-1.2,-1.2,1.2,1.2,col='pink',border=NA)
rect(-1.2,-1.2,0,0,col='lightyellow',border=NA)
rect(0,0,1.2,1.2,col='lightyellow',border=NA)
# Then the x=y reference line
abline(a=0,b=1,lty=3)
# Then the curves
# First, set a seed for reproducibility
set.seed(310712)
for (i in 1:100) {
  r1 <- sample(1:length(col_rook_nb),1)
  r2 <- sample(col_rook_nb[[r1]],2)
  cor_ij1 <- lambda_range*0
  cor_ij2 <- lambda_range*0
  for (k in 1:101) 
    cor_ij1[k] <- cormat(lambda_range[k],col_rook_nb)[r1,r2[1]]
  for (k in 1:101) 
    cor_ij2[k] <- cormat(lambda_range[k],col_rook_nb)[r1,r2[2]]
  lines(cor_ij1,cor_ij2)
}

# Self-Test Question 1
set.seed(19620129)
penn_perms_1 <-  penn_smk_pc |> mutate(plt=1)
penn_perms_2 <-  penn_smk_pc |> mutate(plt=2,smk_pc=sample(smk_pc,replace=TRUE))
penn_perms_3 <-  penn_smk_pc |> mutate(plt=3,smk_pc=sample(smk_pc,replace=TRUE))
penn_perms_4 <-  penn_smk_pc |> mutate(plt=4,smk_pc=sample(smk_pc,replace=TRUE))
penn_perms_5 <-  penn_smk_pc |> mutate(plt=5,smk_pc=sample(smk_pc,replace=TRUE))
penn_perms_6 <-  penn_smk_pc |> mutate(plt=6,smk_pc=sample(smk_pc,replace=TRUE))
# bind together
penn_perms <- bind_rows(
  penn_perms_1,
  penn_perms_2,
  penn_perms_3,
  penn_perms_4,
  penn_perms_5,
  penn_perms_6)
# randomise the plot order
plt_perms <- sample(6)
penn_perms <- penn_perms |> mutate(plt=plt_perms[plt])
real_data <- plt_perms[1]
# plot
ggplot(penn_perms,aes(fill=smk_pc)) + geom_sf() + 
  facet_wrap(~plt,nrow=2) +
  theme_bw() + theme(axis.text = element_blank(),
                     axis.ticks = element_blank())

# Self-Test Question 2
# Load in the data 
require(Intro2R4SpatAnal)
data(leeds_lsoa)
data(crimes)
# Transform 'crimes' to British National Grid and filter out the burglaries
burgs <- crimes |> 
  filter(category=='burglary') |> 
  st_transform(27700)

# Attach an lsoa code to each burglary
burgs <- burgs |> 
  st_join(leeds_lsoa) 

# Count burglaries in each lsoa (use 'code' as the factor to count)
# Here we just want the table,  and dropping the geometry speeds up the process
burg_counts <- burgs |> st_drop_geometry() |> count(code) 

# Join these on to the leeds_lsoa table
lsoa_burgs <- leeds_lsoa |> left_join(burg_counts) |> rename(nburgs=n)

# lsoas with no burglaries don't appear in 'burg_counts',  
# so after the join, these lsoas are NA.  Really they should be zero,  lets
# correct this
lsoa_burgs <- lsoa_burgs |> mutate(nburgs=if_else(is.na(nburgs),0,nburgs))

# Now calculate the burglary rate per 1000 population
lsoa_burgs <- lsoa_burgs |> mutate(burgrate=1000*nburgs/totpop)

# Next,  construct the adjacency information
lsoa_listw <- lsoa_burgs |> poly2nb() |>nb2listw()

# Now do the test
moran.test(lsoa_burgs$burgrate,lsoa_listw)
