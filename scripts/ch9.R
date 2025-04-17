## Code for Chapter 9 of An Introduction to R for Spatial Analysis and Mapping
# Chris Brunsdon & Lex Comber 

library(tidyverse)
library(Intro2R4SpatAnal)
data(nuts2_unemp_de)
ggplot(nuts2_unemp_de, aes(fill=Unemployment)) + 
  geom_sf(col='white')

require(spdep)
require(cols4all) 
# create the spatial weights for neighbours list
nuts2_listw_de <- nuts2_unemp_de |> poly2nb() |> nb2listw()
# undertake the local Moran's I
nuts2_lmoran <- localmoran(nuts2_unemp_de$Unemployment,nuts2_listw_de)
# add the result to the spatial data layer
nuts2_unemp_de <- nuts2_unemp_de |> mutate(Ii=nuts2_lmoran[,'Ii'])
# map the results
ggplot() + 
  geom_sf(data=nuts2_unemp_de, map=aes(fill=Ii), col='white') +
  scale_fill_continuous_c4a_div(palette =  "tableau.classic_orange_blue", 
                                name=expression(I[i]))

moran.test(nuts2_unemp_de$Unemployment,nuts2_listw_de)

nuts2_unemp_de <- nuts2_unemp_de |> mutate(p_i=nuts2_lmoran[,'Pr(z != E(Ii))'])

# specify shading cuts for significance intervals
cut_levels <- cut(nuts2_unemp_de$p_i, breaks = c(0,0.01,0.05,1))
# assign back to data
nuts2_unemp_de <- nuts2_unemp_de |> mutate(p_lev=cut_levels)
# and map
ggplot() + 
  geom_sf(data=nuts2_unemp_de, map=aes(fill=p_lev), col='white') +
  scale_fill_discrete_c4a_seq(palette =  "tableau.classic_area_green", 
                              name='p-values')

nuts2_unemp_de |> 
  filter(p_i <0.05) |>
  ggplot() + geom_sf(fill = "tomato") +
  geom_sf(data = nuts2_unemp_de, fill = NA, col = "black") +
  theme_bw() + ggtitle("Areas of significantly clustered unemployment levels")

1 - (1 - 0.05)^(1/38)

# calculate Bonferroni adjustment
nuts2_unemp_de <- nuts2_unemp_de |> mutate(bonf_p_i = p.adjust(p_i,method='bonferroni'))
# specify shading cuts as before
cut_levels <- cut(nuts2_unemp_de$bonf_p_i,breaks = c(0,0.01,0.05,1))
# assign back to data
nuts2_unemp_de <- nuts2_unemp_de |> mutate(p_lev_bonf=cut_levels)
# and map
ggplot() + 
  geom_sf(data=nuts2_unemp_de, map=aes(fill=p_lev_bonf), col='white') +
  scale_fill_discrete_c4a_seq(palette =  "tableau.classic_area_green", 
                              name='p-values \n(Bonferroni)')

# calculate Holm adjustment
nuts2_unemp_de <- nuts2_unemp_de |> mutate(holm_p_i = p.adjust(p_i,method='holm'))
# specify shading cuts as before
cut_levels <- cut(nuts2_unemp_de$holm_p_i,breaks = c(0,0.01,0.05,1))
nuts2_unemp_de <- nuts2_unemp_de |> mutate(p_lev_holm=cut_levels)
# and map
ggplot() + 
  geom_sf(data=nuts2_unemp_de, map=aes(fill=p_lev_holm), col='white') +
  scale_fill_discrete_c4a_seq(palette =  "tableau.classic_area_green", 
                              name='p-values \n(Holm)')

# calculate FDR adjustment
nuts2_unemp_de <- nuts2_unemp_de |> mutate(fdr_p_i = p.adjust(p_i,method='fdr'))
# specify shading cuts as before
cut_levels <- cut(nuts2_unemp_de$fdr_p_i,breaks = c(0,0.01,0.05,1))
nuts2_unemp_de <- nuts2_unemp_de |> mutate(p_lev_fdr=cut_levels)
# and map
ggplot() + 
  geom_sf(data=nuts2_unemp_de, map=aes(fill=p_lev_fdr), col='white') +
  scale_fill_discrete_c4a_seq(palette =  "tableau.classic_area_green", 
                              name='p-values \n(FDR)')

# determine the spatial weights neighbour list for 300 km
nuts2_listw_de2 <- nuts2_unemp_de |> 
  st_centroid() |> 
  dnearneigh(0,300000) |> 
  nb2listw()
# assign to the data
nuts2_lg <- localG(nuts2_unemp_de$Unemployment,nuts2_listw_de2)
nuts2_unemp_de <- nuts2_unemp_de |> mutate(Gi=as.numeric(nuts2_lg))
# and map 
ggplot() + 
  geom_sf(data=nuts2_unemp_de,map=aes(fill=Gi),col='white') +
  scale_fill_continuous_c4a_div(palette =  "tableau.classic_orange_blue", 
                                name=expression(G[i]))

# apply the FDR adjustment
nuts2_unemp_de <- 
  nuts2_unemp_de |>
  mutate(Gi_fdr = p.adjust(2*(1-pnorm(Gi)),method='fdr'))
# specify cut levels
cut_levels <- cut(nuts2_unemp_de$Gi_fdr,breaks = c(0,0.01,0.05,1))
nuts2_unemp_de <- nuts2_unemp_de |> mutate(Gi_p_lev_fdr=cut_levels)
# map 
ggplot() + 
  geom_sf(data=nuts2_unemp_de,map=aes(fill=Gi_p_lev_fdr),col='white') +
  scale_fill_discrete_c4a_seq(palette =  "tableau.classic_area_green", 
                                name='Gi p-values \n(FDR)')

library(Intro2R4SpatAnal)
data(ewhp)
ewhp_sf <- st_as_sf(ewhp,coords = 1:2,crs=27700)

ewhp_sf <- ewhp_sf |> mutate(PurPrice = PurPrice / 1000)
ggplot(ewhp_sf, aes(x=FlrArea,y=PurPrice)) + 
  geom_point(alpha = 0.5) + 
  xlab('Floor Area (Sqr. Meters)') +
  ylab('Purchase Price (£1000s))') +
  theme_bw() 

library(e1071)
ewhp_sf |> 
  st_drop_geometry() |> 
  summarise(mean(PurPrice),sd(PurPrice),skewness(PurPrice))

ggplot(ewhp_sf, aes(x=PurPrice)) +
  geom_histogram(aes(y=after_stat(density)), col = "lightgrey", fill = "salmon", bins = 20) +
  geom_density(alpha=.4, fill="darksalmon") +
  xlab('Purchase Price (£1000s)') + theme_bw()

library(ggspatial)
ggplot(ewhp_sf) + 
  annotation_map_tile(type='cartolight',zoomin=-1) +
  geom_sf(alpha = 0.5, col = "tomato") + 
  coord_sf(xlim=c(150000,656400),ylim=c(0,574000))

# define function
rownorm <- \(x) sweep(x,1,rowSums(x),'/')
# calculate the distance matrix
dmat <- st_distance(ewhp_sf,ewhp_sf) |>
  units::drop_units()
# calculate the weight matrix
W <- exp(-0.5*(dmat/50000)^2) |> rownorm()
# apply the weight matrix to generate GWSSs
# means
gwmean_PurPrice <- W %*% ewhp$PurPrice
gwmean_FlrArea  <- W %*% ewhp$FlrArea
# std devs
gwstd_PurPrice  <- sqrt(W %*% (ewhp$PurPrice - gwmean_PurPrice)^2)
gwstd_FlrArea   <- sqrt(W %*% (ewhp$FlrArea - gwmean_FlrArea)^2)
# skews
gwskew_PurPrice <- (W %*% (ewhp$PurPrice - gwmean_PurPrice)^3)^(1/3)/gwstd_PurPrice
gwskew_FlrArea  <- (W %*% (ewhp$FlrArea - gwmean_FlrArea)^3)^(1/3)/gwstd_FlrArea

ewhp_gwss <- ewhp_sf |> 
  select(PurPrice, FlrArea) |>
  mutate(
    `PurPrice GWmean` = gwmean_PurPrice,
    `PurPrice GWstd`  = gwstd_PurPrice,
    `PurPrice GWskew` = gwskew_PurPrice,
    `FlrArea GWmean` = gwmean_FlrArea,
    `FlrArea GWstd`  = gwstd_FlrArea,
    `FlrArea GWskew` = gwskew_FlrArea)
head(ewhp_gwss)

lf <- ewhp_gwss  |> 
      select(`PurPrice GWmean`:`FlrArea GWskew`) |>
      pivot_longer(`PurPrice GWmean`:`FlrArea GWskew`) 

# define and apply a rescale function
rescale01 <- \(x) (x - mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
lf <- lf  |> mutate(sval = rescale01(value),.by = name) 
# create the faceted maps
ggplot(lf, aes(col=sval))  + 
  annotation_map_tile(type='cartolight', zoomin=-1) +
  scale_colour_continuous_c4a_div( 
    palette =  "tableau.classic_orange_blue",
    name = "Standardised\nValues") +
  geom_sf(size=0.5) + 
  facet_wrap(~name) + 
  coord_sf(xlim=c(150000,656400), ylim=c(0,574000)) +
  theme_bw() + theme(axis.text = element_blank(),
                     axis.ticks = element_blank())

gwlq <- function(x,W,stem="") {
  igwlq <- function(x,W,i) {
    as_tibble(rbind(
      approx(cumsum(W[i,order(x)]),
             sort(x),
             c(0.25,0.5,0.75))$y))}
  res <- map(1:length(x),~igwlq(x,W,.x)) |> list_rbind()
  colnames(res) <- c("Q1","Q2","Q3")
  res <- res |> 
    mutate(Median=Q2,
           IQR = Q3 - Q1, 
           QI=(Q3 - 2*Q2 + Q1)/(Q3 - Q1)) |>
    select(Median,IQR,QI)
  colnames(res) <- sprintf("%s %s",stem, colnames(res))
  res
}

ewhp_gwlq <- bind_cols(ewhp_sf,  gwlq(ewhp_sf$PurPrice,W,'PurPrice'))
ewhp_gwlq <- bind_cols(ewhp_gwlq,gwlq(ewhp_sf$FlrArea ,W,'FlrArea' ))

# pivot and rescale into long form as before 
lf_r <- ewhp_gwlq  |> 
        select(ends_with('Median') | 
               ends_with('IQR') | 
               ends_with('QI')) |>
      pivot_longer(`PurPrice Median`:`FlrArea QI`) 
lf_r <- lf_r  |> mutate(sval = rescale01(value),.by=name) 
# Plot the result
ggplot(lf_r, aes(col=sval))  + 
  annotation_map_tile(type='cartolight', zoomin=-1) +
  scale_colour_continuous_c4a_div( 
    palette =  "tableau.classic_orange_blue",
    name = "Standardised\nValues") +
  geom_sf(size=0.5) + 
  facet_wrap(~name) + 
  coord_sf(xlim=c(150000,656400), ylim=c(0,574000)) +
  theme_bw() + theme(axis.text = element_blank(),
                     axis.ticks = element_blank())

library(wCorr)
gwProdMom <- imap_dbl(1:nrow(ewhp),
                      ~weightedCorr(ewhp$PurPrice,ewhp$FlrArea,'Pearson',W[.x,]))
gwSpearman<- imap_dbl(1:nrow(ewhp),
                      ~weightedCorr(ewhp$PurPrice,ewhp$FlrArea,'Spearman',W[.x,]))
ewhp_gwss <- ewhp_gwss |> 
  mutate(gwProdMom=gwProdMom,
         gwSpearman = gwSpearman)

lf2 <- ewhp_gwss  |> 
        select(gwProdMom:gwSpearman) |>
      pivot_longer(gwProdMom:gwSpearman) 
ggplot(lf2, aes(col=value))  + 
  annotation_map_tile(type='cartodark',zoomin=-1) +
  geom_sf(size=0.5) + 
  facet_wrap(~name) + 
  scale_colour_viridis_c() +
  coord_sf(xlim=c(150000,656400), ylim=c(0,574000)) +
  theme_bw() + theme(axis.text = element_blank(),
                     axis.ticks = element_blank())

# create a matrix containing the target variable 
Y <- cbind(ewhp$PurPrice)
colnames(Y) <- "PurPrice"
# create a matrix containing the predictor variable plus an intercept 
X <- cbind(1,ewhp$FlrArea)
colnames(X) <- c("Intercept","FlrArea")
# define a GWR function 
qgwr <- function(X,Y,W) { 
  # define a weighted regression function
  w_reg <- \(X,Y,w) {
    # define a weighting function 
    rowscale <- \(M,w) sweep(M,1,sqrt(w),'*')
    # apply to the data
    Xm <- rowscale(X,w)
    Ym <- rowscale(Y,w)
    # return
    as_tibble(t(solve(crossprod(Xm),crossprod(Xm,Ym)))) 
  }
  # apply this using map
  temp <- map(1:nrow(ewhp),~w_reg(X,Y,W[.x,])) |> 
    list_rbind()
  if (! is.null(colnames(X))) 
    colnames(temp) <- sprintf("%s_coef",colnames(X))
  temp
}
# apply the function using the W object defined before
# but restated here
W <- exp(-0.5*(dmat/50000)^2) |> rownorm()
a_vec <- qgwr(X,Y,W)

# Add the gw coef estimates
ewhp_gwr <- bind_cols(ewhp_sf,a_vec)
# Pivot the SDF into long form
lf_gwr <- ewhp_gwr  |> 
        select(ends_with('_coef')) |>
        pivot_longer(ends_with('_coef')) 
lf_gwr <- lf_gwr  |> mutate(sval = rescale01(value),.by=name) 

# Plot the result
ggplot(lf_gwr, aes(col=sval))  + 
  annotation_map_tile(type='cartolight', zoomin=-1) +
  scale_colour_continuous_c4a_div( 
    palette =  "tableau.classic_orange_blue",
    name = "Standardised\nValues") +
  geom_sf(size=0.5) + 
  facet_wrap(~name) + 
  coord_sf(xlim=c(150000,656400), ylim=c(0,574000)) +
  theme_bw() + theme(axis.text = element_blank(),
                     axis.ticks = element_blank())

# Get coordinates
xy <- st_coordinates(ewhp_gwr)
# Compute distance from a point in north east England
dne <- sqrt(rowSums(sweep(xy,2,c(452300,517200),"-")^2))
# Is each location less than 75km away?
in_ne <- dne < 75000
ewhp_sf <- ewhp_sf |> mutate(in_ne=in_ne)
ggplot(ewhp_sf, aes(x=FlrArea, y=PurPrice,col=in_ne)) + 
  geom_point() +
  scale_colour_discrete_c4a_cat(  name = "In the NE")

# A distance matrix of the nearest 100 neighbours to each observation 
d100 <- apply(dmat,1,\(x) sort(x,partial=101)[101])
# the spatial weights   
W2 <- exp(-0.5*(sweep(dmat,1,d100,'/'))^2) |> rownorm()
# apply the GWR and join to the result
a_vec2 <- qgwr(X,Y,W2)
ewhp_gwr2 <- bind_cols(ewhp_sf,a_vec2)

# original coefficient estimates
ewhp_gwr2 |> st_drop_geometry() |> 
  select(Intercept_coef, FlrArea_coef) |> 
  summary()

# lengthen and rescale the data 
lf_gwr2 <- ewhp_gwr2  |> 
        select(ends_with('_coef')) |>
        pivot_longer(ends_with('_coef')) 
lf_gwr2 <- lf_gwr2  |> mutate(sval = rescale01(value),.by=name) 
# map
ggplot(lf_gwr2, aes(col=sval))  + 
  annotation_map_tile(type='cartolight',zoomin=-1) +
  scale_colour_continuous_c4a_div( 
    palette =  "tableau.classic_orange_blue",
    name = "Standardised\nValues") +
  geom_sf(size=0.5) + 
  facet_wrap(~name) + 
  coord_sf(xlim=c(150000,656400), ylim=c(0,574000)) +
  theme_bw() + theme(axis.text = element_blank(),
                     axis.ticks = element_blank())

# Self-Test Question 1
nuts2_unemp_de |> 
  filter(p_i <0.05) |>
  ggplot() + geom_sf(fill = "tomato") +
  geom_sf(data = nuts2_unemp_de, fill = NA, col = "black") +
  theme_bw()

# Self-Test Question 2
# 1. Compute distance matrix and weights
d100 <- apply(dmat,1,\(x) sort(x,partial=101)[101])
W <- exp(-0.5*(sweep(dmat,1,d100,'/'))^2) |> rownorm()
# 2. Apply the weight matrix to generate GWSSs
# means
gwmean_PurPrice <- W %*% ewhp$PurPrice
gwmean_FlrArea  <- W %*% ewhp$FlrArea
# std devs
gwstd_PurPrice  <- sqrt(W %*% (ewhp$PurPrice - gwmean_PurPrice)^2)
gwstd_FlrArea   <- sqrt(W %*% (ewhp$FlrArea - gwmean_FlrArea)^2)
# skews
gwskew_PurPrice <- (W %*% (ewhp$PurPrice - gwmean_PurPrice)^3)^(1/3)/gwstd_PurPrice
gwskew_FlrArea  <- (W %*% (ewhp$FlrArea - gwmean_FlrArea)^3)^(1/3)/gwstd_FlrArea
# 3. Combine these to create a new sf object 
ewhp_gwss_adapt <- 
  ewhp_sf |> 
  select(PurPrice, FlrArea) |>
  mutate(
    `PurPrice GWmean` = gwmean_PurPrice,
    `PurPrice GWstd`  = gwstd_PurPrice,
    `PurPrice GWskew` = gwskew_PurPrice,
    `FlrArea GWmean` = gwmean_FlrArea,
    `FlrArea GWstd`  = gwstd_FlrArea,
    `FlrArea GWskew`= gwskew_FlrArea)
# 4. Create long column data table with sticky geometry
lf_adapt <- ewhp_gwss_adapt  |> 
      select(`PurPrice GWmean`:`FlrArea GWskew`) |>
      pivot_longer(`PurPrice GWmean`:`FlrArea GWskew`) 
# 5. Define and apply a rescale function
rescale01 <- \(x) (x - mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
lf_adapt <- lf_adapt  |> mutate(sval = rescale01(value),.by = name) 
# 6. Plot the faceted maps
ggplot(lf_adapt, aes(col=sval))  + 
  annotation_map_tile(type='cartolight', zoomin=-1) +
  scale_colour_continuous_c4a_div( 
    palette =  "tableau.classic_orange_blue",
    name = "Standardised\nValues") +
  geom_sf(size=0.5) + 
  facet_wrap(~name) + 
  coord_sf(xlim=c(150000,656400), ylim=c(0,574000)) +
  theme_bw() + theme(axis.text = element_blank(),
                     axis.ticks = element_blank())
