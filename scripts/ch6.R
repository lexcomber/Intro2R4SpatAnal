## Code for Chapter 6 of An Introduction to R for Spatial Analysis and Mapping
# Chris Brunsdon & Lex Comber 


require(tidyverse)
require(sf)
# Get the data
require(Intro2R4SpatAnal)
data(leeds_lsoa)
data(crimes)
breach <- crimes |> 
  st_transform(27700) |> 
  filter(category=='public-order')
# Create a map of blocks and incidents
ggplot(leeds_lsoa) + 
  geom_sf(fill='grey75') + 
  geom_sf(data=breach,col='darkblue')


# Function to choose bandwidth according Bowman and Azzalini / Scott's rule
# for use with sf
choose_bw <- function(sf) {
  X <- st_coordinates(sf)
  sigma_x <- sd(X[,1])  * (2 / (3 * nrow(X))) ^ (1/6)
  sigma_y <- sd(X[,2])  * (2 / (3 * nrow(X))) ^ (1/6)
  return(c(sigma_x,sigma_y))
}

# kde_sf computes kernel densities in 2D
kde_sf <- function(pts,bw,...) {
  op_grid <- st_make_grid(breach,...) 
  op_grid_xy <- op_grid |> st_centroid() |> st_coordinates()
  pts_xy <- pts |> st_coordinates()
  if (length(bw) == 1) bw = c(bw,bw)
  dx <- outer(op_grid_xy[,1],pts_xy[,1],'-')
  dy <- outer(op_grid_xy[,2],pts_xy[,2],'-')
  km <- dnorm(dx/bw[1])*dnorm(dy/bw[2])
  density <- rowSums(km)
  density <- density/sum(density)
  op_grid |> st_sf() |> mutate(density=density)
}

kern <- kde_sf(breach,choose_bw(breach),n=50,square=FALSE) 
ggplot(leeds_lsoa) + geom_sf(fill='grey75') + 
  geom_sf(data=kern,map=aes(alpha=density),col=NA,fill='darkred') +
  scale_alpha_continuous(range=c(0,1)) 


drugs <- crimes |>
  filter(category=='drugs') |>
  st_transform(27700)

# create and bind the crimes
dr_br <- bind_rows(breach |> mutate(Crime='Breach of Peace'),
                   drugs  |> mutate(Crime='Drugs'))
# plot
ggplot(leeds_lsoa) + 
  geom_sf(fill='grey75') + 
  geom_sf(data=dr_br,map=aes(col=Crime))


# create and bind the KDE kernels
br_kern <- kde_sf(breach,choose_bw(breach),n=50,square=FALSE) 
dr_kern <- kde_sf(drugs ,choose_bw(drugs) ,n=50,square=FALSE) 
dr_br_kern <- 
  bind_rows(br_kern |> mutate(Crime='Breach of Peace'),
            dr_kern |> mutate(Crime='Drugs'))
# map
ggplot(leeds_lsoa) + 
  geom_sf(fill='grey75') + 
  geom_sf(data=dr_br_kern,
          map=aes(alpha=density),
          col=NA,
          fill='darkred') +
  scale_alpha_continuous(range=c(0,1)) +
  facet_wrap(~Crime,ncol=2)


# K-function code block
# Load the spatstat package
require(spatstat)
# Obtain the bramble cane data
data(bramblecanes)
plot(bramblecanes, main = "")

kf <- Kest(bramblecanes,correction='border')
# Plot it
plot(kf, main = "")

# Code block to produce k-function with envelope
# Envelope function
kf.env <- envelope(bramblecanes,Kest,correction="border")
# Plot it
plot(kf.env, main = "")

mad.test(bramblecanes,Kest,verbose=FALSE)

dclf.test(bramblecanes,Kest,verbose=FALSE)

# Code block to produce k-function with envelope
# Envelope function
lf.env <- envelope(bramblecanes,Lest,correction="border")
# Plot it
plot(lf.env, main = "")

mad.test(bramblecanes,Lest,verbose=FALSE)

# Code block to produce G-function with envelope
# Envelope function
gf.env <- envelope(bramblecanes,Gest,correction="border")
# Plot it
plot(gf.env, main = "")

marks(bramblecanes)[1:20]

cl.bramble <- Lcross(bramblecanes,i=0,j=1,correction='border')
plot(cl.bramble, main = "")

clenv.bramble <- envelope(bramblecanes,Lcross,i=0,j=1,correction='border')
plot(clenv.bramble, main = "")

dclf.test(bramblecanes,Kcross,i=0,j=1,correction='border',verbose=FALSE)

crimes |>  
  st_transform(27700) |>
  filter(category=='burglary' | category=='vehicle-crime') |>  
  transmute(value = (category == 'burglary') + 0) 

# Self-Test Question 1
kern <- kde_sf(breach,choose_bw(breach),n=50,square=TRUE) 
ggplot(leeds_lsoa) + geom_sf(fill='grey75') + 
  geom_sf(data=kern,map=aes(alpha=density),col=NA,fill='darkred') +
  scale_alpha_continuous(range=c(0,1)) 

# Self-Test Question 2
# extract the crimes
carcrime <- crimes |> filter(category=='vehicle-crime') |>  st_transform(27700)
housecrime <- crimes |> filter(category=='burglary') |>  st_transform(27700)
# create and merge the KDEs
cr_kern <- kde_sf(carcrime,choose_bw(carcrime),n=50,square=FALSE) 
hr_kern <- kde_sf(housecrime ,choose_bw(housecrime) ,n=50,square=FALSE) 
cr_hr_kern <- 
  bind_rows(cr_kern |> mutate(Crime='Vehicle Crime'),
            hr_kern |> mutate(Crime='Burglary'))
# plot
ggplot(leeds_lsoa) + 
  geom_sf(fill='grey95') + 
  geom_sf(data=cr_hr_kern,
          map=aes(alpha=density),
          col=NA,
          fill='cornflowerblue') +
  scale_alpha_continuous(range=c(0,1)) +
  facet_wrap(~Crime,ncol=2)

# Self-Test Question 3
# extract and convert the crimes 
posscrime <- crimes |> 
  filter(category=='possession-of-weapons') |>  
  st_transform(27700) |> 
  as.ppp()

# apply plots and tests
# L function
l.env <- envelope(posscrime,Lest,correction="border")
plot(l.env, main = "")
mad.test(posscrime,Lest,verbose=FALSE)

# K function
k.env <- envelope(posscrime,Kest,correction="border")
plot(k.env, main = "")
mad.test(posscrime,Kest,verbose=FALSE)

# G function
g.env <- envelope(posscrime,Gest,correction="border")
plot(g.env, main = "")
mad.test(posscrime,Gest,verbose=FALSE)

# Self-Test Question 4
# extract and convert the crimes to numbers 
crimes.ppp <- 
  crimes |>  
  st_transform(27700) |>
  filter(category=='burglary' | category=='vehicle-crime') |>  
  transmute(value = (category == 'burglary') + 0) |> 
  mutate(value = as.factor(value)) |> 
  as.ppp()

clenv.crimes <- envelope(crimes.ppp,Kcross,i=0,j=1,correction='border')
plot(clenv.crimes, main = "")
