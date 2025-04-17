## Code for Chapter 2 of An Introduction to R for Spatial Analysis and Mapping
# Lex Comber & Chris Brunsdon

# examples of simple assignment
x <- 5 
y <- 4 
# the variables can be used in other operations
x+y
# including defining new variables
z <- x + y
z
# which can then be passed to other functions
sqrt(z)

# example of Vector assignment
hospital.distances <- c(4.3, 7.1, 6.3, 5.2, 3.2, 2.1)
hospital.distances

# multiplied by 2
hospital.distances*2
# squared (to the power of 2)
hospital.distances^2

sum(hospital.distances)
mean(hospital.distances)

max.dist <- max(hospital.distances)
max.dist

hospital.distances
hospital.distances [1]    # first element
hospital.distances[1:3]   # a subset of elements 1 to 3
sqrt(hospital.distances[1:3]) # square roots of the subset
hospital.distances[c(5, 3, 2)]  # elements 5,3,2 (note the ordering)

# example of character assignment
name <- "Lex Comber" 
name
# multiple elements assigned to a vector of character variables
cities <- c("Exeter","Leeds","Leicester","Liverpool","London","Newcastle")
cities
length(cities)

# an example of a logical vector 
northern <- c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE)
northern
# this can be used to subset other variables
cities[northern]


# creates an empty character vector
character(length = 8) 
# conversion
as.character(hospital.distances) 
# tests
is.character(8)
is.character("8")

# creates an empty character vector
numeric(length = 8)
# conversions
as.numeric(c("1980","-8","Geography"))
as.numeric(c(FALSE,TRUE))
# tests
is.numeric(hospital.distances)
is.numeric(c(8, 8, 8, "8"))

logical(length = 7)
# conversion
as.logical(c(7, 5, 0, -4, 5, 0.5))
# TRUE and FALSE can be converted to 1 and 0
as.logical(c(7, 5, 0, -4, 5, 0.5)) * 1
as.logical(c(7, 5, 0, -4, 5, 0.5)) + 0
# different ways to declare TRUE and FALSE 
as.logical(c("True","T","FALSE","Raspberry","9","0", 0))

# data from before
hospital.distances
# a logical test 
index <- (hospital.distances > 5)
index
# used to subset data
hospital.distances[index]
# and to undertake operations
mean(hospital.distances)
mean(hospital.distances[index])

# defining vectors
vector(mode = "numeric", length = 8)
vector(length = 8)
a = 1:6
a
# testing and conversion
is.vector(a)
is.vector(cities)
is.vector(hospital.distances)
# combine equal length vectors 
tmp <- data.frame(ID = a, Place = cities, Dist = hospital.distances)
tmp
is.vector(tmp)
as.vector(tmp)
as.vector(hospital.distances)
as.vector(cities)

# defining matrices
matrix(ncol = 2, nrow = 10)
matrix(hospital.distances)
matrix(cities, ncol = 2)
matrix(cities, ncol = 2, byrow = TRUE)
# conversion and test
as.matrix(6:3)
is.matrix(as.matrix(6:3))

flow <- matrix(c(2000, 1243, 543, 1243, 212, 545, 
    654, 168, 109), nrow = 3, ncol = 3, byrow=TRUE) 
# Rows and columns can have names, not just 1,2,3,...
colnames(flow) <- c("Leeds", "Maynooth"," Elsewhere")
rownames(flow) <- c("Leeds", "Maynooth", "Elsewhere")
# examine the matrix
flow 
# and functions exist to summarise
outflows <- rowSums(flow) 
outflows

z <- c(6,7,8)
names(z) <- c("Newcastle","London","Manchester")
z

# a vector assignment
house.type <- c("Bungalow", "Flat", "Flat", 
                "Detached", "Flat", "Terrace", "Terrace")
# a factor assignment
house.type <- factor(c("Bungalow", "Flat", "Flat", 
                       "Detached", "Flat", "Terrace", "Terrace"), 
                     levels=c("Bungalow","Flat","Detached","Semi","Terrace"))
house.type
# table can be used to summarise
table(house.type)
# 'levels' control what can be assigned 
house.type <- factor(c("People Carrier", "Flat", 
                       "Flat", "Hatchback", "Flat", "Terrace", "Terrace"),
                     levels=c("Bungalow","Flat","Detached","Semi","Terrace"))
house.type

income <-factor(c("High", "High", "Low", "Low", 
                  "Low", "Medium", "Low", "Medium"), 
                levels=c("Low", "Medium", "High"))
income > "Low"
# 'levels' in 'ordered' defines a relative order 
income <-ordered(c("High", "High", "Low", "Low", 
                   "Low", "Medium", "Low", "Medium"), 
                 levels=c("Low", "Medium", "High"))
income > "Low"

sort(income)

tmp.list <- list("Lex Comber",c(2015, 2024), 
                 "Lecturer", matrix(c(6,3,1,2), c(2,2)))
tmp.list
# elements of the list can be selected
tmp.list[[4]]

employee <- list(name="Lex Comber", start.year = 2015, 
                 position="Professor")
employee

append(tmp.list, list(c(7,6,9,1)))

# lappy with different functions
lapply(tmp.list[[2]], is.numeric)
lapply(tmp.list, length)

employee <- list(name="Lex Comber", start.year = 2015, 
                 position="Professor")

class(employee) <- "staff"

print.staff <- function(x) {
  cat("Name: ",x$name,"\n") 
  cat("Start Year: ",x$start.year,"\n") 
  cat("Job Title: ",x$position,"\n")
}
# an example of the print class 
print(employee)

print(unclass(employee))

new.staff <- function(name,year,post) {
  result <- list(name=name, start.year=year, position=post)
  class(result) <- "staff"
  return(result)}

leeds.uni <- vector(mode='list',3)
# assign values to elements in the list
leeds.uni[[1]] <- new.staff("Malleson, Nick", 2016,"Professor")
leeds.uni[[2]] <- new.staff("Comber, Lex", 2015,"Professor")
leeds.uni[[3]] <- new.staff("Langlands, Alan", 2014,"VC")

leeds.uni

df <- data.frame(dist = seq(0,400, 100),  
                 city = c("Leeds", "Nottingham", "Leicester", "Durham", "Newcastle"))
str(df)

# package installation checks
if (!is.element("tidyverse", installed.packages()))
    install.packages("tidyverse", dep = T)
library(tidyverse)
tb <- tibble(dist = seq(0,400, 100),  
             city = c("Leeds", "Nottingham", "Leicester", "Durham", "Newcastle"))

df
tb

df$ci
tb$ci

# 1 column
df[,2]
tb[,2]
class(df[,2])
class(tb[,2])
# 2 columns
df[,1:2]
tb[,1:2]
class(df[,1:2])
class(tb[,1:2])

data.frame(tb) 
as_tibble(df) 

names()
colnames()
rownames()
length() 
ncol()
nrow() 

cbind(df, Pop = c(700,250,230,150,1200))
cbind(tb, Pop = c(700,250,230,150,1200))

vignette("tibble")

access <- factor(c("very good","good","very good","average",
                   "bad","very good","average","bad",
                   "very good","very good","average","bad","bad"),
                 levels=c("very good","good","average","bad","very bad"))

# access[4] <- "poor"
# access

access <- factor(c("very good","good","very good","average",
                   "bad","very good","average","bad",
                   "very good","very good","average","bad","bad"),
                 levels=c("very good","good","average","bad","very bad"))
table(access)

access2 <- c("very good","good","average","bad",
             "very good","average","bad","very good",
             "very good","average","bad")
table(access2)

house.type <- factor(c("owned","owned","rented","owned",
                      "other","rented","other","owned", 
                      "rented","owned","owned","owned","rented"),
                     levels=c("owned","rented","other"))


table(house.type, access)

crosstab <- table(house.type, access)

bedrooms <- ordered(c("1 bedroom","2 bedrooms","1 bedroom","2 bedrooms",
                      "3+ bedrooms","2 bedrooms","3+ bedrooms","1 bedroom",
                      "2 bedrooms","1 bedroom", "1 bedroom","2 bedrooms","2 bedrooms"),
                    levels=c("1 bedroom","2 bedrooms","3+ bedrooms"))

bedrooms > "1 bedroom"

dim(crosstab)       # Matrix dimensions
rowSums(crosstab)   # Row sums
colnames(crosstab)  # Column names

apply(crosstab, 1, max)

apply(crosstab,2,max)

example <- c(1.4,2.6,1.1,1.5,1.2)

# created a data frame
df <- data.frame(dist = seq(0,400, 100),  
                 city = c("Leeds", "Nottingham", "Leicester", "Durham", "Newcastle"))
tmp <- data.frame(ID = a, Place = cities, Dist = hospital.distances)
# used cbind to add a column
cbind(df, Pop = c(700,250,230,150,1200))
cbind(tb, Pop = c(700,250,230,150,1200))

# create data frame
df <-  data.frame(ID = 1:length(access), 
                  access, bedrooms, house.type)
# have a look
df

if (!is.element(c("cols4all"), installed.packages()))
    install.packages("cols4all", dep = T)
library(cols4all)

if (!is.element("devtools", installed.packages()))
    install.packages("devtools", dep = T)
devtools::install_github("lexcomber/Intro2R4SpatAnal") 

library(Intro2R4SpatAnal)

x1 <- rnorm(100)
y1 <- rnorm(100)
plot(x1,y1)

plot(x1, y1, pch=16, col='red')

x2 <- seq(0,2*pi,len=100)
y2 <- sin(x2)

plot(x2, y2, type='l')
plot(x2, y2, type='l', lwd=3, col='darkgreen') 

plot(x2, y2, type='l', col='darkgreen', lwd=3, ylim=c(-1.2,1.2))
y2r <- y2 + rnorm(100,0,0.1)
points(x2,y2r, pch=16, col='darkred')

y4 <- cos(x2)
plot(x2, y2, type='l', lwd=3, col='darkgreen')
lines(x2, y4, lwd=3, lty=2, col='darkblue')

x2 <- seq(0,2*pi,len=100)
y2 <- sin(x2)
y4 <- cos(x2)
# specify the plot layout and order
par(mfrow = c(1,2))
# plot #1
plot(y2, y4)
polygon(y2, y4, col='lightgreen')
# plot #2 
plot(y2, y4, asp=1, type='n') 
polygon(y2, y4,col='lightgreen')
# reset par
par(mfrow = c(1,1))

# load data from Intro2R4SpatAnal
data(lads.polys)
# select the 22nd element 
leeds <- lads.polys[[22]]
head(leeds)
# set the plot extent
plot(leeds, asp=1, type='n', xlab="Easting", ylab="Northing")
# plot the selected features with hatching
polygon(leeds, density=14, angle=135) 

colours()

plot(leeds, asp=1, type='n', xlab="Easting", ylab="Northing")
polygon(leeds, col=rgb(0,0.5,0.7))

plot(leeds, asp=1, type='n', xlab="Easting", ylab="Northing")
polygon(leeds, col=rgb(0,0.5,0.7,0.4))

# set the plot extent
plot(leeds, asp=1, type='n', xlab="Easting", ylab="Northing")
# plot random points
set.seed(123)
points(x = runif(500,413216,446877), 
       y = runif(500, 422593,450175), pch=16, col='red') 
# plot the polygon with a transparency factor
polygon(leeds, col=rgb(0,0.5,0.7,0.4))

plot(leeds, asp=1, type='n', xlab="Easting", ylab="Northing")
polygon(leeds, col="#B3B333")
# add text, specifying its placement, colour and size 
text(431952,436635,"Leeds",cex=1.5) 
text(431952,432598,"in Yorkshire",col='darkred')

plot(x = c(-1.5,1.5), y = c(-1.5,1.5), asp=1, type='n') 
# plot the green/blue rectangle
rect(-0.5,-0.5,0.5,0.5, border=NA, col=rgb(0,0.5,0.5,0.7)) 
# then the second one
rect(0,0,1,1, col=rgb(1,0.5,0.5,0.7))

# load some gridded data 
data(pm10.grid)
# convert to a matrix 
mat <- xtabs(pm102021g ~ X + Y, pm10.grid)
# set some plot parameters (1 row, 2 columns)
par(mfrow = c(1,2))
# plot the points using the default shading
image(mat, asp = 1)
# select and examine a colour palette with 7 classes
greenpal <- c4a("brewer.greens", 11)
# and now use this to plot the data
image(mat, col=greenpal, asp = 1)
# reset par
par(mfrow = c(1,1))

library(tidyverse)

if (!is.element("cowplot", installed.packages()))
    install.packages("cowplot", dep = T)
if (!is.element("sf", installed.packages()))
    install.packages("sf", dep = T)
library(cowplot)
library(sf)

df <- data.frame(x2, y2r)
ggplot(data = df, aes(x = x2, y = y2r)) +
  geom_point(col='darkred') +
  geom_smooth(col="darkgreen", lwd = 1.5, se = FALSE) 

# theme_bw()

# theme_dark()

leeds <- leeds |> data.frame()

# create a data.frame to hold the points 
set.seed(123)
df <- data.frame(x = runif(500,413216,446877), 
                 y = runif(500, 422593,450175))
# now use ggplot to construct the layers of the plot
p1 <- ggplot(data = leeds, aes(x = X, y= Y)) +
  geom_polygon(fill = rgb(0,0.5,0.7), alpha = 0.4) +
  geom_point(data = df, aes(x = x, y = y),col='red') +
  coord_fixed() 
# plot the plot
p1

# ggplot(data = <data frame>, aes(x,y,colour)) + geom_XYZ()

# load leeds LSOA data
data(leeds_lsoa)
# extract the data frame and convert to tibble
tb <- leeds_lsoa |> st_drop_geometry() |>  as_tibble()

tb

tb$Unemployed <- as.factor((tb$unemp < median(tb$unemp)) + 0)
levels(tb$Unemployed) <- list("Unemployed" = 0, "Not Unemployed"=1)

tb$DegClass <- rep("Average", nrow(tb))
tb$DegClass[tb$degree >= 43.00] = "More Educated"
tb$DegClass[tb$degree <= 24.35] = "Less Educated"
tb$DegClass = factor(tb$DegClass, 
                     levels = c("Less Educated", "Average", "More Educated"))

table(tb$DegClass)

ggplot(data = tb, mapping=aes(x=degree, y=manual)) + geom_point()

ggplot(data = tb, mapping=aes(x=degree, y=manual, colour = Unemployed)) + geom_point()

ggplot(data = tb, mapping=aes(x=degree, y=manual)) + 
  geom_point() + 
  geom_smooth(method = "lm")

ggplot(data = tb, mapping = aes(x=degree, y=manual)) + 
  geom_point() +
  geom_smooth(method = "lm", col = "red", fill = "lightsalmon") + 
  theme_bw() +
  xlab("% with bachelor degree") + 
  ylab("% in manual occupations") +
  ggtitle("A scatter plot")

ggplot(tb, aes(x=degree)) + 
  geom_histogram(, binwidth = 5, colour = "red", fill = "grey")

ggplot(tb, aes(x=degree)) + 
  geom_histogram(binwidth=5,colour="white", fill="darksalmon") +
  # Ignore NA values 
  geom_vline(aes(xintercept=median(degree, na.rm=T)),
             color="orangered1", linetype="dashed", size=1)

ggplot(tb, aes(x=manual, y = unemp)) +
  geom_point(color="grey30") +
  facet_wrap(~DegClass) +
  xlab("% Manual Occupations") + 
  ylab("% Unemployed")

ggplot(tb, aes(x = degree)) + 
  geom_boxplot() 

ggplot(tb, aes(DegClass, degree, fill = factor(Unemployed))) + 
  geom_boxplot() +
  scale_fill_manual(name = "",
                    values = c("orange", "firebrick3"),
                    labels = c("Non-Rural"="Not Rural","Rural"="Rural")) +
  xlab("Degree Class") +
  ylab("% Degree")

# display the first six rows
head(leeds)
# display the variable dimensions 
dim(leeds)

write.csv(leeds, file = "test.csv")

write.csv(leeds, file = "test.csv", row.names = F)

tmp.leeds <- read.csv(file = "test.csv")

# this will save everything in the workspace
save(list = ls(), file = "MyData.RData")
# this will save just leeds 
save(list = "leeds", file = "MyData.RData")
# this will save leeds and lads.polys
save(list = c("leeds", "lads.polys"), file = "MyData.RData")

load("MyData.RData")

st_write(leeds, "leeds.gpkg")     # GeoPackage
st_write(leeds, "leeds.shp")      # ESRI shapefile
st_write(leeds, "leeds.geojson")  # GeoJson

st_write(leeds, "leeds.gpkg", delete_layer = TRUE)     # GeoPackage

new.leeds <- st_read("leeds.shp")

vignette(topic = "sf2", package = "sf")

# Self-Test Question 1
access[4] <- "poor"
access

# Self-Test Question 4
# Using the access, house.type and bedrooms variables, write expressions to give the following:
# The access of all residents with more than 1 bedroom
# The counts of house types of all residents with 1 or 2 bedrooms
# The counts of access for rented with 3 or more bedrooms.

# Undo the access[4] <- "poor" line used above
access <- factor(c("very good","good","very good","average",
                   "bad","very good","average","bad",
                   "very good","very good","average","bad","bad"),
                 levels=c("very good","good","average","bad","very bad"))

house.type <- factor(c("owned","owned","rented","owned",
                       "other","rented","other","owned", 
                       "rented","owned","owned","owned","rented"),
                     levels=c("owned","rented","other"))

bedrooms <- ordered(c("1 bedroom","2 bedrooms","1 bedroom","2 bedrooms",
                      "3+ bedrooms","2 bedrooms","3+ bedrooms","1 bedroom",
                      "2 bedrooms","1 bedroom", "1 bedroom","2 bedrooms","2 bedrooms"),
                    levels=c("1 bedroom","2 bedrooms","3+ bedrooms"))

# 1 The access of all residents with more than 1 bedroom
access[bedrooms > "1 bedroom"] 
# 2 The counts of house types of all residents with 1 or 2 bedrooms
table(house.type[bedrooms < "3+ bedrooms"]) 
# 3 The counts of access for rented with 3 or more bedrooms.
table(access[(bedrooms < "3+ bedrooms") & (house.type == "rented")]) 

# Self-Test Question 6
apply(crosstab,1,which.max)

# Self-Test Question 7
# create data frame
df <-  data.frame(ID = 1:length(access), 
                  access, bedrooms, house.type)

df[, 3]

df[5, ]

df[c(3, 5), ]

df[df$ID > 10, ]

# perhaps easier to create an index object
index <- (df$access == "bad" | df$access == "very bad")
df[index, c(1,3)]

head(leeds)
tail(leeds)

ggplot(data = leeds, aes(x = X, y = Y)) + 
  geom_polygon() +
  # specify data and aesthetics for points
  geom_point(data = df, aes(x, y),col='red')
