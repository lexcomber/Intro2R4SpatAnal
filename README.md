## Data and R Scripts for 'An Introduction to R for Spatial Analysis and Mapping' 3e (Chris Brunsdon & Lex Comber)

### Data Package

The `Intro2R4SpatAnal` R package contains 17 datasets. It can be installed using the follow code via the `devtools` package and loaded in the usual way: 

```{r}
devtools::install_github("lexcomber/Intro2R4SpatAnal")
library(Intro2R4SpatAnal)
```

### R Scripts

R scripts containing the code for each chapter are provided. There are 3 options for getting them.

1. They can be downlaoded from https://github.com/lexcomber/Intro2R4SpatAnal/tree/master/scripts.
2. Alternatively the raw code for each script (`ch2.R`, `ch3.R`, etc) can be accessed via https://raw.githubusercontent.com/lexcomber/Intro2R4SpatAnal/refs/heads/master/scripts/ch2.R and changing the last part of the link for each chapter.
3. The R code below dowloads a zipfile containing the R scripts and extracts them to your local (working) directory:

```{r}
# Dropbox share link
zip_url <- "https://www.dropbox.com/scl/fi/p6dbuveohyt7zpbpm9hrv/r_scripts.zip?rlkey=4zqhbztiqjmydvigkfi6kowcm&dl=1"

# Destination file name
zip_file <- "r_scripts.zip"

# Download the zip file
download.file(zip_url, destfile = zip_file, mode = "wb")

# Unzip the contents
unzip(zip_file)
```
