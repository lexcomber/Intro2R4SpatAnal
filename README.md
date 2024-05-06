## Data Package for 'An Introduction to R for Spatial Analysis and Mapping' 3e (Chris Brunsdon & Lex Comber)

We have assembled a number of datasets to illustrate different analysis and data manipulation approaches. These are contained in a package called `Intro2R4SpatAnal` that can be downloaded and installed from GitHub, Details of how to do this are included in Chapter 2 and in Chapter 3. This package contains 15 datasets assembled in different ways. The data are in different formats, including data tables in `data.frame` format (`ewhp`, `lsoa_attribs`, `places`	and `pm10.grid`), `sf` format  spatial point layers (`crimes`, `docs`, `pm10` and `bk_pm10_ave`), a line layer (`roads`) and polygon layers (`leeds_msoa`, `leeds_lsoa`, `bk_prov`, `columbus` and `nuts2_unemp_de`) and another in list form (`lads.polys`). 

Most of the dataset were constructed programatically using R as detailed below. The `leeds_lsoa` and `leeds_msoa` containing socio-economic data from the 2021 UK population census over different reporting units for Leeds, UK and were extracted using the `nomisr` package. The `crimes` point spatial data is of crimes reported in March 2021, the same month as the UK population census, and was extracted from the UK police data portal (https://data.police.uk). Similarly the `nuts2_unemp_de` of unemployment in high level administrative areas in Germany was extracted from the Eurostat API using the `eurostat` and `giscoR` packages. Finally, two datasets were extracted from OpenStreetMap in April 2024 (`docs` and `roads`) and capture the features present in the OpenStreetMap at that time. Other datasets were extracted automatically but not by interrogating a formal interface to a data portal and so are potentially less stable and subject to changes in URL addresses etc. This includes the `pm10` and `pm10.grid` datasets of mean PM10 levels over a 1 km grid in Leeds but in different formats, and the `bk_pm10_ave` of mean PM10 from sensors in and around Bangkok, collected  on  24 April 2024. Some datasets were extracted from other packages: the `ewhp` data is from `GWmodel` and `columbus` is from the `spData` package. This site includes the code used to create some of the datasets (UK census, UK PM10, OSM data), and Chapter 10 of the book illustrates how to create of many of the other datasets, using interfaces to data portals.

```{r}
#### Part 1: UK Census Data 
# based on https://bookdown.org/lexcomber/GEOG3195/getting-data-from-apis.html#the-nomis-api
library(sf)
library(tmap)
library(tidyverse) 
library(nomisr)

# 1. Initial investigations 
# Get list of Topic Summaries to exlore 
x = nomis_search(name = "TS*")
# example search for age
x |> filter(str_detect(name.value, 'Age')) |> select(id, name.value)

# Check for data availability / formats
nomis_get_metadata(id = "NM_2020_1", 
                   concept = "GEOGRAPHY", 
                   type = "type") |> 
                   print(n = "all")
# TYPE151 for LSOAs, TYPE152 for MSOAs

# 2. Get LSOAs
# Get list of all LSOA codes
y = nomis_get_metadata(id = "NM_2020_1", concept = "GEOGRAPHY", type = "TYPE151")
# Create an indices to filter for Leeds and Bradford areas 
leeds.index <- 
    y |> 
    filter(str_detect(label.en, 'Leeds')) |> 
    select(id) |> 
    unlist() |> 
    as.vector() 
brad.index <- 
    y |> 
	filter(str_detect(label.en, 'Bradford') | str_detect(label.en, 'Leeds')) |> 
    select(id) |> 
    unlist() |> 
    as.vector() 

# 3. Define a function to get the data 
extractCensus = function(id = "NM_2072_1", perc = TRUE, index, data_type = "TYPE151") {
    d <- nomis_get_data(id = id, 
                        time = c("latest"), 
                        geography = index)
    # % or values?
    if(perc) val = "Percent" 
    if(!perc) val = "Value"                   
    # variable names
    n = names(d)
    data <- 
        d |> 
        #select(GEOGRAPHY_CODE, C2021_AGE_19_NAME, MEASURES_NAME, OBS_VALUE) |>
        select(GEOGRAPHY_CODE, n[14], MEASURES_NAME, OBS_VALUE) |>
        filter(MEASURES_NAME == val) |> 
        pivot_wider(id_cols = GEOGRAPHY_CODE, 
            values_from = OBS_VALUE, 
            names_from = n[14]) 
    return(data)
}

# 4. Extract data
carvan = extractCensus(id = "NM_2063_1", perc = TRUE, index, data_type = "TYPE151") 
occupancy = extractCensus(id = "NM_2071_1", perc = TRUE, index, data_type = "TYPE151") 
tenure = extractCensus(id = "NM_2072_1", perc = TRUE, index, data_type = "TYPE151") 
econact = extractCensus(id = "NM_2083_1", perc = TRUE, index, data_type = "TYPE151") 
age = extractCensus(id = "NM_2020_1", perc = TRUE, index, data_type = "TYPE151") 
health = extractCensus(id = "NM_2055_1", perc = TRUE, index, data_type = "TYPE151") 
qual = extractCensus(id = "NM_2084_1", perc = TRUE, index, data_type = "TYPE151") 
occup = extractCensus(id = "NM_2080_1", perc = TRUE, index, data_type = "TYPE151") 
ethn = extractCensus(id = "NM_2041_1", perc = TRUE, index, data_type = "TYPE151") 
relig = extractCensus(id = "NM_2049_1", perc = TRUE, index, data_type = "TYPE151") 
mig = extractCensus(id = "NM_2039_1", perc = TRUE, index, data_type = "TYPE151") 
pop = extractCensus(id = "NM_2020_1", perc = FALSE, index, data_type = "TYPE151") 

# 5. Assemble LSOA data table
# download LSOA data from https://geoportal.statistics.gov.uk/datasets/bb427d36197443959de8a1462c8f1c55_0/
leeds <- st_read("lsoa.gpkg", quiet = T) 
# filter for Leeds & Leeds and Bradford
leeds_brad <- 
	leeds |> filter(str_detect(LSOA21NM, 'Bradford') | str_detect(LSOA21NM, 'Leeds')) |> 
	rename(code = LSOA21CD) |>
	select(code, LSOA21NM)
leeds <- leeds |> filter(str_detect(LSOA21NM, 'Leeds')) |> 
	rename(code = LSOA21CD) |>
	select(code, LSOA21NM)
# join the data, mutating as needed
lsoa_data <- 
	leeds |> 
	left_join(
		pop |> 
			mutate(totpop = Total) |>
			select(GEOGRAPHY_CODE, totpop),
		by = c("code" = "GEOGRAPHY_CODE")) |> 		
	left_join(
		age |> 
			mutate(o65 = `Aged 65 to 69 years`+`Aged 70 to 74 years`+`Aged 75 to 79 years`+
							`Aged 80 to 84 years`+`Aged 85 years and over`,
					u20 = `Aged 4 years and under`+`Aged 5 to 9 years`+
							`Aged 10 to 14 years`+`Aged 15 to 19 years`) |> 
			select(GEOGRAPHY_CODE, u20, o65),
		by = c("code" = "GEOGRAPHY_CODE")) |> 
	left_join(
		health |>
		 	mutate(badhealth = `Bad health` + `Very bad health`) |>
		 	select(GEOGRAPHY_CODE, badhealth),
		by = c("code" = "GEOGRAPHY_CODE")) |>
	left_join(
		carvan |> 
			rename(nocar = "No cars or vans in household") |> 
        	select(GEOGRAPHY_CODE, nocar),
		by = c("code" = "GEOGRAPHY_CODE")) |>
    left_join(
        occupancy |> mutate(overcrowd = `Occupancy rating of rooms: -1` + 
            `Occupancy rating of rooms: -2 or less`) |>
        	select(GEOGRAPHY_CODE, overcrowd),
		by = c("code" = "GEOGRAPHY_CODE")) |>
	left_join(
		# see https://www.ons.gov.uk/census/census2021dictionary/variablesbytopic/educationvariablescensus2021/highestlevelofqualification
		qual |> 
			mutate(degree = `Level 4 qualifications or above`) |>
			select(GEOGRAPHY_CODE, degree),
		by = c("code" = "GEOGRAPHY_CODE")) |>
	left_join( 
		# see https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/bulletins/industryandoccupationenglandandwales/census2021#occupation
		occup |>
			mutate(manual = `5. Skilled trades occupations` + `8. Process, plant and machine operatives` +
				`9. Elementary occupations`, 
					carework = `6. Caring, leisure and other service occupations`) |>
			select(GEOGRAPHY_CODE, manual, carework),
		by = c("code" = "GEOGRAPHY_CODE")) |>
	left_join( 
		tenure |> 
			mutate(owned = Owned) |>
			select(GEOGRAPHY_CODE, owned),
		by = c("code" = "GEOGRAPHY_CODE")) |>		
	left_join( 
		econact |> 
			mutate(unemp = `Economically active (excluding full-time students): Unemployed`, 
					students = `Economically active and a full-time student` + `Economically inactive: Student`) |>
			select(GEOGRAPHY_CODE, unemp, students),
		by = c("code" = "GEOGRAPHY_CODE")) |>
	left_join( 
		# check names(ethn); summary(rowSums(ethn[, c(3, 9, 13, 18, 24)]))
		ethn |>
			mutate(#bangladeshi = `Asian, Asian British or Asian Welsh: Bangladeshi`,
					#chinese = `Asian, Asian British or Asian Welsh: Chinese`, 
					indian = `Asian, Asian British or Asian Welsh: Indian`, 
					pakistani = `Asian, Asian British or Asian Welsh: Pakistani`,
					african = `Black, Black British, Black Welsh, Caribbean or African: African`,
					caribbean = `Black, Black British, Black Welsh, Caribbean or African: Caribbean`,
					meg = `Mixed or Multiple ethnic groups`,
					white = White) |>
			select(GEOGRAPHY_CODE, indian, pakistani, african, caribbean, meg, white),
		by = c("code" = "GEOGRAPHY_CODE")) |>
	left_join(
		relig |> 
			mutate(christian = Christian, hindu = Hindu, 
				jewish = Jewish, muslim = Muslim, sikh = Sikh) |>
			select(GEOGRAPHY_CODE, christian, hindu, jewish, muslim , sikh),
		by = c("code" = "GEOGRAPHY_CODE")) |>
	left_join(
		mig |> 
			mutate(incomer1yr = `Migrant from within the UK: Address one year ago was in the UK` +
			`Migrant from outside the UK: Address one year ago was outside the UK`) |>
			select(GEOGRAPHY_CODE, incomer1yr),
		by = c("code" = "GEOGRAPHY_CODE"))  
# 6. Write out
st_write(lsoa_data, "leeds_lsoa.gpkg", delete_layer = T)

# 7. Additional LSOA data for table joins

x |> filter(str_detect(name.value, 'Disability')) |> select(id, name.value)
# Extract data
disab = extractCensus(id = "NM_2056_1", perc = TRUE, index2, data_type = "TYPE151") 
names(disab)

x |> filter(str_detect(name.value, 'Country')) |> select(id, name.value)
# Extract data
bornc = extractCensus(id = "NM_2024_1", perc = TRUE, index2, data_type = "TYPE151") 
head(bornc)

x |> filter(str_detect(name.value, 'Distance')) |> select(id, name.value)
# Extract data
distw = extractCensus(id = "NM_2075_1", perc = TRUE, index2, data_type = "TYPE151") 
names(distw)

x |> filter(str_detect(name.value, 'Method')) |> select(id, name.value)
# Extract data
comute = extractCensus(id = "NM_2078_1", perc = TRUE, index2, data_type = "TYPE151") 
names(comute)

lsoa_attribs <- 
	leeds_b |> 
	left_join(
		disab |>
			transmute(code = GEOGRAPHY_CODE, disabled = `Disabled under the Equality Act` + 
			`Disabled under the Equality Act: Day-to-day activities limited a lot` + 
			`Disabled under the Equality Act: Day-to-day activities limited a little`), 
		by = c("code")) |>
	left_join(
		bornc |>
			transmute(code = GEOGRAPHY_CODE, bornuk = `Europe: United Kingdom`), 
		by = c("code")) |>
	left_join(
		comute |>
			transmute(code = GEOGRAPHY_CODE, metro = `Underground, metro, light rail, tram`,
				train = Train, bus = `Bus, minibus or coach`, 
				car = `Driving a car or van` + `Passenger in a car or van` + Taxi,
				bike = Bicycle, mbike  = `Motorcycle, scooter or moped`, walk = `On foot`), 
		by = c("code")) |>
	select(-LSOA21NM) |>
	st_drop_geometry() |>
	sample_frac(size=1) #|> head()

head(lsoa_attribs)		  
dim(lsoa_attribs)		  
save(lsoa_attribs, file = "lsoa_attribs.RData")
		
# 8. Do the same for MSOAs
# get list of all MSOA codes
y = nomis_get_metadata(id = "NM_2020_1", concept = "GEOGRAPHY", type = "TYPE152")
# create an index to filter for leeds area
index <- 
    y |> 
    filter(str_detect(label.en, 'Leeds')) |> 
    select(id) |> 
    unlist() |> 
    as.vector() 
# Extract data
carvan = extractCensus(id = "NM_2063_1", perc = TRUE, index, data_type = "TYPE152") 
occupancy = extractCensus(id = "NM_2071_1", perc = TRUE, index, data_type = "TYPE152") 
tenure = extractCensus(id = "NM_2072_1", perc = TRUE, index, data_type = "TYPE152") 
econact = extractCensus(id = "NM_2083_1", perc = TRUE, index, data_type = "TYPE152") 
age = extractCensus(id = "NM_2020_1", perc = TRUE, index, data_type = "TYPE152") 
health = extractCensus(id = "NM_2055_1", perc = TRUE, index, data_type = "TYPE152") 
qual = extractCensus(id = "NM_2084_1", perc = TRUE, index, data_type = "TYPE152") 
occup = extractCensus(id = "NM_2080_1", perc = TRUE, index, data_type = "TYPE152") 
ethn = extractCensus(id = "NM_2041_1", perc = TRUE, index, data_type = "TYPE152") 
relig = extractCensus(id = "NM_2049_1", perc = TRUE, index, data_type = "TYPE152") 
mig = extractCensus(id = "NM_2039_1", perc = TRUE, index, data_type = "TYPE152") 
pop = extractCensus(id = "NM_2020_1", perc = FALSE, index, data_type = "TYPE151") 
# Assemble MSOA data table
# download MSOA data from https://geoportal.statistics.gov.uk/datasets/14f2b48cf3704951a40da5218984b98c_0
leeds <- st_read("msoa.gpkg", quiet = T) 
# filter for Leeds
leeds <- leeds |> filter(str_detect(geo_label, 'Leeds')) |> 
	rename(code = geo_code, MSOA21NM = geo_label) |>
	select(code, MSOA21NM)
# join the data, mutating as needed
msoa_data <- 
	leeds |> 
	left_join(
		pop |> 
			mutate(totpop = Total) |>
			select(GEOGRAPHY_CODE, totpop),
		by = c("code" = "GEOGRAPHY_CODE")) |> 		
	left_join(
		age |> 
			mutate(o65 = `Aged 65 to 69 years`+`Aged 70 to 74 years`+`Aged 75 to 79 years`+
							`Aged 80 to 84 years`+`Aged 85 years and over`,
					u20 = `Aged 4 years and under`+`Aged 5 to 9 years`+
							`Aged 10 to 14 years`+`Aged 15 to 19 years`) |> 
			select(GEOGRAPHY_CODE, u20, o65),
		by = c("code" = "GEOGRAPHY_CODE")) |> 
	left_join(
		health |>
		 	mutate(badhealth = `Bad health` + `Very bad health`) |>
		 	select(GEOGRAPHY_CODE, badhealth),
		by = c("code" = "GEOGRAPHY_CODE")) |>
	left_join(
		carvan |> 
			rename(nocar = "No cars or vans in household") |> 
        	select(GEOGRAPHY_CODE, nocar),
		by = c("code" = "GEOGRAPHY_CODE")) |>
    left_join(
        occupancy |> mutate(overcrowd = `Occupancy rating of rooms: -1` + 
            `Occupancy rating of rooms: -2 or less`) |>
        	select(GEOGRAPHY_CODE, overcrowd),
		by = c("code" = "GEOGRAPHY_CODE")) |>
	left_join(
		# see https://www.ons.gov.uk/census/census2021dictionary/variablesbytopic/educationvariablescensus2021/highestlevelofqualification
		qual |> 
			mutate(degree = `Level 4 qualifications or above`) |>
			select(GEOGRAPHY_CODE, degree),
		by = c("code" = "GEOGRAPHY_CODE")) |>
	left_join( 
		# see https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/bulletins/industryandoccupationenglandandwales/census2021#occupation
		occup |>
			mutate(manual = `5. Skilled trades occupations` + `8. Process, plant and machine operatives` +
				`9. Elementary occupations`, 
					carework = `6. Caring, leisure and other service occupations`) |>
			select(GEOGRAPHY_CODE, manual, carework),
		by = c("code" = "GEOGRAPHY_CODE")) |>
	left_join( 
		tenure |> 
			mutate(owned = Owned) |>
			select(GEOGRAPHY_CODE, owned),
		by = c("code" = "GEOGRAPHY_CODE")) |>		
	left_join( 
		econact |> 
			mutate(unemp = `Economically active (excluding full-time students): Unemployed`, 
					students = `Economically active and a full-time student` + `Economically inactive: Student`) |>
			select(GEOGRAPHY_CODE, unemp, students),
		by = c("code" = "GEOGRAPHY_CODE")) |>
	left_join( 
		# check names(ethn); summary(rowSums(ethn[, c(3, 9, 13, 18, 24)]))
		ethn |>
			mutate(#bangladeshi = `Asian, Asian British or Asian Welsh: Bangladeshi`,
					#chinese = `Asian, Asian British or Asian Welsh: Chinese`, 
					indian = `Asian, Asian British or Asian Welsh: Indian`, 
					pakistani = `Asian, Asian British or Asian Welsh: Pakistani`,
					african = `Black, Black British, Black Welsh, Caribbean or African: African`,
					caribbean = `Black, Black British, Black Welsh, Caribbean or African: Caribbean`,
					meg = `Mixed or Multiple ethnic groups`,
					white = White) |>
			select(GEOGRAPHY_CODE, indian, pakistani, african, caribbean, meg, white),
		by = c("code" = "GEOGRAPHY_CODE")) |>
	left_join(
		relig |> 
			mutate(christian = Christian, hindu = Hindu, 
				jewish = Jewish, muslim = Muslim, sikh = Sikh) |>
			select(GEOGRAPHY_CODE, christian, hindu, jewish, muslim , sikh),
		by = c("code" = "GEOGRAPHY_CODE")) |>
	left_join(
		mig |> 
			mutate(incomer1yr = `Migrant from within the UK: Address one year ago was in the UK` +
			`Migrant from outside the UK: Address one year ago was outside the UK`) |>
			select(GEOGRAPHY_CODE, incomer1yr),
		by = c("code" = "GEOGRAPHY_CODE"))  
# Write out
st_write(msoa_data, "leeds_msoa.gpkg", delete_layer = TRUE)

#### PART 2. CRIME POINT DATA from https://data.police.uk
library(httr)
library(jsonlite)
library(sf)
library(tmap)
library(tidyverse) 

# 1, Define Functions
# Get location
getLonLat <- function(x) {
  df = data.frame(lon = as.numeric(x$location.longitude),
                  lat = as.numeric(x$location.latitude))
  # return the dataframe
  return(df)
}
# Get attributes
getAttr <- function(x) {
  df = data.frame(
    #id = x$id,
    category = x$category,
    street_name = x$location.street.name,
    location_type = x$location_type,
    month = substr(x$month, 6,9),
    year = substr(x$month, 1,4))
  # return the data.frame
  return(df)
}
# Join together and make a spatial (sf) object
makeSpatial = function(crimes.loc, crimes.attr){
  # create a data frame
  df = data.frame(longitude = crimes.loc[,1], 
                latitude = crimes.loc[,2],
                crimes.attr)
  # convert to sf
  df_sf = st_as_sf(df, coords = c("longitude", "latitude"), 
                 crs = 4326, agr = "constant")
  # return the sf
  return(df_sf)
}
# Extract coordinates from a single polygon
get_poly_coords = function(x){
  # transform to lat lon
  x = st_transform(x, 4326)
  # extract coordinates
  coords = data.frame(st_coordinates(x)[, c("X", "Y")])
  poly_paste <- paste(paste(coords$Y, coords$X, sep = ","), collapse = ":")
  return(poly_paste)
}
# Get bounding box
bbox_coords = function(x){
	bb = st_bbox(st_transform(x, 4326))
	my.df <- 
		data.frame(
  			ID = rep(1, 5), 
  			X = c(bb[1], bb[3], bb[3], bb[1], bb[1]),
  			Y = c(bb[2], bb[2], bb[4], bb[4], bb[2])) 
  	my.poly <- sfheaders::sf_polygon(obj = my.df, x = "X", y = "Y", polygon_id = "ID")
  	my.poly = st_set_crs(my.poly, 4326)
  	get_poly_coords(my.poly)
}

# 2. Assemble data
# define some results tables
crimes.loc.tab = vector()
crimes.attr.tab = vector()
# start the 1st loop: "for each grid cell do..."
for(i in 1:nrow(msoa_data)){
  	coords = get_poly_coords(msoa_data[i,])
  	# pass to the API
    url.i=paste0("https://data.police.uk/api/crimes-street/all-crime?poly=", 
           coords,
           "&date=2021-03")
    x.i = GET(url.i)  
    if(x.i$status_code < 500) {
	    crimes.i <- as_tibble(
	      	fromJSON(httr::content(x.i, as = "text", encoding = "utf8"), 
	               flatten = T)
	    )
    	crimes.loc.tab <- rbind(crimes.loc.tab, getLonLat(crimes.i))
    	crimes.attr.tab <- rbind(crimes.attr.tab, getAttr(crimes.i))
    		rm(crimes.i)
    }
    # end the second loop
  	# print out a little indicator of progress
  	cat("grid cell", i, "done \t")
}
crimes_sf <- makeSpatial(crimes.loc.tab, crimes.attr.tab)
# check
plot(st_geometry(crimes_sf))
# export
st_write(crimes_sf, "crimes.gpkg", delete_layer = TRUE)

#### PART 3. OSM DATA
library(osmdata)
# make sure the internet is available
curl::has_internet()
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))
# 1. Roads
# extract for Leeds area
osm_sf <- opq ("Leeds, UK") %>%
            add_osm_feature ("highway") %>%
            osmdata_sf 
streets <- osm_sf$osm_lines[, c("name", "highway", "ref")] 
roads <- 
	streets |> 
	filter(highway %in% c("primary", "primary_link" , "secondary", "secondary_link", "tertiary", 
							"tertiary_link", "trunk", "trunk_link", "motorway", "motorway_link"))
roads$highway <- recode(roads$highway, primary_link = "primary", secondary_link = "secondary", 
	tertiary_link = "tertiary", trunk_link = "trunk", "motorway_link" = "motorway") 
							
# plot 
tmap_mode("view")
tm_shape(roads)+tm_lines() +  
	tm_basemap("OpenStreetMap")
# clean and export
roads <- roads |> mutate(length = st_length(roads)) |> relocate(name, highway, ref, length) 
rownames(roads) = 1:nrow(roads)
st_write(roads, "roads.gpkg", delete_layer = TRUE)

# 2. Doctors
# make sure the internet is available
curl::has_internet()
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))
# extract for Leeds area
osm_sf <- opq ("Leeds, UK") |>
            add_osm_feature (key = "amenity", value = "doctors") |>
            osmdata_sf()
docs <- osm_sf$osm_points |> st_transform(27700)
docs <- docs |>dplyr:: select(name)
save(docs, file = "docs.RData")

#### Part 4. PM10 DATA
# for mean 2021 from Defra: https://uk-air.defra.gov.uk/data/pcm-data
pm10 <- read.csv("https://uk-air.defra.gov.uk/datastore/pcm/mappm102021g.csv", skip = 5)
head(pm10)
pm10 <- 
	pm10 |> 
	st_as_sf(coords = c("x", "y"), crs = 27700) 
pm10 <- pm10[msoa_data, ]
rownames(pm10) = 1:nrow(pm10)
pm10<- mutate(pm10, pm102021g =as.numeric(pm102021g))
st_write(pm10, "pm10_leeds.gpkg", delete_layer = TRUE)
# convert to data.frame
coords <- pm10 |> st_coordinates()
pm10.grid <- cbind(coords, pm10 |> st_drop_geometry() )
head(pm10.grid)
save(pm10.grid, file = "pm10.grid.RData")

#### PART 5. LADs
# Download LAD data (low res) from https://geoportal.statistics.gov.uk/datasets/8bed6bf8e3424995bbea980dfa844132_0/explore?location=0.000000%2C-3.265344%2C0.00
lads = st_read("lads_lr.gpkg")
# index of Yorkshire LADs
index <- c(2, 3, 4, 5, 10, 11, 14, 172, 173, 174, 175, 176, 177, 178, 256, 257, 258, 259, 271, 272, 273, 274, 275)
lads <- lads[index, ]
# tmap_mode("view")
# tm_shape(lads) + tm_polygons(fill = NA) + tm_text("LAD21NM")
# tmap_mode("plot")
# create coordinate list
lads.polys <- list()
for (i in 1:nrow(lads2)) {
	lads[i,] |> st_cast("MULTIPOLYGON") |> st_coordinates() |> data.frame() |> 	select(X, Y) |> as.matrix() -> poly.i
	lads.polys[[i]] = poly.i
}
names(lads.polys) <- lads$LAD21NM
save(lads.polys, file = "lads.polys.RData") 

##### END 
```
