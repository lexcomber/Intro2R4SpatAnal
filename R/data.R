#' Spatial data of socio-economic attributes from the 2021 UK Census for Leeds, UK
#'
#' A dataset over Lower Super Output Areas (LSOAS), census reporting areas with ~1500 people, (~500 households), for the Leeds city area generated from a selection of the census Topic Summaries (\url{https://www.nomisweb.co.uk/sources/census_2021_ts})
#'
#' @format A `sf` format MULTIPOLYGON dataset with 488 observations (LSOAs) and 24 attributes
#' \describe{
#' \item{code}{the LSOA area code}
#' \item{LSOA21NM}{the LSOA neamed code}
#' \item{u20}{the % of the population under 20 years old}
#' \item{o65}{the % of the population over 65 years old}
#' \item{badhealth}{the % of the population reporting the health to be 'bad' or 'very bad'}
#' \item{nocar}{the % of households without a car or van}
#' \item{overcrowd}{the % of households with insifficient rooms for the occupants}
#' \item{degree}{the % of the population with a bachelors degree}
#' \item{manual}{the % of the population working in manual occupations (skilled trades occupations;, process, plant and machine operatives and elementary occupations)}
#' \item{carework}{the % of the population working in caring, leisure and other service occupations}
#' \item{unemp}{the % of the economicaly active population that is unemployed}
#' \item{students}{the % of the population that are students}
#' \item{indian}{the % of the population that indicated their ethnicity to be Indian in the census}
#' \item{pakistani}{the % of the population that indicated their ethnicity to be Pakistani in the census}
#' \item{african}{the % of the population that indicated their ethnicity to be African in the census}
#' \item{caribean}{the % of the population that indicated their ethnicity to be Caribean in the census}
#' \item{meg}{the % of the population that indicated their ethnicity to be multiple ethnic groups in the census}
#' \item{christian}{the % of the population that indicated their Religion to be Christian in the census}
#' \item{hindu}{the % of the population that indicated their Religion to be Hindu in the census}
#' \item{jewish}{the % of the population that indicated their Religion to be Jewish in the census}
#' \item{muslim}{the % of the population that indicated their Religion to be Muslim in the census}
#' \item{sikh}{the % of the population that indicated their Religion to be Sikh in the census}
#' \item{incomer1yr}{the % of the population that moved to the LSOA within the lasyt year (from inside or outsie the UK)}
#' \item{deprivation}{The Townsend Index of deprivation score for the LSOA (see \url{https://www.restore.ac.uk/geo-refer/36229dtuks00y19810000.php})}
#' }
#'
#' @source Created by Lex Comber
#'
#' @examples
#' data(leeds_lsoa)
"leeds_lsoa"

#' Spatial data of socio-economic attributes from the 2021 UK Census for Leeds, UK
#'
#' A dataset over Medium Super Output Areas (MSOAS), census reporting areas with ~7500 people, (~2500 households), for the Leeds city area generated from a selection of the census Topic Summaries (\url{https://www.nomisweb.co.uk/sources/census_2021_ts})
#'
#' @format A `sf` format MULTIPOLYGON dataset with 107 observations (LSOAs) and 24 attributes
#' \describe{
#' \item{code}{the LSOA area code}
#' \item{LSOA21NM}{the LSOA neamed code}
#' \item{u20}{the % of the population under 20 years old}
#' \item{o65}{the % of the population over 65 years old}
#' \item{badhealth}{the % of the population reporting the health to be 'bad' or 'very bad'}
#' \item{nocar}{the % of households without a car or van}
#' \item{overcrowd}{the % of households with insifficient rooms for the occupants}
#' \item{degree}{the % of the population with a bachelors degree}
#' \item{manual}{the % of the population working in manual occupations (skilled trades occupations;, process, plant and machine operatives and elementary occupations)}
#' \item{carework}{the % of the population working in caring, leisure and other service occupations}
#' \item{unemp}{the % of the economicaly active population that is unemployed}
#' \item{students}{the % of the population that are students}
#' \item{indian}{the % of the population that indicated their ethnicity to be Indian in the census}
#' \item{pakistani}{the % of the population that indicated their ethnicity to be Pakistani in the census}
#' \item{african}{the % of the population that indicated their ethnicity to be African in the census}
#' \item{caribean}{the % of the population that indicated their ethnicity to be Caribean in the census}
#' \item{meg}{the % of the population that indicated their ethnicity to be multiple ethnic groups in the census}
#' \item{christian}{the % of the population that indicated their Religion to be Christian in the census}
#' \item{hindu}{the % of the population that indicated their Religion to be Hindu in the census}
#' \item{jewish}{the % of the population that indicated their Religion to be Jewish in the census}
#' \item{muslim}{the % of the population that indicated their Religion to be Muslim in the census}
#' \item{sikh}{the % of the population that indicated their Religion to be Sikh in the census}
#' \item{incomer1yr}{the % of the population that moved to the LSOA within the lasyt year (from inside or outsie the UK)}
#' \item{deprivation}{The Townsend Index of deprivation score for the LSOA (see \url{https://www.restore.ac.uk/geo-refer/36229dtuks00y19810000.php})}
#' }
#'
#' @source Created by Lex Comber
#'
#' @examples
#' data(leeds_msoa)
"leeds_msoa"

#' Data of the outlines of Local Authority Districts in the UK
#'
#' A dataset containing the boundaries of the LADs in the UK (\url{https://geoportal.statistics.gov.uk/documents/d1fab2d9fb0a4576a7e08f89ac7e0b72/about})
#'
#' @format A named list object with 363 elements, named by the LAD
#' \describe{
#' \item{X}{the X coordinates of the boundary in OSGB projection (EPSG: 27700)}
#' \item{y}{the Y coordinates of the boundary in OSGB projection (EPSG: 27700)}
#' }
#'
#' @source Created by Lex Comber
#'
#' @examples
#' data(lads.polys)
"lads.polys"

#' A point dataset of the locations of street crimes in the Leeds (UK) area in March 2021
#'
#' A \texttt{sf} format POINT dataset created from data collected from the police API for the Leeds area (see \url{https://data.police.uk} and \url{https://data.police.uk/docs/method/crimes-at-location/})
#'
#' @format A `sf` format POINT object with  with 9180 observations (crimes) and 5 attributes in WGS84 projection (EPSG: 4326)
#' \describe{
#' \item{category}{the type of crime}
#' \item{street_name}{the location of the crime on the street}
#' \item{location_type}{either Force which indicates a normal police force location or BTP which indicates a British Transport Police location}
#' \item{month}{the month of the crime - all 03 (March)}
#' \item{year}{the year of the crime - all 2021}
#'
#' @source Created by Lex Comber
#'
#' @examples
#' data(crimes)
"crimes"

#' Road data for the Leeds area from OpenStreetMap
#'
#' A dataset of the main roads in and around the Leeds areas in the UK, collected from OpenStreetMap in April 2024
#'
#' @format A `sf` format LINESTRING dataset with 8691 observations and 4 fields  in WGS84 projection (EPSG: 4326)
#' \describe{
#' \item{name}{the name of the road where indicated in OSM}
#' \item{highway}{the class of road (primary, secondary, teriary and trunk)}
#' \item{ref}{the road number where indicated in OSM}
#' \item{length}{the road segment length in metres}
#' }
#'
#' @source Created by Lex Comber
#'
#' @examples
#' data(roads)
"roads"

#' A 1km gridded dataset of average annual PM10 for the Leeds area
#'
#' A point dataset of the mean PM10 levels in 2021 from \url{https://uk-air.defra.gov.uk/data/pcm-data}
#'
#' @format A `sf` format POINT dataset with 551 observations and 2 fields in OSGB projection (EPSG: 27700)
#' \describe{
#' \item{gridcode}{the Ordnance Survey 1km grid code}
#' \item{pm102021g}{the mean PM10 level in µg m^3}
#' }
#'
#' @source Created by Lex Comber
#'
#' @examples
#' data(pm10)
"pm10"

