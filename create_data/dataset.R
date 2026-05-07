library(terra)
library(raster)
library(readxl)
library(sf)
library(spData)
library(tidyverse)


#===============================================================================
#Spatial Work
#===============================================================================

##Austrian Municipalities 
munic24 = st_read("/Users/Franzi/Desktop/snow_voting/snow_voting/input_data/STATISTIK_AUSTRIA_GEM_20240101/STATISTIK_AUSTRIA_GEM_20240101.shp")
st_crs(munic24)

#Assigning CRS: EPSG: 3416
munic24 = st_transform (munic24, "EPSG:3416")
st_crs(munic24)

#Converting munic24 into SpatVector
munic24_vect = vect(munic24)

##Snowdepth - Geosphere 1984-2010; 2023-2024

#Räumliche Auflösung: 1 km
#Bounding Box: 46.2 - 49.2 °N, 9.4 - 17.4 °E
#Projektion: ETRS89-AUT [2002] / Austria Lambert (EPSG: 3416)

snow84 = rast("/Users/Franzi/Desktop/snow_voting/snow_voting/input_data/SNOWGRID-CL_snow_depth_1984.nc")
snow85 = rast("/Users/Franzi/Desktop/snow_voting/snow_voting/input_data/SNOWGRID-CL_snow_depth_1985.nc")
snow86 = rast("/Users/Franzi/Desktop/snow_voting/snow_voting/input_data/SNOWGRID-CL_snow_depth_1986.nc")
snow87 = rast("/Users/Franzi/Desktop/snow_voting/snow_voting/input_data/SNOWGRID-CL_snow_depth_1987.nc")
snow88 = rast("/Users/Franzi/Desktop/snow_voting/snow_voting/input_data/SNOWGRID-CL_snow_depth_1988.nc")
snow89 = rast("/Users/Franzi/Desktop/snow_voting/snow_voting/input_data/SNOWGRID-CL_snow_depth_1989.nc")
snow90 = rast("/Users/Franzi/Desktop/snow_voting/snow_voting/input_data/SNOWGRID-CL_snow_depth_1990.nc")
snow91 = rast("/Users/Franzi/Desktop/snow_voting/snow_voting/input_data/SNOWGRID-CL_snow_depth_1991.nc")
snow92 = rast("/Users/Franzi/Desktop/snow_voting/snow_voting/input_data/SNOWGRID-CL_snow_depth_1992.nc")
snow93 = rast("/Users/Franzi/Desktop/snow_voting/snow_voting/input_data/SNOWGRID-CL_snow_depth_1993.nc")
snow94 = rast("/Users/Franzi/Desktop/snow_voting/snow_voting/input_data/SNOWGRID-CL_snow_depth_1994.nc")
snow95 = rast("/Users/Franzi/Desktop/snow_voting/snow_voting/input_data/SNOWGRID-CL_snow_depth_1995.nc")
snow96 = rast("/Users/Franzi/Desktop/snow_voting/snow_voting/input_data/SNOWGRID-CL_snow_depth_1996.nc")
snow97 = rast("/Users/Franzi/Desktop/snow_voting/snow_voting/input_data/SNOWGRID-CL_snow_depth_1997.nc")
snow98 = rast("/Users/Franzi/Desktop/snow_voting/snow_voting/input_data/SNOWGRID-CL_snow_depth_1998.nc")
snow99 = rast("/Users/Franzi/Desktop/snow_voting/snow_voting/input_data/SNOWGRID-CL_snow_depth_1999.nc")
snow00 = rast("/Users/Franzi/Desktop/snow_voting/snow_voting/input_data/SNOWGRID-CL_snow_depth_2000.nc")
snow01 = rast("/Users/Franzi/Desktop/snow_voting/snow_voting/input_data/SNOWGRID-CL_snow_depth_2001.nc")
snow02 = rast("/Users/Franzi/Desktop/snow_voting/snow_voting/input_data/SNOWGRID-CL_snow_depth_2002.nc")
snow03 = rast("/Users/Franzi/Desktop/snow_voting/snow_voting/input_data/SNOWGRID-CL_snow_depth_2003.nc")
snow04 = rast("/Users/Franzi/Desktop/snow_voting/snow_voting/input_data/SNOWGRID-CL_snow_depth_2004.nc")
snow05 = rast("/Users/Franzi/Desktop/snow_voting/snow_voting/input_data/SNOWGRID-CL_snow_depth_2005.nc")
snow06 = rast("/Users/Franzi/Desktop/snow_voting/snow_voting/input_data/SNOWGRID-CL_snow_depth_2006.nc")
snow07 = rast("/Users/Franzi/Desktop/snow_voting/snow_voting/input_data/SNOWGRID-CL_snow_depth_2007.nc")
snow08 = rast("/Users/Franzi/Desktop/snow_voting/snow_voting/input_data/SNOWGRID-CL_snow_depth_2008.nc")
snow09 = rast("/Users/Franzi/Desktop/snow_voting/snow_voting/input_data/SNOWGRID-CL_snow_depth_2009.nc")
snow10 = rast("/Users/Franzi/Desktop/snow_voting/snow_voting/input_data/SNOWGRID-CL_snow_depth_2010.nc")
snow23 = rast("/Users/Franzi/Desktop/snow_voting/snow_voting/input_data/SNOWGRID-CL_snow_depth_2023.nc")
snow24 = rast("/Users/Franzi/Desktop/snow_voting/snow_voting/input_data/SNOWGRID-CL_snow_depth_2024.nc")

#Assigning CRS: EPSG: 3416
snow_list = list(snow84, snow85, snow86, snow87, snow88, snow89, 
                 snow90, snow91, snow92, snow93, snow94, snow95, snow96, snow97, snow98, snow99, 
                 snow00, snow01, snow02, snow03, snow04, snow05, snow06, snow07, snow08, snow09, 
                 snow10, 
                 snow23, snow24)

#Correcting extent of snow raster
snow24
correct_ext <- ext(112518.2, 696518.2, 275472, 604472)

snow_list <- lapply(snow_list, function(x) {
  ext(x) <- correct_ext
  crs(x) <- "EPSG:3416"
  return(x)
})

ext(snow_list[[29]])
ext(munic24_vect)


##Historical Mean in Snowdepth: 1984-2010

#Leap-Years: 366 days
sapply(snow_list[1:27], nlyr)
#1984 
#1988 
#1992 
#1996 
#2000 
#2004
#2008 

#Historical Winter-Seasons
#Winter 84/85:  Nov-Dec snow84 + Jan-Apr snow85
#Winter 85/86:  Nov-Dec snow85 + Jan-Apr snow86
...
#Winter 09/10:  Nov-Dec snow09 + Jan-Apr snow10


#Extract Nov-Dec from 84-09
is_leap = sapply(snow_list[1:27], nlyr) == 366
extract_nov_dec = function(rast_obj, is_leap) { #function to define the leap years, that shifts the day where november starts!
  if (is_leap == TRUE) {
    layers <- 306:366
  } else {
    layers <- 305:365
  }
  return(rast_obj[[layers]])
}
nov_dec_hist = mapply(extract_nov_dec, snow_list[1:26], is_leap[1:26], SIMPLIFY = FALSE)


#Extract Jan-April from 85-10
extract_jan_apr <- function(rast_obj, is_leap) {
  if (is_leap == TRUE) {
    layers <- 1:121
  } else {
    layers <- 1:120
  }
  return(rast_obj[[layers]])
}
jan_apr_hist = mapply(extract_jan_apr, snow_list[2:27], is_leap[2:27], SIMPLIFY = FALSE)


#Combining Nov-Dec + Jan-April for each winter season 84/85 - 09/10
winters_hist = mapply(function(nd, ja) { #this gives me 26 winter seasons, each containing daily layers from Nov-Apr
  c(nd, ja)
}, nov_dec_hist, jan_apr_hist, SIMPLIFY = FALSE)

#Historical Mean in Snow Depth per Winter Season: 84/85 - 09/10 (26 winter seasons)
winter_hist_means = lapply(winters_hist, function(x) {
  mean(x, na.rm = TRUE)
})


#Stack of all historical winter seasons
winter_hist_means_stack = rast(winter_hist_means)


#Mean of snowdepth across all 26 historical winter seasons 1984/85 - 2009/19
snow_hist_mean  = mean(winter_hist_means_stack, na.rm = TRUE)


##Mean snowdepth winter 23/24
nlyr(snow_list[[28]])
nlyr(snow_list[[29]]) #24 = leap year

#Extract Nov-Dec 23
nov_dec_23 = snow_list[[28]][[305:365]]

#Extract Jan-Apr 24 - Leap Year
jan_apr_24 = snow_list[[29]][[1:121]]

#Combining into Winter season 23/24
winter_2324 = c(nov_dec_23, jan_apr_24)

#Mean snowdepth winter 23/24
snow_2324_mean = mean(winter_2324, na.rm = TRUE)

##Snow Anomaly: Historical Mean in Snow - Snow 24/24
snow_anomaly = snow_2324_mean - snow_hist_mean #positive values: more snow than historical average, #negative: less snow than average

## All Snow Rasters stacked together
snow_all = c(snow_hist_mean, snow_2324_mean, snow_anomaly)
names(snow_all) = c("hist_mean", "2324_mean", "anomaly")

##Extracting the SpatRasters (snow_all) + SpatVector(munic24_vect)
snow_sf = terra::extract(snow_all, munic24_vect, fun = mean, na.rm = TRUE, bind = TRUE, touches = TRUE) |>
  st_as_sf()

sum(is.nan(snow_sf$anomaly))


#Questions
#Matching the SpatRaster with the Vector, so they match? 
#Controls: at province level, but analysis at municipality level, is that a problem? 



#===============================================================================
#Non-Spatial Work
#===============================================================================

#Vote Shares per municipality - Statistik Austria
nwahl24 = read_excel("/Users/Franzi/Desktop/snow_voting/snow_voting/input_data/endgueltiges_Ergebnis_Beschluss_Bundeswahlbehoerde_16102024.xlsx")


##Controls 

#Statistik Austria
labmarkstats <- read.csv2("/Users/Franzi/Desktop/snow_voting/snow_voting/input_data/OGDEXT_AEST_GEMTAB_1.csv")

#Tourism Intensity/Overnight Stays
library(readODS)
overnights <- read_ods("/Users/Franzi/Desktop/snow_voting/snow_voting/input_data/TourismusintensitaetNachBundeslaendern1995-2025.ods")
head(overnights)
names(overnights)



