library(terra)
library(raster)
library(readxl)
library(sf)
library(spData)
library(tidyverse)
library(dplyr)
library(elevatr)



#===============================================================================
#Spatial Work
#===============================================================================

##Austrian Municipalities 
munic24 = st_read("/Users/Franzi/Desktop/snow_voting/snow_voting/input_data/STATISTIK_AUSTRIA_GEM_20240101/STATISTIK_AUSTRIA_GEM_20240101.shp")
st_crs(munic24)

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
snow_list = lapply(snow_list, function(x)
  {crs(x) = "EPSG:3416"; x})
munic24_proj = st_transform(munic24, "EPSG: 3416")

#Attaching Altitude to Municipalities: 

# Get altitude for each municipality
altitude = get_elev_raster(munic24, z = 7, clip = "locations")
# Extract mean altitude per municipality
munic24$altitude = terra::extract(rast(altitude), vect(munic24), fun = mean, na.rm = TRUE, ID = FALSE)[,1]


#plot(snow24[[1]])
#plot(munic24[1], add = TRUE)
#plot(munic24)

## Winter Seasons --------------------------------------------------------------
  #Historical Winter-Seasons
  #Winter 84/85:  Nov-Dec snow84 + Jan-Apr snow85
  #Winter 85/86:  Nov-Dec snow85 + Jan-Apr snow86
  #...
  #Winter 09/10:  Nov-Dec snow09 + Jan-Apr snow10

#Identifying leap years
is_leap = sapply(snow_list[1:27], nlyr) == 366

#Extracting Nov-Dec from years 84-09
extract_nov_dec = function(rast_obj, is_leap)
{layers  = if (is_leap == TRUE) 306:366 else 305:365
return(rast_obj[[layers]])}

#Extracting Jan-Apr
extract_jan_apr = function(rast_obj, is_leap)
  {layers = if (is_leap == TRUE) 1:121 else 1:120
  return(rast_obj[[layers]])}

nov_dec_hist = mapply(extract_nov_dec, snow_list[1:26], is_leap [1:26], SIMPLIFY = FALSE)
jan_apr_hist = mapply(extract_jan_apr, snow_list[2:27], is_leap [2:27], SIMPLIFY = FALSE)

#Combining this into 26 historical winterseasons from 1984/85 through to 2009/10
winters_hist = mapply(function(nd, ja) 
  {c(nd, ja)}, nov_dec_hist, jan_apr_hist, SIMPLIFY = FALSE)

#Winter 23/24
is_leap = sapply(snow_list[28:29], nlyr) == 366
nov_dec_23 = snow_list[[28]][[305:365]]
jan_apr_24 = snow_list[[29]][[1:121]] #leap year
winter_2324 = c(nov_dec_23, jan_apr_24)
#-------------------------------------------------------------------------------


## MEAN Snow-Depth per winter season -------------------------------------------------------------------------

#Mean snow-depth; historical 
snow_hist_mean = lapply(winters_hist, function(x) mean (x, na.rm = TRUE)) %>% 
  rast() %>% 
  mean(na.rm = TRUE)

#Mean snow-depth; for winter 23/24
snow_2324_mean = mean(winter_2324, na.rm = TRUE)

## MAX snow-depth in winter season-----------------------------------------------

#Max snow depth on average in historical winter seasons
snow_hist_max_mean = lapply(winters_hist, function(x) max(x, na.rm = TRUE)) %>%  #to get the average maximum value of snowdepth in the historical winters, check if that makes sense????
  rast() %>% 
  median(na.rm = TRUE)

#Max snow-depth for winter 23/24
snow_2324_max = max(winter_2324, na.rm = TRUE)

## Stacking all of the above rasters--------------------------------------------
snow = c(snow_hist_mean, snow_2324_mean, 
             snow_hist_max_mean, snow_2324_max)

names(snow) = c("hist_mean", "mean_2324", 
                "hist_max", "max_2324")


## Extracting this per municipality --------------------------------------------
snow_sf_mean = terra::extract(snow [[c("hist_mean", "mean_2324")]], vect(munic24_proj), fun = "mean", na.rm = TRUE, ID = FALSE, bind = TRUE, touches = TRUE) |> st_as_sf() 
snow_sf_max = terra::extract(snow[[c("hist_max", "max_2324")]], vect(munic24_proj), fun = "max", na.rm = TRUE, ID = FALSE, bind = TRUE, touches = TRUE) |> st_as_sf()                             


## ANOMALIES -------------------------------------------------------------------
snow_sf_mean$anomaly = snow_sf_mean$mean_2324 - snow_sf_mean$hist_mean
snow_sf_max$anomaly = snow_sf_max$max_2324 - snow_sf_max$hist_max

load("/Users/Franzi/Desktop/snow_voting/snow_voting/create_data/snow_data.RData")

## DF --------------------------------------------------------------------------
df_mean = st_drop_geometry(snow_sf_mean)
df_max = st_drop_geometry(snow_sf_max)




#Questions
#Matching the SpatRaster with the Vector, so they match? 
#Controls: at province level, but analysis at municipality level, is that a problem? 

#you can also use the one maximum point per municipality or the top 10
#you can also do fixed effects - factor variable per region and then that controls for everything that varies within 
#you can also do just one month
#you can look for the maximum value in the winter season

load("/Users/Franzi/Desktop/snow_voting/snow_voting/create_data/datasets.RData")

#===============================================================================
#Non-Spatial Work
#===============================================================================
#Adding district - ID (BEZIRKE)

bezirke = tribble(
  ~district_id, ~district_name,
  "101", "Eisenstadt(Stadt)",
  "102", "Rust(Stadt)",
  "103", "Eisenstadt-Umgebung",
  "104", "Güssing",
  "105", "Jennersdorf",
  "106", "Mattersburg",
  "107", "Neusiedl am See",
  "108", "Oberpullendorf",
  "109", "Oberwart",
  "201", "Klagenfurt Stadt",
  "202", "Villach Stadt",
  "203", "Hermagor",
  "204", "Klagenfurt Land",
  "205", "Sankt Veit an der Glan",
  "206", "Spittal an der Drau",
  "207", "Villach Land",
  "208", "Völkermarkt",
  "209", "Wolfsberg",
  "210", "Feldkirchen",
  "301", "Krems an der Donau(Stadt)",
  "302", "Sankt Pölten(Stadt)",
  "303", "Waidhofen an der Ybbs(Stadt)",
  "304", "Wiener Neustadt(Stadt)",
  "305", "Amstetten",
  "306", "Baden",
  "307", "Bruck an der Leitha",
  "308", "Gänserndorf",
  "309", "Gmünd",
  "310", "Hollabrunn",
  "311", "Horn",
  "312", "Korneuburg",
  "313", "Krems(Land)",
  "314", "Lilienfeld",
  "315", "Melk",
  "316", "Mistelbach",
  "317", "Mödling",
  "318", "Neunkirchen",
  "319", "Sankt Pölten(Land)",
  "320", "Scheibbs",
  "321", "Tulln",
  "322", "Waidhofen an der Thaya",
  "323", "Wiener Neustadt(Land)",
  "325", "Zwettl",
  "401", "Stadt Linz",
  "402", "Stadt Steyr",
  "403", "Stadt Wels",
  "404", "Braunau",
  "405", "Eferding",
  "406", "Freistadt",
  "407", "Gmunden",
  "408", "Grieskirchen",
  "409", "Kirchdorf",
  "410", "Linz-Land",
  "411", "Perg",
  "412", "Ried",
  "413", "Rohrbach",
  "414", "Schärding",
  "415", "Steyr-Land",
  "416", "Urfahr-Umgebung",
  "417", "Vöcklabruck",
  "418", "Wels-Land",
  "501", "Salzburg(Stadt)",
  "502", "Hallein",
  "503", "Salzburg-Umgebung",
  "504", "Sankt Johann im Pongau",
  "505", "Tamsweg",
  "506", "Zell am See",
  "601", "Graz(Stadt)",
  "603", "Deutschlandsberg",
  "606", "Graz-Umgebung",
  "610", "Leibnitz",
  "611", "Leoben",
  "612", "Liezen",
  "614", "Murau",
  "616", "Voitsberg",
  "617", "Weiz",
  "620", "Murtal",
  "621", "Bruck-Mürzzuschlag",
  "622", "Hartberg-Fürstenfeld",
  "623", "Südoststeiermark",
  "701", "Innsbruck-Stadt",
  "702", "Imst",
  "703", "Innsbruck-Land",
  "704", "Kitzbühel",
  "705", "Kufstein",
  "706", "Landeck",
  "707", "Lienz",
  "708", "Reutte",
  "709", "Schwaz",
  "801", "Bludenz",
  "802", "Bregenz",
  "803", "Dornbirn",
  "804", "Feldkirch",
  "900", "Wien"
)

#Add it to the df
df_mean = df_mean %>%
  rename(muni_id = g_id)

df_max = df_max %>%
  rename(muni_id = g_id)

df_mean = df_mean %>%
  mutate(district_id = substr(muni_id, 1, 3)) %>%
  left_join(bezirke, by = "district_id")
df_max = df_max %>% 
  mutate(district_id = substr(muni_id, 1, 3)) %>% 
  left_join(bezirke, by = "district_id")


#Vote Shares per municipality - Statistik Austria
nwahl24 = read_excel("/Users/Franzi/Desktop/snow_voting/snow_voting/input_data/endgueltiges_Ergebnis_Beschluss_Bundeswahlbehoerde_16102024.xlsx")
nwahl24 = nwahl24 %>% 
  select(GKZ, Gebietsname, Wahlberechtigte, Abgegebene, 
         FPÖ, "%...12", GRÜNE, "%...14") %>% 
  rename(
    muni_id = GKZ, 
    name = Gebietsname, 
    eligible_voters = Wahlberechtigte, 
    votes_cast = Abgegebene, 
    fpö_votes = FPÖ, 
    fpö_share = "%...12", 
    greens_votes = GRÜNE, 
    greens_share = "%...14"
  ) %>% 
  mutate(turnout = (votes_cast/eligible_voters)*100)

nwahl24 = nwahl24 %>%
  mutate(muni_id = sub("^G", "", muni_id))





#Statistik Austria - Abgestimmten Erwerbsstatistik/Gemeinde
ewstatistik = read.csv2("/Users/Franzi/Desktop/snow_voting/snow_voting/input_data/OGDEXT_AEST_GEMTAB_1.csv")

#Controls
controls = ewstatistik %>%
  filter(JAHR == 2023) %>%
  select(GCD, GEM_NAME, BEV_ABSOLUT, BEV_UEBER65, AUSL_STAATSB, ALQ_15PLUS, EDU_15_TER) %>%
  rename(
    muni_id    = GCD,
    municipality     = GEM_NAME,
    population   = BEV_ABSOLUT,
    pop_over65 = BEV_UEBER65,
    pop_foreign = AUSL_STAATSB,
    unemp_rate   = ALQ_15PLUS,
    share_tertiary_edu = EDU_15_TER
  )

controls$pop_foreign <- as.numeric(gsub(",", ".", controls$pop_foreign))

controls = controls %>% 
  mutate(muni_id = as.character (muni_id)) %>% 
  left_join(st_drop_geometry(munic24) %>% select(g_id, altitude), 
            by = c("muni_id" = "g_id"))

#Tourism Intensity/Overnight Stays
#library(readODS)
#overnights <- read_ods("/Users/Franzi/Desktop/snow_voting/snow_voting/input_data/TourismusintensitaetNachBundeslaendern1995-2025.ods")
#head(overnights)
#names(overnights)


#===============================================================================
#Putting it all together
#===============================================================================

# Merge all together
controls = controls %>% mutate(muni_id = as.character(muni_id))
nwahl24  = nwahl24  %>% mutate(muni_id = as.character(muni_id))
df_mean = df_mean %>%
  left_join(controls, by = "muni_id") %>%
  left_join(nwahl24, by = "muni_id")

df_max = df_max %>%
  left_join(controls, by = "muni_id") %>%
  left_join(nwahl24, by = "muni_id")

save(df_mean, df_max, snow_sf_max, snow_sf_mean, file = "datasets.RData")



names(df_mean)
names(df_max)
nrow(df_max)
nrow(df_mean)


