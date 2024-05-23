###############################################################
###############################################################
#######                                                 ####### 
#######                   PROJECT:                      ####### 
#######         Droughts & child under-nutrition        ####### 
#######                                                 ####### 
#######           CODE: Prepare the agr. data           #######
###############################################################
###############################################################


## Contents:
## 1. Distribution of crop areas. Source: Monfreda et al 2008
## 2. Areas dedicated to cropland and pasture. Source: Ramankutty et al 2008
## 3. Agricultural calendar. Source: Sacks et al 2010   
## 4. Combine all data

## Search "NOTE" for important notes


rm(list =ls())
#install.packages("ncdf4")
library(ncdf4)    # package for netcdf manipulation
#install.packages("raster")
library(raster)   # package for raster manipulation
library(rgdal)    # package for geospatial analysis
library(ggplot2)  # package for plotting
library(viridis)  # better colors for everyone
library(tidyverse)
#install.packages("ggthemes")
library(ggthemes) # package for theme_map()
options(scipen=999)
options(digits=5)


setwd("Data")

################################################################################
## 1. Distribution of crop areas. Source: Monfreda et al 2008
################################################################################
# data source: http://www.earthstat.org/harvested-area-yield-175-crops/

## Import the DHS geolocations
psu <- read.csv("DHS_clust_coord.csv")[-1]

# create a spatial points dataframe
sp <- psu %>% dplyr::select(SurveyId, psu, LATNUM, LONGNUM) %>% distinct()
# convert to spatial points
coordinates(sp) <- c("LONGNUM", "LATNUM")
# assign CRS
proj4string(sp) <- CRS("+proj=longlat +datum=WGS84")
plot(sp)

## Import and combine the gridded crop data (19 crops)
rd1 = raster(paste("./HarvestedAreaYield175Crops_Geotiff/GeoTiff/","barley","/","barley","_HarvestedAreaFraction.tif", sep=""))
crs(rd1)

c = c("cassava", "cotton", "groundnut", "maize", "millet", "oats", "potato", "pulsenes", "rapeseed",
      "rice", "rye", "sorghum", "soybean", "sugarbeet", "sunflower", "sweetpotato", "wheat", "yam")

#i = "cassava"
for (i in c){
  tmp = raster(paste("./HarvestedAreaYield175Crops_Geotiff/GeoTiff/",i,"/",i,"_HarvestedAreaFraction.tif", sep=""))
  rd1 <- stack(rd1, tmp)
}


## Import world map and subset to Africa
#install.packages("rworldmap")
library(rworldmap)
bound <- getMap(resolution = "low")
bound <- bound[bound@data$REGION %in% "Africa", ]
crs(bound)
plot(bound)
bound <- buffer(bound, width=0.25) 
plot(bound, add=T, border="red")


## Restrict the agricultural data to Africa
cd0 <- crop(x = rd1, y = bound)
cd1 <- rasterize(x = bound, y = cd0)
cd2 <- mask(x = cd0, mask = cd1)

cd2@data@names
names <- cd2@data@names
names <- sub("_.*", "", names)  
names
cd2@data@names = names
cd2@data@names
#plot(cd2)



## NOTE: Select buffer size around the PSU; buff = 10000 (10 km)
buff = 10000

## Extract the agricultural data for each PSU location 
cd3 <- raster::extract(cd2,             # raster layer
                       sp,              # SPDF with centroids for buffer
                       buffer=buff,     # buffer size, units depend on CRS
                       fun=mean,        # what value to extract
                       na.rm=TRUE,
                       df=TRUE)         # return a dataframe 

df1 <- cbind(sp@data$SurveyId, sp@data$psu,  cd3) #sp@coords,
colnames(df1)[1] ="SurveyId"
colnames(df1)[2] ="psu"
df1 <- df1 %>% dplyr::select(-c(ID))

crops_dist_psu <- df1

## Determine the dominant crop for each psu unit

crops_dist_psu <- crops_dist_psu %>% 
  mutate(barley = ifelse(is.na(barley), 0, barley)) %>% 
  mutate(cassava = ifelse(is.na(cassava), 0, cassava)) %>% 
  mutate(cotton = ifelse(is.na(cotton), 0, cotton)) %>% 
  mutate(groundnut = ifelse(is.na(groundnut), 0, groundnut)) %>% 
  mutate(maize = ifelse(is.na(maize), 0, maize)) %>% 
  mutate(millet = ifelse(is.na(millet), 0, millet)) %>% 
  mutate(oats = ifelse(is.na(oats), 0, oats)) %>% 
  mutate(potato = ifelse(is.na(potato), 0, potato)) %>% 
  mutate(pulsenes = ifelse(is.na(pulsenes), 0, pulsenes)) %>% 
  mutate(rapeseed = ifelse(is.na(rapeseed), 0, rapeseed)) %>% 
  mutate(rice = ifelse(is.na(rice), 0, rice)) %>% 
  mutate(rye = ifelse(is.na(rye), 0, rye)) %>% 
  mutate(sorghum = ifelse(is.na(sorghum), 0, sorghum)) %>% 
  mutate(soybean = ifelse(is.na(soybean), 0, soybean)) %>% 
  mutate(sugarbeet = ifelse(is.na(sugarbeet), 0, sugarbeet)) %>% 
  mutate(sunflower = ifelse(is.na(sunflower), 0, sunflower)) %>% 
  mutate(sweetpotato = ifelse(is.na(sweetpotato), 0, sweetpotato)) %>% 
  mutate(wheat = ifelse(is.na(wheat), 0, wheat)) %>% 
  mutate(yam = ifelse(is.na(yam), 0, yam)) %>%
  rowwise() %>%
  mutate(crop.tot = sum(c_across(barley:yam), na.rm=TRUE)) %>% 
  ungroup()


dominant <- crops_dist_psu %>%
  dplyr::select(barley:yam) %>%
  rowwise()%>%
  mutate(row_max = names(.)[which.max(c_across(everything()))]) %>% 
  ungroup() %>% 
  dplyr::select(row_max) %>% 
  rename(main.crop = row_max)


crops_dist_psu <- cbind(crops_dist_psu, dominant) %>% 
  mutate(main.crop = ifelse(crop.tot == 0, NA, main.crop))

crops_dist_psu %>% 
  group_by(main.crop) %>% 
  summarize(obs = n()) 


## Plot the crop data

# Retrieve spatial boundaries for the subset of SSA countries included in the analysis
bound_sub <- getMap(resolution = "low")
#unique(as.character(bound_sub@data$NAME))
bound_sub <- bound_sub[bound_sub@data$NAME %in% c(unique(psu$CountryName), 
                                                  "Central African Rep.", 
                                                  "Congo (Kinshasa)",
                                                  "Ivory Coast", 
                                                  "Swaziland"),]

#plot(bound_sub)

bound <- getMap(resolution = "low")
bound <- bound[bound@data$REGION %in% "Africa", ]
plot(bound)


library(data.table)
df0 <- data.frame(rasterToPoints(cd2))
df0 <- melt(setDT(df0), id.vars = c("x","y"), variable.name = "crop")
df0 <- df0 %>% 
  mutate(crop = str_to_title(crop))


# Load the subnational DHS boundaries
load("DHS_data_harmonized.RData")
adm <- dhs %>% dplyr::select(CountryName, SurveyId, LATNUM, LONGNUM, psu, regId) %>% distinct()
rm(dhs)

#plot(bord_sf)
bord_sf <- bord_sf %>% dplyr::select(DHSCC, regId)
bord_df <- bord_sf

library(sf)
st_geometry(bord_df) <- NULL # remove geometry, coerce to data.frame
class(bord_df)


#i = "Barley"
for(i in unique(df0$crop)) {  
  
plot <- df0 %>%
  filter(crop == i) %>% 
  filter(value>0) %>% 
  ggplot() +  
  geom_polygon(data = bound, size=0.3, aes(x = long, y = lat, group = group), fill = "darkgray", colour = "lightgray") +
  geom_tile(aes(x=x, y=y, fill=value, col=value)) +
  scale_fill_gradient(name=i, low = "#FEFFD9", high = "#3C8200", labels = scales::percent, limits=c(0,1)) +
  scale_color_gradient(name=i, low = "#FEFFD9", high = "#3C8200", labels = scales::percent, limits=c(0,1)) +
  coord_equal() +
  theme_void() +
  theme(legend.key.height=unit(2, "cm"),
        legend.title = element_text(size=15)) +
  geom_polygon(data = bound, size=0.3, aes(x = long, y = lat, group = group), fill = NA, colour = "lightgray") +
  geom_sf(data = bord_sf, fill = NA, size = 0.125, colour = "#484848") +
  geom_polygon(data = bound_sub, size=0.3, aes(x = long, y = lat, group = group), fill = NA, colour = "#333333")
  
ggsave(paste("Figures/crops/", i, ".png", sep=""), plot, width = 7, height = 6, dpi=400)

}



## Determine the share of land dedicated to 19 crops for each subnational region (Admin 1)
sd0 <- crop(x = cd2, y = bord_sf)
sd1 <- rasterize(x = bord_sf, y = sd0)
sd2 <- mask(x = sd0, mask = sd1)
# extract values from the spatialbrick
sd3 <- raster::extract(x = sd2, y = bord_sf, method='simple')
str(sd3)
# add a column onto the list with the admin names
sd4 <- mapply(cbind, sd3, DHSCC = bord_df$DHSCC, regId = bord_df$regId)
str(sd4[[1]])

# list to wide data frame, making all columns (except region names numeric)
crops_dist_grid <- sd4 %>% 
  lapply(as.data.frame, stringsAsFactors = FALSE) %>% 
  bind_rows() %>% 
  tbl_df() %>%
  mutate_each(funs(as.numeric), -DHSCC, -regId)

crops_dist_adm <- crops_dist_grid %>%
  group_by(DHSCC, regId) %>%
  summarise(barley = mean(barley, na.rm=TRUE),
            cassava = mean(cassava, na.rm=TRUE),
            cotton = mean(cotton, na.rm=TRUE),
            groundnut = mean(groundnut, na.rm=TRUE),
            maize = mean(maize, na.rm=TRUE),
            millet= mean(millet, na.rm=TRUE),
            oats = mean(oats, na.rm=TRUE),
            potato = mean(potato, na.rm=TRUE),
            pulsenes = mean(pulsenes, na.rm=TRUE),
            rapeseed = mean(rapeseed, na.rm=TRUE),
            rice = mean(rice, na.rm=TRUE),
            rye = mean(rye, na.rm=TRUE),
            sorghum = mean(sorghum, na.rm=TRUE),
            soybean = mean(soybean, na.rm=TRUE),
            sugarbeet = mean(sugarbeet, na.rm=TRUE),
            sunflower = mean(sunflower, na.rm=TRUE),
            sweetpotato = mean(sweetpotato, na.rm=TRUE),
            wheat = mean(wheat, na.rm=TRUE),
            yam = mean(yam, na.rm=TRUE))



## Determine the dominant crop for each admin area 

crops_dist_adm <- crops_dist_adm %>% 
  mutate(barley = ifelse(is.na(barley), 0, barley)) %>% 
  mutate(cassava = ifelse(is.na(cassava), 0, cassava)) %>% 
  mutate(cotton = ifelse(is.na(cotton), 0, cotton)) %>% 
  mutate(groundnut = ifelse(is.na(groundnut), 0, groundnut)) %>% 
  mutate(maize = ifelse(is.na(maize), 0, maize)) %>% 
  mutate(millet = ifelse(is.na(millet), 0, millet)) %>% 
  mutate(oats = ifelse(is.na(oats), 0, oats)) %>% 
  mutate(potato = ifelse(is.na(potato), 0, potato)) %>% 
  mutate(pulsenes = ifelse(is.na(pulsenes), 0, pulsenes)) %>% 
  mutate(rapeseed = ifelse(is.na(rapeseed), 0, rapeseed)) %>% 
  mutate(rice = ifelse(is.na(rice), 0, rice)) %>% 
  mutate(rye = ifelse(is.na(rye), 0, rye)) %>% 
  mutate(sorghum = ifelse(is.na(sorghum), 0, sorghum)) %>% 
  mutate(soybean = ifelse(is.na(soybean), 0, soybean)) %>% 
  mutate(sugarbeet = ifelse(is.na(sugarbeet), 0, sugarbeet)) %>% 
  mutate(sunflower = ifelse(is.na(sunflower), 0, sunflower)) %>% 
  mutate(sweetpotato = ifelse(is.na(sweetpotato), 0, sweetpotato)) %>% 
  mutate(wheat = ifelse(is.na(wheat), 0, wheat)) %>% 
  mutate(yam = ifelse(is.na(yam), 0, yam)) %>%
  rowwise() %>%
  mutate(crop.tot = sum(c_across(barley:yam), na.rm=TRUE)) %>% 
  ungroup()


dominant <- crops_dist_adm %>%
  dplyr::select(barley:yam) %>%
  rowwise()%>%
  mutate(row_max = names(.)[which.max(c_across(everything()))]) %>% 
  ungroup() %>% 
  dplyr::select(row_max) %>% 
  rename(main.crop = row_max)


crops_dist_adm <- cbind(crops_dist_adm, dominant) %>% 
  mutate(main.crop = ifelse(crop.tot == 0, NA, main.crop))

crops_dist_adm %>% 
  group_by(main.crop) %>% 
  summarize(obs = n()) 


rm(list=setdiff(ls(), c('bound', 'sp', 'buff','crops_dist_psu', 'psu', 'sp', "crops_dist_grid", "crops_dist_adm", "bord_sf", "bord_df", "adm")))


################################################################################
## 2. Areas dedicated to cropland and pasture. Source: Ramankutty et al 2008
################################################################################
# data source: http://www.earthstat.org/cropland-pasture-area-2000/

rd2 = raster("./CroplandPastureArea2000_Geotiff/Cropland2000_5m.tif")
rd3 = raster("./CroplandPastureArea2000_Geotiff/Pasture2000_5m.tif")
rd4 <- stack(rd2, rd3)

## Restrict the agricultural data to Africa
cd3 <- crop(x = rd4, y = bound)
cd4 <- rasterize(x = bound, y = cd3)
cd5 <- mask(x = cd3, mask = cd4)
#plot(cd5)

cd5@data@names
names <- c("Cropland", "Pasture")
cd5@data@names = names
cd5@data@names

## Extract the agricultural data for each PSU location 
cd6 <- raster::extract(cd5,             # raster layer
                       sp,              # SPDF with centroids for buffer
                       buffer=buff,     # buffer size, units depend on CRS
                       fun=mean,        # what value to extract
                       na.rm=TRUE,
                       df=TRUE)         # return a dataframe 

df2 <- cbind(sp@data$SurveyId, sp@data$psu, cd6) # sp@coords,
colnames(df2)[1] ="SurveyId"
colnames(df2)[2] ="psu"
df2 <- df2 %>% dplyr::select(-c(ID))

land_dist_psu <- df2

#write.csv(df2, "DHS_clust_CroplandPasture.csv")

## Plot the data
bound_sub <- getMap(resolution = "low")
unique(as.character(bound_sub@data$NAME))

bound_sub <- bound_sub[bound_sub@data$NAME %in% c(unique(psu$CountryName), 
                                                  "Central African Rep.", 
                                                  "Congo (Kinshasa)",
                                                  "Ivory Coast", 
                                                  "Swaziland"),]

#plot(bound_sub)

bound <- getMap(resolution = "low")
bound <- bound[bound@data$REGION %in% "Africa", ]
plot(bound)

land_dist_grid <- data.frame(rasterToPoints(cd5))

# choose color palette: https://r-charts.com/color-palettes/
plot1 <- land_dist_grid %>%
  filter(Pasture > 0) %>% 
  ggplot() +  
  geom_polygon(data = bound, size=0.3, aes(x = long, y = lat, group = group), fill = "darkgray", colour = "lightgray") + #fill = "#dfded6"
  geom_tile(aes(x=x, y=y, fill=Pasture, color=Pasture)) +
  scale_fill_gradient(name="% of land", low = "#EEDBBD", high = "#9F3632", labels = scales::percent) + #na.value = 'grey'
  scale_color_gradient(name="% of land", low = "#EEDBBD", high = "#9F3632", labels = scales::percent) +
  coord_equal() +
  theme_void() +
  theme(
        #legend.position = c(0.3, 0.4),
        legend.direction = "vertical",
        #legend.key.height=unit(0.85, "cm"),
        legend.key.height=unit(1.8, "cm"),
        legend.text=element_text(size=12),
        legend.title=element_text(size=15)) +
  geom_polygon(data = bound, size=0.3, aes(x = long, y = lat, group = group), fill = NA, colour = "lightgray") +
  geom_polygon(data = bound_sub, size=0.3, aes(x = long, y = lat, group = group), fill = NA, colour = "#333333") +
  ggtitle("a) Pastures") +
  theme(plot.title = element_text(size=20, face="bold"))
#+   ggsave("Area_pasture.png", width=5, height=5, dpi=600)


plot2 <- land_dist_grid %>%
  filter(Cropland > 0) %>% 
  ggplot() +  
  geom_polygon(data = bound, size=0.3, aes(x = long, y = lat, group = group), fill = "darkgray", colour = "lightgray") +
  geom_tile(aes(x=x, y=y, fill=Cropland, color=Cropland)) +
  scale_fill_gradient(name="% of land", low = "#FEFFD9", high = "#3C8200", labels = scales::percent) +
  scale_color_gradient(name="% of land", low = "#FEFFD9", high = "#3C8200", labels = scales::percent) +
  coord_equal() +
  theme_void() +
  theme(
    #legend.position = c(0.3, 0.4),
    legend.direction = "vertical",
    #legend.key.height=unit(0.85, "cm"),
    legend.key.height=unit(1.8, "cm"),
    legend.text=element_text(size=12),
    legend.title=element_text(size=15)) +
  geom_polygon(data = bound, size=0.3, aes(x = long, y = lat, group = group), fill = NA, colour = "lightgray") +
  geom_polygon(data = bound_sub, size=0.3, aes(x = long, y = lat, group = group), fill = NA, colour = "#333333") +
  ggtitle("b) Cropland")+
  theme(plot.title = element_text(size=20, face="bold"))
#+  ggsave("Area_cropland.png", width=5, height=5, dpi=600)

library(ggpubr)
ggarrange(plot1, plot2, ncol = 2)

#rm(list=setdiff(ls(), c('bound', "bound_sub", 'sp', 'buff','df1', 'df2')))


################################################################################
## 3. Agricultural calendar. Source: Sacks et al 2010 
################################################################################
# data source: https://sage.nelson.wisc.edu/data-and-models/datasets/crop-calendar-dataset/
# open the ncdf4 file
# R tutorial for ncdf files: https://pjbartlein.github.io/REarthSysSci/netCDF.html#reading-a-netcdf-data-set-using-the-ncdf4-package


a <- list.files(pattern ="\\.nc")
a

df3 <- data.frame(matrix(ncol = 0, nrow = 0))

#i =  "Maize.crop.calendar.nc"

for (i in a){
  ncin <- nc_open(paste(i, "/", i, sep="")) 
  class(ncin)
  print(ncin)

  # get a list of variable names
  var.idNames <-names(ncin$var)
  var.idNames

  # get longitude and latitude
  lon <- ncvar_get(ncin, "longitude")
  nlon <- dim(lon)
  lat <- ncvar_get(ncin, "latitude", verbose = F)
  nlat <- dim(lat)
  head(lon)
  print(c(nlon,nlat))
  
  # get data
  plant.start <- ncvar_get(ncin, "plant.start")
  plant.start <- as.vector(plant.start)

  plant.end <- ncvar_get(ncin, "plant.end")
  plant.end <- as.vector(plant.end)
  
  harvest.start <- ncvar_get(ncin, "harvest.start")
  harvest.start <- as.vector(harvest.start)
  
  harvest.end <- ncvar_get(ncin, "harvest.end")
  harvest.end <- as.vector(harvest.end)
  
  # create dataframe -- reshape data
  # matrix (nlon*nlat rows by 2 cols) of lons and lats
  lonlat <- as.matrix(expand.grid(lon,lat))
  dim(lonlat)

  # create dataframe and add names
  df0 <- data.frame(cbind(lonlat, plant.start, plant.end, harvest.start, harvest.end))
  names(df0) <- c("LON","LAT", "plant.start", "plant.end", "harvest.start", "harvest.end")
  df0$crop = tolower(str_split_fixed(i, "\\.crop", 2))[1]
  
  df0 <- df0 %>% 
    mutate(LON = round(LON, digits=2),
           LAT = round(LAT, digits=2))
  
  df3 <- rbind(df3, df0)
  #df1$date <- as.Date(df1$harvest, format = "%j", origin = "1.1.2000")
  #head(na.omit(df1), 10)
  

}


unique(df3$crop)

## Match the DHS locations with the crop calendar data

df4 <- df3 %>% dplyr::select(LON, LAT) %>% distinct() 
df4$row_num <- seq.int(nrow(df4)) 
spg = df4
coordinates(spg) <- c("LON", "LAT")
spg <- rasterFromXYZ(spg)

## Extract the agricultural calendar data for each PSU location 
cd7 <- raster::extract(spg,             # raster layer
                       sp,              # SPDF with centroids for buffer
                       df=TRUE)         # return a dataframe? 

df5 <- cbind(sp@data$SurveyId, sp@data$psu, cd7) #sp@coords
colnames(df5)[1] ="SurveyId"
colnames(df5)[2] ="psu"
df5 <- df5 %>% 
  dplyr::select(-c(ID)) %>% 
  rename(row_num = layer) %>% 
  left_join(df4) %>% 
  dplyr::select(-c(row_num))


df6 <- reshape(df3,
               idvar = c("LON", "LAT"),
               timevar = "crop",
               direction = "wide") 


df7 <- df5 %>% 
  left_join(df6) %>% 
  dplyr::select(-c('LON', 'LAT')) %>% 
  left_join(adm)


## Note: agricultural calendar dates missing for some PSUs (due to shifted PSU coordinates falling in non-land areas) - fill information
df7 <- df7 %>% 
  group_by(SurveyId, regId) %>% 
  fill(plant.start.barley:harvest.end.yams, .direction = "downup") %>% 
  fill(plant.start.barley:harvest.end.yams, .direction = "up") %>% 
  ungroup()


calendar <- df7

#write.csv(df7, "Crops_calendar.csv")


## Plot the crop calendar data
# import world map 
library(rworldmap)
world <- getMap(resolution = "low")
sort(world@data$REGION)
'%!in%' <- function(x,y)!('%in%'(x,y))
world <- world[world@data$REGION %!in% "Antarctica", ]

crops = c("barley", "barley.winter",  "cassava", "cotton", "groundnuts", "maize",  "maize.2",
          "millet", "oats", "potatoes", "pulses", "rice", "rice.2", "sorghum", "sorghum.2", 
          "soybeans", "sunflower", "sweet.potatoes", "wheat",  "wheat.winter", "yams")

# customize the color scale
n = 365
my_colors <- rainbow(n, s = 1, v = 1, start = 0.8, end = 0.79)


unique(df3$crop)

df3["crop"][df3["crop"]=="barley"] <- "Barley"
df3["crop"][df3["crop"]=="barley.winter"] <- "Winter barley"
df3["crop"][df3["crop"]=="cassava"] <- "Cassava"
df3["crop"][df3["crop"]=="cotton"] <- "Cotton"
df3["crop"][df3["crop"]=="groundnuts"] <- "Groundnut"
df3["crop"][df3["crop"]=="maize.2"] <- "Maize 2"
df3["crop"][df3["crop"]=="maize"] <- "Maize"
df3["crop"][df3["crop"]=="millet"] <- "Millet"
df3["crop"][df3["crop"]=="oats"] <- "Oats"
df3["crop"][df3["crop"]=="oats.winter"] <- "Winter oats"
df3["crop"][df3["crop"]=="potatoes"] <- "Potatoes"
df3["crop"][df3["crop"]=="pulses"] <- "Pulses"
df3["crop"][df3["crop"]=="rapeseed.winter"] <- "Winter Rapeseed"
df3["crop"][df3["crop"]=="rice.2"] <- "Rice 2"
df3["crop"][df3["crop"]=="rice"] <- "Rice"
df3["crop"][df3["crop"]=="rye.winter"] <- "Winter rye"
df3["crop"][df3["crop"]=="sorghum.2"] <- "Sorghum 2"
df3["crop"][df3["crop"]=="sorghum"] <- "Sorghum"
df3["crop"][df3["crop"]=="soybeans"] <- "Soybeans"
df3["crop"][df3["crop"]=="sugarbeets"] <- "Sugarbeets"
df3["crop"][df3["crop"]=="sunflower"] <- "Sunflower"
df3["crop"][df3["crop"]=="sweet.potatoes"] <- "Sweet potatoes"
df3["crop"][df3["crop"]=="wheat"] <- "Wheat"
df3["crop"][df3["crop"]=="wheat.winter"] <- "Winter wheat"
df3["crop"][df3["crop"]=="yams"] <- "Yams"

i='Cassava'

for(i in unique(df3$crop)) {  
  
cal.start <- df3 %>% 
  filter(!is.na(plant.start)) %>% 
  filter(crop == i)%>% 
  ggplot() +  
  geom_tile(aes(x=LON, y=LAT, fill=plant.start)) +
  scale_fill_gradientn(name="Day of year", colours=my_colors, #name=dname
                       breaks=seq(0, 365, by = 15),
                       limits=c(0,365)) +
  coord_equal() +
  theme_map() +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(4, "cm"))+
  geom_polygon(data = world, size=0.3,
               aes(x = long, y = lat, group = group),
               fill = NA, colour = "#333333") +
  labs(title = paste(i, ": Planting start date", sep="")) +
  theme(plot.title = element_text(size = 15, face = "bold"))


cal.end <- df3 %>% 
  filter(!is.na(harvest.start)) %>% 
  filter(crop == i)%>% 
  ggplot() +  
  geom_tile(aes(x=LON, y=LAT, fill=harvest.start)) +
  scale_fill_gradientn(name="Day of year", colours=my_colors, #name=dname
                       breaks=seq(0, 365, by = 15),
                       limits=c(0,365)) +
  coord_equal() +
  theme_map() +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(4, "cm"))+
  geom_polygon(data = world, size=0.3,
               aes(x = long, y = lat, group = group),
               fill = NA, colour = "#333333") +
  labs(title = paste(i,": Harvesting start date", sep="")) +
  theme(plot.title = element_text(size = 15, face = "bold"))

ggsave(paste("Figures/calendar/", i, ".png", sep=""), ggarrange(cal.start, cal.end, nrow = 1), width = 18, height = 6, dpi=400)

}

#rm(list=setdiff(ls(), c('bound', "bound_sub", 'sp', 'buff', 'df1', 'df2', 'df7')))




################################################################################
### 4. Combine the agricultural data
################################################################################

#psu <- read.csv("DHS_clust_coord.csv")[-1]

data <- adm %>% 
  left_join(land_dist_psu) %>% 
  #left_join(crops_dist_adm) %>% 
  left_join(crops_dist_psu) 
%>% 
  left_join(calendar) 


write.csv(data, paste("Agri_data_buffer", buff/1000, ".csv", sep=""))


### Plot the main crops 

main_crop <- data %>% 
  dplyr::select(SurveyId, psu, LATNUM, LONGNUM, main.crop) %>% 
  unique() %>% 
  drop_na() %>% 
  mutate(group.crop = ifelse(main.crop=="maize"|main.crop=="millet"|main.crop=="sorghum"|main.crop=="rice"|main.crop=="wheat"|main.crop=="barley", "Grains",
                             ifelse(main.crop=="cassava"|main.crop=="yam"|main.crop=="sweetpotato"|main.crop=="potato", "Roots and tubers",
                                    ifelse(main.crop=="cotton"|main.crop=="groundnut"|main.crop=="soybean"|main.crop=="sunflower", "Oilseeds and pulsenes",
                                           "Oilseeds and pulsenes")))) %>% 
  mutate(main.crop=ifelse(main.crop=="sweetpotato", "sweet potato", main.crop))

ord <- c("barley","maize","millet","rice","sorghum","wheat",
         "cassava","potato","sweet potato","yam",
         "cotton","groundnut","soybean","sunflower", "pulsenes")

main_crop$main.crop <- factor(main_crop$main.crop,levels=ord)

unique(main_crop$main.crop)
unique(main_crop$group.crop)


library(ggpubr)

### Plot main crop per PSU

# choose color palette: https://r-charts.com/color-palettes/
plot3 <- main_crop %>% 
  ggplot() +  
  geom_polygon(data = bound, linewidth=0.3, aes(x = long, y = lat, group = group), fill = "darkgray", colour = "lightgray") + #fill = "#dfded6"
   geom_point(data=main_crop, 
             aes(x=LONGNUM, y=LATNUM, fill=main.crop), stroke = 0, col ="transparent",
             pch=21, size=1.5) + #, alpha=0.85
  scale_fill_manual(name="Crop", values=c("#588958","#406440","#304B30","#ABB67C","#909D58","#6C7642",
                             "#f9b5ac", "#f7998d","#f47766", "#f25540",
                             "#c3acb6","#b295a2","#987284","#825E6E",
                             "#556476"))+ #"#f7998d", ,"#f25540",
  coord_equal() +
  theme_void() +
  #theme(legend.position="right") +
  theme(
        #legend.position = c(0.3, 0.35),
        legend.direction = "vertical",
        #legend.key.height=unit(0.38, "cm"),
        legend.key.height=unit(0.5, "cm"),
        legend.text=element_text(size=12),
        legend.title=element_text(size=15)) +
  geom_polygon(data = bound, size=0.3, aes(x = long, y = lat, group = group), fill = NA, colour = "lightgray") +
  geom_polygon(data = bound_sub, size=0.3, aes(x = long, y = lat, group = group), fill = NA, colour = "#333333") +
  ggtitle("c) Dominant crop type") +
  theme(plot.title = element_text(size=20, face="bold")) +
  guides(fill = guide_legend(override.aes = list(size=4)))


#ggarrange(plot1, plot2, plot3, ncol = 3)


rm(list=setdiff(ls(), c('bound', "bound_sub", "land_dist_grid", "main_crop", "plot1", "plot2", "plot3")))

save.image(file='Data_for_Plot_1.RData')

load('Data_for_Plot_1.RData')



################################################################################
### 5. Plots
################################################################################

## Bar plot - land cover
## Restrict the agricultural data to Africa
cd3 <- crop(x = rd4, y = bound)
cd4 <- rasterize(x = bound, y = cd3)
cd5 <- mask(x = cd3, mask = cd4)
#plot(cd5)

cd5@data@names
names <- c("cropland", "pasture")
cd5@data@names = names
cd5@data@names

# extract values from the spatialbrick
ld1 <- raster::extract(x = cd5, y = bound, method='simple')
# add a column onto the list with the admin names
ld1 <- mapply(cbind, ld1, Country = as.character(bound@data$NAME))
str(ld1[[1]])
# list to wide data frame, making all columns (except country names numeric)
land_dist_cnt <- ld1 %>% 
  lapply(as.data.frame, stringsAsFactors = FALSE) %>% 
  bind_rows() %>% 
  tbl_df() %>%
  mutate_each(funs(as.numeric), -Country)


land_dist_cnt <- land_dist_cnt[land_dist_cnt$Country %in% c(unique(psu$CountryName), 
                                                  "Central African Rep.", 
                                                  "Congo (Kinshasa)",
                                                  "Ivory Coast", 
                                                  "Swaziland"),]

land_dist_cnt <- land_dist_cnt %>%
  group_by(Country) %>%
  summarise(cropland = mean(cropland, na.rm=TRUE),
            pasture = mean(pasture, na.rm=TRUE)) %>% 
  mutate(Country = ifelse(Country == "Congo (Kinshasa)", "DR Congo", Country)) %>% 
  mutate(Country = ifelse(Country == "Ivory Coast", "Cote d'Ivoire", Country)) %>% 
  mutate(Country = ifelse(Country == "Swaziland", "Eswatini", Country))  
  


land_dist_cnt <- melt(setDT(land_dist_cnt), id.vars = c("Country"), variable.name = "Land")
land_dist_cnt <- land_dist_cnt %>% 
  drop_na()

ggplot(land_dist_cnt, aes(fill=Land, y=reorder(Country, value), x=value)) + 
  geom_bar(position="stack", stat="identity") +
  scale_x_continuous(labels = scales::percent) +
  scale_fill_manual(name="Area", values=c("#d0deaf", 
                             "#d5ab94")) +
  theme_minimal() +
  ylab("") +
  xlab("Agricultural area (% total area)")


## Bar plot - crops
adm2 <-  adm %>% dplyr::select(c("regId", "CountryName")) %>% distinct()

crops_dist_cnt <- crops_dist_grid %>% 
  left_join(adm2) %>% 
  group_by(CountryName) %>% 
  summarise(barley = mean(barley, na.rm=TRUE),
            cassava = mean(cassava, na.rm=TRUE),
            cotton = mean(cotton, na.rm=TRUE),
            groundnut = mean(groundnut, na.rm=TRUE),
            maize = mean(maize, na.rm=TRUE),
            millet= mean(millet, na.rm=TRUE),
            oats = mean(oats, na.rm=TRUE),
            potato = mean(potato, na.rm=TRUE),
            pulsenes = mean(pulsenes, na.rm=TRUE),
            rapeseed = mean(rapeseed, na.rm=TRUE),
            rice = mean(rice, na.rm=TRUE),
            rye = mean(rye, na.rm=TRUE),
            sorghum = mean(sorghum, na.rm=TRUE),
            soybean = mean(soybean, na.rm=TRUE),
            sugarbeet = mean(sugarbeet, na.rm=TRUE),
            sunflower = mean(sunflower, na.rm=TRUE),
            sweetpotato = mean(sweetpotato, na.rm=TRUE),
            wheat = mean(wheat, na.rm=TRUE),
            yam = mean(yam, na.rm=TRUE))


crops_dist_cnt <- melt(setDT(crops_dist_cnt), id.vars = c("CountryName"), variable.name = "Crop")
crops_dist_cnt <- crops_dist_cnt %>% 
  mutate(Crop = as.character(Crop)) %>% 
  mutate(Crop=ifelse(Crop=="sweetpotato", "sweet potato", Crop))

unique(crops_dist_cnt$Crop)

ord <- c("barley","maize","millet","rice","sorghum","wheat",
         "cassava","potato","sweet potato","yam",
         "cotton","groundnut","soybean","sunflower", "pulsenes")

crops_dist_cnt$Crop <- factor(crops_dist_cnt$Crop,levels=ord)
unique(crops_dist_cnt$Crop)

crops_dist_cnt <- crops_dist_cnt %>% 
  drop_na() %>% 
  mutate(CountryName = ifelse(CountryName == "Congo Democratic Republic", "DR Congo", CountryName)) %>% 
  mutate(CountryName = ifelse(CountryName == "Central African Republic", "Central African Rep.", CountryName))
  

ggplot(crops_dist_cnt, aes(fill=Crop, y=reorder(CountryName, value), x=value)) + 
  geom_bar(position="stack", stat="identity") +
  scale_x_continuous(labels = scales::percent) +
  scale_fill_manual(name="Crop", values=c("#588958","#406440","#304B30","#ABB67C","#909D58","#6C7642",
                                               "#f9b5ac","#f7998d", "#f47766","#f25540",
                                               "#c3acb6","#b295a2","#987284","#825E6E",
                                               "#556476"))+ # 
  theme_minimal() +
  ylab("") +
  xlab("Crop area (% total area)") #Share of land



