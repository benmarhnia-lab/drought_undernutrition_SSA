###############################################################
###############################################################
#######                                                 ####### 
#######                   PROJECT:                      ####### 
#######         Droughts & child under-nutrition        ####### 
#######                                                 ####### 
#######   CODE: Additional agro-ecological information  #######
###############################################################
###############################################################


#### Dryland zones (UNEP-WCMC (2007))
## Source: UNEP-WCMC 2007 A spatial analysis approach to the global delineation of dryland areas of relevance to the CBD programme ofwork on dry and subhumid lands. Dataset based on spatial analysis between WWF terrestrial ecoregions (WWF-US, 2004) and aridity zones (CRU/UEA; UNEPGRID, 1991). Dataset checked and refined to remove many gaps, overlaps and slivers (July 2014)
library(sf)
library(rworldmap)

world <- getMap(resolution = "high")
ssa <- "Sub-Saharan Africa (AFR)"
world_ssa <- world[world@data$SRES %in% ssa, ]
plot(world_ssa)

drylands <- st_read("./Regional_level_covariates/drylands_unep_wcmc/Drylands_dataset_2007/Drylands_latest_July2014/drylands_UNCCD_CBD_july2014.shp") %>% 
  dplyr::select(HIX_DESC, geometry) %>% 
  na.omit %>% 
  mutate(HIX_DESC = factor(HIX_DESC, levels = c("Hyperarid", "Arid", "Semiarid", "Dry subhumid")))

colors <- colorRampPalette(c("#a20000", "#de4900", "#f8ab00", "#feff69"))(4)

ggplot() + 
  geom_polygon(data = world, size=0.3,
               aes(x = long, y = lat, group = group),
               fill = "lightgray", colour = NA) +
  geom_sf(data=drylands, mapping = aes(fill=HIX_DESC), color = NA) + 
  scale_fill_manual(values=colors) +
  xlab("Longitude") +
  ylab("Latitude") + 
  theme_minimal() +
  labs(fill = "Dryland zone") +
  geom_polygon(data = world, size=0.3,
               aes(x = long, y = lat, group = group),
               fill = NA, colour = "gray65") 


## Import the DHS geolocations
psu <- read.csv("DHS_clust_coord.csv")[-1]
sp <- psu %>% dplyr::select(SurveyId, psu, LATNUM, LONGNUM) %>% distinct()
coordinates(sp) <- c("LONGNUM", "LATNUM")
proj4string(sp) <- CRS("+proj=longlat +datum=WGS84")
plot(sp)

## Overplay the PSUs with the dryland zones
# convert dryland data to sp
drylands_sp <- sf:::as_Spatial(drylands)
projection(drylands_sp)

## Extract the agricultural data for each PSU location 
drylands_pts <- raster::extract(drylands_sp,             # raster layer
                                sp,              # SPDF with centroids for buffer
                                #buffer=buff,     # buffer size, units depend on CRS
                                #fun=mean,        # what value to extract
                                na.rm=TRUE,
                                df=TRUE)         # return a dataframe 


drylands_pts <- cbind(sp@data$SurveyId, sp@data$psu, drylands_pts) #sp@coords,
colnames(drylands_pts)[1] ="SurveyId"
colnames(drylands_pts)[2] ="psu"
colnames(drylands_pts)[5] ="dryland_zone"
drylands_pts <- drylands_pts %>% dplyr::select(-c(point.ID, poly.ID))

dhs <- dhs %>% 
  left_join(drylands_pts)

dhs %>% 
  group_by(dryland_zone) %>% 
  summarise(n=n())

rm(list=setdiff(ls(), c("dhs", "bord_sf")))
save.image(file='./data for analysis/data_spei_season_length_buffer10.RData')
write.csv(dhs, "./data for analysis/data_spei_season_length_buffer10.csv")



#### Add agro-ecological zones
#### Source: Agro-Ecological Zones for Africa South of the Sahara, https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/M7XIUB

## Import the DHS geolocations
psu <- read.csv("DHS_clust_coord.csv")[-1]
psu <- psu %>% dplyr::select(SurveyId, psu, LATNUM, LONGNUM) %>% distinct()
sp <- psu
coordinates(sp) <- c("LONGNUM", "LATNUM")
proj4string(sp) <- CRS("+proj=longlat +datum=WGS84")

## Retrieve the national boundaries
bound_sub <- getMap(resolution = "low")
unique(as.character(bound_sub@data$NAME))

bound_sub <- bound_sub[bound_sub@data$NAME %in% c(unique(psu$CountryName), 
                                                  "Central African Rep.", 
                                                  "Congo (Kinshasa)",
                                                  "Ivory Coast", 
                                                  "Swaziland"),]

## Retrieve the AEZ data
aez <- read.csv("./Regional_level_covariates/agro-ecological zones/AEZ5 AEZ8 AEZ16 r2.0 - CSV/AEZ5 r2.0 - CSV/AEZ5_CLAS--SSA.csv")
aez <- aez %>% 
  filter(AEZ5_CLAS != "") 
levels <- unique(aez$AEZ5_CLAS)
levels 


ord <- c("Arid",  "Semi-Arid", "Sub-Tropical", "Tropical Highlands", "Sub-Humid", "Humid")
aez$AEZ5_CLAS <- factor(aez$AEZ5_CLAS,levels=rev(ord))
aez <- aez %>% mutate(AEZ5_CLAS = fct_reorder(AEZ5_CLAS, desc(AEZ5_CLAS))) 


colors <- colorRampPalette(c("#d5b093", "#b9b35f", "#82aa54", "#419651", "#30807d", "#144279"))(6)

plot_AEZ <- ggplot() +
  geom_tile(data = aez , aes(x = X, y = Y, fill = AEZ5_CLAS)) + 
  coord_quickmap() +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  theme_minimal() +
  xlab("Longitude") +
  ylab("Latitude") + 
  labs(fill = "Agro-Ecological Zone") +
  xlab("Longitude") +
  ylab("Latitude") +
  coord_equal() +
  theme_bw() + 
  coord_sf() +
  geom_polygon(data = bound_sub, size=0.3, aes(x = long, y = lat, group = group), fill = NA, colour = "#333333") +
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10))

ggsave("./Figures/Fig_SI_AEZ.png", plot_AEZ, width = 8.5, height = 8.5, dpi=700)


## Extract the AEZ data for each PSU location 
aez_rd = raster("./Regional_level_covariates/agro-ecological zones/AEZ5 AEZ8 AEZ16 r2.0 - TIF/AEZ5 r2.0 - TIF/AEZ5_CLAS--SSA.tif")
levels(aez_rd)

aez_pts <- raster::extract(aez_rd,             # raster layer
                           sp,              # SPDF with centroids for buffer
                           #buffer=buff,     # buffer size, units depend on CRS
                           #fun=mean,        # what value to extract
                           na.rm=TRUE,
                           df=TRUE)         # return a dataframe 

aez_pts <- cbind(sp@data$SurveyId, sp@data$psu, aez_pts) # sp@coords,
colnames(aez_pts)[1] ="SurveyId"
colnames(aez_pts)[2] ="psu"
colnames(aez_pts)[4] ="AEZ"
aez_pts <- aez_pts %>% dplyr::select(-c(ID))
aez_pts$AEZ[aez_pts$AEZ == 0] <- "Humid"
aez_pts$AEZ[aez_pts$AEZ == 1] <- "Sub-humid"
aez_pts$AEZ[aez_pts$AEZ == 2] <- "Semi-arid"
aez_pts$AEZ[aez_pts$AEZ == 3] <- "Arid"
aez_pts$AEZ[aez_pts$AEZ == 4] <- "Tropical highlands"
aez_pts$AEZ[aez_pts$AEZ == 5] <- "Sub-tropical"

load('./data for analysis/data_spei_season_length_buffer10.RData')

dhs <- dhs %>% 
  left_join(aez_pts)

rm(list=setdiff(ls(), c("dhs", "bord_sf")))
save.image(file='./data for analysis/data_spei_season_length_buffer10.RData')
write.csv(dhs, "./data for analysis/data_spei_season_length_buffer10.csv")




### Area of land equipped for irrigation  (FAO stats)

library(raster)
library(sp)
library(sf)
library(rworldmap)
library(stars)
#install.packages("viridis")
library(viridis)

world <- getMap(resolution = "low")
world <- world[world@data$REGION %in% "Africa", ]

irrigation <- sf::st_read("./FAOstat/gmia_v5_shp/gmia_v5_aei_pct_cellarea.shp") %>% sf::st_transform(crs = 4326)
projection(irrigation)

## convert irrigation data to sp format
irrigation_sp <- sf:::as_Spatial(irrigation)
projection(irrigation_sp)

## Import the DHS geolocations
psu <- read.csv("DHS_clust_coord.csv")[-1]
psu <- psu %>% dplyr::select(SurveyId, psu, LATNUM, LONGNUM) %>% distinct()
sp <- psu 
coordinates(sp) <- c("LONGNUM", "LATNUM")
proj4string(sp) <- CRS("+proj=longlat +datum=WGS84")
plot(sp)

irrigation_pts <- over(sp, irrigation_sp)
irrigation_pts <- cbind(psu, irrigation_pts) 
irrigation_pts <- irrigation_pts %>% 
  dplyr::select(-ID) %>% 
  mutate(PCT_AEI = ifelse(is.na(PCT_AEI), 0, PCT_AEI))

irrigation_pts_2 <- irrigation_pts %>% 
  filter(PCT_AEI>=0.4)

irrigation_pts$CAT_AEI <- cut(irrigation_pts$PCT_AEI, breaks=c(-1, 0, 1, 5, 10, 25, 50, 75, 100))

irrigation_pts_2 <- irrigation_pts %>% 
  filter(PCT_AEI>0) %>% 
  mutate(CAT_AEI_2 = ifelse(CAT_AEI == "(0,1]", "<1%", CAT_AEI)) %>% 
  mutate(CAT_AEI_2 = ifelse(CAT_AEI == "(1,5]", "1-5%", CAT_AEI_2)) %>% 
  mutate(CAT_AEI_2 = ifelse(CAT_AEI == "(5,10]", "5-10%", CAT_AEI_2)) %>% 
  mutate(CAT_AEI_2 = ifelse(CAT_AEI == "(10,25]", "10-25%", CAT_AEI_2)) %>% 
  mutate(CAT_AEI_2 = ifelse(CAT_AEI == "(25,50]", "25-50%", CAT_AEI_2)) %>% 
  mutate(CAT_AEI_2 = ifelse(CAT_AEI == "(50,75]", "50-75%", CAT_AEI_2)) %>% 
  mutate(CAT_AEI_2 = ifelse(CAT_AEI == "(75,100]", "75-100%", CAT_AEI_2)) 

ord <- c("<1%",  "1-5%", "5-10%", "10-25%", "25-50%", "50-75%", "75-100%")
irrigation_pts_2$CAT_AEI_2 <- factor(irrigation_pts_2$CAT_AEI_2,levels=rev(ord))
irrigation_pts_2 <- irrigation_pts_2 %>% mutate(CAT_AEI_2 = fct_reorder(CAT_AEI_2, desc(CAT_AEI_2))) 

plot_irrigation <- ggplot() + 
  geom_polygon(data = world, linewidth=0.3,
               aes(x = long, y = lat, group = group),
               fill = "lightgray", colour =  "gray65") + 
  geom_point(data=irrigation_pts_2, 
             aes(x=LONGNUM, y=LATNUM, fill=CAT_AEI_2), stroke = 0, col ="transparent",
             pch=21, size=0.85)+
  #scale_fill_manual(values=rev(c("#f72585", "#b5179e", "#7209b7", "#560bad", "#480ca8", "#3a0ca3", "#2D0A7F"))) + 
  scale_fill_viridis(discrete = TRUE, option="magma") +
  geom_polygon(data = world, linewidth=0.3,
               aes(x = long, y = lat, group = group),
               fill = NA, colour = "#333333") +
  theme(legend.position = c(0.3, 0.35),
        legend.direction = "vertical",
        legend.key.height=unit(0.38, "cm")) +
  theme(plot.title = element_text(size=20, face="bold")) +
  labs(fill = "Area equipped for irrigation") +
  xlab("Longitude") +
  ylab("Latitude") +
  coord_equal() +
  theme_bw() + 
  coord_sf() +
  #theme(legend.key.size = unit(6, 'cm')) +
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  guides(fill = guide_legend(override.aes = list(size=3)))

ggsave("./Figures/Fig_SI_AreaIrrigated_by_PSU.png", plot_irrigation, width = 8.5, height = 8.5, dpi=700)


load('./data for analysis/data_spei_season_length_buffer10.RData')

dhs <- dhs %>% 
  left_join(irrigation_pts)

rm(list=setdiff(ls(), c("dhs", "bord_sf")))

save.image(file='./data for analysis/data_spei_season_length_buffer10.RData')
write.csv(dhs, "./data for analysis/data_spei_season_length_buffer10.csv")








