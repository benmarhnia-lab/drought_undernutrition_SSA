###############################################################
###############################################################
#######                                                 ####### 
#######                   PROJECT:                      ####### 
#######         Droughts & child under-nutrition        ####### 
#######                                                 ####### 
#######      CODE: RETRIEVE MONTHLY SPEI DATA           #######
###############################################################
###############################################################
library(cruts)
library(raster)
library(tidyverse)

rm(list =ls())

### Extract SPEI data for each PSU location calculated at different scales (1 to 12 months) and with different buffer zones

setwd("Data")

# load the DHS PSU coordinates
psu <- read.csv("DHS_clust_coord.csv")[-1]
psu <- psu %>% dplyr::select(LATNUM, LONGNUM, psu, SurveyId) %>% distinct()

# create a spatial points dataframe
sp <- psu 
# convert to spatial points
coordinates(sp) <- c("LONGNUM", "LATNUM")
# assign CRS
proj4string(sp) <- CRS("+proj=longlat +datum=WGS84")
plot(sp)




a <- c("spei01", "spei02", "spei03", "spei04", "spei05", "spei06", "spei07", "spei08", "spei09", "spei10", "spei11", "spei12", "spei24", "spei36", "spei48")

#i = "spei06"
for (i in a) {
  load(paste("SPEI_generated/", i, ".RData", sep=""))

  cent_spei <- raster::extract(sd_SPEI,         # raster layer
                               sp,              # SPDF with centroids for buffer
                               df=TRUE)         # return a dataframe? 
  
  cent_spei <- cent_spei[-c(1:949)]
  
  df <- cbind(sp@data, cent_spei)
  #df <- cbind(sp@coords, cent_spei)
  
  df <- df %>%
    gather(key = date, value = spei, -psu, -SurveyId) %>%
    mutate(date = as.Date(substring(date, 2), format = "%Y.%m.%d"),
           year = format(date, format = "%Y"),
           month = format(date, format = "%m")) %>% 
    dplyr::select(-c(date))
  
  df<- df %>% na.omit()

  scale = as.numeric(substr(i, 5, 6))

  write.csv(df, paste("./spei/spei_", scale, ".csv", sep="" ))
}


rm(list = ls)



