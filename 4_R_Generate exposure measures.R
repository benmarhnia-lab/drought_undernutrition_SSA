###############################################################
###############################################################
#######                                                 ####### 
#######                   PROJECT:                      ####### 
#######         Droughts & child under-nutrition        ####### 
#######                                                 ####### 
#######    CODE: Generate seasonal exposure measures    #######
###############################################################
###############################################################
options(scipen=999)
options(digits=5)
library(tidyverse)


## Search "NOTE" for important notes

rm(list =ls())


### NOTE: Select buffer size around the PSU
buff <- 10
df0 <- read.csv(paste("Agri_data_buffer", buff, ".csv", sep=""))[, -1]
psu <- read.csv("DHS_clust_coord.csv")[-1]

df0 <- psu %>% left_join(df0)

## list main crops: 
a <- unique(df0$main.crop)
a <- a[!is.na(a)]
a

df0["Cropland"][is.na(df0["Cropland"])] <- 0
df0["Pasture"][is.na(df0["Pasture"])] <- 0

df0["barley"][is.na(df0["barley"])] <- 0
df0["cassava"][is.na(df0["cassava"])] <- 0
df0["cotton"][is.na(df0["cotton"])] <- 0
df0["groundnut"][is.na(df0["groundnut"])] <- 0
df0["maize"][is.na(df0["maize"])] <- 0
df0["millet"][is.na(df0["millet"])] <- 0
df0["oats"][is.na(df0["oats"])] <- 0
df0["potato"][is.na(df0["potato"])] <- 0
df0["pulsenes"][is.na(df0["pulsenes"])] <- 0
df0["rice"][is.na(df0["rice"])] <- 0
df0["sorghum"][is.na(df0["sorghum"])] <- 0
df0["soybean"][is.na(df0["soybean"])] <- 0
df0["sunflower"][is.na(df0["sunflower"])] <- 0
df0["sweetpotato"][is.na(df0["sweetpotato"])] <- 0
df0["wheat"][is.na(df0["wheat"])] <- 0
df0["yam"][is.na(df0["yam"])] <- 0



## NOTE: names of the following crops in the agri calendar do not match with the crop names in area harvested: 
## groundnuts, potatoes, pulses, rapeseed.winter, soybeans, sweet.potatoes, yams
## Harmonized crop names below
df0 <- df0 %>% 
  rename(
    plant.start.groundnut = plant.start.groundnuts,
    plant.end.groundnut = plant.end.groundnuts,
    harvest.start.groundnut = harvest.start.groundnuts,
    harvest.end.groundnut = harvest.end.groundnuts,
    
    plant.start.potato = plant.start.potatoes,
    plant.end.potato = plant.end.potatoes,
    harvest.start.potato = harvest.start.potatoes,
    harvest.end.potato = harvest.end.potatoes,
    
    plant.start.pulsenes = plant.start.pulses,
    plant.end.pulsenes = plant.end.pulses,
    harvest.start.pulsenes = harvest.start.pulses,
    harvest.end.pulsenes = harvest.end.pulses,
    
    plant.start.soybean = plant.start.soybeans,
    plant.end.soybean = plant.end.soybeans,
    harvest.start.soybean = harvest.start.soybeans,
    harvest.end.soybean = harvest.end.soybeans,
    
    plant.start.sweetpotato = plant.start.sweet.potatoes,
    plant.end.sweetpotato = plant.end.sweet.potatoes,
    harvest.start.sweetpotato = harvest.start.sweet.potatoes,
    harvest.end.sweetpotato = harvest.end.sweet.potatoes,
    
    plant.start.yam = plant.start.yams,
    plant.end.yam = plant.end.yams,
    harvest.start.yam = harvest.start.yams,
    harvest.end.yam = harvest.end.yams
  )



## Loop through every crop and generate crop growing season info (length and end months)
## Note: no observations for rapeseed, sugarbeets, winter oats and winter rye - removed from the analysis
crops = c("barley", "barley.winter", "cassava", "cotton", "groundnut", "maize", "maize.2",
          "millet", "potato", "pulsenes", "rice", "rice.2", "sorghum", "sorghum.2", 
          "soybean", "sunflower", "sweetpotato", "wheat", "wheat.winter", "yam")


#i="barley"


for(i in crops) {  
  
  df0 <-
    df0 %>%
    # add growing season length and end month
    # NOTE: in the code below the crop growing season is defined as the period from the start of planting to the start of harvesting
    # insert the season start date
    mutate(season.start = !!as.name(paste0("plant.start.", i, sep=""))) %>%
    mutate(season.start = format(as.Date(season.start, format = "%j", origin = "1.1.2001"), format = "%m")) %>%
    mutate(season.start = as.numeric(season.start)) %>% 
    # insert the season end date
    mutate(season.end = !!as.name(paste0("harvest.start.", i, sep=""))) %>% 
    mutate(season.end = format(as.Date(season.end, format = "%j", origin = "1.1.2001"), format = "%m")) %>%
    mutate(season.end = as.numeric(season.end)) %>% 
    # estimate the length of the agricultural season (the period between the start of planting and the start of harvesting)
    mutate(season.length = ifelse(season.end < season.start, 12+(season.end-season.start)+1 , (season.end-season.start)+1)) %>% 
    #mutate(season.length = ifelse(round((season.end - season.start)/30) < 0, 12+round((season.end - season.start)/30), round((season.end - season.start)/30))) %>% 
    mutate(!!as.name(paste0("season.length.", i, sep="")) := season.length)  %>% 
    # identify the growing season end month 
    #mutate(season.end = as.numeric(strftime(as.Date(season.end, format = "%j", origin = "1.1.2000"), "%m"))-1) %>% 
    #mutate(season.end = ifelse(season.end==0, 12, season.end)) %>% 
    mutate(!!as.name(paste0("season.end.", i, sep="")) := season.end)  
  
}



unique(df0$season.length.cassava)

df0 <- df0 %>% 
  mutate(season.length.cassava = ifelse(season.length.cassava == 2, 12, season.length.cassava))


## NOTE: No observations or negligible contributions of oats, sugarbeets, rapeseed and rye - removed from analysis
#df0[c(grep("rapeseed", names(df0), value=TRUE))]

## NOTE: Cropland not necessarily equal to crop.tot, different crops may be grown on the same land
#plot(df0$Cropland, df0$crop.tot)

## Generate weights for 15 crops 
## NOTE: oats, sugarbeets, rapeseed and rye are removed because they have negligible contribution to crops; the max harvest area is 0 
## Generate weights for crops with dual seasons; assigning same weights for main and second seasons
df0 <- df0 %>% mutate(barley.winter = ifelse(!is.na(season.length.barley.winter), barley, 0)) 
df0 <- df0 %>% mutate(wheat.winter = ifelse(!is.na(season.length.wheat.winter), wheat, 0)) 
df0 <- df0 %>% mutate(maize.2 = ifelse(!is.na(season.length.maize.2), maize, 0)) 
df0 <- df0 %>% mutate(rice.2 = ifelse(!is.na(season.length.rice.2), rice, 0)) 
df0 <- df0 %>% mutate(sorghum.2 = ifelse(!is.na(season.length.sorghum.2), sorghum, 0)) 


## NOTE: Calendar dates not available for crops with negligible contribution; Remove crops with missing calendar dates
df0 <- df0 %>% mutate(barley = ifelse(is.na(season.length.barley), 0, barley))
df0 <- df0 %>% mutate(barley.winter = ifelse(is.na(season.length.barley.winter), 0, barley.winter)) 
df0 <- df0 %>% mutate(cassava = ifelse(is.na(season.length.cassava), 0, cassava)) 
df0 <- df0 %>% mutate(cotton = ifelse(is.na(season.length.cotton), 0, cotton)) 
df0 <- df0 %>% mutate(groundnut = ifelse(is.na(season.length.groundnut), 0, groundnut)) 
df0 <- df0 %>% mutate(maize = ifelse(is.na(season.length.maize), 0, maize)) 
df0 <- df0 %>% mutate(maize.2 = ifelse(is.na(season.length.maize.2), 0, maize.2)) 
df0 <- df0 %>% mutate(millet = ifelse(is.na(season.length.millet), 0, millet)) 
df0 <- df0 %>% mutate(potato = ifelse(is.na(season.length.potato), 0, potato)) 
df0 <- df0 %>% mutate(pulsenes = ifelse(is.na(season.length.pulsenes), 0, pulsenes)) 
df0 <- df0 %>% mutate(rice = ifelse(is.na(season.length.rice), 0, rice)) 
df0 <- df0 %>% mutate(rice.2 = ifelse(is.na(season.length.rice.2), 0, rice.2)) 
df0 <- df0 %>% mutate(sorghum = ifelse(is.na(season.length.sorghum), 0, sorghum)) 
df0 <- df0 %>% mutate(sorghum.2 = ifelse(is.na(season.length.sorghum.2), 0, sorghum.2)) 
df0 <- df0 %>% mutate(soybean = ifelse(is.na(season.length.soybean), 0, soybean)) 
df0 <- df0 %>% mutate(sunflower = ifelse(is.na(season.length.sunflower), 0, sunflower)) 
df0 <- df0 %>% mutate(sweetpotato = ifelse(is.na(season.length.sweetpotato), 0, sweetpotato)) 
df0 <- df0 %>% mutate(wheat = ifelse(is.na(season.length.wheat), 0, wheat)) 
df0 <- df0 %>% mutate(wheat.winter = ifelse(is.na(season.length.wheat.winter), 0, wheat.winter)) 
df0 <- df0 %>% mutate(yam = ifelse(is.na(season.length.yam), 0, yam)) 

sum(df0$barley)



df0 <- df0 %>% 
  mutate(crop.tot15 = barley + barley.winter + cassava + cotton + groundnut + maize + maize.2 + millet + potato + pulsenes + rice + rice.2 + sorghum + sorghum.2 + soybean + sunflower + sweetpotato + wheat + wheat.winter + yam) %>% 
  mutate(barley.wt      = barley/crop.tot15,
         barley.winter.wt = barley.winter/crop.tot15,
         cassava.wt     = cassava/crop.tot15,
         cotton.wt      = cotton/crop.tot15,
         groundnut.wt   = groundnut/crop.tot15,
         maize.wt       = maize/crop.tot15,
         maize.2.wt     = maize.2/crop.tot15,
         millet.wt      = millet/crop.tot15,
         potato.wt      = potato/crop.tot15,
         pulsenes.wt    = pulsenes/crop.tot15,
         rice.wt        = rice/crop.tot15,
         rice.2.wt      = rice.2/crop.tot15,
         sorghum.wt     = sorghum/crop.tot15,
         sorghum.2.wt   = sorghum.2/crop.tot15,
         soybean.wt     = soybean/crop.tot15,
         sunflower.wt   = sunflower/crop.tot15,
         sweetpotato.wt = sweetpotato/crop.tot15,
         wheat.wt       = wheat/crop.tot15,
         wheat.winter.wt = wheat.winter/crop.tot15,
         yam.wt         = yam/crop.tot15) 



df0 <- df0 %>% 
  mutate(main.crop=ifelse(crop.tot15==0, NA, main.crop)) 
  


df0 <- df0 %>% 
  mutate(barley = ifelse(barley==0, NA, barley)) %>% 
  mutate(barley.winter = ifelse(barley.winter==0, NA, barley.winter)) %>% 
  mutate(cassava = ifelse(cassava==0, NA, cassava)) %>% 
  mutate(cotton = ifelse(cotton==0, NA, cotton)) %>% 
  mutate(groundnut = ifelse(groundnut==0, NA, groundnut)) %>% 
  mutate(maize = ifelse(maize==0, NA, maize)) %>% 
  mutate(maize.2 = ifelse(maize.2==0, NA, maize.2)) %>% 
  mutate(millet = ifelse(millet==0, NA, millet)) %>% 
  mutate(potato = ifelse(potato==0, NA, potato)) %>% 
  mutate(pulsenes = ifelse(pulsenes==0, NA, pulsenes)) %>% 
  mutate(rice = ifelse(rice==0, NA, rice)) %>% 
  mutate(rice.2 = ifelse(rice.2==0, NA, rice.2)) %>% 
  mutate(sorghum = ifelse(sorghum==0, NA, sorghum)) %>% 
  mutate(sorghum.2 = ifelse(sorghum.2==0, NA, sorghum.2)) %>% 
  mutate(soybean = ifelse(soybean==0, NA, soybean)) %>% 
  mutate(sunflower = ifelse(sunflower==0, NA, sunflower)) %>% 
  mutate(sweetpotato = ifelse(sweetpotato==0, NA, sweetpotato)) %>% 
  mutate(wheat = ifelse(wheat==0, NA, wheat)) %>% 
  mutate(wheat.winter = ifelse(wheat.winter==0, NA, wheat.winter)) %>% 
  mutate(yam = ifelse(yam==0, NA, yam)) %>% 
  mutate(barley.wt = ifelse(barley.wt==0, NA, barley.wt)) %>% 
  mutate(barley.winter.wt = ifelse(barley.winter.wt==0, NA, barley.winter.wt)) %>% 
  mutate(cassava.wt = ifelse(cassava.wt==0, NA, cassava.wt)) %>% 
  mutate(cotton.wt = ifelse(cotton.wt==0, NA, cotton.wt)) %>% 
  mutate(groundnut.wt = ifelse(groundnut.wt==0, NA, groundnut.wt)) %>% 
  mutate(maize.wt = ifelse(maize.wt==0, NA, maize.wt)) %>% 
  mutate(maize.2.wt = ifelse(maize.2.wt==0, NA, maize.2.wt)) %>% 
  mutate(millet.wt = ifelse(millet.wt==0, NA, millet.wt)) %>% 
  mutate(potato.wt = ifelse(potato.wt==0, NA, potato.wt)) %>% 
  mutate(pulsenes.wt = ifelse(pulsenes.wt==0, NA, pulsenes.wt)) %>% 
  mutate(rice.wt = ifelse(rice.wt==0, NA, rice.wt)) %>% 
  mutate(rice.2.wt = ifelse(rice.2.wt==0, NA, rice.2.wt)) %>% 
  mutate(sorghum.wt = ifelse(sorghum.wt==0, NA, sorghum.wt)) %>% 
  mutate(sorghum.2.wt = ifelse(sorghum.2.wt==0, NA, sorghum.2.wt)) %>% 
  mutate(soybean.wt = ifelse(soybean.wt==0, NA, soybean.wt)) %>% 
  mutate(sunflower.wt = ifelse(sunflower.wt==0, NA, sunflower.wt)) %>% 
  mutate(sweetpotato.wt = ifelse(sweetpotato.wt==0, NA, sweetpotato.wt)) %>% 
  mutate(wheat.wt = ifelse(wheat.wt==0, NA, wheat.wt)) %>% 
  mutate(wheat.winter.wt = ifelse(wheat.winter.wt==0, NA, wheat.winter.wt)) %>% 
  mutate(yam.wt = ifelse(yam.wt==0, NA, yam.wt)) %>%   
  mutate(season.length.barley = ifelse(is.na(barley), NA, season.length.barley)) %>% 
  mutate(season.length.barley.winter = ifelse(is.na(barley.winter), NA, season.length.barley.winter)) %>% 
  mutate(season.length.cassava = ifelse(is.na(cassava), NA, season.length.cassava)) %>% 
  mutate(season.length.cotton = ifelse(is.na(cotton), NA, season.length.cotton)) %>% 
  mutate(season.length.groundnut = ifelse(is.na(groundnut), NA, season.length.groundnut)) %>% 
  mutate(season.length.maize = ifelse(is.na(maize), NA, season.length.maize)) %>% 
  mutate(season.length.maize.2 = ifelse(is.na(maize.2), NA, season.length.maize.2)) %>% 
  mutate(season.length.millet = ifelse(is.na(millet), NA, season.length.millet)) %>% 
  mutate(season.length.potato = ifelse(is.na(potato), NA, season.length.potato)) %>% 
  mutate(season.length.pulsenes = ifelse(is.na(pulsenes), NA, season.length.pulsenes)) %>% 
  mutate(season.length.rice = ifelse(is.na(rice), NA, season.length.rice)) %>% 
  mutate(season.length.rice.2 = ifelse(is.na(rice.2), NA, season.length.rice.2)) %>% 
  mutate(season.length.sorghum = ifelse(is.na(sorghum), NA, season.length.sorghum)) %>% 
  mutate(season.length.sorghum.2 = ifelse(is.na(sorghum.2), NA, season.length.sorghum.2)) %>% 
  mutate(season.length.soybean = ifelse(is.na(soybean), NA, season.length.soybean)) %>% 
  mutate(season.length.sunflower = ifelse(is.na(sunflower), NA, season.length.sunflower)) %>% 
  mutate(season.length.sweetpotato = ifelse(is.na(sweetpotato), NA, season.length.sweetpotato)) %>% 
  mutate(season.length.wheat = ifelse(is.na(wheat), NA, season.length.wheat)) %>% 
  mutate(season.length.wheat.winter = ifelse(is.na(wheat.winter), NA, season.length.wheat.winter)) %>% 
  mutate(season.length.yam = ifelse(is.na(yam), NA, season.length.yam))
  

## Generate summary statistics for growing season length for each crop
#install.packages("vtable")
library(vtable)
varlist = grep("season.length.", names(df0), value=TRUE)
st(df0, vars = varlist,
   out = 'csv') %>% 
   write.csv("./Results/Descriptive_crop_seasons.csv")


## Generate exposure to droughts during the growing season for each crop and every age period of the child
#i = "cassava"
#j = 6


df0 <- df0 %>% 
  mutate(ID = row_number()) 

#df0 <- df0 %>% 
#  filter(CountryName == "Angola")
#unique(df0$main.crop)  

i = "maize"

for(i in crops) { 
  
  df2 <- df0 %>% 
    dplyr::select(ID, SurveyId, psu, intYr, intMo, intCMC, birthYr, birthMo, birthCMC, paste0("season.length.", i, sep=""), paste0("season.end.", i, sep="")) %>% 
    mutate(season.length = !!as.name(paste0("season.length.", i, sep=""))) %>% 
    mutate(season.length = as.numeric(season.length)) %>% 
    drop_na %>% 
    mutate(season.end = !!as.name(paste0("season.end.", i, sep=""))) %>% 
    mutate(season.end = as.numeric(season.end)) %>% 
    drop_na() %>% 
    mutate(lag0 = intYr,
           lag1 = intYr - 1,
           lag2 = intYr - 2,
           lag3 = intYr - 3
           
           ) %>%
    gather(key = lag, value = lag_year, -c(ID:season.end)) 
  
  
  df4 <- data.frame(ID=integer(),
                    SurveyId=character(),
                    psu=integer(),
                    intYr=integer(),
                    intMo=integer(),
                    intCMC=integer(),
                    birthYr=integer(),
                    birthMo=integer(),
                    birthCMC=integer(),
                    stringsAsFactors=FALSE)
  
  ## NOTE: scale is the SPEI scale which is equivalent to the growing season length
  scale = unique(sort(df2$season.length))  
  
  for(j in scale) { 
    
  df3 <- df2 %>% subset(season.length==j)  
    
  spei <- read.csv(paste("./spei/spei_", j, ".csv", sep=""))[,-1]
  
  df3 <- df3 %>% 
    left_join(spei, by = c("SurveyId" = "SurveyId", "psu" = "psu", "lag_year" = "year", "season.end" = "month")) %>% 
    mutate(lag_month = season.end) %>% 
    mutate(lag_cmc = (lag_year-1900)*12+lag_month) %>% 
    # generate help year, which is equal to year of birth if month of birth is before the start of the growing season and the year after birth otherwise
    mutate(helpYr = ifelse(birthMo>season.end, birthYr+1, birthYr)) %>% 
    mutate(spei_utero    = ifelse(helpYr-1 == lag_year & lag_cmc <= intCMC , spei, NA)) %>%
    mutate(spei_infancy  = ifelse(helpYr   == lag_year & lag_cmc <= intCMC , spei, NA)) %>%
    mutate(spei_age1     = ifelse(helpYr+1 == lag_year & lag_cmc <= intCMC , spei, NA)) %>% 
    mutate(spei_age2     = ifelse(helpYr+2 == lag_year & lag_cmc <= intCMC , spei, NA)) %>% 
    mutate(spei_age3     = ifelse(helpYr+3 == lag_year & lag_cmc <= intCMC , spei, NA)) %>% 
    mutate(spei_age4     = ifelse(helpYr+4 == lag_year & lag_cmc <= intCMC , spei, NA)) %>% 
    # generate lag periods from the year of birth
    mutate(spei_lag1    = ifelse(helpYr-1 == lag_year & lag_cmc <= intCMC , spei, NA)) %>%
    mutate(spei_lag2    = ifelse(helpYr-2 == lag_year & lag_cmc <= intCMC , spei, NA)) %>%
    mutate(spei_lag3    = ifelse(helpYr-3 == lag_year & lag_cmc <= intCMC , spei, NA)) %>%
    arrange(SurveyId, psu, intYr, intMo, birthYr, birthMo) %>% 
    filter(!is.na(spei_utero) | !is.na(spei_infancy) | !is.na(spei_age1) | !is.na(spei_age2) | !is.na(spei_age3) | !is.na(spei_age4)
           | !is.na(spei_lag1) | !is.na(spei_lag2) | !is.na(spei_lag3) ) %>% 
    dplyr::select(-c(lag:helpYr)) %>% 
    gather(key = spei_age, value = spei, -c(ID:season.end)) %>% 
    na.omit() %>% 
    mutate(spei_age = paste(spei_age, "_", i, sep="")) %>% 
    spread(spei_age, spei) %>% 
    dplyr::select(-c(season.end, season.length, paste0("season.length.", i, sep=""), paste0("season.end.", i, sep="")))
  
  
  df4 <- dplyr::bind_rows(df4, df3)
  
  } 
  
  df0 <- df0 %>% left_join(df4)
} 



## Generate drought exposure during the main crop growing season
df0 <- df0 %>% 
  mutate(spei_utero_main = ifelse(main.crop == "barley", spei_utero_barley,
                           ifelse(main.crop == "cassava", spei_utero_cassava,
                           ifelse(main.crop == "cotton", spei_utero_cotton,
                           ifelse(main.crop == "groundnut", spei_utero_groundnut,
                           ifelse(main.crop == "maize", spei_utero_maize,
                           ifelse(main.crop == "millet", spei_utero_millet,
                           ifelse(main.crop == "oats", spei_utero_oats,
                           ifelse(main.crop == "potato", spei_utero_potato,
                           ifelse(main.crop == "pulsenes", spei_utero_pulsenes,
                           ifelse(main.crop == "rice", spei_utero_rice,
                           ifelse(main.crop == "sorghum", spei_utero_sorghum,
                           ifelse(main.crop == "soybean", spei_utero_soybean,
                           ifelse(main.crop == "sunflower", spei_utero_sunflower,
                           ifelse(main.crop == "sweetpotato", spei_utero_sweetpotato,
                           ifelse(main.crop == "wheat", spei_utero_wheat,
                           ifelse(main.crop == "yam", spei_utero_yam, 
                           NA))))))))))))))))) %>%
  mutate(spei_infancy_main = ifelse(main.crop == "barley", spei_infancy_barley,
                             ifelse(main.crop == "cassava", spei_infancy_cassava,
                             ifelse(main.crop == "cotton", spei_infancy_cotton,
                             ifelse(main.crop == "groundnut", spei_infancy_groundnut,
                             ifelse(main.crop == "maize", spei_infancy_maize,
                             ifelse(main.crop == "millet", spei_infancy_millet,
                             ifelse(main.crop == "oats", spei_infancy_oats,
                             ifelse(main.crop == "potato", spei_infancy_potato,
                             ifelse(main.crop == "pulsenes", spei_infancy_pulsenes,
                             ifelse(main.crop == "rice", spei_infancy_rice,
                             ifelse(main.crop == "sorghum", spei_infancy_sorghum,
                             ifelse(main.crop == "soybean", spei_infancy_soybean,
                             ifelse(main.crop == "sunflower", spei_infancy_sunflower,
                             ifelse(main.crop == "sweetpotato", spei_infancy_sweetpotato,
                             ifelse(main.crop == "wheat", spei_infancy_wheat,
                             ifelse(main.crop == "yam", spei_infancy_yam, 
                             NA))))))))))))))))) %>% 
  mutate(spei_age1_main = ifelse(main.crop == "barley", spei_age1_barley,
                          ifelse(main.crop == "cassava", spei_age1_cassava,
                          ifelse(main.crop == "cotton", spei_age1_cotton,
                          ifelse(main.crop == "groundnut", spei_age1_groundnut,
                          ifelse(main.crop == "maize", spei_age1_maize,
                          ifelse(main.crop == "millet", spei_age1_millet,
                          ifelse(main.crop == "oats", spei_age1_oats,
                          ifelse(main.crop == "potato", spei_age1_potato,
                          ifelse(main.crop == "pulsenes", spei_age1_pulsenes,
                          ifelse(main.crop == "rice", spei_age1_rice,
                          ifelse(main.crop == "sorghum", spei_age1_sorghum,
                          ifelse(main.crop == "soybean", spei_age1_soybean,
                          ifelse(main.crop == "sunflower", spei_age1_sunflower,
                          ifelse(main.crop == "sweetpotato", spei_age1_sweetpotato,
                          ifelse(main.crop == "wheat", spei_age1_wheat,
                          ifelse(main.crop == "yam", spei_age1_yam, 
                          NA))))))))))))))))) %>% 
  mutate(spei_age2_main = ifelse(main.crop == "barley", spei_age2_barley,
                          ifelse(main.crop == "cassava", spei_age2_cassava,
                          ifelse(main.crop == "cotton", spei_age2_cotton,
                          ifelse(main.crop == "groundnut", spei_age2_groundnut,
                          ifelse(main.crop == "maize", spei_age2_maize,
                          ifelse(main.crop == "millet", spei_age2_millet,
                          ifelse(main.crop == "oats", spei_age2_oats,
                          ifelse(main.crop == "potato", spei_age2_potato,
                          ifelse(main.crop == "pulsenes", spei_age2_pulsenes,
                          ifelse(main.crop == "rice", spei_age2_rice,
                          ifelse(main.crop == "sorghum", spei_age2_sorghum,
                          ifelse(main.crop == "soybean", spei_age2_soybean,
                          ifelse(main.crop == "sunflower", spei_age2_sunflower,
                          ifelse(main.crop == "sweetpotato", spei_age2_sweetpotato,
                          ifelse(main.crop == "wheat", spei_age2_wheat,
                          ifelse(main.crop == "yam", spei_age2_yam, 
                          NA))))))))))))))))) %>% 
  mutate(spei_age3_main = ifelse(main.crop == "barley", spei_age3_barley,
                          ifelse(main.crop == "cassava", spei_age3_cassava,
                          ifelse(main.crop == "cotton", spei_age3_cotton,
                          ifelse(main.crop == "groundnut", spei_age3_groundnut,
                          ifelse(main.crop == "maize", spei_age3_maize,
                          ifelse(main.crop == "millet", spei_age3_millet,
                          ifelse(main.crop == "oats", spei_age3_oats,
                          ifelse(main.crop == "potato", spei_age3_potato,
                          ifelse(main.crop == "pulsenes", spei_age3_pulsenes,
                          ifelse(main.crop == "rice", spei_age3_rice,
                          ifelse(main.crop == "sorghum", spei_age3_sorghum,
                          ifelse(main.crop == "soybean", spei_age3_soybean,
                          ifelse(main.crop == "sunflower", spei_age3_sunflower,
                          ifelse(main.crop == "sweetpotato", spei_age3_sweetpotato,
                          ifelse(main.crop == "wheat", spei_age3_wheat,
                          ifelse(main.crop == "yam", spei_age3_yam, 
                          NA))))))))))))))))) %>% 
  mutate(spei_age4_main = ifelse(main.crop == "barley", spei_age4_barley,
                          ifelse(main.crop == "cassava", spei_age4_cassava,
                          ifelse(main.crop == "cotton", spei_age4_cotton,
                          ifelse(main.crop == "groundnut", spei_age4_groundnut,
                          ifelse(main.crop == "maize", spei_age4_maize,
                          ifelse(main.crop == "millet", spei_age4_millet,
                          ifelse(main.crop == "oats", spei_age4_oats,
                          ifelse(main.crop == "potato", spei_age4_potato,
                          ifelse(main.crop == "pulsenes", spei_age4_pulsenes,
                          ifelse(main.crop == "rice", spei_age4_rice,
                          ifelse(main.crop == "sorghum", spei_age4_sorghum,
                          ifelse(main.crop == "soybean", spei_age4_soybean,
                          ifelse(main.crop == "sunflower", spei_age4_sunflower,
                          ifelse(main.crop == "sweetpotato", spei_age4_sweetpotato,
                          ifelse(main.crop == "wheat", spei_age4_wheat,
                          ifelse(main.crop == "yam", spei_age4_yam, 
                          NA))))))))))))))))) 


df0 <- df0 %>% 
  mutate(spei_lag1_main = ifelse(main.crop == "barley", spei_lag1_barley,
                                 ifelse(main.crop == "cassava", spei_lag1_cassava,
                                 ifelse(main.crop == "cotton", spei_lag1_cotton,
                                 ifelse(main.crop == "groundnut", spei_lag1_groundnut,
                                 ifelse(main.crop == "maize", spei_lag1_maize,
                                 ifelse(main.crop == "millet", spei_lag1_millet,
                                 ifelse(main.crop == "oats", spei_lag1_oats,
                                 ifelse(main.crop == "potato", spei_lag1_potato,
                                 ifelse(main.crop == "pulsenes", spei_lag1_pulsenes,
                                 ifelse(main.crop == "rice", spei_lag1_rice,
                                 ifelse(main.crop == "sorghum", spei_lag1_sorghum,
                                 ifelse(main.crop == "soybean", spei_lag1_soybean,
                                 ifelse(main.crop == "sunflower", spei_lag1_sunflower,
                                 ifelse(main.crop == "sweetpotato", spei_lag1_sweetpotato,
                                 ifelse(main.crop == "wheat", spei_lag1_wheat,
                                 ifelse(main.crop == "yam", spei_lag1_yam, 
                                 NA))))))))))))))))) %>% 
  mutate(spei_lag2_main = ifelse(main.crop == "barley", spei_lag2_barley,
                                 ifelse(main.crop == "cassava", spei_lag2_cassava,
                                 ifelse(main.crop == "cotton", spei_lag2_cotton,
                                 ifelse(main.crop == "groundnut", spei_lag2_groundnut,
                                 ifelse(main.crop == "maize", spei_lag2_maize,
                                 ifelse(main.crop == "millet", spei_lag2_millet,
                                 ifelse(main.crop == "oats", spei_lag2_oats,
                                 ifelse(main.crop == "potato", spei_lag2_potato,
                                 ifelse(main.crop == "pulsenes", spei_lag2_pulsenes,
                                 ifelse(main.crop == "rice", spei_lag2_rice,
                                 ifelse(main.crop == "sorghum", spei_lag2_sorghum,
                                 ifelse(main.crop == "soybean", spei_lag2_soybean,
                                 ifelse(main.crop == "sunflower", spei_lag2_sunflower,
                                 ifelse(main.crop == "sweetpotato", spei_lag2_sweetpotato,
                                 ifelse(main.crop == "wheat", spei_lag2_wheat,
                                 ifelse(main.crop == "yam", spei_lag2_yam, 
                                 NA))))))))))))))))) %>% 
  mutate(spei_lag3_main = ifelse(main.crop == "barley", spei_lag3_barley,
                                 ifelse(main.crop == "cassava", spei_lag3_cassava,
                                 ifelse(main.crop == "cotton", spei_lag3_cotton,
                                 ifelse(main.crop == "groundnut", spei_lag3_groundnut,
                                 ifelse(main.crop == "maize", spei_lag3_maize,
                                 ifelse(main.crop == "millet", spei_lag3_millet,
                                 ifelse(main.crop == "oats", spei_lag3_oats,
                                 ifelse(main.crop == "potato", spei_lag3_potato,
                                 ifelse(main.crop == "pulsenes", spei_lag3_pulsenes,
                                 ifelse(main.crop == "rice", spei_lag3_rice,
                                 ifelse(main.crop == "sorghum", spei_lag3_sorghum,
                                 ifelse(main.crop == "soybean", spei_lag3_soybean,
                                 ifelse(main.crop == "sunflower", spei_lag3_sunflower,
                                 ifelse(main.crop == "sweetpotato", spei_lag3_sweetpotato,
                                 ifelse(main.crop == "wheat", spei_lag3_wheat,
                                 ifelse(main.crop == "yam", spei_lag3_yam, 
                                 NA)))))))))))))))))

  
  
  
  

## Generate growing season length for the main crop growing season
df0 <- df0 %>% 
  mutate(season.length.main = ifelse(main.crop == "barley", season.length.barley,
                              ifelse(main.crop == "cassava", season.length.cassava,
                              ifelse(main.crop == "cotton", season.length.cotton,
                              ifelse(main.crop == "groundnut", season.length.groundnut,
                              ifelse(main.crop == "maize", season.length.maize,
                              ifelse(main.crop == "millet", season.length.millet,
                              ifelse(main.crop == "oats", season.length.oats,
                              ifelse(main.crop == "potato", season.length.potato,
                              ifelse(main.crop == "pulsenes", season.length.pulsenes,
                              ifelse(main.crop == "rice", season.length.rice,
                              ifelse(main.crop == "sorghum", season.length.sorghum,
                              ifelse(main.crop == "soybean", season.length.soybean,
                              ifelse(main.crop == "sunflower", season.length.sunflower,
                              ifelse(main.crop == "sweetpotato", season.length.sweetpotato,
                              ifelse(main.crop == "wheat", season.length.wheat,
                              ifelse(main.crop == "yam", season.length.yam, 
                              NA))))))))))))))))) 



## Generate weighted drought exposure during all crop growing seasons
df0 <- df0 %>% 
  # in-utero
  mutate(spei_utero_barley_wt      = spei_utero_barley*barley.wt) %>% 
  mutate(spei_utero_barley.winter_wt= spei_utero_barley.winter*barley.winter.wt) %>% 
  mutate(spei_utero_cassava_wt     = spei_utero_cassava*cassava.wt) %>% 
  mutate(spei_utero_cotton_wt      = spei_utero_cotton*cotton.wt) %>% 
  mutate(spei_utero_groundnut_wt   = spei_utero_groundnut*groundnut.wt) %>% 
  mutate(spei_utero_maize_wt       = spei_utero_maize*maize.wt) %>% 
  mutate(spei_utero_maize.2_wt     = spei_utero_maize.2*maize.2.wt) %>% 
  mutate(spei_utero_millet_wt      = spei_utero_millet*millet.wt) %>% 
  #mutate(spei_utero_oats_wt        = spei_utero_oats*oats.wt) %>% 
  mutate(spei_utero_potato_wt      = spei_utero_potato*potato.wt) %>% 
  mutate(spei_utero_pulsenes_wt    = spei_utero_pulsenes*pulsenes.wt) %>% 
  mutate(spei_utero_rice_wt        = spei_utero_rice*rice.wt) %>% 
  mutate(spei_utero_rice.2_wt      = spei_utero_rice.2*rice.2.wt) %>% 
  mutate(spei_utero_sorghum_wt     = spei_utero_sorghum*sorghum.wt) %>% 
  mutate(spei_utero_sorghum.2_wt   = spei_utero_sorghum.2*sorghum.2.wt) %>% 
  mutate(spei_utero_soybean_wt     = spei_utero_soybean*soybean.wt) %>% 
  mutate(spei_utero_sunflower_wt   = spei_utero_sunflower*sunflower.wt) %>% 
  mutate(spei_utero_sweetpotato_wt = spei_utero_sweetpotato*sweetpotato.wt) %>% 
  mutate(spei_utero_wheat_wt       = spei_utero_wheat*wheat.wt) %>% 
  mutate(spei_utero_wheat.winter_wt= spei_utero_wheat.winter*wheat.winter.wt) %>% 
  mutate(spei_utero_yam_wt         = spei_utero_yam*yam.wt) %>% 
  # infancy
  mutate(spei_infancy_barley_wt      = spei_infancy_barley*barley.wt) %>%
  mutate(spei_infancy_barley.winter_wt= spei_infancy_barley.winter*barley.winter.wt) %>% 
  mutate(spei_infancy_cassava_wt     = spei_infancy_cassava*cassava.wt) %>% 
  mutate(spei_infancy_cotton_wt      = spei_infancy_cotton*cotton.wt) %>% 
  mutate(spei_infancy_groundnut_wt   = spei_infancy_groundnut*groundnut.wt) %>% 
  mutate(spei_infancy_maize_wt       = spei_infancy_maize*maize.wt) %>% 
  mutate(spei_infancy_maize.2_wt     = spei_infancy_maize.2*maize.2.wt) %>% 
  mutate(spei_infancy_millet_wt      = spei_infancy_millet*millet.wt) %>% 
  #mutate(spei_infancy_oats_wt        = spei_infancy_oats*oats.wt) %>% 
  mutate(spei_infancy_potato_wt      = spei_infancy_potato*potato.wt) %>% 
  mutate(spei_infancy_pulsenes_wt    = spei_infancy_pulsenes*pulsenes.wt) %>% 
  mutate(spei_infancy_rice_wt        = spei_infancy_rice*rice.wt) %>% 
  mutate(spei_infancy_rice.2_wt      = spei_infancy_rice.2*rice.2.wt) %>% 
  mutate(spei_infancy_sorghum_wt     = spei_infancy_sorghum*sorghum.wt) %>%
  mutate(spei_infancy_sorghum.2_wt   = spei_infancy_sorghum.2*sorghum.2.wt) %>% 
  mutate(spei_infancy_soybean_wt     = spei_infancy_soybean*soybean.wt) %>% 
  mutate(spei_infancy_sunflower_wt   = spei_infancy_sunflower*sunflower.wt) %>% 
  mutate(spei_infancy_sweetpotato_wt = spei_infancy_sweetpotato*sweetpotato.wt) %>% 
  mutate(spei_infancy_wheat_wt       = spei_infancy_wheat*wheat.wt) %>% 
  mutate(spei_infancy_wheat.winter_wt= spei_infancy_wheat.winter*wheat.winter.wt) %>% 
  mutate(spei_infancy_yam_wt         = spei_infancy_yam*yam.wt) %>% 
  # age 1
  mutate(spei_age1_barley_wt      = spei_age1_barley*barley.wt) %>%
  mutate(spei_age1_barley.winter_wt= spei_age1_barley.winter*barley.winter.wt) %>% 
  mutate(spei_age1_cassava_wt     = spei_age1_cassava*cassava.wt) %>% 
  mutate(spei_age1_cotton_wt      = spei_age1_cotton*cotton.wt) %>% 
  mutate(spei_age1_groundnut_wt   = spei_age1_groundnut*groundnut.wt) %>% 
  mutate(spei_age1_maize_wt       = spei_age1_maize*maize.wt) %>% 
  mutate(spei_age1_maize.2_wt     = spei_age1_maize.2*maize.2.wt) %>% 
  mutate(spei_age1_millet_wt      = spei_age1_millet*millet.wt) %>% 
  #mutate(spei_age1_oats_wt        = spei_age1_oats*oats.wt) %>% 
  mutate(spei_age1_potato_wt      = spei_age1_potato*potato.wt) %>% 
  mutate(spei_age1_pulsenes_wt    = spei_age1_pulsenes*pulsenes.wt) %>% 
  mutate(spei_age1_rice_wt        = spei_age1_rice*rice.wt) %>%
  mutate(spei_age1_rice.2_wt      = spei_age1_rice.2*rice.2.wt) %>% 
  mutate(spei_age1_sorghum_wt     = spei_age1_sorghum*sorghum.wt) %>% 
  mutate(spei_age1_sorghum.2_wt   = spei_age1_sorghum.2*sorghum.2.wt) %>% 
  mutate(spei_age1_soybean_wt     = spei_age1_soybean*soybean.wt) %>% 
  mutate(spei_age1_sunflower_wt   = spei_age1_sunflower*sunflower.wt) %>% 
  mutate(spei_age1_sweetpotato_wt = spei_age1_sweetpotato*sweetpotato.wt) %>% 
  mutate(spei_age1_wheat_wt       = spei_age1_wheat*wheat.wt) %>%
  mutate(spei_age1_wheat.winter_wt= spei_age1_wheat.winter*wheat.winter.wt) %>% 
  mutate(spei_age1_yam_wt         = spei_age1_yam*yam.wt) %>% 
  # age 2
  mutate(spei_age2_barley_wt      = spei_age2_barley*barley.wt) %>% 
  mutate(spei_age2_barley.winter_wt= spei_age2_barley.winter*barley.winter.wt) %>% 
  mutate(spei_age2_cassava_wt     = spei_age2_cassava*cassava.wt) %>% 
  mutate(spei_age2_cotton_wt      = spei_age2_cotton*cotton.wt) %>% 
  mutate(spei_age2_groundnut_wt   = spei_age2_groundnut*groundnut.wt) %>% 
  mutate(spei_age2_maize_wt       = spei_age2_maize*maize.wt) %>% 
  mutate(spei_age2_maize.2_wt     = spei_age2_maize.2*maize.2.wt) %>% 
  mutate(spei_age2_millet_wt      = spei_age2_millet*millet.wt) %>% 
  #mutate(spei_age2_oats_wt        = spei_age2_oats*oats.wt) %>% 
  mutate(spei_age2_potato_wt      = spei_age2_potato*potato.wt) %>% 
  mutate(spei_age2_pulsenes_wt    = spei_age2_pulsenes*pulsenes.wt) %>% 
  mutate(spei_age2_rice_wt        = spei_age2_rice*rice.wt) %>% 
  mutate(spei_age2_rice.2_wt      = spei_age2_rice.2*rice.2.wt) %>% 
  mutate(spei_age2_sorghum_wt     = spei_age2_sorghum*sorghum.wt) %>% 
  mutate(spei_age2_sorghum.2_wt   = spei_age2_sorghum.2*sorghum.2.wt) %>% 
  mutate(spei_age2_soybean_wt     = spei_age2_soybean*soybean.wt) %>% 
  mutate(spei_age2_sunflower_wt   = spei_age2_sunflower*sunflower.wt) %>% 
  mutate(spei_age2_sweetpotato_wt = spei_age2_sweetpotato*sweetpotato.wt) %>% 
  mutate(spei_age2_wheat_wt       = spei_age2_wheat*wheat.wt) %>% 
  mutate(spei_age2_wheat.winter_wt= spei_age2_wheat.winter*wheat.winter.wt) %>% 
  mutate(spei_age2_yam_wt         = spei_age2_yam*yam.wt) %>% 
  # age 3
  mutate(spei_age3_barley_wt      = spei_age3_barley*barley.wt) %>% 
  mutate(spei_age3_barley.winter_wt= spei_age3_barley.winter*barley.winter.wt) %>% 
  mutate(spei_age3_cassava_wt     = spei_age3_cassava*cassava.wt) %>% 
  mutate(spei_age3_cotton_wt      = spei_age3_cotton*cotton.wt) %>% 
  mutate(spei_age3_groundnut_wt   = spei_age3_groundnut*groundnut.wt) %>% 
  mutate(spei_age3_maize_wt       = spei_age3_maize*maize.wt) %>% 
  mutate(spei_age3_maize.2_wt     = spei_age3_maize.2*maize.2.wt) %>% 
  mutate(spei_age3_millet_wt      = spei_age3_millet*millet.wt) %>% 
  #mutate(spei_age3_oats_wt        = spei_age3_oats*oats.wt) %>% 
  mutate(spei_age3_potato_wt      = spei_age3_potato*potato.wt) %>% 
  mutate(spei_age3_pulsenes_wt    = spei_age3_pulsenes*pulsenes.wt) %>% 
  mutate(spei_age3_rice_wt        = spei_age3_rice*rice.wt) %>%
  mutate(spei_age3_rice.2_wt      = spei_age3_rice.2*rice.2.wt) %>% 
  mutate(spei_age3_sorghum_wt     = spei_age3_sorghum*sorghum.wt) %>%
  mutate(spei_age3_sorghum.2_wt   = spei_age3_sorghum.2*sorghum.2.wt) %>% 
  mutate(spei_age3_soybean_wt     = spei_age3_soybean*soybean.wt) %>% 
  mutate(spei_age3_sunflower_wt   = spei_age3_sunflower*sunflower.wt) %>% 
  mutate(spei_age3_sweetpotato_wt = spei_age3_sweetpotato*sweetpotato.wt) %>% 
  mutate(spei_age3_wheat_wt       = spei_age3_wheat*wheat.wt) %>%
  mutate(spei_age3_wheat.winter_wt= spei_age3_wheat.winter*wheat.winter.wt) %>% 
  mutate(spei_age3_yam_wt         = spei_age3_yam*yam.wt) %>% 
  # age 4
  mutate(spei_age4_barley_wt      = spei_age4_barley*barley.wt) %>%
  mutate(spei_age4_barley.winter_wt= spei_age4_barley.winter*barley.winter.wt) %>% 
  mutate(spei_age4_cassava_wt     = spei_age4_cassava*cassava.wt) %>% 
  mutate(spei_age4_cotton_wt      = spei_age4_cotton*cotton.wt) %>% 
  mutate(spei_age4_groundnut_wt   = spei_age4_groundnut*groundnut.wt) %>% 
  mutate(spei_age4_maize_wt       = spei_age4_maize*maize.wt) %>% 
  mutate(spei_age4_maize.2_wt     = spei_age4_maize.2*maize.2.wt) %>% 
  mutate(spei_age4_millet_wt      = spei_age4_millet*millet.wt) %>% 
  #mutate(spei_age4_oats_wt        = spei_age4_oats*oats.wt) %>% 
  mutate(spei_age4_potato_wt      = spei_age4_potato*potato.wt) %>% 
  mutate(spei_age4_pulsenes_wt    = spei_age4_pulsenes*pulsenes.wt) %>% 
  mutate(spei_age4_rice_wt        = spei_age4_rice*rice.wt) %>% 
  mutate(spei_age4_rice.2_wt      = spei_age4_rice.2*rice.2.wt) %>% 
  mutate(spei_age4_sorghum_wt     = spei_age4_sorghum*sorghum.wt) %>% 
  mutate(spei_age4_sorghum.2_wt   = spei_age4_sorghum.2*sorghum.2.wt) %>% 
  mutate(spei_age4_soybean_wt     = spei_age4_soybean*soybean.wt) %>% 
  mutate(spei_age4_sunflower_wt   = spei_age4_sunflower*sunflower.wt) %>% 
  mutate(spei_age4_sweetpotato_wt = spei_age4_sweetpotato*sweetpotato.wt) %>% 
  mutate(spei_age4_wheat_wt       = spei_age4_wheat*wheat.wt) %>% 
  mutate(spei_age4_wheat.winter_wt= spei_age4_wheat.winter*wheat.winter.wt) %>% 
  mutate(spei_age4_yam_wt         = spei_age4_yam*yam.wt)



df0 <- df0 %>% 
  # lag 1
  mutate(spei_lag1_barley_wt      = spei_lag1_barley*barley.wt) %>%
  mutate(spei_lag1_barley.winter_wt= spei_lag1_barley.winter*barley.winter.wt) %>% 
  mutate(spei_lag1_cassava_wt     = spei_lag1_cassava*cassava.wt) %>% 
  mutate(spei_lag1_cotton_wt      = spei_lag1_cotton*cotton.wt) %>% 
  mutate(spei_lag1_groundnut_wt   = spei_lag1_groundnut*groundnut.wt) %>% 
  mutate(spei_lag1_maize_wt       = spei_lag1_maize*maize.wt) %>% 
  mutate(spei_lag1_maize.2_wt     = spei_lag1_maize.2*maize.2.wt) %>% 
  mutate(spei_lag1_millet_wt      = spei_lag1_millet*millet.wt) %>% 
  mutate(spei_lag1_potato_wt      = spei_lag1_potato*potato.wt) %>% 
  mutate(spei_lag1_pulsenes_wt    = spei_lag1_pulsenes*pulsenes.wt) %>% 
  mutate(spei_lag1_rice_wt        = spei_lag1_rice*rice.wt) %>% 
  mutate(spei_lag1_rice.2_wt      = spei_lag1_rice.2*rice.2.wt) %>% 
  mutate(spei_lag1_sorghum_wt     = spei_lag1_sorghum*sorghum.wt) %>% 
  mutate(spei_lag1_sorghum.2_wt   = spei_lag1_sorghum.2*sorghum.2.wt) %>% 
  mutate(spei_lag1_soybean_wt     = spei_lag1_soybean*soybean.wt) %>% 
  mutate(spei_lag1_sunflower_wt   = spei_lag1_sunflower*sunflower.wt) %>% 
  mutate(spei_lag1_sweetpotato_wt = spei_lag1_sweetpotato*sweetpotato.wt) %>% 
  mutate(spei_lag1_wheat_wt       = spei_lag1_wheat*wheat.wt) %>% 
  mutate(spei_lag1_wheat.winter_wt= spei_lag1_wheat.winter*wheat.winter.wt) %>% 
  mutate(spei_lag1_yam_wt         = spei_lag1_yam*yam.wt) %>% 
  # lag 2
  mutate(spei_lag2_barley_wt      = spei_lag2_barley*barley.wt) %>%
  mutate(spei_lag2_barley.winter_wt= spei_lag2_barley.winter*barley.winter.wt) %>% 
  mutate(spei_lag2_cassava_wt     = spei_lag2_cassava*cassava.wt) %>% 
  mutate(spei_lag2_cotton_wt      = spei_lag2_cotton*cotton.wt) %>% 
  mutate(spei_lag2_groundnut_wt   = spei_lag2_groundnut*groundnut.wt) %>% 
  mutate(spei_lag2_maize_wt       = spei_lag2_maize*maize.wt) %>% 
  mutate(spei_lag2_maize.2_wt     = spei_lag2_maize.2*maize.2.wt) %>% 
  mutate(spei_lag2_millet_wt      = spei_lag2_millet*millet.wt) %>% 
  mutate(spei_lag2_potato_wt      = spei_lag2_potato*potato.wt) %>% 
  mutate(spei_lag2_pulsenes_wt    = spei_lag2_pulsenes*pulsenes.wt) %>% 
  mutate(spei_lag2_rice_wt        = spei_lag2_rice*rice.wt) %>% 
  mutate(spei_lag2_rice.2_wt      = spei_lag2_rice.2*rice.2.wt) %>% 
  mutate(spei_lag2_sorghum_wt     = spei_lag2_sorghum*sorghum.wt) %>% 
  mutate(spei_lag2_sorghum.2_wt   = spei_lag2_sorghum.2*sorghum.2.wt) %>% 
  mutate(spei_lag2_soybean_wt     = spei_lag2_soybean*soybean.wt) %>% 
  mutate(spei_lag2_sunflower_wt   = spei_lag2_sunflower*sunflower.wt) %>% 
  mutate(spei_lag2_sweetpotato_wt = spei_lag2_sweetpotato*sweetpotato.wt) %>% 
  mutate(spei_lag2_wheat_wt       = spei_lag2_wheat*wheat.wt) %>% 
  mutate(spei_lag2_wheat.winter_wt= spei_lag2_wheat.winter*wheat.winter.wt) %>% 
  mutate(spei_lag2_yam_wt         = spei_lag2_yam*yam.wt) %>% 
  # lag 3
  mutate(spei_lag3_barley_wt      = spei_lag3_barley*barley.wt) %>%
  mutate(spei_lag3_barley.winter_wt= spei_lag3_barley.winter*barley.winter.wt) %>% 
  mutate(spei_lag3_cassava_wt     = spei_lag3_cassava*cassava.wt) %>% 
  mutate(spei_lag3_cotton_wt      = spei_lag3_cotton*cotton.wt) %>% 
  mutate(spei_lag3_groundnut_wt   = spei_lag3_groundnut*groundnut.wt) %>% 
  mutate(spei_lag3_maize_wt       = spei_lag3_maize*maize.wt) %>% 
  mutate(spei_lag3_maize.2_wt     = spei_lag3_maize.2*maize.2.wt) %>% 
  mutate(spei_lag3_millet_wt      = spei_lag3_millet*millet.wt) %>% 
  mutate(spei_lag3_potato_wt      = spei_lag3_potato*potato.wt) %>% 
  mutate(spei_lag3_pulsenes_wt    = spei_lag3_pulsenes*pulsenes.wt) %>% 
  mutate(spei_lag3_rice_wt        = spei_lag3_rice*rice.wt) %>% 
  mutate(spei_lag3_rice.2_wt      = spei_lag3_rice.2*rice.2.wt) %>% 
  mutate(spei_lag3_sorghum_wt     = spei_lag3_sorghum*sorghum.wt) %>% 
  mutate(spei_lag3_sorghum.2_wt   = spei_lag3_sorghum.2*sorghum.2.wt) %>% 
  mutate(spei_lag3_soybean_wt     = spei_lag3_soybean*soybean.wt) %>% 
  mutate(spei_lag3_sunflower_wt   = spei_lag3_sunflower*sunflower.wt) %>% 
  mutate(spei_lag3_sweetpotato_wt = spei_lag3_sweetpotato*sweetpotato.wt) %>% 
  mutate(spei_lag3_wheat_wt       = spei_lag3_wheat*wheat.wt) %>% 
  mutate(spei_lag3_wheat.winter_wt= spei_lag3_wheat.winter*wheat.winter.wt) %>% 
  mutate(spei_lag3_yam_wt         = spei_lag3_yam*yam.wt)

## Change SPEI exposure to NA if land dedicated to specific crop is 0
for(i in crops) { 
  
  df0 <- df0 %>% 
    mutate(!!as.name(paste0("spei_utero_", i, sep="")) := ifelse(!!as.name(i)==0, NA, !!as.name(paste0("spei_utero_", i, sep="")))) %>% 
    mutate(!!as.name(paste0("spei_utero_", i, "_wt", sep="")) := ifelse(!!as.name(i)==0, NA, !!as.name(paste0("spei_utero_", i, "_wt", sep="")))) %>% 
    mutate(!!as.name(paste0("spei_infancy_", i, sep="")) := ifelse(!!as.name(i)==0, NA, !!as.name(paste0("spei_infancy_", i, sep="")))) %>% 
    mutate(!!as.name(paste0("spei_infancy_", i, "_wt", sep="")) := ifelse(!!as.name(i)==0, NA, !!as.name(paste0("spei_infancy_", i, "_wt", sep="")))) %>% 
    mutate(!!as.name(paste0("spei_age1_", i, sep="")) := ifelse(!!as.name(i)==0, NA, !!as.name(paste0("spei_age1_", i, sep="")))) %>% 
    mutate(!!as.name(paste0("spei_age1_", i, "_wt", sep="")) := ifelse(!!as.name(i)==0, NA, !!as.name(paste0("spei_age1_", i, "_wt", sep="")))) %>% 
    mutate(!!as.name(paste0("spei_age2_", i, sep="")) := ifelse(!!as.name(i)==0, NA, !!as.name(paste0("spei_age2_", i, sep="")))) %>% 
    mutate(!!as.name(paste0("spei_age2_", i, "_wt", sep="")) := ifelse(!!as.name(i)==0, NA, !!as.name(paste0("spei_age2_", i, "_wt", sep="")))) %>% 
    mutate(!!as.name(paste0("spei_age3_", i, sep="")) := ifelse(!!as.name(i)==0, NA, !!as.name(paste0("spei_age3_", i, sep="")))) %>% 
    mutate(!!as.name(paste0("spei_age3_", i, "_wt", sep="")) := ifelse(!!as.name(i)==0, NA, !!as.name(paste0("spei_age3_", i, "_wt", sep="")))) %>% 
    mutate(!!as.name(paste0("spei_age4_", i, sep="")) := ifelse(!!as.name(i)==0, NA, !!as.name(paste0("spei_age4_", i, sep="")))) %>% 
    mutate(!!as.name(paste0("spei_age4_", i, "_wt", sep="")) := ifelse(!!as.name(i)==0, NA, !!as.name(paste0("spei_age4_", i, "_wt", sep="")))) %>% 
    
    mutate(!!as.name(paste0("spei_lag1_", i, "_wt", sep="")) := ifelse(!!as.name(i)==0, NA, !!as.name(paste0("spei_lag1_", i, "_wt", sep="")))) %>%  
    mutate(!!as.name(paste0("spei_lag2_", i, "_wt", sep="")) := ifelse(!!as.name(i)==0, NA, !!as.name(paste0("spei_lag2_", i, "_wt", sep="")))) %>%  
    mutate(!!as.name(paste0("spei_lag3_", i, "_wt", sep="")) := ifelse(!!as.name(i)==0, NA, !!as.name(paste0("spei_lag3_", i, "_wt", sep=""))))   

  
}


df0 <- df0 %>%
  # generate weighted SPEI measures
  rowwise() %>%
  mutate(spei_utero_weighted = sum(c_across(spei_utero_barley_wt:spei_utero_yam_wt), na.rm=TRUE)) %>%
  mutate(spei_infancy_weighted = sum(c_across(spei_infancy_barley_wt:spei_infancy_yam_wt), na.rm=TRUE)) %>% 
  mutate(spei_age1_weighted = sum(c_across(spei_age1_barley_wt:spei_age1_yam_wt), na.rm=TRUE)) %>% 
  mutate(spei_age2_weighted = sum(c_across(spei_age2_barley_wt:spei_age2_yam_wt), na.rm=TRUE)) %>% 
  mutate(spei_age3_weighted = sum(c_across(spei_age3_barley_wt:spei_age3_yam_wt), na.rm=TRUE)) %>% 
  mutate(spei_age4_weighted = sum(c_across(spei_age4_barley_wt:spei_age4_yam_wt), na.rm=TRUE)) %>% 
  mutate(spei_lag1_weighted = sum(c_across(spei_lag1_barley_wt:spei_lag1_yam_wt), na.rm=TRUE)) %>% 
  mutate(spei_lag2_weighted = sum(c_across(spei_lag2_barley_wt:spei_lag2_yam_wt), na.rm=TRUE)) %>% 
  mutate(spei_lag3_weighted = sum(c_across(spei_lag3_barley_wt:spei_lag3_yam_wt), na.rm=TRUE)) %>% 
  ungroup()


df0 <- df0 %>% 
  mutate(spei_utero_weighted = ifelse(is.na(spei_utero_barley_wt) & is.na(spei_utero_barley.winter_wt) & is.na(spei_utero_cassava_wt) & is.na(spei_utero_cotton_wt) & is.na(spei_utero_groundnut_wt) & is.na(spei_utero_maize_wt) & is.na(spei_utero_maize.2_wt) & is.na(spei_utero_millet_wt) & is.na(spei_utero_potato_wt ) & is.na(spei_utero_pulsenes_wt) & is.na(spei_utero_rice_wt)  & is.na(spei_utero_rice.2_wt) & is.na( spei_utero_sorghum_wt) & is.na( spei_utero_sorghum.2_wt) & is.na(spei_utero_soybean_wt) & is.na(spei_utero_sunflower_wt) & is.na(spei_utero_sweetpotato_wt) & is.na(spei_utero_wheat_wt) & is.na(spei_utero_wheat.winter_wt) & is.na(spei_utero_yam_wt), NA, spei_utero_weighted)) %>%
  mutate(spei_infancy_weighted = ifelse(is.na(spei_infancy_barley_wt) & is.na(spei_infancy_barley.winter_wt) & is.na(spei_infancy_cassava_wt) & is.na(spei_infancy_cotton_wt) & is.na(spei_infancy_groundnut_wt) & is.na(spei_infancy_maize_wt) & is.na(spei_infancy_maize.2_wt) & is.na(spei_infancy_millet_wt) & is.na(spei_infancy_potato_wt ) & is.na(spei_infancy_pulsenes_wt) & is.na(spei_infancy_rice_wt) & is.na(spei_infancy_rice.2_wt) & is.na( spei_infancy_sorghum_wt) & is.na( spei_infancy_sorghum.2_wt) & is.na(spei_infancy_soybean_wt) & is.na(spei_infancy_sunflower_wt) & is.na(spei_infancy_sweetpotato_wt) & is.na(spei_infancy_wheat_wt) & is.na(spei_infancy_wheat.winter_wt) & is.na(spei_infancy_yam_wt), NA, spei_infancy_weighted)) %>% 
  mutate(spei_age1_weighted = ifelse(is.na(spei_age1_barley_wt) & is.na(spei_age1_barley.winter_wt) & is.na(spei_age1_cassava_wt) & is.na(spei_age1_cotton_wt) & is.na(spei_age1_groundnut_wt) & is.na(spei_age1_maize_wt) & is.na(spei_age1_maize.2_wt) & is.na(spei_age1_millet_wt) & is.na(spei_age1_potato_wt ) & is.na(spei_age1_pulsenes_wt) & is.na(spei_age1_rice_wt) & is.na(spei_age1_rice.2_wt) & is.na( spei_age1_sorghum_wt) & is.na( spei_age1_sorghum.2_wt) & is.na(spei_age1_soybean_wt) & is.na(spei_age1_sunflower_wt) & is.na(spei_age1_sweetpotato_wt) & is.na(spei_age1_wheat_wt) & is.na(spei_age1_wheat.winter_wt) & is.na(spei_age1_yam_wt), NA, spei_age1_weighted)) %>% 
  mutate(spei_age2_weighted = ifelse(is.na(spei_age2_barley_wt) & is.na(spei_age2_barley.winter_wt) & is.na(spei_age2_cassava_wt) & is.na(spei_age2_cotton_wt) & is.na(spei_age2_groundnut_wt) & is.na(spei_age2_maize_wt) & is.na(spei_age2_maize.2_wt) & is.na(spei_age2_millet_wt) & is.na(spei_age2_potato_wt ) & is.na(spei_age2_pulsenes_wt) & is.na(spei_age2_rice_wt) & is.na(spei_age2_rice.2_wt) & is.na( spei_age2_sorghum_wt) & is.na( spei_age2_sorghum.2_wt) & is.na(spei_age2_soybean_wt) & is.na(spei_age2_sunflower_wt) & is.na(spei_age2_sweetpotato_wt) & is.na(spei_age2_wheat_wt) & is.na(spei_age2_wheat.winter_wt) & is.na(spei_age2_yam_wt), NA, spei_age2_weighted)) %>% 
  mutate(spei_age3_weighted = ifelse(is.na(spei_age3_barley_wt) & is.na(spei_age3_barley.winter_wt) & is.na(spei_age3_cassava_wt) & is.na(spei_age3_cotton_wt) & is.na(spei_age3_groundnut_wt) & is.na(spei_age3_maize_wt) & is.na(spei_age3_maize.2_wt) & is.na(spei_age3_millet_wt) & is.na(spei_age3_potato_wt ) & is.na(spei_age3_pulsenes_wt) & is.na(spei_age3_rice_wt) & is.na(spei_age3_rice.2_wt) & is.na( spei_age3_sorghum_wt) & is.na( spei_age3_sorghum.2_wt) & is.na(spei_age3_soybean_wt) & is.na(spei_age3_sunflower_wt) & is.na(spei_age3_sweetpotato_wt) & is.na(spei_age3_wheat_wt) & is.na(spei_age3_wheat.winter_wt) & is.na(spei_age3_yam_wt), NA, spei_age3_weighted))


hist(df0$spei_infancy_weighted)
hist(df0$spei_infancy_main)
hist(df0$spei_lag1_weighted)


## Generate combined weighted drought measures for grains, roots and oils

df0 <- df0 %>% 
  rowwise() %>% 
  mutate(grains.tot = sum(barley,barley.winter,maize,maize.2,millet,rice,rice.2,sorghum,sorghum.2,wheat,wheat.winter, na.rm=T)) %>% 
  mutate(help.barley = spei_infancy_barley*(barley/grains.tot)) %>% 
  mutate(help.barley.winter = spei_infancy_barley.winter*(barley.winter/grains.tot)) %>% 
  mutate(help.maize = spei_infancy_maize*(maize/grains.tot)) %>% 
  mutate(help.maize.2 = spei_infancy_maize.2*(maize.2/grains.tot)) %>% 
  mutate(help.millet = spei_infancy_millet*(millet/grains.tot)) %>% 
  mutate(help.rice = spei_infancy_rice*(rice/grains.tot)) %>% 
  mutate(help.rice.2 = spei_infancy_rice.2*(rice.2/grains.tot)) %>% 
  mutate(help.sorghum = spei_infancy_sorghum*(sorghum/grains.tot)) %>% 
  mutate(help.sorghum.2 = spei_infancy_sorghum.2*(sorghum.2/grains.tot)) %>% 
  mutate(help.wheat = spei_infancy_wheat*(wheat/grains.tot)) %>% 
  mutate(help.wheat.winter = spei_infancy_wheat.winter*(wheat.winter/grains.tot)) %>% 
  rowwise() %>% 
  mutate(spei_infancy_grains = sum(help.barley, help.barley.winter, help.maize, help.maize.2, help.millet, help.rice, help.rice.2, help.sorghum, help.sorghum.2, help.wheat, help.wheat.winter, na.rm=T)) %>% 
  mutate(spei_infancy_grains = ifelse(is.na(help.barley) & is.na(help.barley.winter) & is.na(help.maize) & is.na(help.maize.2) & is.na(help.millet) & is.na(help.rice) & is.na(help.rice.2) & is.na(help.sorghum) & is.na(help.sorghum.2) & is.na(help.wheat) & is.na(help.wheat.winter), NA, spei_infancy_grains)) %>% 
  rowwise() %>% 
  mutate(roots.tot = sum(cassava, potato, sweetpotato, yam, na.rm=T)) %>% 
  mutate(help.cassava = spei_infancy_cassava*(cassava/roots.tot)) %>% 
  mutate(help.potato = spei_infancy_potato*(potato/roots.tot)) %>% 
  mutate(help.sweetpotato = spei_infancy_sweetpotato*(sweetpotato/roots.tot)) %>% 
  mutate(help.yam = spei_infancy_yam*(yam/roots.tot)) %>% 
  rowwise() %>% 
  mutate(spei_infancy_roots = sum(help.cassava, help.potato, help.sweetpotato, help.yam, na.rm=T)) %>% 
  mutate(spei_infancy_roots = ifelse(is.na(help.cassava) & is.na(help.potato) & is.na(help.sweetpotato) & is.na(help.yam), NA, spei_infancy_roots)) %>% 
  rowwise() %>% 
  mutate(oils.pulsenes.tot = sum(cotton, groundnut, soybean, sunflower, pulsenes, na.rm=T)) %>% 
  mutate(help.cotton = spei_infancy_cotton*(cotton/oils.pulsenes.tot)) %>% 
  mutate(help.groundnut = spei_infancy_groundnut*(groundnut/oils.pulsenes.tot)) %>% 
  mutate(help.soybean = spei_infancy_soybean*(soybean/oils.pulsenes.tot)) %>% 
  mutate(help.sunflower = spei_infancy_sunflower*(sunflower/oils.pulsenes.tot)) %>% 
  mutate(help.pulsenes = spei_infancy_pulsenes*(pulsenes/oils.pulsenes.tot)) %>% 
  rowwise() %>% 
  mutate(spei_infancy_oils_pulsenes = sum(help.cotton, help.groundnut, help.soybean, help.sunflower, help.pulsenes, na.rm=T)) %>% 
  mutate(spei_infancy_oils_pulsenes = ifelse(is.na(help.cotton) & is.na(help.groundnut) & is.na(help.soybean) & is.na(help.sunflower) & is.na(help.pulsenes), NA, spei_infancy_oils_pulsenes)) %>% 
  
  mutate(oils.tot = sum(cotton, groundnut, soybean, sunflower, na.rm=T)) %>% 
  mutate(help.cotton = spei_infancy_cotton*(cotton/oils.tot)) %>% 
  mutate(help.groundnut = spei_infancy_groundnut*(groundnut/oils.tot)) %>% 
  mutate(help.soybean = spei_infancy_soybean*(soybean/oils.tot)) %>% 
  mutate(help.sunflower = spei_infancy_sunflower*(sunflower/oils.tot)) %>% 
  rowwise() %>% 
  mutate(spei_infancy_oils= sum(help.cotton, help.groundnut, help.soybean, help.sunflower, na.rm=T)) %>% 
  mutate(spei_infancy_oils = ifelse(is.na(help.cotton) & is.na(help.groundnut) & is.na(help.soybean) & is.na(help.sunflower), NA, spei_infancy_oils)) %>% 
  dplyr::select(-c(help.barley, help.barley.winter, help.maize, help.maize.2, help.millet, help.rice, help.rice.2, help.sorghum, help.sorghum.2, help.wheat, help.wheat.winter, help.cassava, help.potato, help.sweetpotato, help.yam, help.cotton, help.groundnut, help.soybean, help.sunflower, help.pulsenes))


## Load the DHS data
load("DHS_data_harmonized.RData")

dhs <- dhs %>% 
  dplyr::select(CountryName, SurveyId, regId, psu, hh.id, woman.id, wt, stunted, stunted.severe, haz_new, wasted, wasted.severe, whz_new, underweight, underweight.severe, waz_new, sex, bord, age.months, birth.size, age_mother_at_birth, breastfed, edu.level, mass.media, wealth, wealth.score, partner, occupation.head, residence, intYr, intMo, intCMC, birthYr, birthMo, birthCMC)

dhs <- dhs %>% 
  left_join(df0) 

hist(dhs$spei_infancy_weighted)
hist(dhs$spei_infancy_main)

dhs <- dhs[grep("lag4", names(dhs), value = TRUE, invert = TRUE)]
dhs <- dhs[grep("lag5", names(dhs), value = TRUE, invert = TRUE)]
dhs <- dhs[grep("lag6", names(dhs), value = TRUE, invert = TRUE)]
dhs <- dhs[grep("lag7", names(dhs), value = TRUE, invert = TRUE)]
dhs <- dhs[grep("lag8", names(dhs), value = TRUE, invert = TRUE)]
dhs <- dhs[grep("lag9", names(dhs), value = TRUE, invert = TRUE)]
dhs <- dhs[grep("lag10", names(dhs), value = TRUE, invert = TRUE)]


rm(list=setdiff(ls(), c("dhs", "bord_sf")))
save.image(file='./data for analysis/data_spei_season_length_buffer10.RData')
write.csv(dhs, "./data for analysis/data_spei_season_length_buffer10.csv")


