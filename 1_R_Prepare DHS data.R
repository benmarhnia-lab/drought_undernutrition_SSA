###############################################################
###############################################################
#######                                                 ####### 
#######                   PROJECT:                      ####### 
#######         Droughts & child under-nutrition        ####### 
#######                                                 ####### 
#######           CODE: RETRIEVE THE DHS DATA           #######
###############################################################
###############################################################

## Contents:
## 1. Retrieve the survey data for sub-Saharan Africa
## 2. Harmonize and clean the survey data
## 3. Recalculate children's anthropometric scores (HAZ, WAZ & WHZ)  
## 4. Recalculate the household wealth index
## 5. Harmonize the admin areas 


## Search "NOTE" for important notes


rm(list =ls())
library(foreign)
#install.packages("tidyverse")
library(tidyverse)
library(data.table)
#install.packages("zoo")
library(zoo)
#install.packages("psych")
library(psych)
library(lubridate)
#install.packages("MAPLES")
library(MAPLES)
#library("readxl")
#install.packages("rdhs")
library(rdhs)      


################################################################################
## 1. Retrieve the survey data for sub-Saharan Africa
################################################################################
## Set up your DHS credentials (password: barabani1!)
set_rdhs_config(email = XXX,
                project = XXX)

## Make a list of eligible surveys and download them
surveys <- dhs_datasets() %>% 
  dplyr::filter(SurveyType == "DHS") %>% 
  dplyr::filter(FileFormat == "Stata dataset (.dta)") %>% 
  dplyr::filter(FileType == "Children's Recode")

## Add world region and filter to Sub-Saharan Africa only
library(countrycode)
surveys$Region <- countrycode(surveys$CountryName, 'country.name', 'region')  
surveys <- surveys %>% 
  filter(Region == "Sub-Saharan Africa")

unique(surveys$CountryName)
unique(surveys$SurveyId)

downloads <- get_datasets(surveys$FileName, reformat=TRUE, clear_cache = TRUE)
print(downloads)
#vec <- unlist(downloads)
#vec

## Select relevant variables
vars = c(
  #maternal & household characteristics
  "caseid", "midx", "v001", "v002", "v003", "v005", "v006", "v007", "v008","v008a",
  "v012", "v016", "v017", "v021", "v023", "v024", "v025", "v040", "v104", "v106","v113",
  "v115", "v116", "v133", "v134", "v135", "v137", "v139", "v140", "v149", "v151", "v152", 
  "v155", "v157", "v158", "v159", "v160",  "v190", "v191", "v438", "v445", "v467d", "v467b", 
  "v465","v502", "v632", "v704", "v705", "v716", "v717", "v208",
  #feeding practices
  #"v469e", "v469f", "v469x", "v412a", "v414e", "v414f", "v414o", "v411", "v411a", "v414v", "v414p", "v414h", "v414m", 
  #"v414n", "v414a", "v414g", "v414i", "v414j", "v414k", "v414l","v409", "v409a", "v410", "v410a", "v412c", "v413",
  #"m39", 'm39a',
  #child characteristics and anthropometric measures
  "bidx", "bord", "b0", "b1", "b2" ,"b3", "b4", "b5", "b8", "b9", "b11", "b17", "b18", "b19", "b20",
  "hw70", "hw71", "hw72", "hw73", "hw1", "hw2", "hw3", "hw13", "hw16", "hw17", "hw18", "hw19", 
  #infectious disease symptoms
  "h11", "h22", "h31", "h31", "h31b", "h31c",  
  #vaccines
  "h1", "h0", "h2", "h3", "h4", "h5", "h6", "h7", "h8", "h9", "h51", "h52", "h53", "h54", "h55", "h56", "h57", "h58", 
  #healthcare during pregnancy, delivery and breastfeeding
  "m1", "m4", "m5", "m15", "m14", "m18", "m19", "m3a", "m3b", "m3c", "m3d", "m3e", "m3f", "m66",
  #variables for recalculating the wealth index
  "v127", "v128", "v129", "v161", "v119", "v120", "v121", "v122", "v123", "v124", "v125", "v153", "v136", "v745b"
)

questions <- search_variables(surveys$FileName, variables = vars,  reformat=TRUE)

## Extract the data (adding geographical covariates: add_geo = TRUE)
extract <- extract_dhs(questions, add_geo = T)

## Quick check 
head(extract[1])

df0 <- rbindlist(extract, use.names=TRUE, fill=TRUE)

CountryName <- surveys %>%
  select(SurveyId, CountryName)

## Keep only observations with valid GPS coordinates
df0 <- df0 %>% 
  filter(!is.na(LATNUM)) %>% 
  filter(!is.na(LONGNUM)) %>% 
  filter(LATNUM!=0 | LONGNUM!=0) %>%             #LAT=0 and LONG=0 are missing coordinates  
  mutate(DHSCC = substr(SurveyId, 1, 2)) %>%     #add country code identifier 
  left_join(CountryName)                         #add country names   
  
unique(df0$SurveyId)

save.image(file='DHS_data_extract.RData')

rm(list =ls())





