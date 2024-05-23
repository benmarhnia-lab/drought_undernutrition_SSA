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


################################################################################
## 2. Harmonize and clean the survey data
################################################################################
load('DHS_data_extract.RData')

rm(list=setdiff(ls(), c("df0")))

df0 <- df0 %>% 
  dplyr::mutate(SurveyYear = substr(SurveyId, 3, 6))

sort(substr(unique(df0$SurveyId), 3, 6))
sort(unique(df0$SurveyId))

#interview year wrongly coded or missing for some observations - corrected below
aggregate(v007 ~ SurveyYear, data=df0, function(x) {sum(is.na(x))}, na.action = NULL)

#removing upper case letters for easier harmonization
obj <- c("b5", "bord","v190","b4","v106","v151","h2","h0","h4","h6","h8","h3","h5","h7",
         "h9","h1","v135","m18","v157","v158","v159",
         "v155","v502","v467d","v467b","v632","h11","h22","h31","h31b",
         "h31c","v160","v116","v115","v113","v465","m15","m3a","m3b","m3c","m3d",
         "m3e","m3f","m14","m4", "v127","v128","v129",
         "v161","v119","v120","v121","v122","v123","v124","v125")

df0 <- df0 %>%
  mutate_at(vars(obj), funs(tolower(.)))


## Harmonize the data
df1 <- df0 %>% 
  filter(b5 == "yes") %>%             #keep observation only if child is alive
  dplyr::rename(psu           = v001, 
                hh.id         = v002,
                woman.id      = v003,
                intYr         = v007, #year of interview
                intMo         = v006, #month of interview
                intDay        = v016, #day of interview
                intCMC        = v008, #interview date in CMC format (century month code)
                birthYr       = b2,
                birthMo       = b1,
                birthCMC      = b3,
                #wealth        = v190,
                age           = b8,   #age of the child measured in years 
                sex           = b4,
                birth.int     = b11,  #preceding birth interval - only for 2nd and higher order births
                children.u5   = v137, #number of children under 5 in the household
                age.mother    = v012,
                edu.level     = v106,
                edu.years     = v133,
                hh.head       = v151,
                hh.size       = v136, 
                region        = v024,
                residence     = v025,
                stratum       = v023,
                health.card   = h1) %>%
  mutate(wt=v005/1000000)   %>%       #generate woman's weights
  #convert Ethiopian calendar months to numeric
  mutate(intMo = ifelse(intMo=="magabit", 7, 
                 ifelse(intMo=="miyazya", 8,
                 ifelse(intMo=="ginbot", 9,
                 ifelse(intMo=="sene", 10, 
                 #convert Nepalese calendar months to numeric
                 ifelse(intMo== "baisakh", 1,
                 ifelse(intMo== "jestha", 2, 
                 ifelse(intMo== "ashad", 3, 
                 ifelse(intMo== "srawan", 4, 
                 ifelse(intMo== "bhadra", 5, 
                 ifelse(intMo== "aswin", 6, 
                 ifelse(intMo== "kartik", 7, 
                 ifelse(intMo== "mangsir", 8, 
                 ifelse(intMo== "poush", 9,
                 ifelse(intMo== "magh", 10, 
                 ifelse(intMo== "falgun", 11, 
                 ifelse(intMo== "chaitra", 12, intMo))))))))))))))))) %>% 
  mutate(intMo = ifelse(intMo=="june", 6,
                 ifelse(intMo=="july", 7,
                 ifelse(intMo=="august", 8,
                 ifelse(intMo=="september", 9,
                 ifelse(intMo=="october", 10,
                 ifelse(intMo=="november", 11, intMo))))))) %>% 
  mutate(intMo = as.numeric(intMo)) %>% 
  mutate(intYr = ifelse(intYr=="86", 1986,
                 ifelse(intYr=="88", 1988,
                 ifelse(intYr=="90", 1990,
                 ifelse(intYr=="91", 1991,
                 ifelse(intYr=="92", 1992,
                 ifelse(intYr=="93", 1993,
                 ifelse(intYr=="94", 1994,
                 ifelse(intYr=="95", 1995,
                 ifelse(intYr=="96", 1996,
                 ifelse(intYr=="97", 1997,
                 ifelse(intYr=="98", 1998,
                 ifelse(intYr=="99", 1999, 
                 ifelse(intYr=="year 2000", 2000, intYr)))))))))))))) %>% 
  mutate(intYr = ifelse(is.na(intYr), SurveyYear, intYr)) %>% 
  mutate(intYr = as.numeric(intYr)) %>% 
  mutate(birthYr = ifelse(birthYr=="81", 1981,
                   ifelse(birthYr=="82", 1982,
                   ifelse(birthYr=="83", 1983,
                   ifelse(birthYr=="84", 1984,
                   ifelse(birthYr=="85", 1985,
                   ifelse(birthYr=="86", 1986,
                   ifelse(birthYr=="87", 1987,
                   ifelse(birthYr=="88", 1988,
                   ifelse(birthYr=="89", 1989,
                   ifelse(birthYr=="90", 1990,
                   ifelse(birthYr=="91", 1991,
                   ifelse(birthYr=="92", 1992,
                   ifelse(birthYr=="93", 1993,
                   ifelse(birthYr=="94", 1994,
                   ifelse(birthYr=="95", 1995,
                   ifelse(birthYr=="96", 1996,
                   ifelse(birthYr=="97", 1997,
                   ifelse(birthYr=="98", 1998,
                   ifelse(birthYr=="99", 1999,
                   ifelse(birthYr=="year 2000", 2000, birthYr))))))))))))))))))))) %>% 
  mutate(birthYr = as.numeric(birthYr)) %>% 
  #convert Nepalese calendar months to numeric
  mutate(birthMo = ifelse(birthMo== "baisakh", 1,
                   ifelse(birthMo== "jestha", 2, 
                   ifelse(birthMo== "ashad", 3, 
                   ifelse(birthMo== "srawan", 4, 
                   ifelse(birthMo== "bhadra", 5, 
                   ifelse(birthMo== "aswin", 6, 
                   ifelse(birthMo== "kartik", 7, 
                   ifelse(birthMo== "mangsir", 8, 
                   ifelse(birthMo== "poush", 9,
                   ifelse(birthMo== "magh", 10, 
                   ifelse(birthMo== "falgun", 11, 
                   ifelse(birthMo== "chaitra", 12, birthMo))))))))))))) %>% 
  mutate(birthMo = as.numeric(birthMo)) %>% 
  #mutate(birthCMC = mkdate(birthYr, birthMo, cmc = TRUE)) %>% 
  #mutate(intCMC   = mkdate(intYr, intMo, cmc = TRUE)) %>% 
  # convert dates from Ethiopian to Gregorian calendar
  mutate(intCMC   = ifelse(DHSCC == "ET", intCMC+92, intCMC)) %>% 
  mutate(birthCMC = ifelse(DHSCC == "ET", birthCMC+92, birthCMC)) %>% 
  # convert dates from Nepalese to Gregorian calendar
  mutate(intCMC = ifelse(DHSCC == "NP", intCMC-681, intCMC)) %>% 
  mutate(birthCMC = ifelse(DHSCC == "NP", birthCMC-681, birthCMC)) %>% 
  # generate new year and month variables for Ethiopia 
  # using cmc to determine year and month in DHS: https://dhsprogram.com/data/Guide-to-DHS-Statistics/index.cfm
  mutate(intYr_new   = as.integer((intCMC - 1)/12)+1900) %>% 
  mutate(intMo_new   = intCMC - ((intYr_new - 1900)*12)) %>% 
  mutate(birthYr_new = as.integer((birthCMC - 1)/12)+1900) %>% 
  mutate(birthMo_new = birthCMC - ((birthYr_new - 1900)*12)) %>% 
  mutate(intYr       = ifelse(CountryName == "Ethiopia" | CountryName == "Nepal", intYr_new, intYr)) %>% 
  mutate(intMo       = ifelse(CountryName == "Ethiopia" | CountryName == "Nepal", intMo_new, intMo)) %>% 
  mutate(birthYr     = ifelse(CountryName == "Ethiopia" | CountryName == "Nepal", birthYr_new, birthYr)) %>% 
  mutate(birthMo     = ifelse(CountryName == "Ethiopia" | CountryName == "Nepal", birthMo_new, birthMo)) %>%     
  dplyr::select(-c(intYr_new, intMo_new, birthYr_new, birthMo_new)) %>% 
  #calculate the age of the child in months
  mutate(age.months  = intCMC - birthCMC) %>%         #age of the child measured in months
  mutate(age.months  = ifelse(age.months==60, 59, age.months)) %>% 
  mutate(age         = as.integer(age.months/12)) %>% 
  mutate(age_mother_at_birth = age.mother - age) %>%    #age of the mother at giving birth
  mutate(birth.size = ifelse(m18=="Average" | m18=="normal", "average", 
                      ifelse(m18=="Larger than average" | m18=="bigger than normal", "larger than average",
                      ifelse(m18=="Very large" | m18=="very big", "very large",
                      ifelse(m18=="Smaller than average" | m18=="smaller than normal", "smaller than average",
                      ifelse(m18=="Very small", "very small",  
                      ifelse(m18=="don't know" | m18=="dk" | m18=="Don't know" | m18=="9" | m18=="DK", NA, m18))))))) %>% 
  mutate(birth.size = ifelse(birth.size == "average" | birth.size == "larger than average" | birth.size == "very large", "average or larger",
                      ifelse(birth.size == "smaller than average" | birth.size == "very small", "smaller than average", 
                      ifelse(birth.size == "don't know", NA, birth.size)))) %>% 
  mutate(mass.media = ifelse(v157 %in%  "at least once a week"  | v157 %in% "almost every day" |
                             v158 %in%  "at least once a week"  | v158 %in% "almost every day" |
                             v159 %in%  "at least once a week"  | v159 %in% "almost every day", 1, 0)) %>% 
  mutate(literate = ifelse(v155=="cannot read at all", "no",
                    ifelse(v155=="able to read only parts of sentence" | v155=="reads with difficulty", "partly",
                    ifelse(v155=="able to read whole sentence" | v155=="reads easily", "yes", NA)))) %>% 
  mutate(edu.level = ifelse(edu.level=="Higher", "higher",
                     ifelse(edu.level=="No education", "no education",
                     ifelse(edu.level=="Primary", "primary",
                     ifelse(edu.level=="Secondary", "secondary",
                     ifelse(edu.level=="9"  | edu.level=="others", NA, edu.level)))))) %>% 
  mutate(edu.years = ifelse(edu.years=="99" | edu.years=="dk" | edu.years=="dont know" | edu.years=="don't know", NA, edu.years)) %>% 
  mutate(edu.years = as.numeric(edu.years)) %>% 
  #mutate(wealth = ifelse(wealth == "highest", "richest", wealth)) %>% 
  #mutate(wealth = ifelse(wealth == "fourth", "richer", wealth)) %>% 
  #mutate(wealth = ifelse(wealth == "second", "poorer", wealth)) %>% 
  #mutate(wealth = ifelse(wealth == "lowest", "poorest", wealth)) %>%
  # generate new variable for age in years (lots of missing observations in the raw data)
  mutate(sex       = ifelse(sex=="Female", "female",
                     ifelse(sex=="Male", "male", sex))) %>% 
  mutate(hh.head   = ifelse(hh.head=="Female", "female",
                     ifelse(hh.head=="Male", "male", hh.head))) %>% 
  mutate(residence = ifelse(residence=="Rural", "rural",
                     ifelse(residence=="Urban", "urban", residence))) %>% 
  mutate(twin      = ifelse(b0=="single birth" | b0=="Single birth", 0, 1)) %>% 
  mutate(breastfed = ifelse(m4 == "never breastfed" |
                            m4 == "Never breastfed" | 
                            m4 == "94"              |
                            m4 == "0"               | 
                            m4 == "99", "no",
                     ifelse(m4 == "ne sait pas"     |
                            m4 == "don't know"      | 
                            m4 == "dk", NA, "yes"))) %>%                 #ever breastfed 
  mutate(resident = ifelse(v135=="visitor" | v135=="Visitor", 0,
                    ifelse(v135==9, NA, 1))) %>%  #usual resident at the place of interview
  mutate(partner = ifelse(v502=="currently in union/living with a man" | v502=="currently married" | v502=="currently in union" , 1, 0))  %>% 
  # distance to health facility is a big problem for getting medical help
  mutate(distance.health = ifelse(v467d=="big problem" | v467d=="a big problem", 1, 0)) %>%           #distance to health facility is a big problem - missing observations for some countries
  mutate(permission.health = ifelse(v467b=="big problem" | v467b=="a big problem", 1, 0)) %>%         #permission to seek medical help is a big problem as a proxy for health authonomy
  mutate(decision.health = ifelse(v632 == "joint decision" | v632 == "mainly respondent", 1, 0)) %>%  #decision for using contracention  as proxy for health authonomy 
  # undernutrition variables
  mutate(hw72 = as.numeric(hw72)) %>% 
  mutate(hw70 = as.numeric(hw70)) %>% 
  mutate(hw71 = as.numeric(hw71)) %>% 
  mutate(v445 = as.numeric(v445)) %>% 
  mutate(v438 = as.numeric(v438)) %>% 
  mutate(whz                = ifelse(hw72 <= 500, hw72/100, NA)) %>%
  #mutate(wasted             = ifelse(whz <= -2, 1, 0)) %>% 
  #mutate(wasted_severe      = ifelse(whz <= -3, 1, 0)) %>% 
  mutate(haz                = ifelse(hw70 <= 600, hw70/100, NA))  %>%
  #mutate(stunted            = ifelse(haz <= -2, 1, 0)) %>% 
  #mutate(stunted_severe     = ifelse(haz <= -3, 1, 0)) %>% 
  mutate(waz                = ifelse(hw71 <= 500, hw71/100, NA))  %>%
  #mutate(underweight        = ifelse(waz <= -2, 1, 0)) %>% 
  #mutate(underweight_severe = ifelse(waz <= -3, 1, 0)) %>% 
  mutate(BMI                = ifelse(v445 <= 9000, v445/10, NA)) %>% 
  mutate(mother.height      = ifelse(v438 > 0 & v438 < 3000, v438/10, NA)) %>%   #mother's height in cm (missing observations coded 9000 were removed)
  filter(resident == 1)  #remove children who are not usual residents at place of interview

# Harmonize disease symptoms
#df1 <- df1 %>% 
#  mutate(diarrhea = ifelse(h11 =="yes, last two weeks" | h11==2 | h11=="yes, last 2-14 days" | h11=="yes, last 24 hours", 1, 0)) %>%
#  mutate(fever    = ifelse(h22 =="yes", 1, 0)) %>%
#  mutate(cough    = ifelse(h31 =="yes, last two weeks" | h31=="yes, last 24 hours" | h31=="yes, last 2-14 days" | h31=="yes, last 14 days", 1, 0)) %>%
#  mutate(ARI      = ifelse((cough==1 &  h31b=="yes" & h31c=="chest only") | (cough==1 & h31b=="yes" &  h31c=="both") | (cough==1 & h31b=="yes" &  h31c=="yes"), 1, 0)) 


df1 <- df1 %>% 
  mutate(years.at.residence = v104) %>% 
  mutate(years.at.residence = ifelse(v104 == "always", 50, years.at.residence)) %>% 
  mutate(years.at.residence = ifelse(v104 == "ne sait pas", NA, years.at.residence)) %>% 
  mutate(years.at.residence = ifelse(v104 == "visitor", NA, years.at.residence)) %>% 
  mutate(years.at.residence = ifelse(v104 == "99", NA, years.at.residence)) %>% 
  mutate(years.at.residence = as.numeric(years.at.residence))


# Occupation is agricultural or non agricultural
df1 <- df1 %>%
  mutate(occupation.male = ifelse(v705 == "agricultural - self employed"       
                                  |  v705 == "agricultural - employee"        
                                  |  v705 == "Agricultural - self employed"   
                                  |  v705 == "agric-self employed"  
                                  |  v705 == "agricultural - unskilled"
                                  |  v705 == "agric-employee"                 
                                  |  v705 == "Agricultural - employee"        
                                  |  v705 == "agricultural"                   
                                  |  v705 == "agriculture"                    
                                  |  v705 == "production"                      # not sure if production is agriculture
                                  |  v705 == "agriculture, breeding, fishing, forest"  
                                  |  v705 == "Agric-self employed"            
                                  |  v705 == "Agric-employee", "agriculture", "non-agriculture"))  %>%  
  mutate(occupation.female = ifelse(v717 == "agricultural - self employed"      
                                  |  v717 == "agric-self employed"
                                  |  v717=="agricultural - unskilled"
                                  |  v717 == "Agricultural - self employed" 
                                  |  v717 == "agricultural - employee"      
                                  |  v717 == "agric-employee"               
                                  |  v717 == "agricultural"                 
                                  |  v717 == "Agricultural - employee"      
                                  |  v717 == "agriculture"                  
                                  |  v717 == "agriculture, breeding, fishing, forest" 
                                  |  v717 == "Agric-self employed"          
                                  |  v717 == "Agric-employee", "agriculture", "non-agriculture")) %>% 
  mutate(occupation.head = ifelse(hh.head == "male", occupation.male, occupation.female)) 


################################################################################
## 3. Recalculate children's anthropometric scores (HAZ, WAZ & WHZ)  
################################################################################

## (surveys after 2006 use the new WHO guides, for earlier surveys measures need to be recalculated)
#https://cran.r-project.org/web/packages/anthro/anthro.pdf
#https://stackoverflow.com/questions/67185028/anthropometric-z-scores-who-package-anthro-and-keeping-the-ids
#install.packages("anthro")
library(anthro)

datain <- df1 %>% 
  dplyr::select(whz, waz, haz, hw2, hw3, age.months, sex) %>% 
  mutate(sex = ifelse(sex=="male", "M", "F")) %>% 
  mutate(height = as.numeric(hw3)/10) %>% 
  mutate(weight = as.numeric(hw2)/10)

dataout <- with(
  datain,
  anthro_zscores(
    sex = sex, age = age.months, is_age_in_month= TRUE,
    weight = weight, lenhei = height))

datain <- datain %>% 
  cbind(dataout) %>% 
  rename(haz_new = zlen,
         waz_new = zwei,
         whz_new = zwfl) %>% 
  mutate(whz_new = ifelse(fwfl == 1, NA, whz_new),      #remove implausible values
         waz_new = ifelse(fwei == 1, NA, waz_new),      #remove implausible values
         haz_new = ifelse(flen == 1, NA, haz_new)) %>%  #remove implausible values
  dplyr::select(whz_new, waz_new, haz_new)  
  
df1 <- df1 %>% 
  cbind(datain) %>% 
  mutate(wasted             = ifelse(whz_new <= -2, 1, 0)) %>% 
  mutate(wasted.severe      = ifelse(whz_new <= -3, 1, 0)) %>% 
  mutate(stunted            = ifelse(haz_new <= -2, 1, 0)) %>% 
  mutate(stunted.severe     = ifelse(haz_new <= -3, 1, 0)) %>% 
  mutate(underweight        = ifelse(waz_new <= -2, 1, 0)) %>% 
  mutate(underweight.severe = ifelse(waz_new <= -3, 1, 0)) 
  

df1 <- df1 %>% 
  filter(!is.na(haz_new) | !is.na(whz_new) | !is.na(waz_new)) 

sort(unique(df1$SurveyYear))


################################################################################
## 4. Recalculate the household wealth index
################################################################################
# a. Harmonize the data 
# b. Perform PCA
# c. Generate wealth categories

#df1$v116 <- str_remove(df1$v116, "\\.")
#df1$v116 <- str_remove(df1$v116, "\\.")
#df1$v116 <- str_remove(df1$v116, "\\.")
#df1$v116 <- str_remove(df1$v116, "\\.")
#df1$v116 <- str_remove(df1$v116, "\\?")

## Toilet

df1 <- df1 %>% 
  # Check again if all categories have been included (some seem to be coded as numeric)
  mutate(toilet.shared = ifelse(v160=="yes"                        
                                | v160=="yes, other household only" 
                                | v160=="yes, public", 1, 0))

df1$v116 <- str_remove(df1$v116, "\\ÿ")

df1 <- df1 %>% 
  # Check again if all categories have been included (some seem to be coded as numeric)
  mutate(toilet.improved   = ifelse(v116=="flush toilet"                                 
                                    | v116=="flush"                                      
                                    | v116=="own flush toilet"                           
                                    | v116=="private flush toilet"                       
                                    | v116=="flush to piped sewer system"                 
                                    | v116=="Flush to piped sewer system"                
                                    | v116=="flush to septic tank"                       
                                    | v116=="flush to latrine"                           
                                    | v116=="flush to pit latrine"                       
                                    | v116=="Flush to pit latrine"                       
                                    | v116=="flush to somewhere else"                    
                                    | v116=="flush, don't know where"                    
                                    | v116=="flushed to piped sewer system"              
                                    | v116=="flush other"                                 
                                    | v116=="ventilated improved pit latrine (vip)"       
                                    | v116=="covered pit latrine, with slab"             
                                    | v116=="covered pit latrine with slab"              
                                    | v116=="uncovered pit latrine with slab"            
                                    | v116=="pit latrine with slab"                        
                                    | v116=="pit latrine with washable slab"             
                                    | v116=="pit latrine - with slab"                    
                                    | v116=="composting toilet"                           
                                    | v116=="flush - to pit latrine"                     
                                    | v116=="flush - to piped sewer system"               
                                    | v116=="flush - to septic tank"                     
                                    | v116=="flush - to somewhere else"                  
                                    | v116=="flush - don't know where"                   
                                    | v116=="flush or pour flush toilet"                  
                                    | v116=="flush - don't know"                     
                                    | v116=="flush connected to sewer/with septic tank"  
                                    | v116=="flush to pipe connected to ground water"    
                                    | v116=="flush to sewer system"                      
                                    | v116=="flush to vault (bayara)"                    
                                    | v116=="flush toileet"                              
                                    | v116=="flush toilet to piped sewer system"         
                                    | v116=="flush toilet to pit latrine"                
                                    | v116=="flush toilet to septic tank"                
                                    | v116=="flush toilet to somewhere else"             
                                    | v116=="flush toilet: own"                          
                                    | v116=="flush toilet: shared"                       
                                    | v116=="flust to pipe connected to canal"           
                                    | v116=="flust, don't know where"                    
                                    | v116=="indoors: flush to open pit (ditch or river)"
                                    | v116=="inside yard: flush to open pit (ditch or river)"
                                    | v116=="own flush toilet outsite/yard"              
                                    | v116=="own flust toilet into residence"            
                                    | v116=="share flush toilet into residence"          
                                    | v116=="share flush toilet outside/yard"            
                                    | v116=="share latrine with slab"                    
                                    | v116=="septic tank/modern toilet"                  
                                    | v116=="toilet connected to plot/yard"              
                                    | v116=="toilet connected to septic well"            
                                    | v116=="toilet connected to sewer"                  
                                    | v116=="toilet inside"                              
                                    | v116=="toilet outside"                             
                                    | v116=="vent. imp. pit latr."                       
                                    | v116=="vented improved pit latrine"                
                                    | v116=="ventilated improved privy"                  
                                    | v116=="ventilated/improved pit latrine"            
                                    | v116=="water sealed/slab la"                       
                                    | v116=="water sealed/slab latrine"                  
                                    | v116=="own flush toilet"                          
                                    | v116=="ventilated improved pit latrine"           
                                    | v116=="indoors: flush to piped public system"      
                                    | v116=="indoors: flush to septic tank"              
                                    | v116=="inside yard: flush to piped public system"  
                                    | v116=="inside yard: flush to septic tank"          
                                    | v116=="out of yard: flush to piped public system"  
                                    | v116=="out of yard: flush to septic tank"          
                                    | v116=="out of yard: latrine to septic tank"        
                                    | v116=="indoors: latrine to piped public system"    
                                    | v116=="indoors: latrine to septic tank"            
                                    | v116=="inside yard: latrine to piped public system"
                                    | v116=="inside yard: latrine to septic tank"        
                                    | v116=="out of yard: latrine to piped public system"
                                    | v116=="out of yard: latrine to septic tank"        
                                    | v116=="out of yard: flush to open pit (ditch or river)"
                                    | v116=="toilet with flush"                          
                                    | v116=="toilet without flush"                       
                                    | v116=="improved latrine"                           
                                    | v116=="traditional improved latrine"               
                                    | v116=="improved pit toilet latrine"                
                                    | v116=="improved pit latrine"                       
                                    | v116=="non-vip pit latrine with slab"              
                                    | v116=="Pit latrine - ventilated improved pit (VIP)"
                                    | v116=="pit latrine - ventilated improved pit (vip)"
                                    | v116=="ventilated improved pit latrine (laa)"      
                                    | v116=="vent.imp.pit latrine"                       
                                    | v116=="ventilated improved pit latrine"            
                                    | v116=="ventilated improved pit lat"                
                                    | v116=="ventilated improved pit"                    
                                    | v116=="ventilated improv pt"                       
                                    | v116=="vip latrine"                                
                                    | v116=="vent. imp. pit latrine"                   
                                    | v116=="(vip) latrine/blair toilet"                 
                                    | v116=="ventilated improved pit/latrine (vip blair toilet)"
                                    | v116=="ventilated improved pit (vip) latrine"      
                                    | v116=="ventilated improved pit latrine (vip)"      
                                    | v116=="pit latrine, ventilated improved"           
                                    | v116=="pit latrine, with slab"                     
                                    | v116=="private latrine with slab"                  
                                    | v116=="private with septic tank"                   
                                    | v116=="improved (ventilated) pit latrine"          
                                    | v116=="pit latrine ventilated improved pit latrine"
                                    | v116=="pit latrine - ventilated improved"  
                                    | v116=="uncovered pit latrine, with slab"
                                    | v116=="ventilated latrine"                         
                                    | v116=="covered latrine"                            
                                    | v116=="shared flush toilet"                        
                                    | v116=="public flush toilet"
                                    | v116=="flush toilet connected to a septic tank"
                                    | v116=="flush -  to septic tank"
                                    | v116=="flush toilet connected to sewer system"
                                    | v116=="flush toilet does not know connection"
                                    | v116=="latrine connected to sewer/with septic tank"
                                    | v116=="latrine with siphon"
                                    | v116=="vent imp pit latr"
                                    | v116=="ventimppit latrine"
                                    | v116=="vent imp pit latrine"
                                    | v116=="21"                                         
                                    | v116=="22"                                         
                                    | v116=="11"                                         
                                    | v116=="12"                                         
                                    | v116=="ecosan"                                     
                                    | v116=="composting toilet/ecosan"                   
                                    | v116=="Composting toilet"                          
                                    | v116=="composting toilet/arbo loo"                 
                                    | v116=="mobile chemical toilet"                     
                                    | v116=="modern flush"                               
                                    | v116=="modern flush toilet"                        
                                    | v116=="dry toilet", 1,
                                    ifelse(v116 == "99"                                  
                                           | v116=="non de jure resident"
                                           | v116=="not dejure member"
                                           | v116=="not de jure resident"                
                                           | v116=="not a de jure resident"              
                                           | v116=="not a dejure resident"               
                                           | v116=="not dejure resident"                 
                                           | v116=="not de-jure resident"                
                                           | v116 == "97"
                                           | v116=="", NA, 0)))



## Water
df1$v113 <- str_remove(df1$v113, "\\ÿ")

df1 <- df1 %>% 
  mutate(water.premise = ifelse(v115=="on premises"                   
                                | v115=="On premises"                   
                                | v115=="delivered to dwelling"         
                                | v115=="delivered water"  
                                | v115=="0"
                                | v115=="996", 1,
                                ifelse(v115=="997"                        #not a dejure resident 
                                       | v115=="999"                     
                                       | v115=="998"                     
                                       | v115=="don't know ***"         
                                       | v115=="don't know"             
                                       | v115=="Don't know" 
                                       | v115=="dk"             
                                       | v115=="Not a de jure resident" 
                                       | v115=="not a dejure resident"  
                                       | v115=="not a de jure resident" 
                                       | v115=="Not dejure resident"    
                                       | v115=="Not a dejure resident"  
                                       | v115=="not dejure resident", NA, 0 ))) %>% 
  mutate(water.safe = ifelse(v113=="unprotected well"                            
                             | v113=="unprotected spring"                        
                             | v113=="surface water"                            
                             | v113=="surface water(river/dam/lake/pond/stream/canal/irrigation channel" 
                             | v113=="river/dam/lake/ponds/stream/canal/irrigation channel"              
                             | v113=="river/dam/lake/pond/stream/canal/irrigation channel"                
                             | v113=="river/dam/lake/ponds/stream/canal/irirgation channel"               
                             | v113=="lake/pond/river/channel/irrigation channel"                        
                             | v113=="river,spring,pond /ma"                     
                             | v113=="pond/lake/dam"                             
                             | v113=="dam/lake/pond"                             
                             | v113=="river,spring,surf. w"                      
                             | v113=="river/stream not protected"                
                             | v113=="pond, river, stream"                       
                             | v113=="river,stream"                              
                             | v113=="river or stream"                           
                             | v113=="lake or stream"                            
                             | v113=="lake/pond/river/channel/irrigation channel"
                             | v113=="pond lake"                                 
                             | v113=="pond,lake"                                 
                             | v113=="pond/lake"                                 
                             | v113=="pond, lake"                                
                             | v113=="sea, lake"                                 
                             | v113=="river/stream"                              
                             | v113=="river, stream"                             
                             | v113=="ocean/lake"                                
                             | v113=="open spring"                               
                             | v113=="other spring"                              
                             | v113=="Spring"                                    
                             | v113=="spring"                                    
                             | v113=="river"                                     
                             | v113=="resevoir"                                  
                             | v113=="canal"                                     
                             | v113=="spring, not improved"                      
                             | v113=="unprotected public well/spring"            
                             | v113=="unprotected well/spring in yard/plot"      
                             | v113=="open well /hole/cesspool in residence"     
                             | v113=="open well /hole/cesspool outside residence"
                             | v113=="unprotected dug well"                      
                             | v113=="Neighbor's open well"                      
                             | v113=="Neighbor's house"                                                                                 
                             | v113=="well without cover"                        
                             | v113=="well in res, yard"                          
                             | v113=="well outside residence"                    
                             | v113=="well without handpum"                      
                             | v113=="non protected well"                        
                             | v113=="Unprotected well"                          
                             | v113=="....dug - well unprotected"                
                             | v113=="....spring water unprotected"              
                             | v113=="..nile/canals"                             
                             | v113=="34"                                        
                             | v113=="dugout"                                    
                             | v113=="lake, pond"                                
                             | v113=="lake/ pond/ stream"                        
                             | v113=="nile, canal"                               
                             | v113=="open dug well"                             
                             | v113=="open well with sump pump"                  
                             | v113=="open well without sump pump"               
                             | v113=="outside house"                             
                             | v113=="pond/tank/lake"                            
                             | v113=="rier/dam/lake/ponds/stream/canal/irrigation channel"
                             | v113=="river, lake, sea"                          
                             | v113=="river, stream, pond, lake"                 
                             | v113=="river/dam/"                                
                             | v113=="river/stream/pond/lake"                    
                             | v113=="river/stream/pond/lake/dam"                
                             | v113=="river/stream/spring"                       
                             | v113=="semi-protected well"                       
                             | v113=="shallow tubewell"                        
                             | v113=="souce not protected"                       
                             | v113=="source"                                  
                             | v113=="surface water (river/dam)"                 
                             | v113=="surface water (river/dam/lake/pond/stream/canal/irrigation c"
                             | v113=="surface well/other well"                   
                             | v113=="surface/other well"                        
                             | v113=="ubprotected well"                          
                             | v113=="undeveloped spring"                        
                             | v113=="unprotectd well"                           
                             | v113=="unprotected well to yard"                  
                             | v113=="?piped into residence"                     
                             | v113=="?pond/lake"                                
                             | v113=="?public well"                              
                             | v113=="?river/stream"                             
                             | v113=="?spring"                                   
                             | v113=="open well"                                 
                             | v113=="open well in yard/plot"                    
                             | v113=="open well in dwelling"                     
                             | v113=="public well, traditional"                   
                             | v113=="public well, cement, not covered"          
                             | v113=="well in compound"                           
                             | v113=="well in residence"                          
                             | v113=="?well in residence/yard/plot"              
                             | v113=="..well in yard/plot"                       
                             | v113=="open public well"                          
                             | v113=="well with handpump"                         
                             | v113=="well without hndpump"                       
                             | v113=="well in dwelling"                          
                             | v113=="public well, traditional"                   
                             | v113=="public well, cement, not covered"           
                             | v113=="well without handpum"                      
                             | v113=="dam"                                        
                             | v113=="other"                                     
                             | v113=="others"                                    
                             | v113=="forage"                                     # other
                             | v113=="along the road"                            
                             | v113=="in the courtyard"                          
                             | v113=="of a neighbor"                              
                             | v113=="in the house"                               
                             | v113=="in the courtyard"                           
                             | v113=="of a neighbor"                              
                             | v113=="32"                                         # unprotected well
                             | v113=="42"                                         # unprotected spring
                             | v113=="43"                                         # surface water
                             | v113=="22"                                         
                             | v113=="23"                                         
                             | v113=="33"                                         
                             | v113=="44"                                        
                             | v113=="gravity flow scheme"                        
                             | v113=="gravity flow water"                         
                             | v113=="neighbor's tap, nawasa (others recode)"
                             | v113=="neighbor's tap, source unknown (others recode)"
                             | v113=="public well"
                             | v113=="public/neighbor's well"
                             | v113=="well in house/yard/plot"
                             | v113=="well in residence/yard/compound"
                             | v113=="well inside dwelling"
                             | v113=="well into dwelling/yard/plot"
                             | v113=="neighbor's open well"
                             | v113=="public and others unprotected well"
                             | v113=="river/lake"
                             | v113=="spring/stream/waterhole"
                             | v113=="sprong/kuwa"
                             | v113=="river/irrigation channel"
                             | v113=="autre", 0,
                             ifelse(v113=="99"                                   
                                    | v113=="not a dejure resident"              
                                    | v113=="not dejure resident"                
                                    | v113=="not in de jure sample"              
                                    | v113=="not a de jure resident"             
                                    | v113=="not de jure"                        
                                    | v113=="not de-jure resident"               
                                    | v113=="not de jure resident"               
                                    | v113=="not a de-jure resident"             
                                    | v113=="not a dejure place of residence"    
                                    | v113=="97", NA, 1)))  #Note that the updated JMP guidelines from 2017 have been used to classify water source (i.e. packaged or delivered water considered as safe/improved)


df1 <- df1 %>% 
  mutate(stool.disposal = ifelse(v465 == "buried"                        
                                 | v465 == "bury in the yard"              
                                 | v465 == "put/rinsed in toilet/latrine" 
                                 | v465 == "put/rinsed into drain or ditch"
                                 | v465 == "used toilet/latrine"           
                                 | v465 == "always use toilet/latrine"     
                                 | v465 == "use disposable diapers"        
                                 | v465 == "use washable diapers"          
                                 | v465 == "use diapers (unspecified)"      
                                 | v465 == "child used toilet or latrine"  
                                 | v465 == "throw in toilet/latrine", 1, 0)) 

## Floor material
df1$v127 <- str_remove(df1$v127, "\\ÿ")

df1 <- df1 %>% 
  mutate(floor = v127) %>% 
  mutate(floor = ifelse(v127=="mud/clay/earth"                                
                        | v127=="argile, banco"                                
                        | v127=="earth"                                       
                        | v127=="sand"  
                        | v127=="sable"
                        | v127=="dung"                                        
                        | v127=="dirt"                                       
                        | v127=="earth/sand"                                 
                        | v127=="earth /sand"                                
                        | v127=="earth/ sand"                                
                        | v127=="terre/sable"                                # earth/sand
                        | v127=="natural floor - earth/sand"                 
                        | v127=="natural - earth / sand"                     
                        | v127=="natural floor mud/earth"                    
                        | v127=="natural - dung"                             
                        | v127=="earth, sand"                                                              
                        | v127=="dirt/sand"                                                               
                        | v127=="earth/sand/clay"                             
                        | v127=="earth/mud/dung"                             
                        | v127=="earth / sand"                                 
                        | v127=="earth (\"terra batida\")/sand"              
                        | v127=="earth (terra batida)"                        
                        | v127=="earth (terra n?o batida)"                   
                        | v127=="earth, sand, mud"                           
                        | v127=="earth, sand/dung"                           
                        | v127=="earth/sand/mud"                             
                        | v127=="mud/earth/dung"                             
                        | v127=="earth and dung"                             
                        | v127=="animal dung"                                
                        | v127=="mud stones"
                        | v127=="dirt/earth"
                        | v127=="earth, mud, dung"
                        | v127=="earth/ sand"
                        | v127=="earth/bamboo"
                        | v127=="earth/bamboo (katcha)"
                        | v127=="earth/clay"
                        | v127=="manure"
                        | v127=="mud mixed with dung"
                        | v127=="natural floor"
                        | v127=="earth (terra batida)"
                        | v127=="earth (terra n?o batida)"
                        | v127=="earth, sand, dung"
                        | v127=="mud plasterwork"
                        | v127=="tierra"
                        | v127=="stone"                                     
                        | v127=="stones"                                    
                        | v127=="11"                                        
                        | v127=="12"                                         
                        | v127=="13", 0, floor)) %>%
  mutate(floor  = ifelse(v127=="palm / bamboo" 
                         | v127=="palm, bamboo"                              
                         | v127=="bamboo"                                    
                         | v127=="palm/bambou"                               
                         | v127=="palm / bamboo"                             
                         | v127=="palm/bamboo"                               
                         | v127=="palm/ bamboo"                              
                         | v127=="wood planks, bamboo, plam"  
                         | v127=="palm, bamboo, leeds"                       
                         | v127=="palm/bamboo/leeds"                         
                         | v127=="palm/bamboo/leaves"
                         | v127=="reed, bamboo"                              
                         | v127=="cane"                                      
                         | v127=="reed / bamboo"                             
                         | v127== "palm/bamboo", 1, floor)) %>% 
  mutate(floor  = ifelse(v127=="raw wood planks" 
                         | v127=="floor wood planks"                         
                         | v127=="bare wood planks"                          
                         | v127=="wood"                                      
                         | v127=="wood bats (\"tacos de madeira\")"          
                         | v127=="planks"   
                         | v127=="rudimentary floor - wood planks"
                         | v127=="wood planks, palm, bamboo"
                         | v127=="wood/palm/bamboo"
                         | v127=="rudimentary floor wood planks"             
                         | v127=="rudimentary wood planks"                   
                         | v127=="rudimentary - wood planks"                 
                         | v127=="bois/autres végétaux"                     # wood/other plants
                         | v127=="wood plank"                                
                         | v127=="wood planks"   
                         | v127=="bois/autres v?g?taux" 
                         | v127=="house boat"
                         | v127=="21"                                        
                         | v127=="22", 1, floor)) %>% 
  mutate(floor  = ifelse(v127=="brick"                                      
                         | v127=="cement / bricks"                            
                         | v127=="bricks without cement"                     
                         | v127=="broken bricks"  
                         | v127=="tiles/ bricks"
                         | v127=="bricks"                                    
                         | v127=="brick/concrete"                            
                         | v127=="adobe", 2, floor)) %>% 
  mutate(floor  = ifelse(v127=="cement"                                      
                         | v127=="ciment"   
                         | v127=="cement, concrete"
                         | v127=="cement, tiles"
                         | v127=="cement/ tile"
                         | v127=="cement/brick"
                         | v127=="cement/gravel"
                         | v127=="stone, brick"
                         | v127=="cement / bricks"                            
                         | v127=="cement screed"                             
                         | v127=="cement tiles"                              
                         | v127=="cement tiles (mosaic)"                     
                         | v127=="cement/concrete"                           
                         | v127=="concrete/cement"                           
                         | v127=="concrete, cement"                          
                         | v127=="cement screed"                             
                         | v127=="concrete cement"    
                         | v127=="ceramic tiles/terazzo"
                         | v127=="cement tiles/mosaic"
                         | v127=="cements"
                         | v127=="granite"
                         | v127=="Cement"                                    
                         | v127=="concrete"                                  
                         | v127=="finished - cement", 2, floor)) %>%
  mutate(floor = ifelse(v127=="vinyl or asphalt strips"                     
                        | v127=="asphlat tiles/vynil"                        
                        | v127=="vinyl or aspalt strips"                     
                        | v127=="vinyl or asphalt"                           
                        | v127=="vinyl,asphalt strips"                       
                        | v127=="Vinyl, asphalt strips"                      
                        | v127=="vinyl /asphalt strips"                      
                        | v127=="Vinyl/ asphalt strips"
                        | v127=="vinyl/ asphalt strips"
                        | v127=="vinyl"                                      
                        | v127=="vinyl or linoleum"                          
                        | v127=="vinyl, linoleum"                            
                        | v127=="vinyl tile/vinyl carpet"                    
                        | v127=="vinyl, asphalt strips"                      
                        | v127=="floor mat, linoleum, vinyl"                 
                        | v127=="floor mat/linoleum/vinyl"                   
                        | v127=="vinyl(pvc) or asphalt strips"               
                        | v127=="vinyl (pvc) or asphalt strips"              
                        | v127=="finished - vinyl or asphalt strips"         
                        | v127=="floor mat, linoleum, vinyl"                 
                        | v127=="vinyl sheets/tiles"                        
                        | v127=="vinyl tile/vinyl carpet" 
                        | v127=="vinyl / asphalt strips"
                        | v127=="vinyl or alphalt strips"
                        | v127=="vinyl,  linoleum"
                        | v127=="vinyl, linoleum, ceramic"
                        | v127=="vinyl/asphalt strips"
                        | v127=="vynil or asphalt strips"
                        | v127=="lino/gerflex"
                        | v127=="linoleum, carpet"
                        | v127=="linoleum"                                   
                        | v127=="linoleum/rubber carpet", 3, floor)) %>% 
  mutate(floor  = ifelse(v127=="laminated or polished wood"                 
                         | v127=="parquet or polished wood" 
                         | v127==" parquet, polished wood"
                         | v127=="Parquet or polished wood"                  
                         | v127=="parquet, polished wood"                   
                         | v127=="parquet,polished wd"                       
                         | v127=="parquet, polished wd"                      
                         | v127=="parquet /polish wood"                      
                         | v127=="finished floor parquet or polished wood"   
                         | v127=="finished - parquet or polished wood"   
                         | v127=="finished floor - parquet or polished wood or laminate"
                         | v127=="finished floor - vinyl or linoleum"
                         | v127=="parket"
                         | v127=='"machimbre" / parquet'
                         | v127=="parquet / polished wood"
                         | v127=="parquet/ polished wood"
                         | v127=="parquet/polished wood"
                         | v127=="polished wood, parquet"
                         | v127=="31"                                         # finished
                         | v127=="32"                                         # finished
                         | v127=="33"                                         # finsihed 
                         | v127=="34"                                         # finsihed 
                         | v127=="35"                                         # finished 
                         | v127== "polished wood", 3, floor)) %>% 
  mutate(floor  = ifelse(v127=="ceramic mosaics"                             
                         | v127=="mosaic or tiles" 
                         | v127=="marbre,carre,granito"
                         | v127=="ceramic tiles"                             
                         | v127=="ceramic tiles/terrazo"                     
                         | v127=="ceramic/terrazzo tiles"                    
                         | v127=="ceramic tiles/ terrazo tiles"              
                         | v127=="ceramic tiles, terazzo"                   
                         | v127=="ceramic/marble/porcelain tiles / terrazo" 
                         | v127=="chips/terrazzo"                            
                         | v127=="mosaic/ceramic"                            
                         | v127=="tile"                                      
                         | v127=="tiles"                                     
                         | v127=="cement tiles/brick"                        
                         | v127=="ceramic tiles/coastal brick"               
                         | v127=="brick tiles"                                
                         | v127=="finished - ceramic tiles"                  
                         | v127=="carrelage"                                  # floor tile
                         | v127=="ceramic"
                         | v127=="ceramic tiles, marble chips"
                         | v127=="ceramic tiles, vinyl, bricks"
                         | v127=="ceramic tyles"
                         | v127=="ceramic/ marble tile"
                         | v127=="ceramic/ marmol"
                         | v127=="ceramic/marble tiles"
                         | v127=="ceramic/marble tiles tiles"
                         | v127=="ceramic/marble/granite"
                         | v127=="ceramics tiles"
                         | v127=="granite / marble / ceramic"
                         | v127=="granite, ceramic tiles"
                         | v127=="marble, ceramic tile"
                         | v127=="finished floor - carpeted"
                         | v127=="finished floor - cement"
                         | v127=="finished floor - ceramic or marble tiles"
                         | v127=="chips/terrazo"
                         | v127=="tile, ceramic"
                         | v127=="tile/ cement"
                         | v127=="terrazo"
                         | v127=="terrazzo", 3, floor)) %>%
  mutate(floor = ifelse(v127=="marble"                                       
                        | v127=="marble/ceramic tiles"                       
                        | v127=="marble/granite"                             
                        | v127=="polished stone/marble/granite", 3, floor)) %>%
  mutate(floor = ifelse(v127=="other"                                        
                        | v127=="autre"                                      
                        | v127=="other finished"                             
                        | v127=="autres matériaux moderne"                  # other modern materials
                        | v127=="floating house", 3, floor)) %>% 
  mutate(floor  = ifelse(v127=="carpeted"                                     
                         | v127=="carpet"                                    
                         | v127=="carpet/ rug"
                         | v127=="wall to wall carpet"
                         | v127=="rug, carpet"
                         | v127=="woolen carpets/synthetic carpets"
                         | v127=="mats"                                       
                         | v127=="mat"                                       
                         | v127=="finished - carpet"                         
                         | v127=="woolen carpets/ synthetic carpet"          
                         | v127=="autres mat?riaux moderne", 3, floor)) %>% 
  mutate(floor = ifelse(v127=="not a de jure resident"                       
                        | v127=="not a dejure resident"                      
                        | v127=="not de-jure resident"                       
                        | v127=="not de jure"                                
                        | v127=="not de jure resident"                       
                        | v127=="not dejure resident"                        
                        | v127=="not a de jure member"
                        | v127=="not dejure member"
                        | v127=="97"                                         
                        | v127=="99", NA, floor))


#df1$v128 <- str_remove(df1$v128, "\\.")
#df1$v128 <- str_remove(df1$v128, "\\.")
#df1$v128 <- str_remove(df1$v128, "\\.")
#df1$v128 <- str_remove(df1$v128, "\\.")


df1 <- df1 %>% 
  #Wall material
  mutate(wall = v128) %>% 
  mutate(wall = ifelse(v128=="no walls"                              
                       | v128=="No walls"
                       | v128=="without walls"
                       | v128=="dirt"                                
                       | v128=="Dirt"                                
                       | v128=="mud"                                 
                       | v128=="sod"                                 
                       | v128=="earth" 
                       | v128=="animal dung"
                       | v128=="dung"
                       | v128=="hair/wool/cloth"
                       | v128=="estera"
                       | v128=="makeshift/cardboard/reused material"
                       | v128=="thatched"                            
                       | v128=="natural - mud" 
                       | v128=="mud/sand"                            
                       | v128=="mud/stones"                          
                       | v128=="dirt/mud"                            
                       | v128=="dung / mud / sod"                    
                       | v128=="grass"                               
                       | v128=="Grass"                             
                       | v128=="grass/reeds/thatch"
                       | v128=="sticks with mud/clay/dung"
                       | v128=="thatch/bamboo/wood/mud"
                       | v128=="tree trunks"
                       | v128=="interwoven sticks or reeds and mud ('bajareque')"
                       | v128=="paille,nattes,branch"
                       | v128=="trunks with mud"                     
                       | v128=="cane / palm / trunks"                
                       | v128=="Cane / palm / trunks"                
                       | v128=="natural walls cane/tree trunks"      
                       | v128=="cane/tree trunks"                    
                       | v128=="cane/palm/trunks/reed"               
                       | v128=="cane/palm/trunks"                    
                       | v128=="cane, palm tree, trunks"             
                       | v128=="Cane/palm/trunks"                    
                       | v128=="Cane/ palm/ trunks"                  
                       | v128=="Cane/Palm/Trunks"                    
                       | v128=="natural - cane / trunks"             
                       | v128=="tree trunks with mud and cement"      
                       | v128=="cane/palm/trunks/leaves"             
                       | v128=="cane/tree trunks"                     
                       | v128=="cane/trunks"   
                       | v128=="cane/ palm/ trunks"
                       | v128=="palm leaves"
                       | v128=="straw"
                       | v128=="casca"
                       | v128=="masica sticks"
                       | v128=="straw with mud"                       
                       | v128=="tatchd/straw"                        
                       | v128=="thatched/straw"                      
                       | v128=="straw, thatch mats"                  
                       | v128=="Straw, thatch mats"                  
                       | v128=="straw/thatch mats"                   
                       | v128=="Straw"
                       | v128=="mud blocks"
                       | v128=="mud or mud bricks"
                       | v128=="slag"
                       | v128=="yagua"
                       | v128=="natural walls"
                       | v128=="rudimentary walls"
                       | v128=="wood and grass"                       
                       | v128=="Masica sticks"                        
                       | v128=="Sticks (paus maticados)"              
                       | v128=="11"                                   
                       | v128=="12"                                   
                       | v128=="13"                                   
                       | v128=="sod" , 1, wall)) %>%
  mutate(wall = ifelse(v128=="canvas/tent"                               
                       | v128=="cardboard"                           
                       | v128=="Cardboard"  
                       | v128=="cardboard, plastic" 
                       | v128=="tin/ cardborad/ paper/ bags"
                       | v128=="cardboard/plastic"
                       | v128=="paperboard"
                       | v128=="plastic / cardboard"
                       | v128=="zinc, canvas, plastics"
                       | v128=="tent"
                       | v128=="waste materials"
                       | v128=="plastic"
                       | v128=="sheet"                               
                       | v128=="tarpaulin"                           
                       | v128=="Tin/ cardborad/ paper/ bags" 
                       | v128=="plastic/cardboard/worn sheet metal"
                       | v128=="tin/cardborad/paper/bags"
                       | v128=="waste material"                      
                       | v128=="carton"                              
                       | v128=="Casca"                               
                       | v128=="Shells (Casca)"              
                       | v128=="makeshift/cardboard", 1, wall)) %>% 
  mutate(wall = ifelse(v128=="pau-a-pique"                           
                       | v128=="\"pau-a-pique\""                     
                       | v128=="pole with mud"                       
                       | v128=="poles with mud"                      
                       | v128=="Poles and mud"                       
                       | v128=="trunks with mud"
                       | v128=="poles and mud"
                       | v128=="mud and pole"                        
                       | v128=="mud and poles"                       
                       | v128=="mud and sticks"                      
                       | v128=="Mud and sticks"                      
                       | v128=="wattle and daub"                     
                       | v128=="adobe not covered/bamboo/wood with mud"                                 
                       | v128=="Uncovered adobe"                     
                       | v128=="uncovered adobe", 1, wall)) %>% 
  mutate(wall = ifelse(v128=="bamboo"                                
                       | v128=="bamboo with mud"                     
                       | v128=="Bamboo with mud"                     
                       | v128=="bamboo/wood with mud"                
                       | v128=="bamboo / tree trunk with mud"        
                       | v128=="bamboo/tree trunks with mud"         
                       | v128=="bamboo / pole with mud"              
                       | v128=="bamboo/cane/palm/trunks"             
                       | v128=="bamboo/sticks/mud"
                       | v128=="bark/straw/palm/bamboo"
                       | v128=='"tabique", "quinche"'
                       | v128=="semi-dur"
                       | v128=="semi-hard (hard or other)"
                       | v128=="bamboo with mud plaster"
                       | v128=="bamboo with mud/clay/dung"
                       | v128=="bamboo without plaster"
                       | v128=="bamboo(guadua), straw, other plants"
                       | v128=="bambu with mud"
                       | v128=="jute/bamboo/mud (katcha)"
                       | v128=="bark / straw / palm / bamboo"
                       | v128=="cane"
                       | v128=="cane / palm / trunks/ bamboo"
                       | v128=="cane/palm"
                       | v128=="cane/palm/trunks/bamboo"             
                       | v128=="cane/palm tree/sticks/bamboo"        
                       | v128=="cane / trunks / bamboo / reed"       
                       | v128=="cane/trunks/bamboo/reed"             
                       | v128=="bamboo / wood wit"
                       | v128=="tejamanil"
                       | v128=="meshed bamboo"                       
                       | v128=="palm, bamboo"  
                       | v128=="palm"
                       | v128=="palm branches"
                       | v128=="palm/bamboo/thatch"                  
                       | v128=="Bamboo/Cane/Palm/Trunks", 1, wall)) %>% 
  mutate(wall = ifelse(v128=="corrugated iron/zinc"                  
                       | v128=="corrugated metal"                    
                       | v128=="corrugated iron sheets"              
                       | v128=="galvanized iron/aluminum"            
                       | v128=="gi/metal/asbestos sheets"            
                       | v128=="iron sheets"                          
                       | v128=="Metalic sheets"                      
                       | v128=="metal"   
                       | v128=="sheet metal"
                       | v128=="metal/ galvanized sheet" 
                       | v128=="rustic mat"
                       | v128=="tin"                                 
                       | v128=="zinc"                                
                       | v128=="Zinc, metal"
                       | v128=="metal / zinc"
                       | v128=="metal sheets"
                       | v128=="metalic sheets"
                       | v128=="zinc, metal"
                       | v128=="Metal / zinc"                        
                       | v128=="zinc, metal [new code]"              
                       | v128=="zinc/metal"
                       | v128=="asbestos, wood, zinc"
                       | v128=="asbestos/wood,zinc"
                       | v128=="corrugated asbestos"
                       | v128=="metal (iron or zinc sheet)"
                       | v128=="Metal sheets", 2, wall))  %>% 
  mutate(wall = ifelse(v128=="mud bricks with stones"
                       | v128=="mud brick"
                       | v128=="mixed brick"
                       | v128=="mud bricks"                          
                       | v128=="mudbrick"  
                       | v128=="briques en banco"
                       | v128=="Mud bricks"                          
                       | v128=="mud with cement mix"                 
                       | v128=="sun-dried bricks /mud bricks"        
                       | v128=="Sun-dried bricks"                    
                       | v128=="unbaked bricks/mud"                  
                       | v128=="unbaked bricks"                      
                       | v128=="unburnt brick"                       
                       | v128=="unburnt bricks"                      
                       | v128=="un-burnt bricks"                     
                       | v128=="unburnt bricks with plaster"         
                       | v128=="unburnt bricks with mud"             
                       | v128=="un-burnt bricks and plaster"         
                       | v128=="unburnt bricks with cement"          
                       | v128=="unburnt bricks with plaster", 2, wall)) %>% 
  mutate(wall = ifelse(v128=="adobe"                                 
                       | v128=="Adobe"  
                       | v128=="adobe (mud bricks)"
                       | v128=="adobe (sun-dried brick)"
                       | v128=="adobe with sod"
                       | v128=='adobe/"tapial"'
                       | v128=="adobe covered"                       
                       | v128=="Covered adobe"                       
                       | v128=="covered adobe with cement"             
                       | v128=="covered adobe", 2, wall)) %>% 
  mutate(wall = ifelse(v128=="stone with clay"
                       | v128=="Stones with mud"                     
                       | v128=="rudimentary - stone with mud"        
                       | v128=="rudimentary walls stone with mud"    
                       | v128=="stones with mud"
                       | v128=="cut stone"
                       | v128=="Stones"                              
                       | v128=="stone"                                
                       | v128=="stones"
                       | v128=="natural stone"
                       | v128=="cut stones"
                       | v128=="mud stones"
                       | v128=="Stone with mud"
                       | v128=="21"                                   # rudimentary
                       | v128=="22"                                   # rudimentary
                       | v128=="23"                                   # rudimentary
                       | v128=="24"                                   # rudimentary
                       | v128=="26"                                   # rudimentary
                       | v128=="stone with mud", 3, wall)) %>% 
  mutate(wall = ifelse(v128=="plywood"                                         
                       | v128=="raw/reused wood"
                       | v128=="plywood sheets"
                       | v128=="planche"
                       | v128=="Plywood"                             
                       | v128=="plywood, reused wood"                 
                       | v128=="rudimentary - reused wood"           
                       | v128=="Reused wood"                         
                       | v128=="reused wood"                         
                       | v128=="Plywood"                             
                       | v128=="wood"
                       | v128=="compressed wood"
                       | v128=="wood planks"                          
                       | v128=="Wood planks"
                       | v128=="shingle (tejamanil)"
                       | v128=="wood plank / shingles"
                       | v128=="woods"     
                       | v128=="planks"  
                       | v128=="wood planks/ shingles"
                       | v128=="wood poles"
                       | v128=="wood, timer"
                       | v128=="wood/ metal planks"
                       | v128=="wood planks / shingles"              
                       | v128=="Wood planks / shingles"              
                       | v128=="Wood planks/ shingles"               
                       | v128=="wood planks/shingles"                 
                       | v128=="wood planks/shingles/metal/corrugated"
                       | v128=="wood, timber"                        
                       | v128=="Wood, timer"   
                       | v128=="plank"
                       | v128=="timber"                              
                       | v128=="rudimentary - plywood" 
                       | v128=="polished wood"
                       | v128=="finished - wood planks / shingles"  
                       | v128=="31"                                   # finished
                       | v128=="32"                                   # finished
                       | v128=="33"                                   # finished
                       | v128=="34"                                   # finished
                       | v128=="35"                                   # finished
                       | v128=="36"                                   # finished
                       | v128=="Wood/ metal planks", 3, wall)) %>% 
  mutate(wall = ifelse(v128=="baked bricks"                          
                       | v128=="clay"                                
                       | v128=="clay blocks"                         
                       | v128=="Baked bricks"                        
                       | v128=="bricks"                              
                       | v128=="Bricks"                              
                       | v128=="bricks (\"tijolos\")"                
                       | v128=="burnt bricks"                        
                       | v128=="finished bricks"                     
                       | v128=="finished - bricks"                   
                       | v128=="oven fired bricks"                   
                       | v128=="oven fired bricks with cement"       
                       | v128=="burnt bricks with cement"            
                       | v128=="burnt bricks with mud"   
                       | v128=="bare brick, cement blocks"
                       | v128=="block/cements/bricks"
                       | v128=="briks"
                       | v128=="sun-dried bricks"
                       | v128=="brick"
                       | v128=="brick or stone"
                       | v128=="brick/cement"
                       | v128=="brick/cement blocks"
                       | v128=="bricks/cement block"
                       | v128=="bricks/polished wood/premanufactured material"
                       | v128=="Briks", 4, wall)) %>% 
  mutate(wall = ifelse(v128=="cement"                                
                       | v128=="Cement"                              
                       | v128=="cement block"                        
                       | v128=="cement block/concrete"               
                       | v128=="cement blocks"                       
                       | v128=="Cement bloks"                        
                       | v128=="Cement blocks"                       
                       | v128=="cement bricks"                       
                       | v128=="cement hollow blocks"                
                       | v128=="cement/concrete" 
                       | v128=="cement blocks or panels"
                       | v128=="cement blocks/cement"
                       | v128=="cement blocks/cement stones"
                       | v128=="cement/monolit"
                       | v128=="parpaing / cement"
                       | v128=="concrete"                            
                       | v128=="cut stone and concrete"              
                       | v128=="cement or stone blocks"              
                       | v128=="finished - cement"                   
                       | v128=="finished - cement blocks"            
                       | v128=="finished walls cement"
                       | v128=="cement bloks"
                       | v128=="briques en ciment"
                       | v128=="stone with cement"
                       | v128=="stone with whitewash/cement"
                       | v128=="prefabricated material"
                       | v128=="prefab material"
                       | v128=="semi-hard (hard and other)"
                       | v128=="walls cement", 4, wall)) %>% 
  mutate(wall = ifelse(v128=="stone with lime / cement"              
                       | v128=="stone with lime/cement"              
                       | v128=="stome with lime / cement"            
                       | v128=="Stone with lime/ cement"             
                       | v128=="Stone with lime/cement"              
                       | v128=="Stone with lime / cement"  
                       | v128=="finished - stone with lime / cement" 
                       | v128=="Stone with whitewash/cement" 
                       | v128=="Stone blocks" 
                       | v128=="stone blocks"
                       | v128=="stone with cal or cement"
                       | v128=="stone with lime or cement"
                       | v128=="stone with lime/ cement"
                       | v128=="stones with lime / cement"
                       | v128=="other"  
                       | v128=="autre"
                       | v128=="Other"                               
                       | v128=="OTHER", 4, wall)) %>% 
  mutate(wall = ifelse(v128=="97"                                    
                       | v128=="99", NA, wall)) %>% 
  mutate(wall = ifelse(v128=="not a de jure resident"  
                       | v128=="non de jure resident"
                       | v128=="not a de jure household member"
                       | v128=="not de jure resident"
                       | v128=="not dejure member"
                       | v128=="visitor - dont know"
                       | v128=="Not a de jure resident"              
                       | v128=="not a dejure resident"               
                       | v128=="Not a dejure resident"               
                       | v128=="not de-jure resident"                
                       | v128=="not dejure resident"                 
                       | v128=="Not dejure resident", NA, wall ))




#df1$v129 <- str_remove(df1$v129, "\\.")
#df1$v129 <- str_remove(df1$v129, "\\.")
#df1$v129 <- str_remove(df1$v129, "\\.")
#df1$v129 <- str_remove(df1$v129, "\\.")


df1 <- df1 %>% 
  #Roof material
  mutate(roof = v129) %>% 
  mutate(roof = ifelse(v129=="no roof"                        
                       | v129=="No roof" 
                       | v129=="no roofing"
                       | v129=="natural - no roof" 
                       | v129=="waste material"
                       | v129=="clods of earth"               
                       | v129=="dung/ mud"                    
                       | v129=="dung / mud / sod"             
                       | v129=="mud/sod"                      
                       | v129=="Grass/ thatch/ palm leaf"     
                       | v129=="grass/palm"                   
                       | v129=="grass/thatch/palm leaf"       
                       | v129=="mud"                          
                       | v129=="sod"                          
                       | v129=="Sod"                          
                       | v129=="straw"                          
                       | v129=="mud bricks"                   
                       | v129=="sod/grass"                    
                       | v129=="sod/grass (cogon)"            
                       | v129=="sod/mud and grass mixture"    
                       | v129=="straw, palm"                  
                       | v129=="thatch"                       
                       | v129=="thatched"                     
                       | v129=="thatch/grass"                 
                       | v129=="thatch, grass"                
                       | v129=="grass/thatch/mud"             
                       | v129=="thatch / grass / makuti"      
                       | v129=="thatch / palm leaf"           
                       | v129=="Thatch / palm leaf"           
                       | v129=="thatch / palm leaf / leaf"    
                       | v129=="thatch/mud"                    
                       | v129=="thatching/grass"              
                       | v129=="thatch/palm leaf (nipa)"      
                       | v129=="thatch/palm leaf"             
                       | v129=="Thatch/palm leaf"             
                       | v129=="thatch/palm/leaf"             
                       | v129=="thatch/leaf/mud"               
                       | v129=="thatch / palm leaf"           
                       | v129=="Thatch/ palm leaf"            
                       | v129=="Thatch/Palm leaf"             
                       | v129=="thatch / leaf"                
                       | v129=="Thatch/Palm/Leaves" 
                       | v129=="palm branches"
                       | v129=="palm leaf / thatched"
                       | v129=="no walls"
                       | v129=="earth"
                       | v129=="earth/bamboo"
                       | v129=="estera"
                       | v129=="katcha (bamboo/thatch)"
                       | v129=="bark / straw / palm / bamboo"
                       | v129=="cana"
                       | v129=="cane/ palm/ mud"   
                       | v129=="grass"
                       | v129=="leaves"
                       | v129=="skin"
                       | v129=="sticks with mud and dung"
                       | v129=="straw and palm leaves"
                       | v129=="thatch / straw"
                       | v129=="thatch/grass/makuti"
                       | v129=="thatch, palm leaf"
                       | v129=="thatch/palm leaf/ grass"
                       | v129=="thatch/palm/bamboo/bark }"
                       | v129=="grass/ thatch/ palm leaf"
                       | v129=="thatch/ palm leaf"
                       | v129=="thatch/palm/leaves"
                       | v129=="sod/mud/dung"
                       | v129=="palm leave"
                       | v129=="chaume/palme/feuilles"         # thatch/palm/leaves
                       | v129=="thatch / grass / makuti"       
                       | v129=="natural roofing thatch/grass" 
                       | v129=="natural - thatch"             
                       | v129=="\"pau-a-pique\""               # wattle & daub
                       | v129=="11"                            # natural
                       | v129=="12"                            # natural 
                       | v129=="13"                            # natural 
                       | v129=="paille, branchage"
                       | v129=="Skin"                         
                       | v129=="wattle and daub" , 1, roof)) %>% 
  mutate(roof = ifelse(v129=="cardboard"                      
                       | v129=="Cardboard"
                       | v129=="carton" 
                       | v129=="plastic/cardboard/worn sheet metal"
                       | v129=="makeshift/cardboard", 1, roof)) %>% 
  mutate(roof = ifelse(v129=="bamboo/thatch/palm leaf"         
                       | v129=="palm / bamboo / mats"         
                       | v129=="palm / bamboo / grass"        
                       | v129=="palms/bamboo"
                       | v129=="bamboo or rustic mat with mud"
                       | v129=="bark/straw/falms/bamboo"
                       | v129=="bamboo/sticks/mud"
                       | v129=="bambu with mud"
                       | v129=="palm"
                       | v129=="palm bamboo"
                       | v129=="palm/ bamboo"
                       | v129=="palm/bamboo/thatch"
                       | v129=="paywood"
                       | v129=="reed/bamboo"                  
                       | v129=="reed / bamboo"                
                       | v129=="palm / bamboo"                
                       | v129=="bamboo"                       
                       | v129=="Palm/bamboo"                  
                       | v129=="Palm/ bamboo"                 
                       | v129=="Palm / bamboo"                
                       | v129=="Palm/Bamboo"                  
                       | v129=="palme/bambou"                 
                       | v129=="palm/bamboo", 2, roof)) %>%
  mutate(roof = ifelse(v129=="canvas/tent"                    
                       | v129=="tent"                         
                       | v129=="tarpaulin"                    
                       | v129=="Tarpaulin"                    
                       | v129=="tarpaulin, plastic"           
                       | v129=="Tarpaulin, plastic"           
                       | v129=="taule (ruberoid)"  , 2, roof)) %>% 
  mutate(roof = ifelse(v129=="mud bricks with stones"         
                       | v129=="unburnt bricks"               
                       | v129=="mud with cement mix", 2, roof)) %>% 
  mutate(roof = ifelse(v129=="plastic"                         
                       | v129=="plastic sheet"                
                       | v129=="rustic material/plastic"      
                       | v129=="rustic mat/plastic"           
                       | v129=="rustic mat/plastic sheets"    
                       | v129=="rustic mat / plastic sheets"  
                       | v129=="mat"                          
                       | v129=="Mat"                          
                       | v129=="Rustic mat"
                       | v129=="carton/plastic"
                       | v129=="plaque from different materials"
                       | v129=="plastic / cardboard"
                       | v129=="plastic sheet/tent"
                       | v129=="plastic/pvc"
                       | v129=="natte"                         
                       | v129=="rustic mat"                   
                       | v129=="rudimentary - rustic mat"      
                       | v129=="plastic/polythene sheeting", 2, roof)) %>% 
  mutate(roof = ifelse(v129=="loosely packed stone" , 2, roof)) %>% 
  mutate(roof = ifelse(v129=="other"                          
                       | v129== "OTHER"                       
                       | v129=="banco"                         
                       | v129=="mobile roofs of nomads"        
                       | v129=="Other" , 2, roof)) %>% 
  mutate(roof = ifelse(v129=="wood"                           
                       | v129=="Wood"                         
                       | v129=="wood / mud"                   
                       | v129=="planches en bois"              # wooden planks
                       | v129=="wood panel"                    
                       | v129=="wood planks"                  
                       | v129=="wood/planks"                  
                       | v129=="Wood planks"                 
                       | v129=="finished - wood"              
                       | v129=="rudimentary roofing wood planks"
                       | v129=="rudimentary - wood planks"    
                       | v129=="22"                            # rudimentary
                       | v129=="23"                            # rudimentary
                       | v129=="24"                            # rudimentary
                       | v129=="raw wood planks/timber" , 2, roof)) %>% 
  mutate(roof = ifelse(v129=="asbestos"                        
                       | v129=="Asbestos"                     
                       | v129=="asbestos sheet"               
                       | v129=="asbestos sheets" 
                       | v129=="asbesto"
                       | v129=="asbestos, wood, zinc"
                       | v129=="asbestos/zinc"
                       | v129=="asbestos/cement fiber"         
                       | v129=="asbestos sheets, shingles"   
                       | v129=="Asbestos sheets, shingles"    
                       | v129=="asbestos sheets/roofing shingles"  
                       | v129=="Roofing shingles"              
                       | v129=="calamine / cement fiber (asbestos)" 
                       | v129=="asphalt, asbestos"             
                       | v129=="finished - asbestos"          
                       | v129=="asbestos/slate roofing sheets", 3, roof)) %>% 
  mutate(roof = ifelse(v129=="corrugated iron/zinc"           
                       | v129=="corrugated iron"              
                       | v129=="corrugated iron/metal"        
                       | v129=="corregated iron (mabati)"     
                       | v129=="finished roofing metal"       
                       | v129=="galvanized sheet/metal"       
                       | v129=="iron sheets"                  
                       | v129=="Iron sheets"  
                       | v129=="iron sheets/metal"
                       | v129=="metal sheet"                  
                       | v129=="finished - metal"
                       | v129=="iron sheets/asbestos"
                       | v129=="corrugate iron"
                       | v129=="corrugated asbestos"
                       | v129=="corrugated iron sheet"
                       | v129=="galvanized iron/aluminium }"
                       | v129=="galvanized iron/aluminum"
                       | v129=="metal and ceiling"
                       | v129=="metal only"
                       | v129=="metal/ zink"
                       | v129=="t-iron/wood/brick"
                       | v129=="zinc"
                       | v129=="metal"                        
                       | v129=="Metal"                        
                       | v129=="metalic sheets"  
                       | v129=="sheet metal and ceiling"
                       | v129=="sheet metal only"
                       | v129=="metal/iron sheets"            
                       | v129=="metal / iron sheet"           
                       | v129=="metal / iron sheets"          
                       | v129=="metal/ corrugated iron"       
                       | v129=="metal/corrugated"             
                       | v129=="metal/galvanized iron/alumi"  
                       | v129=="metal/gi"                     
                       | v129=="metal/zinc"                   
                       | v129=="metal/ zinc"                  
                       | v129=="t?le"                         
                       | v129=="tin"                          
                       | v129=="tins"                         
                       | v129=="tin cans"                     
                       | v129=="zinc / cement fiber"
                       | v129=="zinc, metal, aluminum"        
                       | v129=="zinc/metal/aluminum"          
                       | v129=="zinc plates"                  
                       | v129=="zinc/cement fiber"            
                       | v129=="zinc, metal"                  
                       | v129=="Zinc/cement fiber"            
                       | v129=="tôle"     
                       | v129=="bac alu"       # metal sheet
                       | v129=="zinc/metal"  , 3, roof)) %>% 
  mutate(roof = ifelse(v129=="bricks"                         
                       | v129=="burnt brick" , 4, roof)) %>% 
  mutate(roof = ifelse(v129=="calamine / cement fiber"        
                       | v129=="Calamine/ cement fiber"       
                       | v129=="Calamine / cement fiber"      
                       | v129=="calamine/cement fiber"         
                       | v129=="calamine / cement fiber (asbestos)"  
                       | v129=="calamine/cement fibre" , 4, roof)) %>% 
  mutate(roof = ifelse(v129=="cement"                         
                       | v129=="Cement"                       
                       | v129=="finished - cement"            
                       | v129=="cement fiber"
                       | v129=="cement tiles"
                       | v129=="cement/rcc"                   
                       | v129=="concrete"     
                       | v129=="b‚ton"
                       | v129=="Concrete"                     
                       | v129=="concrete slab"                       
                       | v129=="dalles en b?ton"              
                       | v129=="cement/concrete"              
                       | v129=="concrete/cement"              
                       | v129=="cement / concrete"            
                       | v129=="concrete, cement"             
                       | v129=="Concrete, cement"             
                       | v129=="Concrete slab/ cement"  
                       | v129=="cement/beton blocks"
                       | v129=="cement/concrete/tiled"
                       | v129=="losade hormigon armado"
                       | v129=="brick/concrete"
                       | v129=="concret"
                       | v129=="reinforced concrete"
                       | v129=="concrete plaque"
                       | v129=="b,ton"
                       | v129=="calamina/ plancha"
                       | v129=="calamine /fibre of cement"
                       | v129=="calamine/ cement fiber"
                       | v129=="rcc/rbc/cement/concrete"      
                       | v129=="reinforced brick cement/rcc"  
                       | v129=="schiefer/cement fiber"
                       | v129=="tuile /tole en ciment"
                       | v129=="dalles en béton"               # concrete slabs
                       | v129=="31"                            # finished
                       | v129=="32"                            # finished
                       | v129=="33"                            # finished
                       | v129=="34"                           
                       | v129=="35"                            # finished
                       | v129=="slab"  , 4, roof)) %>% 
  mutate(roof = ifelse(v129=="ceramic tiles"                  
                       | v129=="Ceramic tiles"                
                       | v129=="clay tiles"                   
                       | v129=="mud tiles"                    
                       | v129=="ceramic/clay tiles"           
                       | v129=="roofing shingles"             
                       | v129=="roofing shingles soft"        
                       | v129=="shingles"                     
                       | v129=="Roofing shingles"
                       | v129=="ceramic tiles/brick tiles"
                       | v129=="ceramic/brick tiles"
                       | v129=="roofing shingles/shifer"
                       | v129=="teja"
                       | v129=="tejas"
                       | v129=="tar"
                       | v129=="taule"
                       | v129=="taule (tarred rough paper)"
                       | v129=="tuale (tarred roofing paper)"
                       | v129=="tuile"
                       | v129=="slate"                        
                       | v129=="finished - tiles"             
                       | v129=="ceramic tiles/harvey tiles"   
                       | v129=="ceramic tiles / harvey tiles" 
                       | v129=="tile (mud/ceramic/concrete)"  
                       | v129=="local tiling"                 
                       | v129=="roofing tiles"                
                       | v129=="industrial tiles"             
                       | v129=="tiles" 
                       | v129=="tile"
                       | v129=="tile (tole)"
                       | v129=="tile (tuile)"
                       | v129=="tiles / slates"
                       | v129=="tiles/cement/concrete/fibrous }"
                       | v129=="Tiles"                        
                       | v129=="tole"                          #tile 
                       | v129=="tuiles/ardoise/eternit"        #tiles, slate, eternit
                       | v129=="Tiles/slate"                  
                       | v129=="tiles/slate" 
                       | v129=="autre", 4, roof)) %>% 
  mutate(roof = ifelse(v129=="97"                             
                       | v129=="99", NA, roof)) %>% 
  mutate(roof = ifelse(v129=="not a de jure resident"
                       | v129=="non de jure resident"
                       | v129=="not a de jure household member"
                       | v129=="not de jure resident"
                       | v129=="not dejure member"
                       | v129=="visitor, dont know"
                       | v129=="Not a de jure resident"       
                       | v129=="not a dejure resident"        
                       | v129=="Not a dejure resident"        
                       | v129=="not de-jure resident"         
                       | v129=="not dejure resident"          
                       | v129=="Not dejure resident", NA, roof ))



df1 <- df1 %>%  
  #cooking fuel
  mutate(fuel = v161) %>% 
  mutate(fuel = ifelse(v161=="coal, lignite"                 
                       | v161=="Coal, lignite"               
                       | v161=="coal/lignite"                
                       | v161=="briquette"                    #solid, biofuel subst for coal/charcoal
                       | v161=="charcoal"                    
                       | v161=="Charcoal"                    
                       | v161=="cardboard/paper"             
                       | v161=="straw/shrubs/grass"          
                       | v161=="Straw/shrubs/grass"          
                       | v161=="straw / shrubs / grass"      
                       | v161=="Straw / shrubs / grass"      
                       | v161=="maize or other crop waste"   
                       | v161=="agricultural crop"           
                       | v161=="Agricultural crop"           
                       | v161=="crop waste"                  
                       | v161=="saw dust"                    
                       | v161=="sawdust/wood chips"          
                       | v161=="firewood/straw"              
                       | v161=="firewood, straw"             
                       | v161=="wood chips"                  
                       | v161=="animal dung"                 
                       | v161=="Animal dung"                 
                       | v161=="dung"
                       | v161=="improved smokeless chulo"
                       | v161=="coal, wood"
                       | v161=="firewood, charcoal"
                       | v161=="crop residue/grass"
                       | v161=="mineral coal"
                       | v161=="sawdust / wood chips"
                       | v161=="traditional firewood/charcoal/dung"
                       | v161=="wood"                        
                       | v161=="9"                            # Straw/shrubs/grass
                       | v161=="Wood", 1, fuel)) %>% 
  mutate(fuel = ifelse(v161=="biogas"                        
                       | v161=="Biogas"                      
                       | v161=="bottled gas"                 
                       | v161=="Bottled gas"                 
                       | v161=="natural gas"                 
                       | v161=="Natural gas"                 
                       | v161=="natural gas/biogas"
                       | v161=="cylinder gas"
                       | v161=="gasoline"
                       | v161=="propane gas"
                       | v161=="alcohol/ethanol"
                       | v161=="kerosene, oil, cocinol, diesel, gasoline, alcohol" , 2, fuel)) %>% 
  mutate(fuel = ifelse(v161=="electricity"                   
                       | v161=="Electricity"                 
                       | v161=="electricity from generator"  
                       | v161=="electricity from other source", 2, fuel)) %>% 
  mutate(fuel = ifelse(v161=="kerosene"                      
                       | v161=="Kerosene"                    
                       | v161=="paraffin"                    
                       | v161=="kerosene/paraffin"           
                       | v161=="paraffin/kerosine"              
                       | v161=="parafin/ kerosene"           
                       | v161=="jelly"                       
                       | v161== "petroleum/kerosene", 2, fuel)) %>%  
  mutate(fuel = ifelse(v161=="lpg"                           
                       | v161=="LPG"                          
                       | v161=="lpg/natural gas"             
                       | v161=="lpg / natural gas"           
                       | v161=="lpg, natural gas"   
                       | v161=="biogaz"
                       | v161=="lpg/cylinder gas", 2, fuel)) %>% 
  mutate(fuel = ifelse(v161=="solar energy"                  
                       | v161=="solar power"                 
                       | v161=="solar", 2, fuel)) %>%    
  mutate(fuel = ifelse(v161=="other"                         
                       | v161=="Other", 2, fuel)) %>% 
  mutate(fuel = ifelse(v161=="no food cooked in house" 
                       | v161=="no cocina"
                       | v161=="no food cooked in hh"        
                       | v161=="no food cooked in household" 
                       | v161=="No food cooked in household" 
                       | v161=="do not cook"                 
                       | v161=="does not cook"               
                       | v161=="No food cooked in house", 2, fuel)) %>% 
  mutate(fuel = ifelse(v161=="97"                            
                       | v161=="99", NA, fuel)) %>% 
  mutate(fuel = ifelse(v161=="not a de jure resident"        
                       | v161=="Not a de jure resident"      
                       | v161=="not a dejure resident"       
                       | v161=="Not a dejure resident"       
                       | v161=="not de-jure resident"        
                       | v161=="not dejure resident"         
                       | v161=="Not dejure resident"         
                       | v161=="not dejure member", NA, fuel ))


## Ownership of select assets

df1 <- df1 %>% 
  #Assets - electricity, radio, TV, fridge, bicycle, motorbike, car
  mutate(electricity = ifelse(v119=="yes" | v119=="Yes", 1, 0))  %>% 
  mutate(radio = ifelse(v120=="yes" | v120=="Yes", 1, 0))  %>% 
  mutate(tv = ifelse(v121=="yes" | v121=="Yes", 1, 0))  %>% 
  mutate(fridge = ifelse(v122=="yes" | v122=="Yes", 1, 0))  %>% 
  mutate(bike = ifelse(v123=="yes" | v123=="Yes", 1, 0))  %>% 
  mutate(motorbike = ifelse(v124=="yes" | v124=="Yes", 1, 0)) %>%  
  mutate(car = ifelse(v125=="yes" | v125=="Yes", 1, 0)) 
  #mutate(telephone = ifelse(v153=="yes" | v153=="Yes" | is.na(v153), 1, 0))

#lots of missing: fridge
aggregate(fridge ~ SurveyId, data=df1, function(x) {sum(is.na(x))}, na.action = NULL)

df1 <- df1 %>% 
  mutate(floor = as.numeric(floor)) %>% 
  filter(!is.na(floor) | !is.na(electricity) | !is.na(radio) | !is.na(tv) | !is.na(fridge) | !is.na(bike) | !is.na(motorbike) | !is.na(car)) 
  

## Perform PCA
library(tidyverse) 
#install.packages("devtools")
library(devtools)
#install_github("kassambara/factoextra")
#install.packages("factoextra")
library(factoextra)

pca.df <- df1 %>% 
  dplyr::select(floor, electricity, radio, tv, bike, motorbike, car, toilet.improved, water.safe) %>% 
  na.omit()

str(pca.df) #all variables need to be numeric

pca.model <- prcomp(pca.df, scale = TRUE)
fviz_eig(pca.model)
pca.pred <- predict(pca.model, newdata = df1)
pca.pred.df <- as.data.frame(pca.pred) %>% 
  dplyr::select(PC1) %>% 
  rename(wealth.score = PC1)

df1 <- df1 %>% #add the new scores to the main data
  cbind(pca.pred.df) %>% 
  mutate(v191 = v191/100000) 

## Generate 5 equally sized wealth groups - 1 (poorest) to 5 (wealthiest)
#install.packages("Hmisc")
library(Hmisc) # cut2
df1$wealth <- as.numeric(cut2(df1$wealth.score, g=5))

unique(df1$wealth)
unique(df1$v190)


#ggplot(df1, aes(x=v191, y=wealth.score)) + geom_point()


var <- c(
  ##general
  "SurveyId", "CountryName", "DHSCC", "LATNUM", "LONGNUM", "psu", "wt", "hh.id", "woman.id", "region", "residence", "stratum",
  "intYr", "intMo", "intCMC", "intDay", "birthYr", "birthMo", "birthCMC", "age", "age.months", "sex", "birth.int", "bord",
  "years.at.residence",
  #"b5", "v104",
  "resident", "birth.size","twin", "breastfed", "health.card",
  ##undernutrition
  "whz_new", "wasted", "wasted.severe", "haz_new", "stunted", "stunted.severe", "waz_new", "underweight", "underweight.severe",
  #"diarrhea", "cough", "fever", "ARI",
  ##mother's characteristics
  "age.mother", "age_mother_at_birth", "edu.level", "edu.years", "literate", "mass.media",
  #"permission.health", "stool.disposal", "distance.health",
  "partner", "BMI", "mother.height", 
  ##household characteristics
  "hh.size", "children.u5","hh.head", "occupation.male", "occupation.female", "occupation.head", 
  #"toilet.shared", "toilet.improved", "water.premise", "water.safe", 
  ##assets and wealth index
  #"floor", "wall", "roof", "fuel", 
  #"electricity", "radio", "tv", "fridge", "bike", "motorbike", "car", 
  #"wealth.old", "score.old", 
  "wealth", "wealth.score")


dhs <- df1 %>% 
  dplyr::select(var) 

dhs <- dhs %>% 
  filter(LATNUM >= 0.00005 | LATNUM <= -0.00005)  #missing obs. --> remove

## Save the harmonized survey data
rm(list=setdiff(ls(), c("dhs")))

save.image(file='DHS_data_harmonized.RData')


## Save the GPS coordinates of the PSUs - to be used to link the survey and the climate data later
clust <- dhs %>% 
  dplyr::select(CountryName, SurveyId, psu, LATNUM, LONGNUM, birthYr, birthMo, birthCMC, intYr, intMo, intCMC) %>% 
  unique()

write.csv(clust, "./DHS_clust_coord.csv")


################################################################################
## 5. Harmonize the admin areas 
################################################################################
library(tidyverse)
library(tmap)
library(sf)
library(units)
library(rgeos)
library(smoothr)
library(tiff)
library(sp)
library(raster)
library(rasterVis)
library(tmap)
library(tidyverse)
library(data.table)
library(rdhs)

rm(list =ls())

load("DHS_data_harmonized.RData")

unique(dhs$region)

# regiona names to lower case 
dhs <- dhs %>% 
  mutate_at(vars(region), funs(tolower(.)))
  

# retrieve the latest DHS spatial boundaries for each country using most recent SurveyId
IDs <- dhs %>% 
  mutate(SurveyYr = substr(SurveyId, 3, 6)) %>%      #add survey year
  dplyr::select(CountryName, SurveyId, SurveyYr) %>% 
  unique() %>% 
  group_by(CountryName) %>%
  filter(SurveyYr == max(SurveyYr)) 
IDs <-  IDs$SurveyId
IDs

bord_list_tmp <- download_boundaries(surveyId = "AO2015DHS", method = "sf")
bord_sf_tmp <- do.call(rbind.data.frame, bord_list_tmp) #convert to special feature dataframe
bord_sf <- bord_sf_tmp                                  #bind the spatial data together
#plot(bord_sf[1])

IDs <- IDs[-(1)]
IDs

for (a in IDs){
  bord_list_tmp <- download_boundaries(surveyId = a, method = "sf")
  bord_sf_tmp <- do.call(rbind.data.frame, bord_list_tmp) #convert to special feature dataframe
  bord_sf <- rbind(bord_sf, bord_sf_tmp)                  #bind the spatial data together
}

unique(bord_sf$DHSCC)
plot(bord_sf[1])

unique(bord_sf$CNTRYNAMEE)

bord_sf <- bord_sf %>%
  #dplyr::select(CNTRYNAMEE, DHSREGEN, REGNAME) %>% 
  mutate_at(vars(DHSREGEN), funs(tolower(.))) %>%
  mutate_at(vars(REGNAME), funs(tolower(.))) %>% 
  mutate(CNTRYNAMEE = ifelse(CNTRYNAMEE == "CAR", "Central African Republic", CNTRYNAMEE)) %>% 
  mutate(CNTRYNAMEE = ifelse(CNTRYNAMEE == "Swaziland", "Eswatini", CNTRYNAMEE)) 

bord_sp <- sf:::as_Spatial(bord_sf)


save.image(file='DHS_data_harmonized.RData')

## harmonize the DHS regions names with the spatial boundaries

## identify the regions that do not match
help1 <- dhs %>% 
  dplyr::select(CountryName, region) %>% 
  unique() %>% 
  mutate(srv_check = 1)

help2 <- bord_sf %>% 
  dplyr::select(CNTRYNAMEE, DHSREGEN) %>% 
  unique() %>% 
  rename(CountryName = CNTRYNAMEE,
         region = DHSREGEN) %>% 
  mutate(spt_check = 1)

st_geometry(help2) <- NULL # remove geometry, coerce to data.frame

help3 <- help1 %>% 
  left_join(help2) %>% 
  filter(is.na(srv_check) | is.na(spt_check))


dhs <- dhs %>% 
  mutate(region = ifelse(CountryName == "Benin" & region == "atlantique", "atlantic", region)) %>% 
  mutate(region = ifelse(CountryName == "Benin" & region == "ouémé", "oueme", region)) %>% 
  mutate(region = ifelse(CountryName == "Burkina Faso" & region == "east", "est", region)) %>% 
  mutate(region = ifelse(CountryName == "Burkina Faso" & region == "centre (sans ouagadougou)", "centre", region)) %>% 
  mutate(region = ifelse(CountryName == "Burkina Faso" & region == "central /south", "centre-sud", region)) %>% 
  mutate(region = ifelse(CountryName == "Burkina Faso" & region == "central/south", "centre-sud", region)) %>% 
  mutate(region = ifelse(CountryName == "Burkina Faso" & region == "north", "nord", region)) %>% 
  mutate(region = ifelse(CountryName == "Burkina Faso" & region == "ouagadougou", "centre", region)) %>% 
  mutate(region = ifelse(CountryName == "Burkina Faso" & region == "west", "hauts-bassins", region)) %>% 
  mutate(region = ifelse(CountryName == "Burkina Faso" & region == "boucle de mouhoun", "boucle du mouhoun", region)) %>% 
  mutate(region = ifelse(CountryName == "Burkina Faso" & region == "hauts bassins", "hauts-bassins", region)) %>% 
  mutate(region = ifelse(CountryName == "Burkina Faso" & region == "hauts basins", "hauts-bassins", region)) %>% 
  mutate(region = ifelse(CountryName == "Cameroon" & region == "adamaoua", "adamawa", region)) %>% 
  mutate(region = ifelse(CountryName == "Cameroon" & region == "centre (without yaounde)", "centre", region)) %>% 
  mutate(region = ifelse(CountryName == "Cameroon" & region == "est", "east", region)) %>% 
  mutate(region = ifelse(CountryName == "Cameroon" & region == "extrême-nord", "far north", region)) %>% 
  mutate(region = ifelse(CountryName == "Cameroon" & region == "extreme nor", "far north", region)) %>% 
  mutate(region = ifelse(CountryName == "Cameroon" & region == "far-north", "far north", region)) %>% 
  mutate(region = ifelse(CountryName == "Cameroon" & region == "littoral (without douala)", "littoral", region)) %>% 
  mutate(region = ifelse(CountryName == "Cameroon" & region == "nord", "north", region)) %>% 
  mutate(region = ifelse(CountryName == "Cameroon" & region == "nord-ouest", "northwest", region)) %>% 
  mutate(region = ifelse(CountryName == "Cameroon" & region == "nord ouest", "northwest", region)) %>% 
  mutate(region = ifelse(CountryName == "Cameroon" & region == "north-west", "northwest", region)) %>% 
  mutate(region = ifelse(CountryName == "Cameroon" & region == "ouest", "west", region)) %>% 
  mutate(region = ifelse(CountryName == "Cameroon" & region == "south-west", "southwest", region)) %>% 
  mutate(region = ifelse(CountryName == "Cameroon" & region == "sud", "south", region)) %>% 
  mutate(region = ifelse(CountryName == "Cameroon" & region == "sud-ouest", "southwest", region)) %>% 
  mutate(region = ifelse(CountryName == "Cameroon" & region == "sud ouest", "southwest", region)) %>% 
  mutate(region = ifelse(CountryName == "Cameroon" & region == "yaound‚ /douala", "yaounde", region)) %>% 
  mutate(region = ifelse(CountryName == "Cameroon" & region == "yaoundé", "yaounde", region)) %>% 
  mutate(region = ifelse(CountryName == "Central African Republic" & region == "rs i", "region 1", region)) %>% 
  mutate(region = ifelse(CountryName == "Central African Republic" & region == "rs ii", "region 2", region)) %>% 
  mutate(region = ifelse(CountryName == "Central African Republic" & region == "rs iii", "region 3", region)) %>% 
  mutate(region = ifelse(CountryName == "Central African Republic" & region == "rs iv", "region 4", region)) %>% 
  mutate(region = ifelse(CountryName == "Central African Republic" & region == "rs v", "region 5", region)) %>% 
  mutate(region = ifelse(CountryName == "Chad" & region == "ennedi", "ennedi est/ennedi ouest", region)) %>% 
  mutate(region = ifelse(CountryName == "Comoros" & region == "mohéli", "mwali", region)) %>% 
  mutate(region = ifelse(CountryName == "Comoros" & region == "ndzouani", "ndzuwani", region)) %>% 
  mutate(region = ifelse(CountryName == "Congo Democratic Republic" & region == "kasaï occident", "kasai-occidental", region)) %>% 
  mutate(region = ifelse(CountryName == "Congo Democratic Republic" & region == "kasaï oriental", "kasai-oriental", region)) %>% 
  mutate(region = ifelse(CountryName == "Cote d'Ivoire" & region == "center", "central", region)) %>% 
  mutate(region = ifelse(CountryName == "Cote d'Ivoire" & region == "north east", "northeast", region)) %>% 
  mutate(region = ifelse(CountryName == "Cote d'Ivoire" & region == "center east", "east central", region)) %>% 
  mutate(region = ifelse(CountryName == "Cote d'Ivoire" & region == "south west", "southwest", region)) %>% 
  mutate(region = ifelse(CountryName == "Cote d'Ivoire" & region == "center west", "west central", region)) %>% 
  mutate(region = ifelse(CountryName == "Cote d'Ivoire" & region == "center north", "north central", region)) %>% 
  mutate(region = ifelse(CountryName == "Cote d'Ivoire" & region == "north west", "northwest", region)) %>% 
  mutate(region = ifelse(CountryName == "Cote d'Ivoire" & region == "capital (abidjan)", "abidjan", region)) %>% 
  mutate(region = ifelse(CountryName == "Cote d'Ivoire" & region == "sud sans abidjan", "south", region)) %>% 
  mutate(region = ifelse(CountryName == "Cote d'Ivoire" & region == "ouest", "west", region)) %>% 
  mutate(region = ifelse(CountryName == "Cote d'Ivoire" & region == "nord-est", "northeast", region)) %>% 
  mutate(region = ifelse(CountryName == "Cote d'Ivoire" & region == "centre-nord", "north central", region)) %>% 
  mutate(region = ifelse(CountryName == "Cote d'Ivoire" & region == "centre", "central", region)) %>% 
  mutate(region = ifelse(CountryName == "Cote d'Ivoire" & region == "ville d'abidjan", "abidjan", region)) %>% 
  mutate(region = ifelse(CountryName == "Cote d'Ivoire" & region == "sud-ouest", "southwest", region)) %>% 
  mutate(region = ifelse(CountryName == "Cote d'Ivoire" & region == "centre-ouest", "west central", region)) %>% 
  mutate(region = ifelse(CountryName == "Cote d'Ivoire" & region == "centre-est", "east central", region)) %>%   
  mutate(region = ifelse(CountryName == "Cote d'Ivoire" & region == "nord", "north", region)) %>% 
  mutate(region = ifelse(CountryName == "Cote d'Ivoire" & region == "nord-ouest", "northwest", region)) %>%   
  mutate(region = ifelse(CountryName == "Ethiopia" & region == "affar", "afar", region)) %>% 
  mutate(region = ifelse(CountryName == "Ethiopia" & region == "oromiya", "oromia", region)) %>% 
  mutate(region = ifelse(CountryName == "Ethiopia" & region == "ben-gumz", "benishangul-gumuz", region)) %>% 
  mutate(region = ifelse(CountryName == "Ethiopia" & region == "snnp", "snnpr", region)) %>% 
  mutate(region = ifelse(CountryName == "Ethiopia" & region == "addis", "addis ababa", region)) %>% 
  mutate(region = ifelse(CountryName == "Ethiopia" & region == "addis abeba", "addis ababa", region)) %>%  
  mutate(region = ifelse(CountryName == "Ethiopia" & region == "benishangul", "benishangul-gumuz", region)) %>% 
  mutate(region = ifelse(CountryName == "Ethiopia" & region == "addis adaba", "addis ababa", region)) %>% 
  mutate(region = ifelse(CountryName == "Gabon" & region == "ogooué-lolo", "ogooue lolo", region)) %>% 
  mutate(region = ifelse(CountryName == "Gabon" & region == "ngounié", "ngounie", region)) %>% 
  mutate(region = ifelse(CountryName == "Gabon" & region == "libreville-port-gentil", "liberville/port gentil", region)) %>% 
  mutate(region = ifelse(CountryName == "Gabon" & region == "moyen-ogooué", "moyen ogooue", region)) %>% 
  mutate(region = ifelse(CountryName == "Gabon" & region == "ogooué-ivindo", "ogooue ivindo", region)) %>% 
  mutate(region = ifelse(CountryName == "Gabon" & region == "ogooué maritime", "ogooue maritime", region)) %>% 
  mutate(region = ifelse(CountryName == "Gabon" & region == "woleu-ntem", "woleu ntem", region)) %>% 
  mutate(region = ifelse(CountryName == "Gabon" & region == "haut-ogooué", "haut ogooue", region)) %>% 
  mutate(region = ifelse(CountryName == "Ghana" & region == "ashanti region", "ashanti", region)) %>% 
  mutate(region = ifelse(CountryName == "Ghana" & region == "brong-ahafo", "brong ahafo", region)) %>% 
  mutate(region = ifelse(CountryName == "Ghana" & region == "brong ahafo region", "brong ahafo", region)) %>% 
  mutate(region = ifelse(CountryName == "Ghana" & region == "central region", "central", region)) %>% 
  mutate(region = ifelse(CountryName == "Ghana" & region == "eastern region", "eastern", region)) %>% 
  mutate(region = ifelse(CountryName == "Ghana" & region == "greater accra region", "greater accra", region)) %>% 
  mutate(region = ifelse(CountryName == "Ghana" & region == "northern region", "northern", region)) %>% 
  mutate(region = ifelse(CountryName == "Ghana" & region == "upper east region", "upper east", region)) %>% 
  mutate(region = ifelse(CountryName == "Ghana" & region == "upper west region", "upper west", region)) %>% 
  mutate(region = ifelse(CountryName == "Ghana" & region == "volta region", "volta", region)) %>% 
  mutate(region = ifelse(CountryName == "Ghana" & region == "western region", "western", region)) %>%
  mutate(region = ifelse(CountryName == "Lesotho" & region == "botha-bothe", "butha-buthe", region)) %>% 
  mutate(region = ifelse(CountryName == "Lesotho" & region == "butha-buthe", "butha-buthe", region)) %>% 
  mutate(region = ifelse(CountryName == "Lesotho" & region == "qacha's-nek", "qacha's nek", region)) %>% 
  mutate(region = ifelse(CountryName == "Lesotho" & region == "qasha's nek", "qacha's nek", region)) %>% 
  mutate(region = ifelse(CountryName == "Lesotho" & region == "thaba tseka", "thaba-tseka", region))%>% 
  mutate(region = ifelse(CountryName == "Madagascar" & region == "antananarivo", "antananarivo capital", region)) %>% 
  mutate(region = ifelse(CountryName == "Madagascar" & region == "antsiranana", "atsinanana", region)) %>% 
  mutate(region = ifelse(CountryName == "Madagascar" & region == "analamanga", "analamanga excluding capital", region)) %>% 
  mutate(region = ifelse(CountryName == "Madagascar" & region == "anamoroni'i mania", "amoron'i mania", region)) %>% 
  mutate(region = ifelse(CountryName == "Madagascar" & region == "amoron i mania", "amoron'i mania", region)) %>% 
  mutate(region = ifelse(CountryName == "Malawi" & region == "north", "northern", region)) %>% 
  mutate(region = ifelse(CountryName == "Malawi" & region == "south", "southern", region)) %>% 
  mutate(region = ifelse(CountryName == "Malawi" & region == "southern region", "southern", region)) %>% 
  mutate(region = ifelse(CountryName == "Malawi" & region == "northern region", "northern", region)) %>% 
  mutate(region = ifelse(CountryName == "Malawi" & region == "central region", "central", region)) %>% 
  mutate(region = ifelse(CountryName == "Mali" & region == "s‚gou", "segou", region)) %>% 
  mutate(region = ifelse(CountryName == "Mali" & region == "timbuktu", "tombouctou", region)) %>% 
  mutate(region = ifelse(CountryName == "Mali" & region == "toumbouctou", "tombouctou", region)) %>% 
  mutate(region = ifelse(CountryName == "Mozambique" & region == "maputo provincia", "maputo", region)) %>% 
  mutate(region = ifelse(CountryName == "Mozambique" & region == "maputo cidade", "maputo city", region)) %>% 
  mutate(region = ifelse(CountryName == "Namibia" & region == "karas", "//karas", region)) %>% 
  mutate(region = ifelse(CountryName == "Niger" & region == "tillab‚ri", "tillaberi", region)) %>% 
  mutate(region = ifelse(CountryName == "Niger" & region == "tahoua /agadez", "agadez", region)) %>% 
  mutate(region = ifelse(CountryName == "Niger" & region == "zinda /diffa", "diffa", region)) %>% 
  mutate(region = ifelse(CountryName == "Nigeria" & region == "northwest", "north west", region)) %>% 
  mutate(region = ifelse(CountryName == "Nigeria" & region == "southeast", "south east", region)) %>% 
  mutate(region = ifelse(CountryName == "Nigeria" & region == "northeast", "north east", region)) %>% 
  mutate(region = ifelse(CountryName == "Nigeria" & region == "southwest", "south west", region)) %>% 
  mutate(region = ifelse(CountryName == "South Africa" & region == "kwazulu-natal", "kwazulu natal", region)) %>% 
  mutate(region = ifelse(CountryName == "Togo" & region == "lom‚", "greater lomé area", region)) %>% 
  mutate(region = ifelse(CountryName == "Togo" & region == "grande agglomération de lomé", "greater lomé area", region)) %>% 
  mutate(region = ifelse(CountryName == "Togo" & region == "maritime (sans agglomération de lomé)", "maritime (excluding greater lomé area)", region)) %>% 
  mutate(region = ifelse(CountryName == "Togo" & region == "marities", "maritime (excluding greater lomé area)", region)) %>% 
  mutate(region = ifelse(CountryName == "Uganda" & region == "north", "northern", region)) %>% 
  mutate(region = ifelse(CountryName == "Uganda" & region == "southwest", "south west", region)) %>% 
  mutate(region = ifelse(CountryName == "Uganda" & region == "west-nile", "west nile", region)) %>% 
  mutate(region = ifelse(CountryName == "Uganda" & region == "bugisu", "bugishu", region)) %>% 
  mutate(region = ifelse(CountryName == "Zambia" & region == "northwestern", "north western", region)) %>% 
  mutate(region = ifelse(CountryName == "Zimbabwe" & region == "harare", "harare chitungwiza", region)) %>% 
  mutate(region = ifelse(CountryName == "Zimbabwe" & region == "matebeleland north", "matabeleland north", region)) %>% 
  mutate(region = ifelse(CountryName == "Zimbabwe" & region == "matebeleland south", "matabeleland south", region)) %>% 
  #mutate(region = ifelse(CountryName == "Kenya" & region == "northeastern", "north eastern", region)) %>% 
  mutate(region = ifelse(CountryName == "Liberia" & region == "monrovia", "montserrado incl. monrovia", region)) %>% 
  mutate(region = ifelse(CountryName == "Senegal" & region == "north east", "north", region)) %>% 
  mutate(region = ifelse(CountryName == "Senegal" & region == "saint-louis", "north", region)) %>% 
  mutate(region = ifelse(CountryName == "Senegal" & region == "thiès", "west", region)) %>% 
  mutate(region = ifelse(CountryName == "Senegal" & region == "dakar", "west", region)) %>% 
  mutate(region = ifelse(CountryName == "Senegal" & region == "kolda", "south", region))

## match the rest using the GPS coordinates
dhs$regId <- paste(dhs$CountryName,"_",dhs$region, sep="")

unique(dhs$regId)

help1 <- dhs %>% 
  dplyr::select(regId) %>% 
  unique() %>% 
  mutate(srv_check = 1)


bord_sf$regId <- paste(bord_sf$CNTRYNAMEE,"_",bord_sf$DHSREGEN, sep="")
unique(bord_sf$regId)

help2 <- bord_sf %>% 
  dplyr::select(regId) %>% 
  unique() %>% 
  mutate(spt_check = 1)

st_geometry(help2) <- NULL # remove geometry, coerce to data.frame

help3 <- help1 %>% 
  left_join(help2) %>% 
  filter(is.na(srv_check) | is.na(spt_check)) #filter observations with incorrect region match

df2 <- dhs %>% 
  filter(regId %in% help3$regId)

## PSU coordinates to spatial data
pts <- df2 %>% 
  dplyr::select(CountryName, SurveyId, region, psu, LATNUM, LONGNUM) %>% 
  unique() %>% 
  filter(LATNUM >= 0.00005 | LATNUM <= -0.00005)  #missing obs. --> remove

pts_sp <- pts
coordinates(pts_sp) <- cbind(pts_sp$LONGNUM , pts_sp$LATNUM)
proj4string(pts_sp) = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
plot(pts_sp)

df3 <- over(pts_sp, bord_sp)
df3 <- cbind(pts, df3)
df3 <- df3 %>% mutate_at(vars(DHSREGEN), funs(tolower(.)))
df3 <- df3 %>% 
  dplyr::select(CountryName, SurveyId, region, psu, LATNUM, LONGNUM, CNTRYNAMEE, DHSREGEN)

df3 %>% 
  filter(CountryName != CNTRYNAMEE)

df3 <- df3 %>% 
  mutate(region_new = DHSREGEN) %>% 
  mutate(region_new = ifelse(region == "tambacounda", "south", region_new)) 

df3 <- df3 %>%
  mutate(region_new = ifelse(SurveyId == "KE2008DHS" & psu == 8, "kisumu", region_new)) %>% 
  mutate(region_new = ifelse(SurveyId == "KE2008DHS" & psu == 73, "turkana", region_new)) %>% 
  mutate(region_new = ifelse(SurveyId == "KE2008DHS" & psu == 249, "busia", region_new)) %>% 
  mutate(region_new = ifelse(SurveyId == "KE2008DHS" & psu == 261, "homa bay", region_new)) %>% 
  mutate(region_new = ifelse(SurveyId == "KE2008DHS" & psu == 341, "mandera", region_new)) 
  
df3 <- df3 %>% 
  mutate(region_new = ifelse(SurveyId == "TG1988DHS", DHSREGEN, region_new))

df3 %>% 
  filter(is.na(region_new))

df3 <- df3 %>% 
  #dplyr::select(-c(CNTRYNAMEE, DHSREGEN, LATNUM, LONGNUM)) %>% 
  dplyr::select(SurveyId, psu, region_new)

dhs <- dhs %>% 
  dplyr::select(-c(regId))

dhs <- dhs %>% 
  left_join(df3)

dhs <- dhs %>% 
  mutate(region = ifelse(!is.na(region_new), region_new, region))

dhs <- dhs %>% 
  mutate(region = ifelse(CountryName == "Kenya" & region == "bukedi", "busia", region))

## check again if all region names have been harmonized

help1 <- dhs %>% 
  dplyr::select(CountryName, region) %>% 
  unique() %>% 
  mutate(srv_check = 1)

help2 <- bord_sf %>% 
  dplyr::select(CNTRYNAMEE, DHSREGEN) %>% 
  unique() %>% 
  rename(CountryName = CNTRYNAMEE,
         region = DHSREGEN) %>% 
  mutate(spt_check = 1)

st_geometry(help2) <- NULL # remove geometry, coerce to data.frame

help3 <- help1 %>% 
  left_join(help2) %>% 
  filter(is.na(srv_check) | is.na(spt_check))


dhs <- dhs %>% 
  dplyr::select(-c(region_new))

dhs$regId <- paste(dhs$CountryName,"_",dhs$region, sep="")


unique(dhs$regId)
unique(bord_sf$DHSREGEN)

rm(list=setdiff(ls(), c("dhs", "bord_sf")))


save.image(file='DHS_data_harmonized.RData')


