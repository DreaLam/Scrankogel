
## CHECKS BETWEEN DIFFERENT SURVEY YEARS

## a) How many plots were recorded in all 4 years (194, 2004, 2014 and 2023); how many in 1994, 2014 and 2023
## b) Check species occurrence between surveys, especially, when same species from same genus alternating: Check with orig. forms and with pictures, if possible.


library(RODBC)
library(stringr)
library(tidyverse)
library(dplyr)

## use 
 # plotlist, 
 # spec and 
 # lutPlots from 1a_Prep




##################
## a) List of all plots recorded in all 4 years and recorded in 1994 and 2023
#############################

### all 4 years
all4 <- lutPlots %>% filter (recordedImmer == 1) %>% droplevels
all4 <- as.factor(all4$fl_num)   ### 355 plots

# only94_14_23
all94_14_23 <- lutPlots %>% filter (recorded1994 == 1 & recorded2014 == 1 & recorded2023 == 1) %>% droplevels
all3 <- as.factor(all94_14_23$fl_num)   ### 661 plots

rm(all94_14_23, all3)
#############################
### b) Check species occurrence between surveys, especially, when same species from same genus alternating
######################################

## Cross table of Species cover per species and plot in every of the 4 years

checkSP4 <- spec %>% select(-5) %>% filter(fl_num %in% all4) %>%  spread(year, cover) %>% droplevels()
str(checkSP4)


write.table(checkSP4, "../Check_4surv_species_per_Plot.csv", sep = ";" , row.names = F)


## table with species cover per species and plot in every year, when spec not occurred in every year
checkSP4_N <- checkSP4  %>%  mutate (no_occ = rowSums(is.na(checkSP4) )) %>% filter (no_occ != 0)


write.table(checkSP4_N, "../Check_4surv_species_per_Plot_with NAs.csv", sep = ";" , row.names = F)



## add a Column for same Genus in same plot:
checkSP4_G <- checkSP4 %>%  mutate (genus = as.factor(str_sub(species, 1,4)))  
nG <- checkSP4_G %>% 
group_by(fl_num) %>%
count(genus)  %>% ungroup ()

          
checkSP4_G <- merge (checkSP4_G , nG, all.x = T)

checkSP4_G_more <- checkSP4_G %>% filter (n>1)

### same with talbes where NA occurs (so species not in all 4 summits):
checkSP4_N_G <- checkSP4_N %>%  mutate (genus = as.factor(str_sub(species, 1,4)))  
nNG <- checkSP4_N_G %>% 
  group_by(fl_num) %>%
  count(genus)  %>% ungroup ()


checkSP4_N_G <- merge (checkSP4_N_G , nNG, all.x = T)

checkSP4_N_G_more <- checkSP4_N_G %>% filter (n>1)        #### 'only' 191 rows to check

write.table(checkSP4_N_G_more, "../Check_4surv_species_per_Plot_with_NAs_same_genus.csv", sep = ";" , row.names = F)


### same with data from 1994_2014_2023
checkSP3 <- spec %>% select(-5) %>% filter(fl_num %in% all3, year != '2004') %>%  spread(year, cover) %>% droplevels()  
checkSP3 <- checkSP3 %>%  
  mutate (no_occ = rowSums(is.na(checkSP3) )) %>% filter (no_occ != 0) 

checkSP3 <- checkSP3 %>% 
  mutate (genus = as.factor(str_sub(species, 1,4)))

nG3 <- checkSP3 %>% 
  group_by(fl_num) %>%
  count(genus)  %>% ungroup ()

checkSP3_G <- merge (checkSP3 , nG3, all.x = T)

checkSP3_G <- checkSP3_G %>% filter (n>1)      ### 506 rows to check

### add a column which of the plots are also in all 4
checkSP3_G <- checkSP3_G %>% mutate (inAll4 = ifelse(fl_num %in% all4, 1, 0)) 

write.table(checkSP3_G, "../Check_3surv_species_per_Plot_with_NAs_same_genus.csv", sep = ";" , row.names = F)


rm(checkSP3,checkSP3_G,checkSP4,checkSP4_G,checkSP4_G_more,checkSP4_N,checkSP4_N_G,checkSP4_N_G_more,nG, nG3, nNG)


