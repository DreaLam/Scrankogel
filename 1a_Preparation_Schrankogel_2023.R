
## PREPARATION OF SCHRANKOGEL DATA FROM 2023

## a) Check new species: species occurred for the first time in 2023. Check plausibility together with Hari.
## b) Check number of plots surveyed in 2023 and compare with orig. forms
## c) Check, if cover of species and number of species per plot are plausible
## d) Check, if rare species are entered on purpose and not accidentally instead of common species within same genus. 
     ## When entering the data, species where listed alphabetically and therefore there is a risk of wrong entries. Check with orig Forms.

rm(list=ls(all=TRUE))
getwd()  ## should be "C:/Users/andreal/Documents/Projects/Schrankogel/Schrankogel_2023/Analysis/Schrankogel_4s"
# if not setwd("./../") 


library(RODBC)
library(stringr)
library(tidyverse)

#############################
### Access to Access-DB  ;-)#
#############################
 

#myconn <-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/andreal/Documents/Projects/Schrankogel/Schrankogel_2023/Analysis/Schrankogel_4s/SCHRAN_1994_to_2023_MicroClim_v3.accdb")
#myconn <-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=E:/Schrankogel/Schrankogel_2023/Analysis/Schrankogel_4s/SCHRAN_1994_to_2023_MicroClim_v3.accdb")
#myconn <-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=X:/projects/SCHRANKOGEL/SCHRANKOGEL2023/Dateneingabe/SCHRAN_1994_to_2023_MicroClim_v3.accdb")
myconn <-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/andrea/Documents/Projekte/Microclim/Schrankogel_GLORIA_analyse/SCHRAN_1994_to_2023_MicroClim_v3.accdb")





## via ODBC-Datenquellen-Administrator (64bit)
## Benutzer DSN: choose 'MS Access Database', 'Hinzufuegen', choose 'Microsoft Access Driver', Namen for connection (here: "Schrankogel_2023"), Datenbank Auswaehlen, OK, OK


#myconn <-odbcConnect("Schrankogel_2023", uid="", pwd="")


specAllSK <- sqlFetch(myconn, "lutSpecies") #opens connection in Access: tab with all species occur on Schrankogel and the year of first occurrence
plotlist<- sqlFetch(myconn, "tabPlotsSurveys") # all head data per plot from SK, including top cover of surfaces
spec <- sqlFetch(myconn, "tabSpeciesPlots") # all species in all plots of 2023, including cover
lutPlots <- sqlFetch(myconn, "lutPlots") # different information about plots and there surveys
PlotsDes <- sqlFetch(myconn, 'tabPlotsCategoryChanges') #flagged plots of 2023

close(myconn)



###########################################################
##Preparing the data of 2023##
###########################################################
str(spec)

colnames(spec)[2] <- "year"
#subset(spec , spec$ref_num < 1)       # use only plots with ref_num = 1 for analysis. However, in this database no other than 1 -> delete column cause it is not necessary
spec <- spec[,-3]  ## remove column ref_num (anyway only ref 1 plots in the database)
# for Gesamtschrankogel-DB: plotlist$plottyp != v (are 'Verdichtungsflächen')


colnames(plotlist)[2] <- "year"
colnames(plotlist)[6] <- "date"

spec$year <- as.factor(spec$year)
spec$fl_num <- as.factor(spec$fl_num)
spec$species <- as.factor(spec$species)
plotlist$year <- as.factor(plotlist$year)
plotlist$fl_num <- as.factor(plotlist$fl_num)


###################################
## a) New species in 2023
##################################

#new species of 2023
Sp23 <- specAllSK %>% filter(year == 2023) %>% select(species_code:Artname)
## including number of occurrence
Sp23nr <- merge(spec,Sp23[,c(1,2,3)], by = "species") %>% mutate(year.y = 1) %>% group_by(species) %>% summarize(number = sum(year.y))
Sp23 <- merge(Sp23,Sp23nr , by = "species")
rm(Sp23nr)
write.table(Sp23, "../NewSpecies2023.csv", sep = ";" , row.names = F)
rm(Sp23)
# control number of new species
count(specAllSK,year == 2023) # 12 new species


#####################################
## b) Plots surveyed in 2023 
#######################################

#check number of plots surveyed in 2023
plotlist %>%  filter(year == '2023') %>% count(!is.na(kartierer)|!is.na(date)) # 685 plots surveyed in 2023; no Plot in Plotlist without head info


####################################
## c) Cover of species and number of species per plot
######################################


# range of cover
max(spec[spec$year %in% '2023',]$cover, na.rm = TRUE)  # 60   is confirmed by photo (Care curv)
min(spec[spec$year %in% '2023',]$cover, na.rm = TRUE)  # 0,001 make sense

# Number of species per plot
SpNrPl23 <- spec %>% filter(year == '2023') %>% droplevels() %>% 
  group_by(fl_num, .drop=FALSE) %>%
  summarise(no_spec = length(fl_num)) %>% ungroup() ## so 5 plots without species
str(SpNrPl23)
## add plots with 0 species
SpNrPl23 <- merge(SpNrPl23, plotlist[plotlist$year %in% '2023',c(1,2,19)], by = 'fl_num', all.y = T)
SpNrPl23[is.na(SpNrPl23)] <- 0
colnames(SpNrPl23)[4]<- 'veg_cover'
SpNrPl23$veg_cover = round(SpNrPl23$veg_cover,1)
write.table(SpNrPl23, "../NrSpeciesPlot2023.csv", sep = ";" , row.names = F)

rm(SpNrPl23)


##################################
## d) Rare species
##################################

## Check if rare species with common opponents of the same genus are really on the orig Form: 
## List all rare species in the plot where it is unique (so no species from same genus occurred in that plot). 
## It can happen, that errors appeared while data entry. Check them by checking all the cases in rare23 checked with orig Form:

  ### Show all Agro alpi, where there is no Agro rupe in the same plot
  rarelist <- data.frame(rare = c('AGROALPI', 'ANDROBTU','ARENBIFL', 'CERACERA','CERAPEDU','DRABDUBI','FESTHALL', 'GENTBRAC','LUZUALPI','MINUGERA','PRIMHIRS','SAXIANDR','SAXIEXAR','SAXIMOSC') , comm = c('AGRORUPE','ANDRALPI','ARENCILI','CERAUNIF','CERAUNIF','DRABFLAD','FESTINTE','GENTBAVA','LUZUSPIC','MINUSEDO','PRIMGLUT','SAXIBRYO','SAXIBRYO','SAXIBRYO'))
         
  rare23 <- data.frame(matrix(ncol = 6, nrow = 0))   
  colnames(rare23) <- c("fl_num"    ,  "year"    ,    "species"  ,   "cover"     ,  "speccomment" ,"nr"    )


  for (i in 1:length(rarelist$rare)) {
    spR <- rarelist$rare[i]
    spC <- rarelist$comm[i]
    rare <- spec %>% filter(year == '2023') %>% filter(species == spR | species == spC) %>% mutate (nr = 1) %>% group_by (fl_num) %>%  summarise(nr = sum(nr)) %>%  ungroup()
    rare <- merge (spec[spec$year %in% '2023',], rare, by = 'fl_num', all.y = T) 
    rare <- rare %>% filter(species == spR ) %>% filter(nr == 1)
    rare23 <- rbind(rare23, rare) }
  
  write.table(rare23, "../rareSpeciesToCheck2023.csv", sep = ";" , row.names = F) ## check with orig Forms AND photo (when cover is big enough)

rm(rarelist, rare23, rare)
  
  # remove plots which were identified as real outliers: use therefor PlotsDes
   ## the following where removed in 2014, because of a hugh stone first time in  in 2014: at the moment they are in with lots of spec.
#spec <- droplevels(subset(spec , ! fl_num %in% c("110126" , "110226" , "110227" , "110127") & ! tr %in% "12"))
# new removes?
  
  