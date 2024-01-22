### PREPARE FOR ANALYSIS: general preparing

## a) Use libraries and tables from 1a: plotlist, lutPlots, spec, specAllSK

# prepare tables for different analysis, starting with adding general infos: 
## b)  prepare Plotinfo (for general information of plots):  add 'block' per plot and delete unnecessary columns. Delete plots never recorded (after c and comparing with PlotinfoYear)!
## c)  prepare PlotinfoYear (for plot information and the respective year)
## d)  prepare SpecInfo (for general information of species): add 'altirank', F (Feuchtezahl acc. to Flora indicativa) and lifeform (LF acc. to F.indicativa) per species
#                       !! add new Nomenklatur after Fischadler 4? Or? With what does MicroClim work? Can I have a synonym list?



## e) decide, what happens with annuals (Euphrasia minima, Gentianella tenella)
## f) decide, which plots should be removed: see PlotsDes (destroyed plots 2023 described here)



#############
## b) prepare Plotinfo (for general information of plots):  add 'block' per plot and delete unnecessary columns
##############

Plotinfo <- lutPlots  %>% mutate ('block' = ifelse (tr < 6, 'A', 
                                                      ifelse (tr < 10 & tr > 5, 'B', 
                                                              ifelse ( tr >9 & tr < 15, 'C',
                                                                       ifelse (tr > 14, 'D', 'X')))))


Plotinfo <- Plotinfo[,-c(4:18,20,21)]
str(Plotinfo)

Plotinfo <- Plotinfo %>% mutate_at(c('fl_num', 'tr', 'block'), as.factor)

Plotinfo <- Plotinfo[,c(1,2,3,5,4)]


write.table(Plotinfo, "../Plotinfo.csv", sep = ";" , row.names = F) ## for general information of plots

#############
## c)  prepare PlotinfoYear (for plot information and the respective year)
###############

PlotinfoYear <- plotlist[,-c(3:11, 26:70)]
write.table(PlotinfoYear, "../PlotinfoYear.csv", sep = ";" , row.names = F) ## for information of plots in respective year

str(PlotinfoYear)  

## list of plots and when recorded
PlotYears <- PlotinfoYear[,1:2] 
PlotYears <-PlotYears %>% mutate(occ = 1) %>% spread(year,occ)

str(PlotYears) ### 824 levels! Not 827 as in lutPlots

PlotYears <- PlotYears %>% mutate(all4 = ifelse( rowSums(.[2:5],na.rm=TRUE) == 4, 'Y', 'N'))        ### recorded in all 4 years
PlotYears <- PlotYears %>% mutate(all3 = ifelse( rowSums(.[c(2,4,5)],na.rm=TRUE) == 3, 'Y', 'N'))   ### recorded in the years 1994, 2014 and 2023

write.table(PlotYears, "../PlotYears.csv", sep = ";" , row.names = F)

### number of plots recorded in all 4 years: 
PlotYears %>%  count (all4)  ## 355 plots

### number of plots recorded in all 4 years:
PlotYears %>%  count (all3)  ## 661 plots



## before PlotYears and plotinfo not same number of plots. Check again:
#diff <- Plotinfo %>% mutate(PlY = ifelse(fl_num %in% levels(PlotYears$fl_num), 'Y', 'N'))
#diff[diff$PlY == 'N',]  ### 0 rows, because already changed in DB


#lutPlots %>% filter(fl_num %in% diff[diff$PlY == 'N',]$fl_num)   ### those 3 plots never recorded, therefore delete from Plotinfo
#PlotsNO <- diff[diff$PlY == 'N',]
#write.table(PlotsNO, "../PlotsNO.csv", sep = ";" , row.names = F)

#rm(PlotsNO, diff)

### addition to b) Delete plots never recorded from Plotinfo:  already done in Access DB -> new version: v3

#Plotinfo <- Plotinfo %>% filter(fl_num %in% diff[diff$PlY == 'Y',]$fl_num)
#write.table(Plotinfo, "../Plotinfo.csv", sep = ";" , row.names = F)



########################
## d)  prepare SpecInfo (for general information of species): add 'altirank', F (Feuchte?) and lifeform per species
##########################
### all infos now also in Access DB, so included in specAllSK; no need to go through all the steps

   #SpecInfo <- read.csv( '../Arten_mit_indicator_value.csv', sep = ';')

    ##add missing species
  #SpecInfo <- merge(specAllSK[,2:4], SpecInfo, by = 'species', all.x = T)
    ## only species really occur
  #specList <- levels(spec$species) ### 85 species really occurred in one of the plots at least in one year.
  # SpecInfo <- SpecInfo %>% filter(species %in% specList)

  #str(specAllSK)
  #str(SpecInfo)

  #SpecInfo <- SpecInfo %>% mutate_at(c('species'), as.factor)

  #colnames(SpecInfo)[4] <-'alt'  ## for altirank
  #SpecInfo<- SpecInfo[,1:5]
  #SpecInfo<- SpecInfo[,-3]  ### remove year, it just shows, when first occurr

  #colnames(SpecInfo)[2] <-'speciesName'

   ##adding missing altiranks plus check others
  #SpAlti <- read.csv( '../SpeciesAltiranks.csv', sep = ';')
  #str(SpAlti)
  #SpAlti$species <- as.factor(SpAlti$species)

  #SpecInfo <- merge(SpecInfo,SpAlti, by = 'species')
  #SpecInfo <- SpecInfo %>%  mutate (check = ifelse(alt.x %in% alt.y, 'Y', 'N'))    ## OK, all present data where the same, so alt.y can be accepted
  #SpecInfo <- SpecInfo[,c(1,2,5,4)]
  #colnames(SpecInfo)[3] <-'alt'

    ## add missing F and lifeforms from Flora indicativa
  #SpF_LF <- read.csv( '../SpecF_LF.csv', sep = ';')
  #SpF_LF[SpF_LF$F %in% '03.Mai',]$F <- 3.5   ### xls was doing awful things
  #SpF_LF[SpF_LF$F %in% '02.Mai',]$F <- 2.5   ### xls was doing awful things

  #SpecInfo <- merge(SpecInfo,SpF_LF[,-c(2,5)], by = 'species')
  #SpecInfo <- SpecInfo %>% mutate(check = ifelse (F.x %in% F.y, 'Y','N'))   ### F.y more complete, otherwise same
  #SpecInfo <- SpecInfo[,c(1:3,5,6)]
  #colnames(SpecInfo)[4]<- 'F'

SpecInfo <- specAllSK
  specList <- levels(spec$species) ### 85 species really occurred in one of the plots at least in one year.
SpecInfo <- SpecInfo %>% filter(species %in% specList)
SpecInfo <-SpecInfo[,-c(1,4)]

SpecInfo$species <- as.factor(SpecInfo$species)
specAllSK$species <- as.factor(specAllSK$species)

   ## diff of species in specAllSK and SpecInfo
#diff <- specAllSK %>% mutate(SpY = ifelse(species %in% levels(SpecInfo$species), 'Y', 'N'))

#SpecNO <- specAllSK %>% filter(species %in% diff[diff$SpY == 'N',]$species)   ### those 8 species never recorded, therefore delete from SpecAllSK!
#write.table(SpecNO, "../SpecNO.csv", sep = ";" , row.names = F)

## remove SpecNO from SpecInfo: done in Access DB -> new version: v3

#SpecInfo <- SpecInfo %>% filter(species %in% diff[diff$SpY == 'Y',]$species)
str(SpecInfo)

write.table(SpecInfo, "../SpecInfo.csv", sep = ";" , row.names = F)

rm(SpecNO, diff, SpAlti, SpF_LF)


### Attention: Polygonum viviparum 2 x: POLYVIVI und PERSVIVI!!!    SAVED!!! and corrected in Access DB
##PolyVivi <- spec %>% filter(species %in% 'POLYVIVI' | species %in% 'PERSVIVI')  ### <2023 PersVivi, 2023 Polyvivi!!!!!
#rm(PolyVivi)
### Changed in ACCESS Schrankogel DB!


#############
## check of empty plots
###########

## in total 824 plots recorded in one of the 4 years. In spec: 808 plots with spec in at least one of the 4 years
diff <- Plotinfo %>% mutate(flY = ifelse(fl_num %in% levels(spec$fl_num), 'Y', 'N'))

Empty_plot <- Plotinfo %>%  filter(fl_num %in% diff[diff$flY == 'N',]$fl_num)

Empty_plot <- merge (Empty_plot , lutPlots[,c(1,6:9)], by = 'fl_num')   ### checked and confirmed
write.table(Empty_plot, "../Empty_plots.csv", sep = ";" , row.names = F)

rm(Empty_plot, diff)

rm(lutPlots, plotlist, specAllSK)

### f) Suggestion HP: remove same plots as in 2014 (for Lamprecht etal 2018): "110126" , "110226" , "110227" , "110127"
spec <- droplevels(subset(spec , ! fl_num %in% c("110126" , "110226" , "110227" , "110127") & ! tr %in% "12"))
