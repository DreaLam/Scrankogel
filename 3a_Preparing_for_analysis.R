### PREPARE FOR ANALYSIS: general preparing

## a) Use libraries and tables from 1a: plotlist, lutPlots, spec, specAllSK

# prepare tables for different analysis, starting with adding general infos: 
## b)  prepare Plotinfo (for general information of plots):  Things are already done in db. BEFORE: add 'block' per plot and delete unnecessary columns. Delete plots never recorded (after c and comparing with PlotinfoYear)!
## c)  prepare PlotinfoYear (for plot information and the respective year)
## d)  prepare SpecInfo (for general information of species): add 'altirank', F (Feuchtezahl acc. to Flora indicativa) and lifeform (LF acc. to F.indicativa) per species
#                       !! add new Nomenklatur after Fischadler 4? Or? With what does MicroClim work? Can I have a synonym list?



## e) decide, what happens with annuals (Euphrasia minima, Gentianella tenella)
## f) prepare different datasets for dat4 = all 4 years (355plots), dat3 (1994,2014,2023: 661 pots), dat2 (1994, 2023; 683 plots)



#############
## b) prepare Plotinfo (for general information of plots):  add 'block' per plot and delete unnecessary columns
##############

#Plotinfo <- lutPlots  %>% mutate ('block' = ifelse (tr < 6, 'A', 
                                                 #     ifelse (tr < 10 & tr > 5, 'B', 
                                                #              ifelse ( tr >9 & tr < 15, 'C',
                                               #                        ifelse (tr > 14, 'D', 'X')))))


Plotinfo <- lutPlots[,c(2, 4, 5, 8)]
str(Plotinfo)

Plotinfo <- Plotinfo %>% mutate_at(c('fl_num', 'tr', 'block'), as.factor)

write.table(Plotinfo, "../Plotinfo.csv", sep = ";" , row.names = F) ## for general information of plots

#############
## c)  prepare PlotinfoYear (for plot information and the respective year)
###############

PlotinfoYear <- plotlist[,c(1:4, 8:17)]

write.table(PlotinfoYear, "../PlotinfoYear.csv", sep = ";" , row.names = F) ## for information of plots in respective year

str(PlotinfoYear)  
PlotinfoYear$fl_num <- as.factor(PlotinfoYear$fl_num)
PlotinfoYear$yearF <- as.factor(PlotinfoYear$year)

## list of plots and when recorded
PlotYears <- PlotinfoYear[,1:2] 
PlotYears <-PlotYears %>% mutate(occ = 1) %>% spread(year,occ)

str(PlotYears) ### 824 levels

PlotYears <- PlotYears %>% mutate(all4 = ifelse( rowSums(.[2:5],na.rm=TRUE) == 4, 'Y', 'N'))        ### recorded in all 4 years
PlotYears <- PlotYears %>% mutate(all3 = ifelse( rowSums(.[c(2,4,5)],na.rm=TRUE) == 3, 'Y', 'N'))   ### recorded in the years 1994, 2014 and 2023
PlotYears <- PlotYears %>% mutate(all2 = ifelse( rowSums(.[c(2,5)],na.rm=TRUE) == 2, 'Y', 'N'))   ### recorded in the years 1994 and 2023


write.table(PlotYears, "../PlotYears.csv", sep = ";" , row.names = F)

### number of plots recorded in all 4 years: 
PlotYears %>%  count (all4)  ## 355 plots

### number of plots recorded in 3 years (1994,2014,2023):
PlotYears %>%  count (all3)  ## 661 plots

### number of plots recorded in 2 years (1994,2023):
PlotYears %>%  count (all2)  ## 683 plots

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
  # specList <- levels(spec$species) ### 86 species really occurred in one of the plots at least in one year.
 # SpecInfo <- SpecInfo %>% filter(species %in% specList)
SpecInfo <-SpecInfo[,-c(5,7)]

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

#rm(SpecNO, diff, SpAlti, SpF_LF)


### Attention: Polygonum viviparum 2 x: POLYVIVI und PERSVIVI!!!    SAVED!!! and corrected in Access DB
##PolyVivi <- spec %>% filter(species %in% 'POLYVIVI' | species %in% 'PERSVIVI')  ### <2023 PersVivi, 2023 Polyvivi!!!!!
#rm(PolyVivi)
### Changed in ACCESS Schrankogel DB!


#############
## check of empty plots
###########

## in total 824 plots recorded in one of the 4 years. In spec: 808 plots with spec in at least one of the 4 years
#diff <- Plotinfo %>% mutate(flY = ifelse(fl_num %in% levels(spec$fl_num), 'Y', 'N'))
#diff <- diff %>% filter(flY == 'N')
Empty_plot <- Plotinfo %>%  filter(fl_num %in% diff[diff$flY == 'N',]$fl_num)   ### 

write.table(Empty_plot, "../Empty_plots.csv", sep = ";" , row.names = F)

rm(Empty_plot, diff)

rm(lutPlots, plotlist, specAllSK)


########################################
## e) decide, what happens with annuals (Euphrasia minima, Gentianella tenella)
##########################

Annual <- spec %>% filter(species == 'EUPHMINI' | species == 'GENTTENE') %>% group_by(year,species)  %>% summarise( n_spec = length(year)) %>% ungroup()
write.table(Annual, "../Annuals.csv", sep = ";" , row.names = F)

Annual4 <- merge(spec, Plotinfo[,c(1,3)], all.x = T) %>% filter(species == 'EUPHMINI' | species == 'GENTTENE') %>% filter(fl_num %in% levels(dat4$fl_num)) %>% group_by(year,species)  %>% summarise( n_spec = length(year)) %>% ungroup()
rm(Annual4)

spec_inklAnnuals <- spec  ### to use spec without annuals
spec <- spec %>% filter(species != 'EUPHMINI') %>% filter(species != 'GENTTENE')

rm(Annual)
##################
# f) Create 3 different datasets: dat4 = all 4 years (355plots), dat3 (1994,2014,2023: 661 pots), dat2 (1994, 2023; 683 plots)
dat <- PlotinfoYear[,1:2] %>% mutate(occ=1)
dat <- dat %>% group_by(fl_num) %>% spread(year, occ) %>% ungroup()
dat4 <- dat %>% filter( rowSums(.[2:5]) == 4)  ### 355 plots
dat4 <- dat4 %>% mutate(dat4 = 'YES')
dat4$fl_num <- as.factor(dat4$fl_num)
dat4 <- droplevels(dat4)

dat3 <- dat %>% filter( rowSums(.[c(2,4,5)]) == 3) ### 661 plots
dat3 <- dat3 %>% mutate(dat3 = 'YES')
dat3$fl_num <- as.factor(dat3$fl_num)
dat3 <- droplevels(dat3)

dat2 <- dat %>% filter( rowSums(.[c(2,5)]) == 2) ### 683 plots
dat2 <- dat2 %>% mutate(dat2 = 'YES')
dat2$fl_num <- as.factor(dat2$fl_num)
dat2 <- droplevels(dat2)

rm(dat)
