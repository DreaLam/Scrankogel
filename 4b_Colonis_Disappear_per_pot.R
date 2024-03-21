#### ANALYSE Colonisations and disappearances per plot ###
## I used workspace Schrankogel23 to create this workspace SK23_codi
library(stringr)
library(tidyverse)
#library(plyr)
library(ggplot2)
library(Matrix)
library(lme4)
library(emmeans)
library(lsmeans)
lsmeans <- lsmeans::lsmeans

#detach(package:plyr) ### when tidyverse functions are not working

## Colonisations: number of species per plot present at the time of the resurvey, absent in the respective plot at the previous survey.
spec0 <- merge(spec , PlotinfoYear[,c(1,2)], all = T)  ### to add 0
spec0$cover <- spec0$cover %>% replace(is.na(.), 0) 


colo <- spec0[,-c(3,6:8)] %>% group_by(fl_num,species) %>% spread(year, cover)
colo[,3:6] <- colo[,3:6] %>% replace(is.na(.),0)
str(colo)
colnames(colo)[-c(1,2)] <- paste0("Y", colnames(colo)[-c(1,2)])

colo <- colo %>% mutate(P9404 = ifelse((Y2004 > 0 & Y1994 == 0), 1,0))
colo <- colo %>% mutate(P0414 = ifelse((Y2014 > 0 & Y2004 == 0), 1,0))
colo <- colo %>% mutate(P1423 = ifelse((Y2023 > 0 & Y2014 == 0), 1,0))

colo <- colo %>% mutate(P9414 = ifelse((Y2014 > 0 & Y1994 == 0), 1,0))

colo <- colo %>% mutate(P9423 = ifelse((Y2023 > 0 & Y1994 == 0), 1,0))

colo <- colo %>% mutate(type = 'colo')

## Disappearance: number of species per plot absent at the time of the resurvey, present in the respective plot at the previous survey.
disa <- spec0[,-c(3,6:8)] %>% group_by(fl_num,species) %>% spread(year, cover)
disa[,3:6] <- disa[,3:6] %>% replace(is.na(.),0)
str(disa)
colnames(disa)[-c(1,2)] <- paste0("Y", colnames(disa)[-c(1,2)])

disa <- disa %>% mutate(P9404 = ifelse((Y2004 == 0 & Y1994 > 0), 1,0))
disa <- disa %>% mutate(P0414 = ifelse((Y2014 == 0 & Y2004 > 0), 1,0))
disa <- disa %>% mutate(P1423 = ifelse((Y2023 == 0 & Y2014 > 0), 1,0))

disa <- disa %>% mutate(P9414 = ifelse((Y2014 == 0 & Y1994 > 0), 1,0))

disa <- disa %>% mutate(P9423 = ifelse((Y2023 == 0 & Y1994 > 0), 1,0))

disa <- disa %>% mutate(type = 'disa')

codi <- rbind(colo,disa)

codi <- codi[,-c(2:6)]
codi <- codi %>% group_by(fl_num, type) %>% summarise(p9404=sum(P9404), p0414 = sum(P0414), p1423=sum(P1423), p9414 = sum(P9414), p9423 = sum(P9423)) %>%  ungroup()
str(codi)
codi$type <- as.factor(codi$type)

codi <- merge(codi, Plotinfo, all.x = T)
codi <- codi[,c(1,8,9,10,2:7)]


codi4 <-  codi %>% filter(fl_num %in% levels(dat4$fl_num))
codi4 <- droplevels(codi4)
codi4 <- codi4[,-c(9,10)]
str(codi4)
codi4 <- codi4 %>%  gather(key='period', value = 'number', p9404:p1423)
  

codi3 <-  codi %>% filter(fl_num %in% levels(dat3$fl_num) )
codi3 <- droplevels(codi3)
codi3 <- codi3[,c(1:5,9,8)]
str(codi3)  ### all 661, including always empty plots
codi3 <- codi3 %>%  gather(key='period', value = 'number', p9414:p1423)

codi2 <-  codi %>% filter(fl_num %in% levels(dat2$fl_num) )
codi2 <- droplevels(codi2)
codi2 <- codi2[-c(6:9)]
str(codi2)  ### all 863, including always empty plots


## B) colo/disa per tr and period: RAW -----------
library(plyr)
codi_mean_tr <-  ddply(codi4, c("tr", 'type', "period"), summarise,
                      N    = length(number),
                      mean = mean(number),
                      sd   = sd(number),
                      se   = sd / sqrt(N)
)

codi_mean_tr[,5:7] <- round(codi_mean_tr[,5:7], 2)
colnames(codi_mean_tr)[7] <- 'SE'


ggplot(codi_mean_tr[codi_mean_tr$type =='colo',],  aes(x=period, y=mean,  colour= tr)) +
  geom_point(size=2 )+
  geom_line(lty = 2) +
  scale_colour_brewer(palette="Set2") +
  geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2) +
  ylab ("Colo tr and period (mean +/- SE)") +
  ggtitle("Colonisation, raw") +
  theme_bw() +
  ggsave("./../plots_graphs/colo4_mean_tr_years_raw.tiff")


ggplot(codi_mean_tr[codi_mean_tr$type =='disa',],  aes(x=period, y=mean,  colour= tr)) +
  geom_point(size=2 )+
  geom_line(lty = 2) +
  scale_colour_brewer(palette="Set2") +
  geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2) +
  ylab ("Disa tr and period (mean +/- SE)") +
  ggtitle("Disappearances, raw") +
  theme_bw() +
  ggsave("./../plots_graphs/disa4_mean_tr_years_raw.tiff")





## C) mean colo per period: RAW -----------
    codi_mean <-  ddply(codi4, c( 'type', "period"), summarise,
                       N    = length(number),
                       mean = mean(number),
                       sd   = sd(number),
                       se   = sd / sqrt(N)
    )
  
  codi_mean[,4:6] <- round(codi_mean[,4:6], 2)
  colnames(codi_mean)[6] <- 'SE'
  
  ggplot(codi_mean[codi_mean$type =='colo',],  aes(x=period, y=mean)) +
    geom_point(size=2 )+
    geom_line(lty = 2) +
    scale_colour_brewer(palette="Set2") +
    geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2) +
    ylab ("Colo per plot (mean +/- SE)") +
    ggtitle("Colonisation, raw") +
    theme_bw() +
    ggsave("./../plots_graphs/colo4_mean_period_raw.tiff")
  
  ggplot(codi_mean[codi_mean$type =='disa',],  aes(x=period, y=mean)) +
    geom_point(size=2 )+
    geom_line(lty = 2) +
    scale_colour_brewer(palette="Set2") +
    geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2) +
    ylab ("Disa per plot (mean +/- SE)") +
    ggtitle("Disappearance, raw") +
    theme_bw() +
    ggsave("./../plots_graphs/disa4_mean_period_raw.tiff")
  
  
  
  
  rm(codi_mean, codi_mean_tr)






