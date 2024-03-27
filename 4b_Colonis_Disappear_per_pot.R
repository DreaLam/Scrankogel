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
library(MASS)
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
codi4$period <- as.factor(codi4$period)  

codi3 <-  codi %>% filter(fl_num %in% levels(dat3$fl_num) )
codi3 <- droplevels(codi3)
codi3 <- codi3[,c(1:5,9,8)]
str(codi3)  ### all 661, including always empty plots
codi3 <- codi3 %>%  gather(key='period', value = 'number', p9414:p1423)
codi3$period <- as.factor(codi3$period)  

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
  theme_bw()  +
  ggsave("./../plots_graphs/disa4_mean_tr_years_raw.tiff")

codi_mean_tr <-  ddply(codi2, c("tr", 'type'), summarise,
                       N    = length(p9423),
                       mean = mean(p9423),
                       sd   = sd(p9423),
                       se   = sd / sqrt(N)
)
### plot makes no sense for codi2

rm(codi_mean_tr)


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
  
  write.table(codi_mean, "../codi4_mean.csv", sep = ";", row.names = F)
  
  codi_mean <-  ddply(codi2, c( 'type'), summarise,
                      N    = length(p9423),
                      mean = mean(p9423),
                      sd   = sd(p9423),
                      se   = sd / sqrt(N)
  )
  
  
  rm(codi_mean)


  
  ## D) colo/disa per block and period: RAW -----------
  
  codi_mean_bl <-  ddply(codi3, c("block", 'type', "period"), summarise,
                         N    = length(number),
                         mean = mean(number),
                         sd   = sd(number),
                         se   = sd / sqrt(N)
  )
  
  codi_mean_bl[,5:7] <- round(codi_mean_bl[,5:7], 2)
  colnames(codi_mean_bl)[7] <- 'SE'
  
  
  ggplot(codi_mean_bl[codi_mean_bl$type =='colo',],  aes(x=period, y=mean,  colour= block)) +
    geom_point(size=2 )+
    geom_line(lty = 2) +
    scale_colour_brewer(palette="Set2") +
    geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2) +
    ylab ("Number of colonisation (mean +/- SE)") +
    ggtitle("Colonisation per block, raw") +
    theme_bw() +
    ggsave("./../plots_graphs/colo3_mean_block_years_raw.tiff")
  
  
  ggplot(codi_mean_bl [codi_mean_bl$type =='disa',],  aes(x=period, y=mean,  colour= block)) +
    geom_point(size=2 )+
    geom_line(lty = 2) +
    scale_colour_brewer(palette="Set2") +
    geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2) +
    ylab ("Number of disappearances (mean +/- SE)") +
    ggtitle("Disappearances per block, raw") +
    theme_bw()  +
    ggsave("./../plots_graphs/disa3_mean_block_years_raw.tiff")
  
  codi_mean_bl <-  ddply(codi2, c("block", 'type'), summarise,
                         N    = length(p9423),
                         mean = mean(p9423),
                         sd   = sd(p9423),
                         se   = sd / sqrt(N)
  )

  rm(codi_mean_bl)

### E) analysis
  ## in Lamprecht et al. 2018: GLMM(glmmer) with Negative binomial
  
  ## Data exploration following Zuur et al. 2009
  
  ggplot( codi4[codi4$type=='colo',], aes(x= number))  + geom_histogram()
  
  ggplot( codi4[codi4$type=='disa',], aes(x= number))  + geom_histogram()
  
  # quite skewed to low values, count data: Poisson, neg binom?
  
  
  ##################################################
   # Building the null models to choose distribution for counts
  glm_PO<-glm(codi4$number~1,family="poisson")
  glm_NB<-glm.nb(codi4$number~1)
  # Diagnostics on the NULL models
  par(mfrow=c(2,4))
  plot(glm_PO, main="Poisson")
  plot(glm_NB, main="Negative binomial")
  AIC(glm_PO)
  AIC(glm_NB)
  # neg bin has lower AIC
  
  rm(glm_PO, glm_NB)
  
  codi_Y <- lmer(number ~ period*type +  (1 |block/tr/fl_num), data = codi4)
  codi_Y_P <- glmer(number ~ period*type +  (1 |block/tr/fl_num), data = codi4, family = poisson())
  codi_Y_P_PQL <- glmmPQL(number ~ period*type,~ 1 |block/tr/fl_num, data = codi4, family = poisson())
  codi_nb <- glmer.nb(number ~ period*type +  (1|block/tr/fl_num) , data=codi4) ## warnings: Iterationsgrenze erreicht
  anova(codi_Y,codi_Y_P, codi_Y_P_PQL, codi_nb )   ### AIC is lowest with nb
  summary(codi_nb)
  
  rm(codi_Y,codi_Y_P, codi_Y_P_PQL, codi_nb ) 
  
  ## just colo
  colo_Y <- lmer(number ~ period +  (1 |block/tr/fl_num), data = codi4[codi4$type =='colo',])
  colo_Y_P <- glmer(number ~ period +  (1 |block/tr/fl_num), data = codi4[codi4$type =='colo',], family = poisson())
  colo_Y_P_PQL <- glmmPQL(number ~ period,~ 1 |block/tr/fl_num, data = codi4[codi4$type =='colo',], family = poisson())
  colo_nb <- glmer.nb(number ~ period +  (1|block/tr/fl_num) , data=codi4[codi4$type =='colo',])
  anova(colo_Y,colo_Y_P, colo_Y_P_PQL, colo_nb )   ### AIC is lowest with nb

  summary(colo_nb)
  summary(colo_Y_P)
    
  rm(colo_Y,colo_Y_P, colo_Y_P_PQL) 
  
  ## just disa
  disa_Y <- lmer(number ~ period +  (1 |block/tr/fl_num), data = codi4[codi4$type =='disa',])
  disa_Y_P <- glmer(number ~ period +  (1 |block/tr/fl_num), data = codi4[codi4$type =='disa',], family = poisson())
  disa_Y_P_PQL <- glmmPQL(number ~ period,~ 1 |block/tr/fl_num, data = codi4[codi4$type =='disa',], family = poisson())
  disa_nb <- glmer.nb(number ~ period +  (1|block/tr/fl_num) , data=codi4[codi4$type =='disa',])
  anova(disa_Y,disa_Y_P, disa_Y_P_PQL, disa_nb )   ### AIC is lowest with nb
  
  summary(disa_nb)
  summary(disa_Y_P)
  
  rm(disa_Y,disa_Y_P, disa_Y_P_PQL)
  
  colo_Y_emmeans <- pairs(emmeans(colo_nb, ~period))
  colo_Y_emmeans<-as.data.frame(colo_Y_emmeans)
  colo_Y_emmeans$type <- 'colo'
  
  disa_Y_emmeans <- pairs(emmeans(disa_nb, ~period))
  disa_Y_emmeans<-as.data.frame(disa_Y_emmeans)
  disa_Y_emmeans$type <- 'disa'

  codi_emmeans <- rbind(colo_Y_emmeans,disa_Y_emmeans)
  codi_emmeans[,2:3] <- round(codi_emmeans[,2:3], 2)
  codi_emmeans[,5:6] <- round(codi_emmeans[,5:6], 4)
  codi_emmeans$period <- 'NA'  
  codi_emmeans[codi_emmeans$contrast == 'p0414 - p1423',]$period <- 'P23'
  codi_emmeans[codi_emmeans$contrast == 'p0414 - p9404',]$period <- 'P21'
  codi_emmeans[codi_emmeans$contrast == 'p1423 - p9404',]$period <- 'P31'
  codi_emmeans$mod <- 'glmer.nb'

  write.table(codi_emmeans , "../model_codi4_emmeans.csv", sep = ";", row.names = F)
  