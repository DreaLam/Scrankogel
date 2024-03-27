#### ANALYSE thermophilisation 
## I used workspace Schrankogel23 to create this workspace SK23_therm
#library(Matrix)
#library(lme4)
library(plyr)
library(tidyverse)
library(ggplot2)
library(MASS)

detach(package:plyr)  ### when tidyverse functions are not working

str(spec)

## A) thermic indicator per plot and year
TI <- merge(spec , PlotinfoYear[,c(1,2)], all = T)  ### to add empty plots
str(TI)  ## now 824 levels of fl_num

TI$cover <- TI$cover %>% replace(is.na(.), 0) 

TI <- merge(TI[,-c(6,7)] , Plotinfo[,c(1:3)], by='fl_num', all.x = T) ### add tr and block for new plots
TI$yearF <- as.factor(TI$year)
str(TI)

### add altirank and other traits
TI <- merge(TI, SpecInfo[,c(4,6:8)])
TI <- TI %>% mutate(covXalt = cover*alti_rank)
TI <-TI %>% group_by(fl_num,tr,block,year, yearF) %>% summarise( TI = sum(covXalt)/sum(cover)) %>%  ungroup()

## TI not with empty plots, because 0 as TI is not possible.

TI4 <- TI %>%  filter(fl_num %in% levels(dat4$fl_num))
TI4 <- droplevels(TI4)
str(TI4)  ### 355 plots

TI3 <- TI %>%  filter(fl_num %in% levels(dat3$fl_num)& year != '2004')
TI3 <- droplevels(TI3)
str(TI3)  ### 658 plots (without empty plots)

TI2 <- TI %>%  filter(fl_num %in% levels(dat2$fl_num) & year != '2004' & year != '2014')
TI2 <- droplevels(TI2)
str(TI2)  ### 680 plots (without empty plots)



### mean TI per year
library(plyr)
TI_mean <-  ddply(TI4, 'year', summarise,
                   N    = length(TI),
                   mean = mean(TI),
                   sd   = sd(TI),
                   se   = sd / sqrt(N)
)

TI_mean[,3:5] <- round(TI_mean[,3:5], 2)
colnames(TI_mean)[5] <- 'SE'

ggplot(TI_mean,  aes(x=year, y=mean)) +
  geom_point(size=2 )+
  #geom_line(lty = 2) +
  scale_colour_brewer(palette="Set2") +
  geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2) +
  ylab ("Thermic indicator per plot (mean +/- SE)") +
  ggtitle("Thermic indicator per year, raw") +
  theme_bw() +
  ggsave("./../plots_graphs/TI4_mean_years_raw.tiff")

write.table(TI_mean, "../TI4_mean_year.csv", sep = ";" , row.names = F)

## mean TI per block and year
TI_mean_bl <-  ddply(TI4, c('year','block'), summarise,
                  N    = length(TI),
                  mean = mean(TI),
                  sd   = sd(TI),
                  se   = sd / sqrt(N)
)

TI_mean_bl[,4:6] <- round(TI_mean_bl[,4:6], 2)
colnames(TI_mean_bl)[6] <- 'SE'

ggplot(TI_mean_bl,  aes(x=year, y=mean, color = block)) +
  geom_point(size=2 )+
  geom_line(lty = 2) +
  scale_colour_brewer(palette="Set2") +
  geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2) +
  ylab ("Thermic indicator per plot (mean +/- SE)") +
  ggtitle("Thermic indicator per block, raw") +
  theme_bw()  +
  ggsave("./../plots_graphs/TI4_mean_block_years_raw.tiff")




##### Analysis

## in Lamprecht et al. 2018: LMM(lmer)
hist( TI4$TI , xlab = 'year',  ylab='covSum')   ### solala

TI_Y <- lmer(TI ~ yearF +  (1 |block/tr/fl_num), data = TI4) ###
TI_Y_P <- glmer(TI ~ yearF  +  (1 |block/tr/fl_num), data = TI4, family = poisson())  ## warnings
TI_Y_P_PQL <- glmmPQL(TI ~ yearF ,~ 1 |block/tr/fl_num, data = TI4, family = poisson()) ## warnings
TI_nb <- glmer.nb(TI ~ yearF  + (1|block/tr/fl_num) , data=TI4)  ## warnings
anova(TI_Y,TI_Y_P,TI_Y_P_PQL , TI_nb)   ### AIC is lowest for gauss. To this without warnings and the same as 2018
summary(TI_Y)
plot(TI_Y)  ### seems to be quite OK

TI_Y_emmeans <- pairs(emmeans(TI_Y, ~yearF, data = TI4))
TI_Y_emmeans<-as.data.frame(TI_Y_emmeans)
#plot(emmeans(TI_Y, ~yearF, data = TI4), comparisons = TRUE)

TI_Y_emmeans[,2:3] <- round(TI_Y_emmeans[,2:3], 2)
TI_Y_emmeans[,5:6] <- round(TI_Y_emmeans[,5:6], 4)
TI_Y_emmeans$mod <- 'lmer'
TI_Y_emmeans$type <- 'TI'


write.table(TI_Y_emmeans, "../model_TI4_emmeams.csv", sep = ";" , row.names = F)

  