#### ANALYSE Richness change per plot ###
## I used workspace Schrankogel23 to create this workspace SK23_richness
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

## A) richness per plot and year
rich <- spec %>%  group_by(fl_num, yearF, year) %>% summarise( n_spec = length(yearF)) %>% ungroup()
##test
#richT <- spec %>% mutate(nr = 1) %>% aggregate(cbind(nr)~ +fl_num + year, sum)
#richT <- merge(rich, richT) %>% mutate(test = n_spec - nr)  ### always 0
#rm(richT)

## include empty plots
rich <- merge(rich, PlotinfoYear[,c(1,2,15)], all = T)
rich <- rich %>% replace(is.na(.),0)


## include infos  
rich <- merge(rich, Plotinfo, all.x = T)
rich <- rich[,c(1,5,6,7,2,3,4)]

write.table(rich , "../richness_per_plot_year.csv", sep = ";", row.names = F)

rich4 <- rich %>%  filter(fl_num %in% levels(dat4$fl_num))
rich4 <- droplevels(rich4)
str(rich4)  ###355 plots

rich3 <- rich %>%  filter(fl_num %in% levels(dat3$fl_num) & year != '2004')
rich3 <- droplevels(rich3)
str(rich3)  ## 661 plots

rich2 <- rich %>%  filter(fl_num %in% levels(dat2$fl_num) & year !=  '2004' & year !=  '2014')
rich2 <- droplevels(rich2)
str(rich2) ## 683 plots


## B) mean richness per tranesct and year: RAW -----------
library(plyr)
rich_mean_tr <-  ddply(rich2, c("tr", "year"), summarise,
                       N    = length(n_spec),
                       mean = mean(n_spec),
                       sd   = sd(n_spec),
                       se   = sd / sqrt(N)
)

rich_mean_tr[,4:6] <- round(rich_mean_tr[,4:6], 2)
colnames(rich_mean_tr)[6] <- 'SE'
write.table(rich_mean_tr , "../rich4_mean_tr_RAW.csv", sep = ";", row.names = F)

#plot(rich$year, rich$n_spec)
#plot(rich_mean_tr$year, rich_mean_tr$mean)

ggplot(rich_mean_tr,  aes(x=year, y=mean,  colour= tr)) +
  geom_point(size=2 )+
  geom_line(lty = 2) +
  scale_colour_brewer(palette="Set2") +
  geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2) +
  ylab ("Species richness per plot (mean +/- SE)") +
  ggtitle("plots, raw") +
  theme_bw() +
  ggsave("./../plots_graphs/richness2_mean_tr_years_raw.tiff")      

rm(rich_mean_tr)

##mean richness per block and year: RAW -----------
 
rich_mean_bl <-  ddply(rich4, c("block", "year"), summarise,
                       N    = length(n_spec),
                       mean = mean(n_spec),
                       sd   = sd(n_spec),
                       se   = sd / sqrt(N)
)

rich_mean_bl[,4:6] <- round(rich_mean_bl[,4:6], 2)
colnames(rich_mean_bl)[6] <- 'SE'
write.table(rich_mean_bl , "../rich4_mean_block_RAW.csv", sep = ";", row.names = F)

#plot(rich$year, rich$n_spec)
#plot(rich_mean_tr$year, rich_mean_tr$mean)

ggplot(rich_mean_bl,  aes(x=year, y=mean,  colour= block)) +
  geom_point(size=2 )+
  geom_line(lty = 2) +
  scale_colour_brewer(palette="Set2") +
  geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2) +
  ylab ("Species richness per plot (mean +/- SE)") +
  ggtitle("Richness per block, raw") +
  theme_bw()  +
  ggsave("./../plots_graphs/richness4_mean_block_years_raw.tiff")      


rm(rich_mean_bl)

## rich mean per year
rich_mean <-  ddply(rich4, c("year"), summarise,
                       N    = length(n_spec),
                       mean = mean(n_spec),
                       sd   = sd(n_spec),
                       se   = sd / sqrt(N)
)

rich_mean[,3:5] <- round(rich_mean[,3:5], 2)
colnames(rich_mean)[5] <- 'SE'
write.table(rich_mean , "../rich4_mean_RAW.csv", sep = ";", row.names = F)

ggplot(rich_mean,  aes(x=year, y=mean)) +
  geom_point(size=2 )+
  scale_colour_brewer(palette="Set2") +
  geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2) +
  ylab ("Number of species richness (mean +/- SE)") +
  ggtitle("Species richness per survey, raw") +
  theme_bw() +
  ggsave("./../plots_graphs/richness4_mean_years_raw.tiff")      

rm(rich_mean)

### C) Analyses
## in Lamprecht et al. 2018: GLMM(glmmPQL) with Poisson distribution
hist( rich4$n_spec , xlab= 'year', ylab = 'n_spec' )   ### beautiful GAUSS distribution

### Choose an error distribution and link function (e.g. Poisson distribution and log link for count data).
glm_gaus<-glm(rich4$n_spec~1)
glm_pois<-glm(rich4$n_spec~1, family = "poisson") 
#glm_nb <- glm.nb(rich4$n_spec~1) ## lot of warnings.
#glm_gamma <- glm(rich4$n_spec~1, family = "Gamma") ## non-positive values not allowed for the 'Gamma' family

# Diagnostics on the NULL models
par(mfrow=c(3,4))
plot(glm_gaus, main="gaussian")  
plot(glm_pois, main="poisson")
#plot(glm_nb, main="neg.bin")
AIC(glm_gaus, glm_pois)  ### lower for gauss

rm(glm_gaus,glm_pois)

rich_Y <- lmer(n_spec ~ yearF +  (1 |block/tr/fl_num), data = rich4)
rich_Y_P <- glmer(n_spec ~ yearF +  (1 |block/tr/fl_num), data = rich4, family = poisson())
rich_Y_P_PQL <- glmmPQL(n_spec ~ yearF,~ 1 |block/tr/fl_num, data = rich4, family = poisson())
anova(rich_Y,rich_Y_P, rich_Y_P_PQL )   ### AIC is lower with gauss
summary(rich_Y)

# numeric year
#rich_Yn <- lmer(n_spec ~ year +  (1 |block/tr/fl_num), data = rich4)
#rich_YN <- lmer(n_spec ~ year +  (1 |block/tr/fl_num), data = rich4)
#rich_Y_Pn <- glmer(n_spec ~ year +  (1 |block/tr/fl_num), data = rich4, family = poisson())
#anova(rich_Yn,rich_YN, rich_Y_Pn)   ### AIC is lower with gauss
#summary(rich_Yn)
#summary(rich_YN)
#anova(rich_YN)
#anova(rich_Y)

rich_Y_emmeans <- pairs(emmeans(rich_Y, ~yearF))
rich_Y_emmeans<-as.data.frame(rich_Y_emmeans)
plot(emmeans(rich_Y, ~yearF), comparisons = TRUE)
dev.off()

rich_Y_emmeans[,2:3] <- round(rich_Y_emmeans[,2:3], 2)
rich_Y_emmeans[,5:6] <- round(rich_Y_emmeans[,5:6], 4)
rich_Y_emmeans$mod <- 'lmer'
rich_Y_emmeans$type <- 'richness'

write.table(rich_Y_emmeans,"../model_rich4_emmeans.csv", sep = ";", row.names = F)

## incl. altiranks  ### ATTENTION: NO USEFUL MODEL YET!
## in Lamprecht et al. 2018: GLMM(glmmPQL) with negative binomial distribution
richAlti <- merge(spec, SpecInfo[,c(4,6)])
detach(package:plyr) ### when tidyverse functions are not working
richAlti <-richAlti %>%   group_by(fl_num, yearF, year, alti_rank) %>% summarise( n_spec = length(yearF)) %>% ungroup()
##test
#richT <- spec %>% mutate(nr = 1) %>% aggregate(cbind(nr)~ +fl_num + year, sum)
#richT <- merge(rich, richT) %>% mutate(test = n_spec - nr)  ### always 0
#rm(richT)

## include empty plots
richAlti <- merge(richAlti, PlotinfoYear[,c(1,2,15)], all = T)
richAlti <- richAlti %>% replace(is.na(.),0)


## include infos  
richAlti <- merge(richAlti, Plotinfo, all.x = T)
richAlti <- richAlti[,c(1,6,7,8,2,3,4,5)]

write.table(richAlti , "../richness_per_plot_year_altirank.csv", sep = ";", row.names = F)

richAlti4 <- richAlti %>%  filter(fl_num %in% levels(dat4$fl_num))
richAlti4 <- droplevels(richAlti4)
str(richAlti4)  ###355 plots

richAlti4$alti_rankF <- as.factor(richAlti4$alti_rank)

rich_YA <- lmer(n_spec ~ yearF*alti_rankF +  (1 |block/tr/fl_num), data = richAlti4)
rich_Y_PA <- glmer(n_spec ~ yearF*alti_rankF +  (1 |block/tr/fl_num), data = richAlti4, family = poisson())
rich_Y_P_PQLA <- glmmPQL(n_spec ~ yearF*alti_rankF,~ 1 |block/tr/fl_num, data = richAlti4, family = poisson())
rich_Y_nb_PQLA <- glmmPQL(n_spec ~ yearF*alti_rankF,~ 1 |block/tr/fl_num, data = richAlti4, family = neg.bin())
anova(rich_YA,rich_Y_PA, rich_Y_P_PQLA )   ### AIC is lower with gauss
summary(rich_YA)
summary(rich_Y_PA)
summary(rich_Y_PQLA)
