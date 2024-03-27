#### ANALYSE COVER CHANGE PER PLOT
## I used workspace Schrankogel23 to create this workspace SK23_cover
#library(Matrix)
#library(lme4)
library(plyr)
library(tidyverse)
library(ggplot2)
library(MASS)

#detach(package:plyr)  ### when tidyverse functions are not working

str(spec)

## A) Species cover per plot and year
specCov <- merge(spec, Plotinfo[,c(1,3,4)], all.x = T)
specCov$yearN <- as.numeric(specCov$year)
str(specCov)

specCov <- specCov %>%  group_by(fl_num,tr,block,year) %>% summarise( covSum = sum(cover, na.rm = TRUE)) %>%  ungroup()

str(specCov)
specCov <- merge(specCov , PlotinfoYear[,c(1,2)], all = T)  ### to add 0
specCov <- specCov %>% replace(is.na(.), 0) 
specCov <- merge(specCov[,-c(3,4)] , Plotinfo[,c(1,3,4)], by='fl_num', all.x = T) ### add tr and block for new plots


specCov4 <- specCov %>%  filter(fl_num %in% levels(dat4$fl_num))
specCov4 <- droplevels(specCov4)
str(specCov4)  ### 355 plots

specCov3 <- specCov %>%  filter(fl_num %in% levels(dat3$fl_num) & year != '2004')
specCov3 <- droplevels(specCov3)
str(specCov3)  ### 661 plots

specCov2 <- specCov %>%  filter(fl_num %in% levels(dat2$fl_num) & year != '2004' & year != '2014')
specCov2 <- droplevels(specCov2)
str(specCov2)  ### 683 plots


ggplot(specCov4, aes(x=covSum)) + geom_histogram(binwidth=.5)  ### right-skewed distribution


## B) mean cover per transect and year: RAW -----------
library(plyr)
cov_mean_tr <-  ddply(specCov4, c("tr", "year"), summarise,
                       N    = length(covSum),
                       mean = mean(covSum),
                       sd   = sd(covSum),
                       se   = sd / sqrt(N)
)

cov_mean_tr[,4:6] <- round(cov_mean_tr[,4:6], 2)
colnames(cov_mean_tr)[6] <- 'SE'

ggplot(cov_mean_tr,  aes(x=year, y=mean,  colour= tr)) +
  geom_point(size=2 )+
  geom_line(lty = 2) +
  scale_colour_brewer(palette="Set2") +
  geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2) +
  ylab ("Species cover per plot (mean +/- SE)") +
  ggtitle("plots, raw") +
  theme_bw() +
  ggsave("./../plots_graphs/cover4_mean_tr_years_raw.tiff")

rm(cov_mean_tr)


### mean cover per block and year: RAW -----------
cov_mean_bl <-  ddply(specCov4, c("block", "year"), summarise,
                      N    = length(covSum),
                      mean = mean(covSum),
                      sd   = sd(covSum),
                      se   = sd / sqrt(N)
)

cov_mean_bl[,4:6] <- round(cov_mean_bl[,4:6], 2)
colnames(cov_mean_bl)[6] <- 'SE'

write.table(cov_mean_bl,"../cover4_mean_block.csv", sep = ";", row.names = F)

cov_mean_bl$yearN <- as.numeric(substr(cov_mean_bl$year,1,4))  ## lines between points are only possible when year is numeric

ggplot(cov_mean_bl,  aes(x=yearN, y=mean,  colour= block)) +
  geom_point(size=2 )+ 
  geom_line(lty = 2) + 
  scale_colour_brewer(palette="Set2") +
  geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2) +
  ylab ("Cover sum (dm², mean +/- SE)") +
  ggtitle("Species cover per block, raw") +
  theme_bw() +
  ggsave("./../plots_graphs/cover4_mean_block_years_raw.tiff")

rm(cov_mean_bl)

## C) mean cover per year: RAW -----------
cov_mean <-  ddply(specCov4, 'year', summarise,
                      N    = length(covSum),
                      mean = mean(covSum),
                      sd   = sd(covSum),
                      se   = sd / sqrt(N)
)

cov_mean[,3:5] <- round(cov_mean[,3:5], 2)
colnames(cov_mean)[5] <- 'SE'

ggplot(cov_mean,  aes(x=year, y=mean)) +
  geom_point(size=2 )+
  geom_line(lty = 2) +
  scale_colour_brewer(palette="Set2") +
  geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2) +
  ylab ("Cover sum (dm², mean +/- SE)") +
  ggtitle("Species cover per survey, raw") +
  theme_bw() +
  ggsave("./../plots_graphs/cover4_mean_years_raw.tiff")


rm(cov_mean)

#########
#########
### D) Analyses
## in Lamprecht et al. 2018: LMM(lmer)
hist( specCov4$covSum , xlab = 'year',  ylab='covSum')   ### absolutely right skewed

glm_gaus<-glm(specCov4$covSum~1)
glm_pois<-glm(specCov4$covSum~1, family = "poisson")  ## lot of warnings.
glm_nb <- glm.nb(specCov4$covSum~1) ## lot of warnings.
#glm_gamma <- glm(specCov4$covSum~1, family = "Gamma") ## non-positive values not allowed for the 'Gamma' family

# Diagnostics on the NULL models
par(mfrow=c(3,4))
plot(glm_gaus, main="gaussian")  
plot(glm_pois, main="poisson")
plot(glm_nb, main="neg.bin")
AIC(glm_gaus, glm_pois, glm_nb)  ### lowest nb

cover_Y <- lmer(covSum ~ year +  (1 |block/tr/fl_num), data = specCov4) ### boundary (singular) fit: model did fit, but it generated that warning because random effects are very small. 
cover_Y_P <- glmer(covSum ~ year +  (1 |block/tr/fl_num), data = specCov4, family = poisson())  ## warnings
cover_Y_P_PQL <- glmmPQL(covSum ~ year,~ 1 |block/tr/fl_num, data = specCov4, family = poisson()) ## warnings
cover_nb <- glmer.nb(covSum ~ year + (1|block/tr/fl_num) , data=specCov4)  ## warnings
anova(cover_Y,cover_Y_P, cover_Y_P_PQL , cover_nb)   ### AIC is lowest nb, however, only with gauss no warnings
summary(cover_Y)
summary(cover_nb)
plot(cover_Y)  ### seems to be quite OK
plot(cover_nb)

cover_Y_emmeans <- pairs(emmeans(cover_Y, ~year, data = specCov4))
cover_Y_emmeans<-as.data.frame(cover_Y_emmeans)
#plot(emmeans(cover_Y, ~year, data = specCov4), comparisons = TRUE)

cover_Y_emmeans[,2:3] <- round(cover_Y_emmeans[,2:3], 2)
cover_Y_emmeans[,5:6] <- round(cover_Y_emmeans[,5:6], 4)
cover_Y_emmeans$mod <- 'lmer'
cover_Y_emmeans$type <- 'cover'


write.table(cover_Y_emmeans, "../model_cover4_emmeams.csv", sep = ";" , row.names = F)


## not the same p-values as in Lamprecht 2018 for first 3 years! test without 2023   
specCov4_3 <- specCov4 %>% filter(year != '2023')
  
test <- lmer(covSum ~ year  + (1|block/tr/fl_num),  data = specCov4_3 )   ### lots of warnings
summary(test)

test_emmeans <- pairs(emmeans(test, ~year, data = specCov4_3))
test_emmeans<-as.data.frame(test_emmeans)
test_emmeans[,5:6] <- round(test_emmeans[,5:6], 4)   ### OK, now it is the same as in Lamprecht et al. 2018
plot(emmeans(test, ~year, data = specCov4_3), comparisons = TRUE)
write.table(test_emmeans, "../cover_94_04_14_emmeamsGauss.csv", sep = ";" , row.names = F)

rm(specCov4_3, test, test_emmeans)
