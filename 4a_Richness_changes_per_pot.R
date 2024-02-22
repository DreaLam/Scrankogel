#### ANALYSE Richness change per plot ###
library(stringr)
library(tidyverse)
library(plyr)
library(ggplot2)
library(Matrix)
library(lme4)
library(emmeans)
library(lsmeans)
lsmeans <- lsmeans::lsmeans


## A) richness per plot and year
rich <- spec %>%  group_by(fl_num,year)  %>% summarise( n_spec = length(year)) %>% ungroup()
##test
#richT <- spec %>% mutate(nr = 1) %>% aggregate(cbind(nr)~ +fl_num + year, sum)
#richT <- merge(rich, richT) %>% mutate(test = n_spec - nr)  ### always 0
#rm(richT)

## include empty plots
rich <- merge(rich, PlotinfoYear[,c(1,2)], all = T)
rich <- rich %>% replace(is.na(.),0)


## include infos  
rich <- merge(rich, Plotinfo[,c(1,3,4)], all.x = T)
rich <- rich[,c(1,4,5,2,3)]

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


## B) mean richness per TR and year: RAW -----------
library(plyr)
rich_mean_tr <-  ddply(rich2, c("tr", "year"), summarise,
                       N    = length(n_spec),
                       mean = mean(n_spec),
                       sd   = sd(n_spec),
                       se   = sd / sqrt(N)
)

rich_mean_tr[,4:6] <- round(rich_mean_tr[,4:6], 2)
colnames(rich_mean_tr)[6] <- 'SE'
write.table(rich_mean_tr , "../rich2_mean_tr_RAW.csv", sep = ";", row.names = F)

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



### C) Analyses
hist( rich4$n_spec , xlab= 'year', ylab = 'n_spec' )   ### beautiful GAUSS distribution

rich4 <- rich4 %>% mutate(yearNr = as.numeric(year))
rich4 <- rich4 %>% mutate(yearNR = as.numeric(paste(year)))


### Choose an error distribution and link function (e.g. Poisson distribution and log link for count data).
glm_gaus<-glm(rich4$n_spec~1)
glm_pois<-glm(rich4$n_spec~1, family = "poisson")  ## lot of warnings.
#glm_nb <- glm.nb(rich4$n_spec~1) ## lot of warnings.
#glm_gamma <- glm(rich4$n_spec~1, family = "Gamma") ## non-positive values not allowed for the 'Gamma' family

# Diagnostics on the NULL models
par(mfrow=c(3,4))
plot(glm_gaus, main="gaussian")  
plot(glm_pois, main="poisson")
plot(glm_nb, main="neg.bin")
AIC(glm_gaus, glm_pois)  ### lower for gauss

rm(glm_gaus,glm_pois)

rich_Y <- lmer(n_spec ~ year +  (1 |block/tr/fl_num), data = rich4)
rich_Y_P <- glmer(n_spec ~ year +  (1 |block/tr/fl_num), data = rich4, family = poisson())
anova(rich_Y,rich_Y_P)   ### AIC is lower with gauss
summary(rich_Y)
# numeric year
rich_Yn <- lmer(n_spec ~ yearNr +  (1 |block/tr/fl_num), data = rich4)
rich_YN <- lmer(n_spec ~ yearNR +  (1 |block/tr/fl_num), data = rich4)
rich_Y_Pn <- glmer(n_spec ~ yearNr +  (1 |block/tr/fl_num), data = rich4, family = poisson())
anova(rich_Yn,rich_YN, rich_Y_Pn)   ### AIC is lower with gauss
summary(rich_Yn)
summary(rich_YN)
anova(rich_YN)
anova(rich_Y)

rich_Y_emmeans <- pairs(emmeans(rich_Y, ~year))
rich_Y_emmeans<-as.data.frame(rich_Y_emmeans)
plot(emmeans(rich_Y, ~year), comparisons = TRUE)


