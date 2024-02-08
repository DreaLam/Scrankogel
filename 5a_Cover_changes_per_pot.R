#### ANALYSE COVER CHANGE PER PLOT

#library(Matrix)
#library(lme4)
library(plyr)
library(tidyverse)


str(spec)

## A) Species cover per plot and year
specCov <- merge(spec, Plotinfo[,c(1,3,4)], all.x = T)
specCov$yearN <- as.numeric(specCov$year)
str(specCov)

specCov <- specCov %>%  group_by(fl_num,year)  %>% summarise( covSum = sum(cover, na.rm = TRUE), n_spec = length(year)) 

str(specCov)
specCov <- merge(specCov , PlotinfoYear[,c(1,2)], all = T) 
specCov <- merge(specCov , Plotinfo[,c(1,3,4)], all = T) 

specCov <- specCov %>% replace(is.na(.), 0)



ggplot(specCov, aes(x=covSum)) + geom_histogram(binwidth=.5)  ### right-skewed distribution
### using glmer with family Gamma?

### plot raw data
ggplot(specCov,  aes(x=year, y=covSum,  colour= tr)) +
  geom_point(size=2 )+
  geom_line(lty = 2) +
  scale_colour_brewer(palette="Set2") +
  #geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2) +
  ylab ("Species cover per plot (mean +/- SE)") +
  ggtitle("plots, raw") +
  theme_bw()# +
  #ggsave("./../plots/Cov_SUC_years_raw.tiff")


## B) mean cover per tr and year: RAW -----------
Cover_mean_tr <- ddply(specCov, c("tr", "year"), summarise,
                          N    = length(covSum),
                          mean = mean(covSum),
                          sd   = sd(covSum),
                          se   = sd / sqrt(N)
)

Cover_mean_tr[,4:6] <- round(Cover_mean_SN_TR[,4:6], 2)
colnames(Cover_mean_tr)[6] <- 'SE'
write.table(Cover_mean_tr , "../Cover_mean_tr_RAW.csv", sep = ";", row.names = F)

plot(specCov$year, specCov$covSum)
plot(Cover_mean_tr$year, Cover_mean_tr$mean)

ggplot(Cover_mean_tr,  aes(x=year, y=mean,  colour= tr)) +
  geom_point(size=2 )+
  geom_line(lty = 2) +
  scale_colour_brewer(palette="Set2") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2) +
  ylab ("Species cover per plot (mean +/- SE)") +
  ggtitle("plots, raw") +
  theme_bw() +
ggsave("./../plots_graphs/Cov_mean_tr_years_raw.tiff")      ####### it seems, that mean cover in tr1 increased in 2023, others decreased. Be careful with 2004: number of plots is much smaller.

rm(Cover_mean_tr)
#########
### C) Analyses

histogram( ~ covSum | year,  data=specCov)   ### absolutely right skewed

glm_gaus<-glm(specCov$covSum~1)
glm_pois<-glm(specCov$covSum~1, family = "poisson")  ## lot of warnings.
glm_nb <- glm.nb(specCov$covSum~1) ## lot of warnings.
glm_gamma <- glm(specCov$covSum~1, family = "Gamma") ## non-positive values not allowed for the 'Gamma' family
# Diagnostics on the NULL models
par(mfrow=c(3,4))
plot(glm_gaus, main="gaussian")  
plot(glm_pois, main="poisson")
plot(glm_nb, main="neg.bin")
AIC(glm_gaus, glm_pois, glm_nb)  ### lowest nb

CovChange_all4 <- glm.nb(covSum ~ year  + (1|block/tr/fl_num),  data = specCov )   ### lots of warnings
summary(CovChange_all4)
