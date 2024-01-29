#### ANALYSE COVER CHANGE PER PLOT

library(Matrix)
library(lme4)

str(spec)

## Species per plot
specCov <- merge(spec, Plotinfo[,c(1,3,4)], all.x = T)
specCov$yearN <- as.numeric(specCov$year)
str(specCov)

specCov <- specCov %>%  group_by(fl_num,year)  %>% summarise( covSum = sum(cover, na.rm = TRUE), n_spec = length(year)) 

str(specCov)
specCov <- merge(specCov , PlotinfoYear[,c(1,2)], all = T) 
specCov <- merge(specCov , Plotinfo[,c(1,3,4)], all = T) 

specCov <- specCov %>% replace(is.na(.), 0)



ggplot(specCov, aes(x=covSum)) + geom_histogram(binwidth=.5)  ### right-skewed distribution



#########
#########



CovChange_all4 <- lmer(covSum ~ year  + (1|block/tr/fl_num),  data = specCov)
summary(CovChange_all4)

ggplot(specCov, aes(x = yearN, y = cover)) + geom_point()
