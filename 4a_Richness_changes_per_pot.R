#### ANALYSE Richness change per plot ###
library(plyr)
library(ggplot2)


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
histogram( ~ n_spec | year, type = "count" , data=rich4)
glm_gaus<-glm(rich$n_spec~1)
glm_pois<-glm(rich$n_spec~1, family = "poisson")
glm_NB<-glm.nb(rich$n_spec~1)
# Diagnostics on the NULL models
par(mfrow=c(2,4))
plot(glm_gaus, main="gaussian")  
plot(glm_pois, main="poisson")
anova(glm_gaus,glm_pois, glm_NB)  

AIC(glm_gaus)
AIC(glm_pois)
AIC(glm_NB)

rm(glm_gaus,glm_nb,glm_NB, glm_pois)



