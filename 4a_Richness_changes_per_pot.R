#### ANALYSE Richness change per plot ###

## A) richness per plot and year
rich <- spec %>%  group_by(fl_num,year)  %>% summarise( n_spec = length(year)) 
##test
#richT <- spec %>% mutate(nr = 1) %>% aggregate(cbind(nr)~ +fl_num + year, sum)
#richT <- merge(rich, richT) %>% mutate(test = n_spec - nr)  ### always 0
#rm(richT)

rich <- merge(rich, Plotinfo[,c(1,3,4)], all.x = T)
rich <- rich[,c(1,4,5,2,3)]

write.table(rich , "../richness_per_plot_year.csv", sep = ";", row.names = F)


## B) mean richness per TR and year: RAW -----------
rich_mean_tr <-  ddply(rich, c("tr", "year"), summarise,
                       N    = length(n_spec),
                       mean = mean(n_spec),
                       sd   = sd(n_spec),
                       se   = sd / sqrt(N)
)

rich_mean_tr[,4:6] <- round(rich_mean_tr[,4:6], 2)
colnames(rich_mean_tr)[6] <- 'SE'
write.table(rich_mean_tr , "../rich_mean_tr_RAW.csv", sep = ";", row.names = F)

plot(rich$year, rich$n_spec)
plot(rich_mean_tr$year, rich_mean_tr$mean)

ggplot(rich_mean_tr,  aes(x=year, y=mean,  colour= tr)) +
  geom_point(size=2 )+
  geom_line(lty = 2) +
  scale_colour_brewer(palette="Set2") +
  geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2) +
  ylab ("Species richness per plot (mean +/- SE)") +
  ggtitle("plots, raw") +
  theme_bw() +
  ggsave("./../plots_graphs/richness_mean_tr_years_raw.tiff")      ####### Be careful with 2004: number of plots is much smaller.

rm(rich_mean_tr)

### C) Analyses
histogram( ~ n_spec | year, type = "count" , data=rich)
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
