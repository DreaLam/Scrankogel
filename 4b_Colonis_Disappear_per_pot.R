#### ANALYSE Colonisations and disappearances per plot ###
library(stringr)
library(tidyverse)
library(plyr)
library(ggplot2)
library(Matrix)
library(lme4)
library(emmeans)
library(lsmeans)
lsmeans <- lsmeans::lsmeans


## Colonisations: number of species per plot present at the time of the resurvey, absent in the respective plot at the previous survey.
colo <- spec[,-5] %>% group_by(fl_num,species) %>% spread(year, cover)
colo <- colo %>% replace(is.na(.),0)
str(colo)
colnames(colo)[-c(1,2)] <- paste0("Y", colnames(colo)[-c(1,2)])

colo <- colo %>% mutate(P9404 = ifelse((Y2004 > 0 & Y1994 == 0), 1,0))
colo <- colo %>% mutate(P0414 = ifelse((Y2014 > 0 & Y2004 == 0), 1,0))
colo <- colo %>% mutate(P1423 = ifelse((Y2023 > 0 & Y2014 == 0), 1,0))

colo <- colo %>% mutate(P9423 = ifelse((Y2023 > 0 & Y1994 == 0), 1,0))

colo <- colo %>% mutate(type = 'colo')

## Disappearance: number of species per plot absent at the time of the resurvey, present in the respective plot at the previous survey.
disa <- spec[,-5] %>% group_by(fl_num,species) %>% spread(year, cover)
disa <- disa %>% replace(is.na(.),0)
str(disa)
colnames(disa)[-c(1,2)] <- paste0("Y", colnames(disa)[-c(1,2)])

disa <- disa %>% mutate(P9404 = ifelse((Y2004 == 0 & Y1994 > 0), 1,0))
disa <- disa %>% mutate(P0414 = ifelse((Y2014 == 0 & Y2004 > 0), 1,0))
disa <- disa %>% mutate(P1423 = ifelse((Y2023 == 0 & Y2014 > 0), 1,0))

disa <- disa %>% mutate(P9423 = ifelse((Y2023 == 0 & Y1994 > 0), 1,0))

disa <- disa %>% mutate(type = 'disa')

codi <- rbind(colo,disa)
