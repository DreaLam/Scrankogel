## Changes of richness per plot for GIS

## workspace: SK23_richness
## I use rich2 for preparation for GIS tables

ChRich2 <- rich2[,-6] %>% group_by (fl_num,tr,block) %>% spread(yearF, n_spec) %>% rename(y1994 = '1994', y2023 = '2023')
ChRich2 <-ChRich2 %>% mutate(abChRich = y2023 - y1994)
ChRich2 <-ChRich2 %>% mutate(relChRich = y2023*100/y1994 -100)
ChRich2 <-ChRich2 %>% mutate(relChRich = round(relChRich,2) )          
                             