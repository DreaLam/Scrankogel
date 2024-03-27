## Changes of richness per plot for GIS

## We use rich2 for that

AbChRich2 <- rich2 %>% group_by (fl_num,tr,block) %>% summarise(richChA = rich2[rich2$yearF=='2023',]$n_spec - rich2[rich2$yearF=='1994',]$n_spec)

