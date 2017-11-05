# calc var of prop.  
# opting not to use for now since point ests vary considerably from pooled values. 

## Load ####
library(tidyverse)
library(reshape2)

read.csv("data/SiteStatArea_LUT.csv") -> siteStatLUT
read.csv('data/AWL_171004.csv') %>% 
  select(year = YEAR, Event = EVENT_ID, site = SITE_ID, Station = STATION_ID, pot = POT_ID, species = FK_SPECIES_CODE,
         sex = FK_SEX_CODE, freq = FREQUENCY, cl = CARAPACE_LENGTH_MM, wt = WEIGHT_GRAMS, eggDev = SHRIMP_EGG_DEVEL_CODE, 
         breed = SHRIMP_BREEDING_CODE, eggCol = SHRIMP_EGG_COLOR_CODE, eggDead = SHRIMP_DEAD_EGG_COUNT, parasite = SHRIMP_PARASITE_CODE) -> awl
read.csv('data/Pot_Performance_171004.csv') %>% select( Event = EVENT_ID, site = SITE_ID, pot = POT_ID, Station = STATION, perf = FK_GEAR_PERFORMANCE_CODE, 
                                                        gearComment = GEAR_COMMENTS, sample = SAMPLE_POT ) -> pp 

# join and filter
awl %>% select(Event, site, Station, pot, year,species, sex, freq, cl) -> awl 
pp %>% select(Event, pot,perf, sample) -> pp 
left_join(awl, pp) %>%
  filter (site != 11, Station %in% c('A','B','C','D','W','X','Y','Z'), species == 965, perf == 1) -> awl

# Replicate freq 2s - these are from 2005, when only half males were measured. 
awl %>% filter(freq == 2) -> twos
rbind(awl,twos) -> awl
awl$freq <- 1
awl %>% mutate(Sex = as.factor(sex)) -> awl 
awl %>% mutate(Year = as.factor(year)) -> awl 
awl %>% left_join (siteStatLUT, by = c('site' ='SiteNum')) -> awl 

# calc pfem by pot
awl %>% filter (sex %in% c(1,2)) %>% group_by(year,sex,Event,pot,ShrimpArea) %>% summarise(cnt =  sum(freq)) -> sx
sx %>% spread(sex, cnt, fill = 0) -> wid 
colnames(wid) <- c('year','event','pot', 'Area','m','f')
wid %>% group_by(year,event,pot,Area) %>% summarise (pf = f/(m+f)) -> pfem_byPot

# calc mean and se by year. 
pfem_byPot %>% group_by(year) %>% 
  summarize (Area = first(Area),
             pf_bar = mean(pf, na.rm = T),  # this mean differs a little from that in pFemByYear.csv. Probably due to pooling by year vs mean over pots. 
                #Plan to use the mean there, since clsoe to previous tables and var from here.  
             n = n(),
             pf_var = var(pf, na.rm = T), 
             pf_se = (pf_var^.5)/(n^.5)) -> pfem_var 

pfem_byPot %>% group_by(year,Area) %>% 
  summarize (pf_bar = mean(pf, na.rm = T),  # this mean differs a little from that in pFemByYear.csv. Probably due to pooling by year vs mean over pots. 
             #Plan to use the mean there, since close to previous tables and var from here.  
             n = n(),
             pf_var = var(pf, na.rm = T), 
             pf_se = (pf_var^.5)/(n^.5)) -> pfem_byArea_var 

#opting not to include var for now, since point ests differ considerbly from pooled values.   