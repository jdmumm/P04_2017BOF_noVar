#agregate comm effort by stat area and year for plotting in arcmap for use in 
#deciding which stat areas to include in report. 

library(tidyverse)
library(reshape2)
library (lattice)
lib
read.csv("data/PWS Shrimp All.csv") %>% # from K:\MANAGEMENT\SHELLFISH\PWS Shrimp All.xlsx
  select (year = DOL.Year, species = Species.Code, stat=Stat.Area, pots = Effort..sum., lbs = Whole.Weight..sum.) -> harv

#by StatArea
harv %>% group_by (year,stat) %>% summarize (
  pots = sum(pots)) -> PotsByStatYear  
dcast(PotsByStatYear, stat ~ year, value.var = "pots" ) -> PotsByStatYear
str(PotsByStatArea)
write.csv(PotsByStatYear, "C:/temp/PotsByStatYear.csv")

