# estimate length at 50% fem using logistic regression
# Modified from spotLengthPlots_150730.R.  Sloppy, could use much cleaning. 
# 171027

#LOAD ----
library(tidyverse)

read.csv('data/AWL_171004.csv') %>% 
  select(year = YEAR, Event = EVENT_ID, site = SITE_ID, Station = STATION_ID, pot = POT_ID, species = FK_SPECIES_CODE,
         sex = FK_SEX_CODE, freq = FREQUENCY, cl = CARAPACE_LENGTH_MM, wt = WEIGHT_GRAMS, eggDev = SHRIMP_EGG_DEVEL_CODE, 
         breed = SHRIMP_BREEDING_CODE, eggCol = SHRIMP_EGG_COLOR_CODE, eggDead = SHRIMP_DEAD_EGG_COUNT, parasite = SHRIMP_PARASITE_CODE) -> awl
read.csv('data/Pot_Performance_171004.csv') %>% select( Event = EVENT_ID, site = SITE_ID, pot = POT_ID, Station = STATION, perf = FK_GEAR_PERFORMANCE_CODE, 
                                                        gearComment = GEAR_COMMENTS, sample = SAMPLE_POT ) -> pp 
read.csv("data/SiteStatArea_LUT.csv") -> siteStatLUT

##PREP ---- 
#filter
awl %>% select(Event, site, Station, pot, year,species, sex, freq, cl) -> awl 
pp %>% select(Event, pot,perf) -> pp 
left_join(awl, pp) %>%
  filter (site != 11, Station %in% c('A','B','C','D','W','X','Y','Z'), species == 965, perf == 1, cl>0, sex < 3, sex > 0 ) -> awl 

# Replicate freq 2s - these are from 2005, when only half males were measured. 
awl %>% filter(freq == 2) -> twos
rbind(awl,twos) -> awl
awl$freq <- 1
awl %>% mutate(Sex = as.factor(sex)) -> awl 
awl %>% mutate(Year = as.factor(year)) -> awl 
awl %>% left_join (siteStatLUT, by = c('site' ='SiteNum')) %>% arrange(year) -> dat   

# Filter by area.  Manually changed for byAreaFile 
#dat%>% filter (ShrimpArea == 1) -> dat

## ALL YEARS COMBINED ----

#create 0 to 1 fem variable
dat %>% mutate ( fem = sex - 1) -> dat 

# plot proportion points
par(mfrow=c(1,1))
#set up empty titled plot with approriate ranges
plot(fem ~ cl, data = dat, pch = "|", xlim = c(10,55),
     ylab = "Prop Fem", xlab = "ln",
     main = "PropFem vs. length", type = "p", ylim = c(0, 1))

#bin data 
  BREAKS <- c(seq(10,55,1))  # break points for CL bins
  LN <- cut(dat$cl, BREAKS)         
  LN                       # Categorical variable with one level for each LN increment
  table(dat$fem, LN)      # summerizes number of males and fems points in each LN bin
  fem.prop <- tapply(dat$fem, LN, function(x) sum(x>0)/length(x))
  fem.prop                  # propotion of fems of total obs in each LN bin

# add proportion Points
  x <- seq(10.5,54.5,1) # midpoints for each LN bin, for ploting. 
  points(x, fem.prop, pch=19, cex = 1.2, col = "blue") # adds proportion points to plot 

# add curve to plot
  mod <- glm(fem ~ cl, family = "binomial", data = dat)  # logistic GLM of sex vs cl
  cf <- coef(mod)     # etraxts coefficients from model, stores in vector cf[]
  curve( exp(cf[1] + cf[2]*x)/(1+exp(cf[1] + cf[2]*x)), 
         ylab = "Probability of fem", col ="red", add=T, lwd=2)  
  summary(mod) # note sig, AIC, resid deviance ~ = df, coefs

## BY YEAR  ----
unique(dat$year) -> yrs

par(mfcol=c(4,6))

#create df to hold fem50
f50 <- rep(999,length(yrs))
fem50 <-(cbind(yrs,f50))
Fem50 <- as.data.frame(fem50)

for( i in yrs )
{
  
  datT <- dat[dat$year == i, ]  
  plot(fem ~ cl, data = datT, pch = "|", xlim = c(32,58),
       ylab = "Prop Fem", xlab = "Carapace Length",
       main = i, type = "p", ylim = c(0, 1))
  
  BREAKS <- c(seq(32,56,1))  # break points for CL bins
  LN <- cut(datT$cl, BREAKS)         
  LN                       # Categorical variable with one level for each LN increment
  table(datT$sex, LN)      # summerizes number of males and fems points in each LN bin
  fem.prop <- tapply(datT$sex, LN, function(x) sum(x>1)/sum(x>0))
  fem.prop                  # propotion of fems of total obs in each LN bin
  
  
  x <- seq(32.5,55.5,1) # midpoints for each LN bin, for ploting. 
  points(x, fem.prop, pch=19, cex = 1.2, col = "red") # adds proportion points to plot 

  modT <- glm(fem ~ cl, family = "binomial", data = datT)  # logistic GLM of sex vs cl
  cfT <- coef(modT)     # etraxts coefficients from model, stores in vector cf[]
  curve( exp(cfT[1] + cfT[2]*x)/(1+exp(cfT[1] + cfT[2]*x)), 
         ylab = "Probability of fem", col ="red", add=T, lwd=2)  
  
  fem50T <- -cfT[1]/cfT[2]
  abline(v = fem50T, col = "red", lwd = 2 )
  
  Fem50[Fem50$yrs == i,2] <- fem50T
  
  curve( exp(cf[1] + cf[2]*x)/(1+exp(cf[1] + cf[2]*x)), 
         ylab = "Probability of fem", col ="blue", add=T, lwd=2)  

  fem50 <- -cf[1]/cf[2]
  abline(v = fem50, col = "blue", abline = 2 )
}  

par(mfrow=c(1,1))
plot(f50~yrs, dat = Fem50,type = "l", main = "Length @ 50% Female. Blue is '92 to '16 mean", xlab = "year", ylab = "carapace length")
points(f50~yrs, dat = Fem50,  pch = 19, col="red", cex = 1.5, add = "TRUE")
abline(h=mean(Fem50$f50), col = "blue", lwd = 2)

#Fem50 -> Fem50_1  # mannually change for by area 
# colnames(Fem50_1) <- c('year', 'f50_1') 
# colnames(Fem50_2) <- c('year', 'f50_2') 
# colnames(Fem50_3) <- c('year', 'f50_3') 
# Fem50_1 %>% left_join(Fem50_2) %>% left_join(Fem50_3) -> Fem50_byArea

#write.csv(Fem50_byArea, "output/f50_byArea_92to16.csv")
#write.csv(Fem50,"output/f50_92to16.csv")
