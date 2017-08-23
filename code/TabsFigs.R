## TabsFigs ####
# assemble tables and figures for 2017 P04 BOF reports
# This entire project is using the existing survey summary numbers (e.g. w/out variance)
# All from the 2016 survey summary Spreadsheet created for KG Jan 2017.  These numbers don't exactly 
# match the current query outputs in the DB.  However, they are close, w/in 1.1% surveywide for all years.
# Going with the spreadsheet since those numbers should match those published elsewhere, 
# as requested by KG and JR.  

## Load ####
library(tidyverse)
library(reshape2)
read.csv("data/surveyWide_from16SS.csv") -> surv  #Survey-wide summary by year
read.csv("data/bySite_from16SS.csv")%>%
  transmute(year = Year, site = Site_ID, pots = Pot_Count, all_cnt = Total_Spot_Count, all_lb = Total_Spot_Wt_KG * 2.20462, propLrg = Proportion_Large, 
         lrg_cnt = Est_Count_LG, lrg_lb = Est_Wt_Large * 2.20462, cpue_all_lbs = CPUE_All_LB, cpue_all_cnt = CPUE_All_Count, cpue_lrg_cnt=CPUE_Large_Count) ->site
read.csv("data/SiteStatArea_LUT.csv") -> siteStatLUT
read.csv("data/yearArea_LUT.csv") -> yearAreaLUT
read.csv("data/femEggBySite.csv") -> egg
read.csv("data/PWS Shrimp All.csv") %>%
  select (year = DOL.Year, species = Species.Code, stat=Stat.Area, pots = Effort..sum., lbs = Whole.Weight..sum.) -> harv

## ASSEMBLE TABLES ON JRs LIST ####
# CPUE (ALL_LB) by ShrimpArea and StatArea - both survey and harvest 
  #Aggregate Survey ####
    # join statArea and ShrimpArea to CPUE 
      site %>% select(year,site,pots,all_lb) %>%
      left_join (
        siteStatLUT %>% select(-Comments), by = c("site" = "SiteNum")) %>%
        filter(site != 11) -> cpueBySite         #exclude valdez 
    
    #by ShrimpArea
      cpueBySite %>% group_by (year,ShrimpArea) %>% summarize (
        cpueAllLb = sum(all_lb)/sum(pots)) -> cpueByArea
      dcast(cpueByArea, year ~ ShrimpArea, value.var = "cpueAllLb") -> cpueByArea 
      #write.csv(cpueByArea,"output/CPUE_allLb_byShrmpArea.csv")
      
      # get commercial data and join to above before writing 
   
    #by StatArea
      cpueBySite %>% group_by (year,StatArea) %>% summarize (
        cpueAllLb = sum(all_lb)/sum(pots)) -> cpueByStat  
      dcast(cpueByStat, year ~ StatArea, value.var = "cpueAllLb" ) -> cpueByStat
      #write.csv(cpueByStat,"output/CPUE_allLb_byStatArea.csv")
  
  # Aggregate Harvest ####   ## still need to rename and join to survey values
    # join shrimpArea to harvest
      harv %>% left_join (yearAreaLUT) %>% 
        na.omit -> harv #exclude those 814 records with null effort
     # by shrimpArea
      harv %>% group_by (year,area) %>% summarize (
        cpueAllLb = sum(lbs)/sum(pots)) -> cpueByArea
      dcast(cpueByArea, year ~ area, value.var = "cpueAllLb") -> cpueByArea 
      
      #by StatArea
      harv %>% group_by (year,stat) %>% summarize (
        cpueAllLb = sum(lbs)/sum(pots)) -> cpueByStat  
      dcast(cpueByStat, year ~ stat, value.var = "cpueAllLb" ) -> cpueByStat
      
# prop egg bearing by stat ####
    # join statArea to propEggBearing 
    egg %>% select(YEAR,SITE_ID,males,fems,femsWEggs,femsWValidEggCode) %>%
      left_join (
        siteStatLUT %>% select(SITE_ID=SiteNum,SiteName,StatArea,ShrimpArea)) %>%
      filter(SITE_ID != 11) -> eggsBySite 
    # Aggregate by year and stat area
    eggsBySite %>% group_by(YEAR,StatArea) %>% summarise(
      perFemWEgg = round(100*sum(femsWEggs)/sum(femsWValidEggCode),2)) -> eggByStat
      dcast(eggByStat, YEAR ~ StatArea, value.var = "perFemWEgg") -> eggByStat
    write.csv(eggByStat,"output/eggByStat.csv")
    
    eggsBySite %>% group_by(YEAR) %>% summarise(
      perFemWEgg = round(100*sum(femsWEggs)/sum(femsWValidEggCode),2)) -> eggByYear
    
    
    
    
    
    
    
    
    
    
    
    
############################################################################################    
# hasty plot of CPUE Alls by area ####
  par (mfrow = c(3,1))
  par (pch = 19)
  plot(cpueByArea[,1],cpueByArea[,2], main = "1", ylim = c(0,10), xlab = "year", ylab = "CPUE ALLs Lbs/pot")
  plot(cpueByArea[,1],cpueByArea[,3], main = "2", ylim = c(0,10))
  plot(cpueByArea[,1],cpueByArea[,4], main = "3", ylim = c(0,10))         

  par(mfrow = c(1,1), pch = 19, lwd = 2)  
  plot(cpueByArea[,1],cpueByArea[,2], main = "CPUE_All_Lbs", ylim = c(0,4), type = "l", col = "red", 
        xlab = "year", ylab = "CPUE ALLs Lbs/pot")
  lines(cpueByArea[,1],cpueByArea[,3], col = "purple") 
  lines(cpueByArea[,1],cpueByArea[,4], col = "blue")
legend ("topleft", lwd = 2,  c("1","2","3"), col = c("red","purple", "blue"))
  
  
  