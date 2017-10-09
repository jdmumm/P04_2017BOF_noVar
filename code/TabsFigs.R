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
library(extrafont)
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=12,base_family='Times New Roman')+ 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))

read.csv("data/surveyWide_from16SS.csv") -> surv  #Survey-wide summary by year
read.csv("data/bySite_from16SS.csv")%>%
  transmute(year = Year, site = Site_ID, pots = Pot_Count, all_cnt = Total_Spot_Count, all_lb = Total_Spot_Wt_KG * 2.20462, propLrg = Proportion_Large, 
         lrg_cnt = Est_Count_LG, lrg_lb = Est_Wt_Large * 2.20462, cpue_all_lbs = CPUE_All_LB, cpue_all_cnt = CPUE_All_Count, cpue_lrg_cnt=CPUE_Large_Count) ->site
read.csv("data/SiteStatArea_LUT.csv") -> siteStatLUT
read.csv("data/yearArea_LUT.csv") -> yearAreaLUT
read.csv("data/femEggBySite.csv") -> egg
read.csv("data/PWS Shrimp All.csv") %>% # from K:\MANAGEMENT\SHELLFISH\PWS Shrimp All.xlsx
  select (year = DOL.Year, species = Species.Code, stat=Stat.Area, pots = Effort..sum., lbs = Whole.Weight..sum.) -> harv
read.csv('data/AWL_171004.csv') %>% 
         select(year = YEAR, Event = EVENT_ID, site = SITE_ID, Station = STATION_ID, pot = POT_ID, species = FK_SPECIES_CODE,
         sex = FK_SEX_CODE, freq = FREQUENCY, cl = CARAPACE_LENGTH_MM, wt = WEIGHT_GRAMS, eggDev = SHRIMP_EGG_DEVEL_CODE, 
         breed = SHRIMP_BREEDING_CODE, eggCol = SHRIMP_EGG_COLOR_CODE, eggDead = SHRIMP_DEAD_EGG_COUNT, parasite = SHRIMP_PARASITE_CODE) -> awl
read.csv('data/Pot_Performance_171004.csv') %>% select( Event = EVENT_ID, site = SITE_ID, pot = POT_ID, Station = STATION, perf = FK_GEAR_PERFORMANCE_CODE, 
                                                        gearComment = GEAR_COMMENTS, sample = SAMPLE_POT ) -> pp 

## ASSEMBLE TABLES ON JRs LIST ####

# CPUE (ALL_LB) by ShrimpArea and StatArea - both survey and harvest 
#Aggregate SURVEY ####
  # join statArea and ShrimpArea to CPUE 
    site %>% select(year,site,pots,all_lb) %>%
    left_join (
      siteStatLUT %>% select(-Comments), by = c("site" = "SiteNum")) -> cpueBySite
      
  #by ShrimpArea
    cpueBySite %>% filter(site != 11) %>% group_by (year,ShrimpArea) %>% summarize (    #exclude valdez 
      cpueAllLb = sum(all_lb)/sum(pots)) -> cpueByArea
    dcast(cpueByArea, year ~ ShrimpArea, value.var = "cpueAllLb") -> cpueByArea_s 

  #by StatArea
    cpueBySite %>% group_by (year,StatArea) %>% summarize (
      cpueAllLb = sum(all_lb)/sum(pots)) -> cpueByStat  
    dcast(cpueByStat, year ~ StatArea, value.var = "cpueAllLb" ) -> cpueByStat_s

# Aggregate HARVEST ####   
  # join shrimpArea to harvest
    harv %>% left_join (yearAreaLUT) %>% 
      na.omit -> harv #exclude those 814 records with null effort
   # by shrimpArea
    harv %>% group_by (year,area) %>% summarize (
      cpueAllLb = sum(lbs)/sum(pots)) -> cpueByArea
    dcast(cpueByArea, year ~ area, value.var = "cpueAllLb") -> cpueByArea_h 
    
    #by StatArea
    harv %>% group_by (year,stat) %>% summarize (
      cpueAllLb = sum(lbs)/sum(pots)) -> cpueByStat  
    dcast(cpueByStat, year ~ stat, value.var = "cpueAllLb" ) -> cpueByStat_h

# Join HARVEST to SURVEY and write ####
  #by ShrimpArea
    left_join(cpueByArea_s,cpueByArea_h, by = "year", suffix = c("_s","_h")) -> cpueByArea
    #write.csv(cpueByArea,"output/CPUEallLb_byShirmpArea.csv")
  #by StatArea
    as.character(unique(siteStatLUT$StatArea)) %>% sort -> surveyedStats
    cpueByStat_h %>% select(c(year,one_of( surveyedStats))) -> cpueByStat_h_surveyed  # limit com stats to those that contain survey sites. 
      left_join(cpueByStat_s,cpueByStat_h_surveyed, by = "year", suffix = c("__s","_h")) %>%
      select(order(colnames(.)))  -> cpueByStat
      #write.csv(cpueByStat,"output/CPUEallLb_byStatArea.csv")

# prop egg bearing by stat ####   
    # join statArea to propEggBearing 
    egg %>% select(YEAR,SITE_ID,males,fems,femsWEggs,femsWValidEggCode) %>%
      left_join (
        siteStatLUT %>% select(SITE_ID=SiteNum,SiteName,StatArea,ShrimpArea)) -> eggsBySite 
    # Aggregate by year and stat area
    eggsBySite %>% group_by(YEAR,StatArea) %>% summarise(
      perFemWEgg = round(100*sum(femsWEggs)/sum(femsWValidEggCode),2)) -> eggByStat
      dcast(eggByStat, YEAR ~ StatArea, value.var = "perFemWEgg") -> eggByStat
    # Survey wide 
    eggsBySite %>% filter(SITE_ID != 11) %>% group_by(YEAR) %>% summarise(                   # excluding valdez for survey-wide 
      surveyWide = round(100*sum(femsWEggs)/sum(femsWValidEggCode),2)) -> eggByYear
    #join by stat area to survey-wide 
    left_join(eggByStat,eggByYear) -> eggsByStatYear  # Percent of females with eggs by stat area and year w surveywide. Vldz excluded from surveywide.
    write.csv(eggsByStatYear,"output/eggByStat.csv")
# Main Survey Summary Table ####
    surv %>% transmute (Year, Pots = Pot_Count,
                        all_lb =  Total_Spot_Wt_KG  * 2.20462 ,
                        all_cnt = Total_Spot_Count, 
                        all_cpue_lb = CPUE_All_LB,
                        all_cpue_cnt = CPUE_All_Count,
                        lrg_lb = Est_Wt_Large * 2.20462, 
                        lrg_cnt = Est_Ct_LG, 
                        lrg_cpue_lb = CPUE_Large_LB, 
                        lrg_cpue_cnt = CPUE_Large_Count) -> main
    write.csv(main,"output/main.csv")
# calculate prop sex and prop egg bearing
    # eggsBySite %>% filter(SITE_ID != 11) %>% group_by(YEAR) %>% summarise(                   # excluding valdez for survey-wide 
    #   surveyWidePropFem = round(100 * sum(fems)/(sum(males)+sum(fems)),1),
    #   surveyWidePropEgg = round(100*sum(femsWEggs)/sum(femsWValidEggCode),2)) -> eggAndSexByYear
         # this matches PropSexForBOF report table, except that some years are null here.  Plan to just copy and paste
         # prop sex and prop fem data from BOF table to the other main survey summary table. 

# Biological - calc mean length by year and sex. 
    pp %>% select(Event, site, Station, pot, perf) %>%   
      right_join (awl)   %>% 
      filter (site != 11, !Station %in% c("E","E1","E2"), 
              perf == 1, species == 965, sex %in% c(1,2)  ) -> awls   # excluding transitionals 
    
    awls %>% group_by(year, sex) %>% summarise(   # there is issue where freq = 2 in 2005 for males, but since grouping by sex and almost all males in 2005 were 2, ok as is 
      n = n(),   
      len = mean (cl), 
      sd = var(cl)^.5) -> meanLen
   
    meanLen %>% select (year, sex, len) %>% spread(sex,len) -> cl
    meanLen %>% select (year, sex, n) %>% spread(sex,n) -> n 
    meanLen %>% select (year, sex, sd) %>% spread(sex,sd) -> sd 
    
    left_join (n,cl, by = 'year') %>% left_join(sd, by = 'year') -> CL
    colnames(CL) <- c('Year','n_m', 'n_f', 'cl_m', 'cl_f', 'sd_m', 'sd_f')
    write.csv(CL,'output/clByYearSex.csv') 
    
# Survey-wide CPUE plot ----
 
    
    str(surv)
    
    surv %>% select (Year, CPUE_All_LB, CPUE_Large_LB) %>% gather(class, cpue_lb, c(CPUE_Large_LB,CPUE_All_LB)) -> surv_l
    

    f <-  ggplot(data = surv_l, 
          aes(x = Year, y = cpue_lb, group = class, colour = class) ) +
          scale_color_grey(start=.7, end=0.1,  name = '', labels = c("All Sizes", "Larges (>32mm)")) +
          #scale_shape_discrete( start = 19, end = 17, name = '', labels = c("All Sizes", "Larges (>32mm)")) + 
          theme(legend.position = c(.2,.8)) +
          scale_x_continuous(breaks = seq(1990,2016,2))  +
          scale_y_continuous(breaks = seq(0,3,.5)) + 
          labs( x= 'Year', y = 'Mean weight per pot (lb)') +
          geom_point(size = 2)+ 
          geom_line ()
    f
      
    ggsave("./figs/surveyWideCPUE_lbs.png", dpi=300, height=4.5, width=6.5, units="in")
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
############################################################################################    
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
  
  
  