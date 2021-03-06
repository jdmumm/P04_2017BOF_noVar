## CPUE ####
# Calculates CATCH and CPUE for both Large and Alls from PWS spot shrimp pot survey. Used for 2017 BOF report.
# Josh Mumm 

## PREP ----
library(tidyverse)
cpp <- read.csv('data/cpp_oq.csv') 

#rename vars 
cpp %>% transmute(year = Year, 
                 Event = EVENT_ID, 
                 Site = as.factor(SITE_ID), 
                 Station = STATION, 
                 Pot = as.factor(POT_ID),
                 Sample = SAMPLE_POT, 
                 all_cnt = totCt, 
                 all_kg = totWt, 
                 lrg_cnt = lrgCt,
                 lrg_kg = lrgWt) -> cpp

## ALLS ----
  #survey-wide 
  cpp %>% filter (Site != "11") %>% group_by(year) %>% 
    summarise ( 
      N = n(),
      tau_all_cnt = sum(all_cnt), 
      tau_all_kg = sum(all_kg),
      mu_all_cnt = tau_all_cnt/N,
      mu_all_kg = tau_all_kg/N,  
      var_all_cnt = sum((all_cnt - mu_all_cnt)^2)/N,
      var_all_kg = sum((all_kg - mu_all_kg)^2)/N ,
      cv_all_cnt = 100* (var_all_cnt^.5)/mu_all_cnt, 
      cv_all_kg = 100* (var_all_kg^.5)/mu_all_kg) -> all_byYear
  #bySite
  cpp %>% group_by(year, Site) %>% 
    summarise ( 
      N = n(),
      tau_all_cnt = sum(all_cnt), 
      tau_all_kg = sum(all_kg),
      mu_all_cnt = tau_all_cnt/N,
      mu_all_kg = tau_all_kg/N, 
      var_all_cnt = sum((all_cnt - mu_all_cnt)^2)/N,
      var_all_kg = sum((all_kg - mu_all_kg)^2)/N )  -> all_bySite

## LARGES ----
  # bySite 
    all_bySite %>% select(year, Site, N, mu_all_cnt, mu_all_kg) %>% right_join(cpp) %>% # join site-level stats to CPP
    filter (Sample == "Sample") %>% group_by(year, Site) %>% 
      summarise ( 
        n = n(),
        N = first(N),
        rh_cnt = sum(lrg_cnt, na.rm = T)/sum(all_cnt, na.rm = T), 
        rh_kg  = sum(lrg_kg, na.rm = T)/sum(all_kg, na.rm = T),
        mu_lrg_cnt = rh_cnt * first(mu_all_cnt), 
        mu_lrg_kg  = rh_kg * first(mu_all_kg),
        tau_lrg_cnt = mu_lrg_cnt * N,
        tau_lrg_kg =  mu_lrg_kg * N, 
        var_rh_cnt = sum(((lrg_cnt - rh_cnt*all_cnt)^2), na.rm = T)/(n-1),
        var_rh_kg  = sum(((lrg_kg - rh_kg*all_kg)^2), na.rm = T)/(n-1), 
        var_mu_lrg_cnt =(var_rh_cnt/n), 
        var_mu_lrg_kg = (var_rh_kg/n), 
        var_tau_lrg_cnt = var_mu_lrg_cnt * N^2,
        var_tau_lrg_kg = var_mu_lrg_kg * N^2,
        cv_lrg_cnt = 100* (var_rh_cnt^.5)/mu_lrg_cnt, 
        cv_all_kg = 100* (var_rh_kg^.5)/mu_lrg_kg) -> large_bySite           
  
  #byYear 
    large_bySite %>% filter (Site != "11") %>% group_by (year) %>% 
      summarise (
        N = sum(N),
        tau_lrg_cnt = sum(tau_lrg_cnt),
        tau_lrg_kg = sum(tau_lrg_kg),
        mu_lrg_cnt = tau_lrg_cnt / N,
        mu_lrg_kg  = tau_lrg_kg / N, 
        var_tau_lrg_cnt = sum(var_tau_lrg_cnt), 
        var_tau_lrg_kg  = sum(var_tau_lrg_kg), 
        var_mu_lrg_cnt = var_tau_lrg_cnt/(N^2),
        var_mu_lrg_kg  = var_tau_lrg_kg/(N^2),
        cv_lrg_cnt = 100* (var_tau_lrg_cnt^.5)/mu_lrg_cnt, 
        cv_all_kg = 100* (var_tau_lrg_kg^.5)/mu_lrg_kg ) -> large_byYear
###############################################################################################
## errror checking.  compare to old output  Not Complete----
    options(scipen = 999)
    
    read.csv("data/surveyWide_from16SS.csv") -> surv
    surv %>% left_join(large_byYear, by = c('Year' = 'year')) %>% left_join(all_byYear, by = c('Year' = 'year')) -> comp
    comp %>% transmute(Year, Pot_Count, N.x, Est_Wt_Large, round(tau_lrg_kg,2), CPUE_Large_KG, round(mu_lrg_kg,2),
                       Total_Spot_Wt_KG, round(tau_all_kg,2), CPUE_All_KG, round(mu_all_kg,2))
    
    
    
    
    
    
    
    
    
    
    
    
    
    # ALLS      
    old <- read.csv('data/temp/2016QueryOutput/QUERY_CPUE_ANNUAL_SUMMARY_all_170127.csv')
    str(old)
    str(all_byYear)
    old %>% select (year = Year,
                    N = Pot_Count,
                    tau_all_cnt = Total_Spot_Count, 
                    tau_all_kg = Total_Spot_Wt_KG, 
                    mu_all_cnt = CPUE_All_Count,
                    mu_all_kg = CPUE_All_KG
                    ) -> o
    
    all_byYear[,1:6] -> n 
    
    left_join(n, by = "year" ) -> comp 
    comp
    str(comp)
    comp[ , order(names(comp))] -> comp# reorder columns
    
    #calc difs 
    o[,order(names(o))] -> o
    n[,order(names(n))] -> n 
    str(o)
    str(n)
    
    dif_year <- o[,1:5] - n[,1:5] 
    cbind(dif_year,year = o$year) -> dif_year
    
    per_dif_year <- 100* dif_year[,-6]/o[,-6]
    cbind(per_dif_year,year = o$year) -> per_dif_year
    
    # Larges ####  
    old %>% select (year = Year,
                    N = Pot_Count,
                    tau_lrg_cnt = Est_Ct_LG, 
                    tau_lrg_kg = Est_Wt_Large, 
                    mu_lrg_cnt = CPUE_Large_Count,
                    mu_lrg_kg = CPUE_Large_KG) -> o_l
    large_byYear[,1:6] -> n_l 
    o_l[,order(names(o_l))] -> o_l
    n_l[,order(names(n_l))] -> n_l 
    
    dif_year_l <- o_l[,1:5] - n_l[,1:5] 
    cbind(dif_year_l,year = o_l$year) -> dif_year_l
    
    per_dif_year_l <- 100* dif_year_l[,-6]/o_l[,-6]
    cbind(per_dif_year_l,year = o_l$year) -> per_dif_year_l
    per_dif_year_l %>% transmute(year = year,
                    mu_lrg_cnt = round(mu_lrg_cnt,1),
                    mu_lrg_kg = round(mu_lrg_kg,1),
                    tau_lrg_cnt = round(tau_lrg_cnt,1),
                    tau_lrg_kg = round(tau_lrg_kg,1)) -> per_dif_year_l_r
   
    
    
    summary(per_dif_year_l)
    cpp %>% filter (Sample == "Sample", is.na(lrg_cnt)) %>% group_by (year) %>% summarize ( n())

