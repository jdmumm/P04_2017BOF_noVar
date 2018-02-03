# Examine Precision relative to target in op-plan 180201

library(tidyverse)
# survey wide 
  all_v <- read.csv('./P04_2017BOF/output/var_byYear_xz_w17.csv') 
  core_v <- read.csv('./P04_2017BOF/output/var_byYear_xz_w17_core.csv')
  all_p <- read.csv('./data/surveyWide_from17SS.csv')
  core_p <- read.csv('./data/surveyWide_core.csv')
  
  all_p %>%  select (year = Year, CPUE_All_LB, CPUE_Large_LB) %>% left_join(all_v) -> all 
  core_p %>% select (year = Year, CPUE_All_LB, CPUE_Large_LB) %>% left_join(core_v) -> core
  
# By Area
  all_v_byArea <- read.csv('./P04_2017BOF/output/var_byArea_xz_w17.csv')
  core_v_byArea <- read.csv('./P04_2017BOF/output/var_byArea_xz_w17_core.csv')
  core_p_byArea <- read.csv('./output/CPUELB_byShrimpArea_w17.csv')
  all_p_byArea <- read.csv('./output/CPUELB_byShrimpArea_core.csv')
    
  all_p_byArea %>% left_join(all_v_byArea, by = c('year'= 'year', 'ShrimpArea' = 'Area')) %>% 
    select(year, area = ShrimpArea, cpueAllLb, cpueLrgLb, N,n, var_all_kg, se_all_kg, var_tau_lrg_kg, se_lrg_kg)  -> all_byArea 
  core_p_byArea %>% left_join(core_v_byArea, by = c('year'= 'year', 'ShrimpArea' = 'Area')) %>% 
    select(year, area = ShrimpArea, cpueAllLb, cpueLrgLb, N,n, var_all_kg, se_all_kg, var_tau_lrg_kg, se_lrg_kg)  -> core_byArea 
  
#SURVEY-WIDE ----
  # was targret precision achieved when using ALL site and stations? Yes, for all years for both ALL and LRGs. 
  all %>% transmute (year,
                 cpue_lrg = CPUE_Large_LB, 
                 cpue_all = CPUE_All_LB, 
                 df = n-1,
                 t = abs(qt(.05/2,df)), 
                 se_lrg = 2.20462 * se_lrg_kg,
                 se_all = 2.20462 * se_lrg_kg,
                 cv_all = 100* ((2.20462 * var_all_kg)^.5)/cpue_all, # cv_lrg omitted because don't have var_large, only var_tau_large
                 ci_lrg = t*se_lrg, 
                 ci_all = t*se_all,
                 peX15_lrg = cpue_lrg * .15,
                 peX15_all = cpue_all * .15, 
                 metGoal_lrg = ifelse((ci_lrg < peX15_lrg), "YES","NO"), 
                 metGoal_all = ifelse((ci_all < peX15_all), "YES","NO")) -> all_summary
        write.csv(all_summary, './output/precisionSummaries/all_summary.csv')
  # Core only. Met goal for years except 2017 for larges. 
  core %>% transmute (year,
                 cpue_lrg = CPUE_Large_LB, 
                 cpue_all = CPUE_All_LB, 
                 df = n-1,
                 t = abs(qt(.05/2,df)), 
                 se_lrg = 2.20462 * se_lrg_kg,
                 se_all = 2.20462 * se_lrg_kg,
                 cv_all = 100* ((2.20462 * var_all_kg)^.5)/cpue_all, # cv_lrg omitted because don't have var_large, only var_tau_large
                 ci_lrg = t*se_lrg, 
                 ci_all = t*se_all,
                 peX15_lrg = cpue_lrg * .15,
                 peX15_all = cpue_all * .15, 
                 metGoal_lrg = ifelse((ci_lrg < peX15_lrg), "YES","NO"), 
                 metGoal_all = ifelse((ci_all < peX15_all), "YES","NO")) -> core_summary
         write.csv(core_summary, './output/precisionSummaries/core_summary.csv')
  #By-Area ----
  all_byArea %>% transmute (year, area,
                     cpue_lrg = cpueLrgLb, 
                     cpue_all = cpueAllLb, 
                     df = n-1,
                     t = abs(qt(.05/2,df)), 
                     se_lrg = 2.20462 * se_lrg_kg,
                     se_all = 2.20462 * se_lrg_kg,
                     cv_all = 100* ((2.20462 * var_all_kg)^.5)/cpue_all, # cv_lrg omitted because don't have var_large, only var_tau_large
                     ci_lrg = t*se_lrg, 
                     ci_all = t*se_all,
                     peX15_lrg = cpue_lrg * .15,
                     peX15_all = cpue_all * .15, 
                     metGoal_lrg = ifelse((ci_lrg < peX15_lrg), "YES","NO"), 
                     metGoal_all = ifelse((ci_all < peX15_all), "YES","NO")) -> all_byArea_summary
         write.csv(all_byArea_summary, './output/precisionSummaries/all_byArea_summary.csv')
  # Core only. Met goal for years except 2017 for larges. 
  core_byArea %>% transmute (year, area,
                      cpue_lrg = cpueLrgLb, 
                      cpue_all = cpueAllLb, 
                      df = n-1,
                      t = abs(qt(.05/2,df)), 
                      se_lrg = 2.20462 * se_lrg_kg,
                      se_all = 2.20462 * se_lrg_kg,
                      cv_all = 100* ((2.20462 * var_all_kg)^.5)/cpue_all, # cv_lrg omitted because don't have var_large, only var_tau_large
                      ci_lrg = t*se_lrg, 
                      ci_all = t*se_all,
                      peX15_lrg = cpue_lrg * .15,
                      peX15_all = cpue_all * .15, 
                      metGoal_lrg = ifelse((ci_lrg < peX15_lrg), "YES","NO"), 
                      metGoal_all = ifelse((ci_all < peX15_all), "YES","NO")) -> core_byArea_summary
          write.csv(core_byArea_summary, './output/precisionSummaries/core_byArea_summary.csv')
