# Plot site specific CPUE for preseason fleet meeting on JRs request. 
#190304

library(tidyverse)
library(reshape2)
library(extrafont)
library(gridExtra)
font_import()
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=12,base_family='Times New Roman')+ 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())+ 
            theme(legend.title = element_blank()))

read.csv("data/surveyWide_from18SS.csv") -> surv  #Survey-wide summary by year
read.csv("data/bySite_from18SS.csv")%>%
  transmute(year = Year, Site = Site_ID, pots = Pot_Count, all_cnt = Total_Spot_Count, all_lb = Total_Spot_Wt_KG * 2.20462, propLrg = Proportion_Large, 
            lrg_cnt = Est_Count_LG, lrg_lb = Est_Wt_Large * 2.20462, cpue_all_lbs = CPUE_All_LB,
            cpue_lrg_lbs = CPUE_Large_LB, cpue_all_cnt = CPUE_All_Count, cpue_lrg_cnt=CPUE_Large_Count) ->site
read.csv("data/SiteStatArea_LUT.csv") %>% select (Site = 'SiteNum', SiteName, ShrimpArea) -> area

# join areas to site CPUE
site %>% left_join (area) -> site
site %>% filter (Site != 11) %>% mutate (Site = as.factor(Site)) -> site
  gsub("Prince Of Wales", "Prince of Wales", site$SiteName) -> site$SiteName


labels <- c('1' = "Area 1", '2' = "Area 2", '3' = "Area 3")


# 3 seperate plots - LARGES ####
site %>% filter (ShrimpArea == 1) -> site.a1
site %>% filter (ShrimpArea == 2) -> site.a2
site %>% filter (ShrimpArea == 3) -> site.a3

a1 <-  ggplot(data = site.a1, 
             aes(x = year, y = cpue_lrg_lbs, group = Site, colour = SiteName, shape = SiteName) ) +
  scale_color_manual (values = c("gray10", "gray50", "gray70")) +
  scale_shape_manual (values = c(15,16,17)) +
  theme(legend.position = c(.15,.7), legend.background = element_rect (fill = "transparent" )) +
  scale_x_continuous(breaks = seq(1990,2018,2))  +
  scale_y_continuous(breaks = seq(0,8,1), limits = c(0,8)) + 
  labs( x= 'Year', y = 'Mean weight per pot (lb)') +
  geom_point(size = 1.5)+ 
  geom_line ()  +
  theme( axis.text.x  = element_text(angle=0, vjust= 0)) +
  facet_wrap(~ShrimpArea, ncol=1, labeller=labeller(ShrimpArea = labels), strip.position = "r")

a2 <-  ggplot(data = site.a2, 
              aes(x = year, y = cpue_lrg_lbs, group = Site, colour = SiteName, shape = SiteName) ) +
  scale_color_manual (values = c("gray10", "gray50", "gray70")) +
  scale_shape_manual (values = c(15,16,17)) +
  theme(legend.position = c(.15,.7), legend.background = element_rect (fill = "transparent" )) +
  scale_x_continuous(breaks = seq(1990,2018,2))  +
  scale_y_continuous(breaks = seq(0,8,1),limits = c(0,8)) + 
  labs( x= 'Year', y = 'Mean weight per pot (lb)') +
  geom_point(size = 1.5)+ 
  geom_line ()  +
  theme( axis.text.x  = element_text(angle=0, vjust= 0)) +
  facet_wrap(~ShrimpArea, ncol=1, labeller=labeller(ShrimpArea = labels), strip.position = "r")

a3 <-  ggplot(data = site.a3, 
              aes(x = year, y = cpue_lrg_lbs, group = Site, colour = SiteName, shape = SiteName) ) +
  scale_color_manual (values = c("gray10", "gray50", "gray70", "gray0")) +
  scale_shape_manual (values = c(15,16,17,4)) +
  theme(legend.position = c(.15,.7), legend.background = element_rect (fill = "transparent" )) +
  scale_x_continuous(breaks = seq(1990,2018,2))  +
  scale_y_continuous(breaks = seq(0,8,1), limits = c(0,8)) + 
  labs( x= 'Year', y = 'Mean weight per pot (lb)') +
  geom_point(size = 1.5)+ 
  geom_line ()  +
  theme( axis.text.x  = element_text(angle=0, vjust= 0)) +
  facet_wrap(~ShrimpArea, ncol=1, labeller=labeller(ShrimpArea = labels), strip.position = "r")

LRG <-arrangeGrob(a1,a2,a3, ncol=1)
LRG %>% ggsave(file = "./figs/cpueLrg_byAreaSite.png", dpi=300, height=8.5, width=6.5, units="in")

# 3 seperate plots - ALL ####
a1.all <-  ggplot(data = site.a1, 
              aes(x = year, y = cpue_all_lbs, group = Site, colour = SiteName, shape = SiteName) ) +
  scale_color_manual (values = c("gray10", "gray50", "gray70")) +
  scale_shape_manual (values = c(15,16,17)) +
  theme(legend.position = c(.15,.7), legend.background = element_rect (fill = "transparent" )) +
  scale_x_continuous(breaks = seq(1990,2018,2))  +
  scale_y_continuous(breaks = seq(0,8,1), limits = c(0,8)) + 
  labs( x= 'Year', y = 'Mean weight per pot (lb)') +
  geom_point(size = 1.5)+ 
  geom_line ()  +
  theme( axis.text.x  = element_text(angle=0, vjust= 0)) +
  facet_wrap(~ShrimpArea, ncol=1, labeller=labeller(ShrimpArea = labels), strip.position = "r")

a2.all <-  ggplot(data = site.a2, 
              aes(x = year, y = cpue_all_lbs, group = Site, colour = SiteName, shape = SiteName) ) +
  scale_color_manual (values = c("gray10", "gray50", "gray70")) +
  scale_shape_manual (values = c(15,16,17)) +
  theme(legend.position = c(.15,.7), legend.background = element_rect (fill = "transparent" )) +
  scale_x_continuous(breaks = seq(1990,2018,2))  +
  scale_y_continuous(breaks = seq(0,8,1),limits = c(0,8)) + 
  labs( x= 'Year', y = 'Mean weight per pot (lb)') +
  geom_point(size = 1.5)+ 
  geom_line ()  +
  theme( axis.text.x  = element_text(angle=0, vjust= 0)) +
  facet_wrap(~ShrimpArea, ncol=1, labeller=labeller(ShrimpArea = labels), strip.position = "r")

a3.all <-  ggplot(data = site.a3, 
              aes(x = year, y = cpue_all_lbs, group = Site, colour = SiteName, shape = SiteName) ) +
  scale_color_manual (values = c("gray10", "gray50", "gray70", "gray0")) +
  scale_shape_manual (values = c(15,16,17,4)) +
  theme(legend.position = c(.15,.7), legend.background = element_rect (fill = "transparent" )) +
  scale_x_continuous(breaks = seq(1990,2018,2))  +
  scale_y_continuous(breaks = seq(0,8,1), limits = c(0,8)) + 
  labs( x= 'Year', y = 'Mean weight per pot (lb)') +
  geom_point(size = 1.5)+ 
  geom_line ()  +
  theme( axis.text.x  = element_text(angle=0, vjust= 0)) +
  facet_wrap(~ShrimpArea, ncol=1, labeller=labeller(ShrimpArea = labels), strip.position = "r")

ALL <-arrangeGrob(a1.all,a2.all,a3.all, ncol=1)

ALL %>% ggsave(file = "./figs/cpueAll_byAreaSite.png", dpi=300, height=8.5, width=6.5, units="in")




















# 
# # one plot - didn't end go this route because ggplot isn't setup to recycycle symbols for each facet, and too many shades of gray to use diffent gray for each site####
# 
# a <-  ggplot(data = site, 
#              aes(x = year, y = cpue_lrg_lbs, group = Site, colour = Site) ) +
#   scale_color_grey() +
#   theme(legend.position = c(.85,.8), legend.background = element_rect (fill = "transparent" )) +
#   scale_x_continuous(breaks = seq(1990,2018,2))  +
#   scale_y_continuous(breaks = seq(0,4,.5)) + 
#   labs( x= 'Year', y = 'Mean weight per pot (lb)') +
#   geom_point(size = 1.5, aes(shape = Site))+ 
#   geom_line ()  +
#   theme( axis.text.x  = element_text(angle=90, vjust=0.5)) +
#   facet_wrap(~ShrimpArea, ncol=1, labeller=labeller(ShrimpArea = labels))
# a
