library(tidyverse)
read.csv('./data/harvest.csv') -> hrv
hrv %>% select (year,spots_com,tot_com,spots_nc, spots_nc, tot) -> hrv 

str(hrv)
plot(hrv$year, hrv$tot_com, pch=19, cex = 2)
lines(hrv$year, hrv$tot_com, lwd = 4, add = T)


points(hrv$year, hrv$spots_nc, col = 'blue', add = T, cex = 2)
points(hrv$year, hrv$tot, pch =19, col = 'red')
lines(hrv$year, hrv$tot, lwd = 2, col = 'red')
