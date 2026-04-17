library(epiextractr)
library(tidyverse)
library(MetricsWeighted)
library(zoo)
library(openxlsx)
library(epidatatools)

cps_basic <- load_basic(1979:2026, year, month, age, basicwgt, finalwgt, unemp, lfstat, schenrl, educ, emp, selfemp, mocc03, mind03, ind22, ind17) |> 
  mutate(year_new = if_else(month >= 4, year + 1L, year),
         wgt = if_else(year_new==2026, basicwgt/11, basicwgt/12), 
         idled = if_else(schenrl==0 & emp==0, 1, 0),
         tech_ind = if_else(ind22 %in% c(3365, 3370, 3390, 3380, 3590, 6490, 6695, 6781, 7290, 7380, 7460) |
                            ind17 %in% c(3365, 3370, 3390, 3380, 3590, 6490, 6695, 6780, 7290, 7380, 7460), 1, 0))


