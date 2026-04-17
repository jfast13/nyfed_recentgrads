library(epiextractr)
library(tidyverse)
library(MetricsWeighted)
library(zoo)
library(openxlsx)

cps_basic <- load_basic(2013:2026, year, month, age, basicwgt, unemp, lfstat, schenrl, educ, emp, selfemp, mocc03, mind03)

source("nyfed_enrolled.R")
source("ny_fed_unemp.R")
source("by_occupation.R")
source("export_spreadsheet.R")

