library(epiextractr)
library(tidyverse)
library(MetricsWeighted)
library(zoo)
library(openxlsx)


# --- Build data (sourced from clean_data.R) -----------------------------------
if (!exists("cps_basic") || !all(c("mocc03", "mind03") %in% names(cps_basic))) {
  cps_basic <- load_basic(2013:2026, year, month, age, basicwgt, unemp, lfstat, schenrl, educ, emp, selfemp, mocc03, mind03)
}

base <- cps_basic |>
  filter(basicwgt > 0, lfstat != 3, schenrl == 0)

young_workers <- base |>
  filter(age >= 22, age <= 27, emp == 1)


# --- Workers by occupation ----------------------------------------------------

workers_by_occ <- young_workers |>
  summarize(workers = sum(basicwgt), n = n(), .by = c(year, month, mocc03)) |>
  arrange(mocc03, year, month) |>
  mutate(workers_12ma = rollmean(workers, k = 12, fill = NA, align = "right"),
         .by = mocc03) |>
  mutate(date = as.Date(paste(year, month, "01", sep = "-")))


# --- Workers by industry ------------------------------------------------------

workers_by_ind <- young_workers |>
  summarize(workers = sum(basicwgt), n = n(), .by = c(year, month, mind03)) |>
  arrange(mind03, year, month) |>
  mutate(workers_12ma = rollmean(workers, k = 12, fill = NA, align = "right"),
         .by = mind03) |>
  mutate(date = as.Date(paste(year, month, "01", sep = "-")))