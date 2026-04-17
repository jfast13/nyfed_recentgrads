library(tidyverse)
library(MetricsWeighted)
library(zoo)
library(openxlsx)

if (!exists("unemp_groups_e")) source("nyfed_enrolled.R")
if (!exists("workers_by_occ")) source("by_occupation.R")


# --- Not-enrolled unemp (schenrl == 0) ----------------------------------------

base_nonenroll <- cps_basic |>
  filter(basicwgt > 0, lfstat != 3, schenrl == 0)

unemp_groups_nonenroll <- bind_rows(
  base_nonenroll |> filter(age >= 16, age <= 64) |>
    summarize(wt_num = sum(basicwgt * unemp, na.rm = TRUE), wt_den = sum(basicwgt), .by = c(year, month)) |>
    mutate(group = "all_workers"),
  base_nonenroll |> filter(age >= 22, age <= 64, educ >= 4) |>
    summarize(wt_num = sum(basicwgt * unemp, na.rm = TRUE), wt_den = sum(basicwgt), .by = c(year, month)) |>
    mutate(group = "college_grads"),
  base_nonenroll |> filter(age >= 22, age <= 27, educ >= 4) |>
    summarize(wt_num = sum(basicwgt * unemp, na.rm = TRUE), wt_den = sum(basicwgt), .by = c(year, month)) |>
    mutate(group = "recent_grads"),
  base_nonenroll |> filter(age >= 22, age <= 27, educ < 4) |>
    summarize(wt_num = sum(basicwgt * unemp, na.rm = TRUE), wt_den = sum(basicwgt), .by = c(year, month)) |>
    mutate(group = "young_workers")
) |>
  arrange(group, year, month) |>
  mutate(
    unemp_rate_12ma = rollsum(wt_num, k = 12, fill = NA, align = "right") /
                      rollsum(wt_den, k = 12, fill = NA, align = "right") * 100,
    .by = group
  ) |>
  select(year, month, group, unemp_rate_12ma) |>
  pivot_wider(names_from = group, values_from = unemp_rate_12ma, names_glue = "{group}_12ma") |>
  mutate(date = as.Date(paste(year, month, "01", sep = "-")))


# --- Not-enrolled epop (schenrl == 0) -----------------------------------------

base_epop_nonenroll <- cps_basic |>
  filter(basicwgt > 0, schenrl == 0)

epop_groups_nonenroll <- bind_rows(
  base_epop_nonenroll |> filter(age >= 16, age <= 64) |>
    summarize(wt_num = sum(basicwgt * replace_na(emp, 0)), wt_den = sum(basicwgt), .by = c(year, month)) |>
    mutate(group = "all_workers"),
  base_epop_nonenroll |> filter(age >= 22, age <= 64, educ >= 4) |>
    summarize(wt_num = sum(basicwgt * replace_na(emp, 0)), wt_den = sum(basicwgt), .by = c(year, month)) |>
    mutate(group = "college_grads"),
  base_epop_nonenroll |> filter(age >= 22, age <= 27, educ >= 4) |>
    summarize(wt_num = sum(basicwgt * replace_na(emp, 0)), wt_den = sum(basicwgt), .by = c(year, month)) |>
    mutate(group = "recent_grads"),
  base_epop_nonenroll |> filter(age >= 22, age <= 27, educ < 4) |>
    summarize(wt_num = sum(basicwgt * replace_na(emp, 0)), wt_den = sum(basicwgt), .by = c(year, month)) |>
    mutate(group = "young_workers")
) |>
  arrange(group, year, month) |>
  mutate(
    epop_12ma = rollsum(wt_num, k = 12, fill = NA, align = "right") /
                rollsum(wt_den, k = 12, fill = NA, align = "right") * 100,
    .by = group
  ) |>
  select(year, month, group, epop_12ma) |>
  pivot_wider(names_from = group, values_from = epop_12ma, names_glue = "{group}_12ma") |>
  mutate(date = as.Date(paste(year, month, "01", sep = "-")))


# --- Export -------------------------------------------------------------------

sheet_wide <- function(df) df |> select(date, ends_with("_12ma"))

wb <- createWorkbook()

addWorksheet(wb, "unemp_nonenrolled")
writeData(wb, "unemp_nonenrolled", sheet_wide(unemp_groups_nonenroll))

addWorksheet(wb, "unemp_enrolled")
writeData(wb, "unemp_enrolled", sheet_wide(unemp_groups_e))

addWorksheet(wb, "epop_nonenrolled")
writeData(wb, "epop_nonenrolled", sheet_wide(epop_groups_nonenroll))

addWorksheet(wb, "epop_enrolled")
writeData(wb, "epop_enrolled", sheet_wide(epop_groups))

addWorksheet(wb, "enrollment")
writeData(wb, "enrollment", sheet_wide(enroll_groups))

addWorksheet(wb, "workers_by_occupation")
writeData(wb, "workers_by_occupation",
  workers_by_occ |>
    select(year, month, mocc03, workers_12ma) |>
    pivot_wider(names_from = mocc03, values_from = workers_12ma)
)

addWorksheet(wb, "workers_by_industry")
writeData(wb, "workers_by_industry",
  workers_by_ind |>
    select(year, month, mind03, workers_12ma) |>
    pivot_wider(names_from = mind03, values_from = workers_12ma)
)

saveWorkbook(wb, "outputs/nyfed_data.xlsx", overwrite = TRUE)
