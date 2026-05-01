 
## ANNUAL STATISTICS  

lf_stats <- cps_basic |>                                                                                                            
    filter(age >= 22, age <= 27, educ==4) |>
    summarize(epop= weighted.mean(emp, wgt, na.rm = TRUE), 
              unemp = weighted.mean(unemp[emp == 1 | unemp == 1], wgt[emp == 1 | unemp == 1], na.rm = TRUE), 
              employment= sum(wgt * emp,na.rm = TRUE), 
              labor_force = sum(wgt[lf], na.rm = TRUE), 
              idled = weighted.mean(idled, wgt, na.rm = TRUE),
              population = sum(wgt,na.rm = TRUE ),
              .by = year_new) 

write.csv(lf_stats, "outputs/lf_stats.csv", row.names = FALSE)

## MONTHLY STATISTICS  

lf_stats_unemp_demo = cps_basic |>
  filter(age >= 22, age <= 27, educ == 4) |>
  (\(d) bind_rows(
    d |> mutate(group = if_else(female == 1, "female", "male")),
    d |>
      mutate(group = case_when(
        wbhao == 1 ~ "white",
        wbhao == 2 ~ "black",
        wbhao == 3 ~ "hispanic",
        wbhao == 4 ~ "asian"
      )) |>
      filter(!is.na(group))
  ))() |>
  summarize(
    unemp = weighted.mean(unemp[emp == 1 | unemp == 1], wgt[emp == 1 | unemp == 1], na.rm = TRUE),
    .by = c(year_new, group)
  ) |>
  pivot_wider(id_cols = year_new, names_from = group, values_from = unemp) |>
  arrange(year_new)

write_csv(lf_stats_unemp_demo, "outputs/lf_stats_unemp_demo.csv")

# Rolling 12-month (11 if window includes Oct 2025) stats for every end-month.
# Use basicwgt (not the pre-divided wgt) so that dividing levels by n_months is
# always correct regardless of how many months fall in each window.
monthly_sums <- cps_basic |>
  filter(age >= 22, age<=27, educ==4) |>
  summarize(
    wgt_total  = sum(basicwgt, na.rm = TRUE),
    wgt_emp    = sum(basicwgt * emp, na.rm = TRUE),
    wgt_unemp  = sum(basicwgt[unemp == 1], na.rm = TRUE),
    wgt_lf_den = sum(basicwgt[emp == 1 | unemp == 1], na.rm = TRUE),
    wgt_lf     = sum(basicwgt[lf], na.rm = TRUE),
    wgt_idled  = sum(basicwgt * idled, na.rm = TRUE),
    .by = c(year, month)
  ) |>
  arrange(year, month)

# Complete month grid so the Oct 2025 gap appears as an explicit zero-weight row
# rather than a missing row; this keeps slider's .before = 11 aligned to calendar months.
monthly_grid <- tibble(
  date = seq(
    as.Date(sprintf("%04d-%02d-01", monthly_sums$year[1], monthly_sums$month[1])),
    as.Date(sprintf("%04d-%02d-01", tail(monthly_sums$year, 1), tail(monthly_sums$month, 1))),
    by = "month"
  )
) |>
  mutate(year = as.integer(format(date, "%Y")), month = as.integer(format(date, "%m")))

monthly_complete <- monthly_grid |>
  left_join(monthly_sums, by = c("year", "month")) |>
  mutate(
    has_data = !is.na(wgt_total),
    across(starts_with("wgt_"), \(x) replace_na(x, 0))
  )

lf_stats_monthly <- monthly_complete |>
  mutate(
    r_total  = slide_dbl(wgt_total,  sum, .before = 11),
    r_emp    = slide_dbl(wgt_emp,    sum, .before = 11),
    r_unemp  = slide_dbl(wgt_unemp,  sum, .before = 11),
    r_lf_den = slide_dbl(wgt_lf_den, sum, .before = 11),
    r_lf     = slide_dbl(wgt_lf,     sum, .before = 11),
    r_idled  = slide_dbl(wgt_idled,  sum, .before = 11),
    n_months = slide_dbl(as.numeric(has_data), sum, .before = 11)
  ) |>
  filter(has_data, n_months >= 11) |>
  transmute(
    date        = as.Date(sprintf("%04d-%02d-01", year, month)),
    n_months,
    epop        = r_emp   / r_total,
    unemp       = r_unemp / r_lf_den,
    employment  = r_emp   / n_months,
    labor_force = r_lf    / n_months,
    idled       = r_idled / r_total,
    population  = r_total / n_months
  ) |> 
  select(-n_months)

write.csv(lf_stats_monthly, "outputs/lf_stats_monthly.csv", row.names = FALSE)