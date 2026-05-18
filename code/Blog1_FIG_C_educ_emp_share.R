# Share of employed 22-27 year-olds with a BA (educ==4) or advanced degree (educ==5)

educ_emp_share <- cps_basic |>
  filter(age >= 22, age <= 27, emp == 1) |>
  summarize(
    share_ba  = sum(wgt[educ == 4], na.rm = TRUE) / sum(wgt, na.rm = TRUE),
    share_adv = sum(wgt[educ == 5], na.rm = TRUE) / sum(wgt, na.rm = TRUE),
    .by = year_new
  )

# Rolling 12-month stats
monthly_sums_educ <- cps_basic |>
  filter(age >= 22, age <= 27, emp == 1) |>
  summarize(
    wgt_emp_total = sum(basicwgt, na.rm = TRUE),
    wgt_emp_ba    = sum(basicwgt[educ == 4], na.rm = TRUE),
    wgt_emp_adv   = sum(basicwgt[educ == 5], na.rm = TRUE),
    .by = c(year, month)
  ) |>
  arrange(year, month)

monthly_grid_educ <- tibble(
  date = seq(
    as.Date(sprintf("%04d-%02d-01", monthly_sums_educ$year[1], monthly_sums_educ$month[1])),
    as.Date(sprintf("%04d-%02d-01", tail(monthly_sums_educ$year, 1), tail(monthly_sums_educ$month, 1))),
    by = "month"
  )
) |>
  mutate(year = as.integer(format(date, "%Y")), month = as.integer(format(date, "%m")))

monthly_complete_educ <- monthly_grid_educ |>
  left_join(monthly_sums_educ, by = join_by(year, month)) |>
  mutate(
    has_data = !is.na(wgt_emp_total),
    across(starts_with("wgt_"), \(x) replace_na(x, 0))
  )

educ_emp_share_monthly <- monthly_complete_educ |>
  mutate(
    r_emp_total = slide_dbl(wgt_emp_total, sum, .before = 11),
    r_emp_ba    = slide_dbl(wgt_emp_ba,    sum, .before = 11),
    r_emp_adv   = slide_dbl(wgt_emp_adv,   sum, .before = 11),
    n_months    = slide_dbl(as.numeric(has_data), sum, .before = 11)
  ) |>
  filter(has_data, n_months >= 11) |>
  transmute(
    date      = as.Date(sprintf("%04d-%02d-01", year, month)),
    share_ba  = r_emp_ba  / r_emp_total,
    share_adv = r_emp_adv / r_emp_total
  )

write.csv(educ_emp_share_monthly, "outputs/educ_emp_share_monthly.csv", row.names = FALSE)
