by_tech_grads <- cps_basic |>
  filter(age >= 22, age <= 27, educ == 4) |>
  summarize(
    tech_emp_grads_22_27 = sum(wgt * tech_ind, na.rm = TRUE),
    sample_grads   = sum(tech_ind),
    .by = year_new
  )


by_tech_all <- cps_basic |>
  filter(age >= 16) |>
  summarize(
    tech_emp_all = sum(wgt * tech_ind, na.rm = TRUE),
    sample_all   = sum(tech_ind),
    .by = year_new
  )

by_tech_all_grads <- cps_basic |>
  filter(age >= 16, educ == 4) |>
  summarize(
    tech_emp_all_grads = sum(wgt * tech_ind, na.rm = TRUE),
    sample_grads   = sum(tech_ind),
    .by = year_new
  )

by_tech_young <- cps_basic |>
  filter(age >= 22, age <= 27) |>
  summarize(
    tech_emp_22_27 = sum(wgt * tech_ind, na.rm = TRUE),
    sample_grads   = sum(tech_ind),
    .by = year_new
  )


tech <- by_tech_all |>
  left_join(by_tech_grads, by = join_by(year_new)) |>
  left_join(by_tech_all_grads, by = join_by(year_new)) |>
  left_join(by_tech_young, by = join_by(year_new)) 

write_csv(tech, "outputs/tech.csv")