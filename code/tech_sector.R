by_tech_grads <- tech_master |>
  filter(age >= 22, age <= 27, educ == 4) |>
  summarize(
    tech_emp_grads = sum(wgt * tech_ind, na.rm = TRUE),
    sample_grads   = sum(tech_ind),
    .by = year_new
  )

by_tech_all <- tech_master |>
  filter(age >= 16) |>
  summarize(
    tech_emp_all = sum(wgt * tech_ind, na.rm = TRUE),
    sample_all   = sum(tech_ind),
    .by = year_new
  )

tech <- by_tech_all |>
  left_join(by_tech_grads, by = join_by(year_new)) |>
  select(year_new, tech_emp_all, tech_emp_grads, sample_all, sample_grads)

write_csv(tech, "outputs/tech.csv")