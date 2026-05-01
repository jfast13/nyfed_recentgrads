wages = load_org(1979:2026, year, month, age, orgwgt, finalwgt, unemp, lfstat, schenrl, educ, emp, selfemp, mocc03, mind03, ind22, ind17, wage, female, wbhao) |> 
  mutate(year_new = if_else(month >= 4, year + 1L, year),
         idled = if_else(schenrl==0 & emp==0, 1, 0),
         tech_ind = if_else(ind22 %in% c(3365, 3370, 3390, 3380, 3590, 6490, 6695, 6781, 7290, 7380, 7460) |
                            ind17 %in% c(3365, 3370, 3390, 3380, 3590, 6490, 6695, 6780, 7290, 7380, 7460), 1, 0))


cpi_base = realtalk::c_cpi_u_extended_monthly_nsa |>
  filter(year == 2026, month == 3) |>
  pull(c_cpi_u_extended)

wages_grads = wages |>
  filter(age>=16, selfemp==0, orgwgt>0, emp==1) |>
  left_join(realtalk::c_cpi_u_extended_monthly_nsa, by = join_by(year, month)) |>
  mutate(
    group = if_else(age %in% c(22:27) & educ==4, "grads_22-27", "all_16plus"),
    real_wage_mar26 = wage * cpi_base / c_cpi_u_extended
  ) |>
  summarize(
    average_wage_mar26 = weighted.mean(real_wage_mar26, orgwgt, na.rm=TRUE),
    median_wage_mar26 = averaged_quantile(real_wage_mar26, orgwgt, p=0.5, na.rm=TRUE),
    low_wage_mar26 = averaged_quantile(real_wage_mar26, orgwgt, p=0.1, na.rm=TRUE),
    .by = c(year_new, group)
  ) |> 
  arrange(group, year_new)

write_csv(wages_grads, "outputs/wages.csv")

wages_grads_demo = wages |>
  filter(age %in% 22:27, educ == 4, selfemp == 0, orgwgt > 0, emp == 1) |>
  left_join(realtalk::c_cpi_u_extended_monthly_nsa, by = join_by(year, month)) |>
  mutate(real_wage_mar26 = wage * cpi_base / c_cpi_u_extended)

wage_gaps = bind_rows(
  wages_grads_demo |>
    mutate(group = "all"),
  wages_grads_demo |>
    mutate(group = if_else(female == 1, "female", "male")),
  wages_grads_demo |>
    mutate(group = case_when(
      wbhao == 1 ~ "white",
      wbhao == 2 ~ "black",
      wbhao == 3 ~ "hispanic",
      wbhao == 4 ~ "asian"
    )) |>
    filter(!is.na(group))
) |>
  cross_join(tibble(offset = 0:2)) |>
  mutate(year_new = year_new + offset) |>
  summarize(
    average_wage_mar26 = weighted.mean(real_wage_mar26, orgwgt, na.rm = TRUE),
    median_wage_mar26 = averaged_quantile(real_wage_mar26, orgwgt, p = 0.5, na.rm = TRUE),
    low_wage_mar26 = averaged_quantile(real_wage_mar26, orgwgt, p = 0.1, na.rm = TRUE),
    .by = c(year_new, group)
  ) |>
  arrange(group, year_new)

write_csv(wage_gaps, "outputs/wage_demogrpahics.csv")





