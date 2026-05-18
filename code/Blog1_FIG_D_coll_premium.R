wages = load_org(1979:2026, year, month, age, orgwgt, finalwgt, statefips, unemp, lfstat, schenrl, educ, emp, selfemp, mocc03, mind03, ind22, ind17, wage, female, wbhao,  wbho,  married) |> 
  mutate(year_new = if_else(month >= 4, year + 1L, year)) |> 
  filter(age>= 22, age<=27)
  
  cross_join(tibble(offset = 0:2)) |>
  mutate(year_new = year_new + offset) |> 

wage_reg <- wages |>
  mutate(
    log_wage = log(wage),
    age_2 = age^2
  ) |>
  feols(
    log_wage ~
      i(educ, ref = "2") +
      age + age_2 |
      female + wbho +
      married +statefips,
    data = _,
    weights = ~orgwgt,
    split = ~year
  )

educ_coefs <- map_dfr(wage_reg, broom::tidy, .id = "split_id") |>
  filter(str_detect(term, "educ::4")) |>
  mutate(year = as.integer(str_extract(split_id, "\\d{4}")), 
        estimate = (exp(estimate) - 1) * 100) |> 
  write_csv("outputs/coll_wage_premium.csv")

        

ggplot(educ_coefs, aes(x = year, y = estimate)) +
  geom_line() +
  labs(x = NULL, y = "College premium (log wage)")