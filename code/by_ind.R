
by_ind_grads <- cps_basic |>
  filter(age >= 22, age <= 27, educ == 4) |>
  summarize(employment = sum(wgt * emp, na.rm = TRUE),
            sample = n(),
             .by = c(year_new, mind03)) |> 
  mutate(group = "grads_22_27")

by_ind_all <- cps_basic |>
  filter(age >= 16) |>
  summarize(employment = sum(wgt * emp, na.rm = TRUE), 
            sample = n(), 
            .by = c(year_new, mind03)) |> 
   mutate(group = "all_16plus")

by_ind = by_ind_all |> 
  bind_rows(by_ind_grads) |> 
  arrange(group, mind03) |> 
  pivot_wider(names_from = c(mind03), values_from = c(employment, sample))

write_csv(by_ind, "outputs/by_ind.csv")