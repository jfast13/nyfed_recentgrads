cps_basic_ind <- cps_basic |> 
  mutate(ind = if_else(pubsec==1,13,mind03))

by_ind_grads <- cps_basic_ind |>
  filter(age >= 22, age <= 27, educ == 4) |>
  summarize(employment = sum(wgt * emp, na.rm = TRUE),
            sample = n(),
             .by = c(year_new, ind)) |> 
  mutate(group = "grads_22_27")

by_ind_all <- cps_basic_ind |>
  filter(age >= 16) |>
  summarize(employment = sum(wgt * emp, na.rm = TRUE), 
            sample = n(), 
            .by = c(year_new, ind)) |> 
   mutate(group = "all_16plus")

by_ind_all_grads <- cps_basic_ind |>
  filter(age >= 16, educ==4) |>
  summarize(employment = sum(wgt * emp, na.rm = TRUE), 
            sample = n(), 
            .by = c(year_new, ind)) |> 
   mutate(group = "all_grads")


by_ind_22_27 <- cps_basic_ind |>
  filter(age >= 22, age<=27, educ<4) |>
  summarize(employment = sum(wgt * emp, na.rm = TRUE), 
            sample = n(), 
            .by = c(year_new, ind)) |> 
   mutate(group = "noncollege_22_27")

by_ind = by_ind_all |> 
  bind_rows(by_ind_grads) |> 
  bind_rows(by_ind_all_grads) |> 
  bind_rows(by_ind_22_27) |> 
  arrange(group, ind) |> 
  pivot_wider(names_from = c(ind), values_from = c(employment, sample))

write_csv(by_ind, "outputs/by_ind.csv")