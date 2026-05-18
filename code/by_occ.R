# --- mocc03 ---

by_mocc_grads <- cps_basic |>
  filter(age >= 22, age <= 27, educ == 4) |>
  summarize(employment = sum(wgt * emp, na.rm = TRUE),
            sample = n(),
            .by = c(year_new, mocc03)) |>
  mutate(group = "grads_22_27")

by_mocc_all <- cps_basic |>
  filter(age >= 16) |>
  summarize(employment = sum(wgt * emp, na.rm = TRUE),
            sample = n(),
            .by = c(year_new, mocc03)) |>
  mutate(group = "all_16plus")

by_mocc_all_grads <- cps_basic |>
  filter(age >= 16, educ == 4) |>
  summarize(employment = sum(wgt * emp, na.rm = TRUE),
            sample = n(),
            .by = c(year_new, mocc03)) |>
  mutate(group = "all_grads")

by_mocc_22_27 <- cps_basic |>
  filter(age >= 22, age <= 27, educ < 4) |>
  summarize(employment = sum(wgt * emp, na.rm = TRUE),
            sample = n(),
            .by = c(year_new, mocc03)) |>
  mutate(group = "noncollege_22_27")

by_mocc <- by_mocc_all |>
  bind_rows(by_mocc_grads) |>
  bind_rows(by_mocc_all_grads) |>
  bind_rows(by_mocc_22_27) |>
  arrange(group, mocc03) |>
  pivot_wider(names_from = mocc03, values_from = c(employment, sample))

write_csv(by_mocc, "outputs/by_mocc.csv")

# --- docc03 ---

by_docc_grads <- cps_basic |>
  filter(age >= 22, age <= 27, educ == 4) |>
  summarize(employment = sum(wgt * emp, na.rm = TRUE),
            sample = n(),
            .by = c(year_new, docc03)) |>
  mutate(group = "grads_22_27")

by_docc_all <- cps_basic |>
  filter(age >= 16) |>
  summarize(employment = sum(wgt * emp, na.rm = TRUE),
            sample = n(),
            .by = c(year_new, docc03)) |>
  mutate(group = "all_16plus")

by_docc_all_grads <- cps_basic |>
  filter(age >= 16, educ == 4) |>
  summarize(employment = sum(wgt * emp, na.rm = TRUE),
            sample = n(),
            .by = c(year_new, docc03)) |>
  mutate(group = "all_grads")

by_docc_22_27 <- cps_basic |>
  filter(age >= 22, age <= 27, educ < 4) |>
  summarize(employment = sum(wgt * emp, na.rm = TRUE),
            sample = n(),
            .by = c(year_new, docc03)) |>
  mutate(group = "noncollege_22_27")

by_docc <- by_docc_all |>
  bind_rows(by_docc_grads) |>
  bind_rows(by_docc_all_grads) |>
  bind_rows(by_docc_22_27) |>
  arrange(group, docc03) |>
  pivot_wider(names_from = docc03, values_from = c(employment, sample))

write_csv(by_docc, "outputs/by_docc.csv")
