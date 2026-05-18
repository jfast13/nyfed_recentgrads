library(ggridges)

ai_exposure <- read.csv("data_inputs/ai_exposure_occ2020.csv")

wages_occ <- load_org(2018:2026, year, month, age, orgwgt, emp, selfemp, occ18, wage) |>
  filter(emp == 1, selfemp == 0, orgwgt > 0, !is.na(wage), !is.na(occ18)) |>
  left_join(ai_exposure, by = join_by(occ18 == cps_code))

quintile_cuts <- map_dbl(
  c(0.2, 0.4, 0.6, 0.8),
  \(p) averaged_quantile(wages_occ$wage, wages_occ$orgwgt, p = p, na.rm = TRUE)
)

pca_by_wage_quintile <- wages_occ |>
  filter(!is.na(pca_score)) |>
  mutate(wage_quintile = cut(wage, breaks = c(0, quintile_cuts, Inf), labels = 1:5, include.lowest = TRUE)) |>
  summarize(
    avg_pca_score = weighted.mean(pca_score, orgwgt, na.rm = TRUE),
    .by = wage_quintile
  ) |>
  arrange(wage_quintile)

write_csv(pca_by_wage_quintile, "outputs/pca_by_wage_quintile.csv")

cps_basic_ai <- cps_basic |>
  filter(emp == 1, !is.na(occ18)) |>
  left_join(ai_exposure, by = join_by(occ18 == cps_code))

pca_by_group <- bind_rows(
  cps_basic_ai |> filter(age >= 16) |> mutate(group = "all_16plus"),
  cps_basic_ai |> filter(age >= 22, age <= 27, educ == 4) |> mutate(group = "grads_22_27"),
  cps_basic_ai |> filter(age >= 16, educ == 4) |> mutate(group = "all_grads"),
  cps_basic_ai |> filter(age >= 22, age <= 27, educ < 4) |> mutate(group = "noncollege_22_27")
) |>
  summarize(avg_pca_score = weighted.mean(pca_score, wgt, na.rm = TRUE),
            .by = c( group))

write_csv(pca_by_group, "outputs/pca_by_group.csv")

occ_ai_shares <- cps_basic_ai |>
  filter(!is.na(pca_score), year_new > 2022,!is.na(basicwgt)) |>
  summarize(
    pca_score = mean(pca_score, na.rm = TRUE),
    wgt_grads_22_27 = sum(basicwgt[age >= 22 & age <= 27 & educ == 4], na.rm = TRUE),
    wgt_noncollege_22_27 = sum(basicwgt[age >= 22 & age <= 27 & educ < 4], na.rm = TRUE),
    wgt_all_grads = sum(basicwgt[educ == 4 & age >= 16], na.rm = TRUE),
    wgt_all_16plus = sum(basicwgt[age >= 16], na.rm = TRUE),
    .by = occ18
  ) |>
  mutate(
    share_grads_22_27 = wgt_grads_22_27 / sum(wgt_grads_22_27),
    share_noncollege_22_27 = wgt_noncollege_22_27 / sum(wgt_noncollege_22_27),
    share_all_grads = wgt_all_grads / sum(wgt_all_grads),
    share_all_16plus = wgt_all_16plus / sum(wgt_all_16plus)
  ) |>
  rename(cps_occ_code = occ18) |>
  arrange(cps_occ_code)

write_csv(occ_ai_shares, "outputs/by_occ_ai.csv")

occ_ai_long <- bind_rows(
  cps_basic_ai |> filter(age >= 16)                           |> mutate(group = "All workers (16+)"),
  cps_basic_ai |> filter(age >= 16, educ == 4)                |> mutate(group = "All college grads"),
  cps_basic_ai |> filter(age >= 22, age <= 27, educ < 4)      |> mutate(group = "Non-college (22-27)"),
  cps_basic_ai |> filter(age >= 22, age <= 27, educ == 4)     |> mutate(group = "Recent grads (22-27)")
) |>
  filter(!is.na(pca_score), year_new > 2022, !is.na(basicwgt)) |>
  mutate(group = fct_rev(factor(group, levels = c(
    "All workers (16+)", "All college grads", "Non-college (22-27)", "Recent grads (22-27)"
  ))))

fig_ridges <- ggplot(occ_ai_long, aes(x = pca_score, y = group, weight = basicwgt, fill = factor(after_stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE, quantiles = 4, quantile_lines = TRUE,
    alpha = 0.8, scale = 1.5, rel_min_height = 0.01
  ) +
  scale_fill_viridis_d(name = "Quartile") +
  scale_color_manual(values = c("#4477AA", "#EE6677", "#228833", "#AA3377")) +
  labs(x = "AI exposure (PCA score)", y = NULL) +
  theme_ridges() +
  theme(legend.position = "right")

ggsave("outputs/fig_ridges.png", fig_ridges, width = 8, height = 5)
