library(haven)

occ_crosswalk <- cps_basic |>
  distinct(occ18, mocc03) |>
  filter(!is.na(occ18), !is.na(mocc03)) |>
  mutate(
    occ18_label  = as_factor(occ18),
    mocc03_label = as_factor(mocc03),
    across(c(occ18, mocc03), as.integer)
  )

cps_basic_ai_lab <- cps_basic |>
  filter(emp == 1, !is.na(occ18)) |>
  left_join(ai_exposure, by = join_by(occ18 == cps_code))

occ_ai_labeled <- cps_basic_ai_lab |>
  filter(!is.na(pca_score), year_new > 2022, !is.na(basicwgt)) |>
  summarize(
    pca_score            = mean(pca_score, na.rm = TRUE),
    wgt_grads_22_27      = sum(basicwgt[age >= 22 & age <= 27 & educ == 4], na.rm = TRUE),
    wgt_noncollege_22_27 = sum(basicwgt[age >= 22 & age <= 27 & educ < 4],  na.rm = TRUE),
    wgt_all_grads        = sum(basicwgt[educ == 4 & age >= 16],              na.rm = TRUE),
    wgt_all_16plus       = sum(basicwgt[age >= 16],                          na.rm = TRUE),
    .by = occ18
  ) |>
  mutate(
    share_grads_22_27      = wgt_grads_22_27      / sum(wgt_grads_22_27),
    share_noncollege_22_27 = wgt_noncollege_22_27 / sum(wgt_noncollege_22_27),
    share_all_grads        = wgt_all_grads        / sum(wgt_all_grads),
    share_all_16plus       = wgt_all_16plus        / sum(wgt_all_16plus)
  ) |>
  left_join(occ_crosswalk, by = join_by(occ18)) |>
  select(occ18, occ18_label, mocc03, mocc03_label, pca_score,
         starts_with("share_"), starts_with("wgt_")) |>
  arrange(occ18)

write_csv(occ_ai_labeled, "outputs/by_occ_ai_labeled.csv")