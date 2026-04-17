library(epiextractr)
library(tidyverse)
library(MetricsWeighted)
library(zoo)
library(openxlsx)


# --- Build data (sourced from clean_data.R) -----------------------------------

if (!exists("cps_basic")) {
  cps_basic <- load_basic(2013:2026, year, month, age, basicwgt, unemp, lfstat, schenrl, educ, emp, selfemp)
}

base <- cps_basic |>
  filter(basicwgt > 0,  schenrl == 0)

unemp_groups <- bind_rows(
  base |> filter(age >= 16, age <= 64,lfstat != 3) |>
    summarize(unemp_rate = weighted_mean(unemp, w = basicwgt, na.rm = TRUE), .by = c(year, month)) |>
    mutate(group = "all_workers"),
  base |> filter(age >= 22, age <= 64, educ >= 4) |>
    summarize(unemp_rate = weighted_mean(unemp, w = basicwgt, na.rm = TRUE), .by = c(year, month)) |>
    mutate(group = "college_grads"),
  base |> filter(age >= 22, age <= 27, educ >= 4) |>
    summarize(unemp_rate = weighted_mean(unemp, w = basicwgt, na.rm = TRUE), .by = c(year, month)) |>
    mutate(group = "recent_grads"),
  base |> filter(age >= 22, age <= 27, educ < 4) |>
    summarize(unemp_rate = weighted_mean(unemp, w = basicwgt, na.rm = TRUE), .by = c(year, month)) |>
    mutate(group = "young_workers")
) |>
  pivot_wider(names_from = group, values_from = unemp_rate) |>
  arrange(year, month) |>
  mutate(across(
    c(all_workers, college_grads, recent_grads, young_workers),
    ~ rollmean(.x, k = 3, fill = NA, align = "right") * 100,
    .names = "{.col}_3ma"
  )) |>
  mutate(date = as.Date(paste(year, month, "01", sep = "-")))

# Pivot to long for ggplot
plot_data <- unemp_groups |>
  select(date, ends_with("_3ma")) |>
  pivot_longer(-date, names_to = "group", values_to = "unemp_rate") |>
  mutate(group = factor(group,
    levels = c("recent_grads_3ma", "college_grads_3ma", "young_workers_3ma", "all_workers_3ma"),
    labels = c("Recent college graduates (aged 22-27)", "All college graduates",
               "Young workers (aged 22-27)", "All workers")
  ))


# --- EPOP data ----------------------------------------------------------------

# Full population base (no lfstat filter) for EPOP denominator
base_epop <- cps_basic |>
  filter(basicwgt > 0, schenrl == 0)

epop_groups <- bind_rows(
  base_epop |> filter(age >= 16, age <= 64) |>
    summarize(wt_num = sum(basicwgt * replace_na(emp, 0)), wt_den = sum(basicwgt), .by = c(year, month)) |>
    mutate(group = "all_workers"),
  base_epop |> filter(age >= 22, age <= 64, educ >= 4) |>
    summarize(wt_num = sum(basicwgt * replace_na(emp, 0)), wt_den = sum(basicwgt), .by = c(year, month)) |>
    mutate(group = "college_grads"),
  base_epop |> filter(age >= 22, age <= 27, educ >= 4) |>
    summarize(wt_num = sum(basicwgt * replace_na(emp, 0)), wt_den = sum(basicwgt), .by = c(year, month)) |>
    mutate(group = "recent_grads"),
  base_epop |> filter(age >= 22, age <= 27, educ < 4) |>
    summarize(wt_num = sum(basicwgt * replace_na(emp, 0)), wt_den = sum(basicwgt), .by = c(year, month)) |>
    mutate(group = "young_workers")
) |>
  arrange(group, year, month) |>
  mutate(
    epop_12ma = rollsum(wt_num, k = 12, fill = NA, align = "right") /
                rollsum(wt_den, k = 12, fill = NA, align = "right") * 100,
    .by = group
  ) |>
  select(year, month, group, epop_12ma) |>
  pivot_wider(names_from = group, values_from = epop_12ma, names_glue = "{group}_12ma") |>
  mutate(date = as.Date(paste(year, month, "01", sep = "-")))

# --- NBER recession bands -----------------------------------------------------
recessions <- tibble(
  start = as.Date(c("2020-02-01")),
  end   = as.Date(c( "2020-04-01"))
)


# --- Plot ---------------------------------------------------------------------

group_colors <- c(
  "Recent college graduates (aged 22-27)" = "#2166ac",
  "All college graduates"                 = "#d6604d",
  "Young workers (aged 22-27)"            = "#74add1",
  "All workers"                           = "#4d4d4d"
)

ggplot() +
  geom_rect(
    data = recessions,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
    fill = "grey85", alpha = 0.6, inherit.aes = FALSE
  ) +
  geom_line(
    data = plot_data,
    aes(x = date, y = unemp_rate, color = group),
    linewidth = 0.6, na.rm = TRUE
  ) +
  scale_color_manual(values = group_colors) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y", expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, NA), labels = scales::label_number(suffix = "")) +
  labs(
    x = NULL,
    y = "Percent",
    color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position      = "top",
    legend.justification = "left",
    legend.text          = element_text(size = 8),
    panel.grid.minor     = element_blank(),
    panel.grid.major.x   = element_blank(),
    axis.line.x          = element_line(color = "black", linewidth = 0.3),
    axis.ticks.x         = element_line(color = "black", linewidth = 0.3),
    plot.margin          = margin(10, 15, 10, 10)
  )

ggsave("images/unemp_groups.png", width = 8, height = 4.5, dpi = 300)
