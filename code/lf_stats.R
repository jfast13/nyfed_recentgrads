  lf_stats <- cps_basic |>                                                                                                            
    filter(age >= 22, age <= 27, educ==4) |>
    summarize(epop= weighted.mean(emp, wgt, na.rm = TRUE), 
              unemp = weighted.mean(unemp[emp == 1 | unemp == 1], wgt[emp == 1 | unemp == 1], na.rm = TRUE), 
              employment= sum(wgt * emp,na.rm = TRUE), 
              labor_force= sum(wgt * emp + wgt * unemp, na.rm = TRUE),
              idled = weighted.mean(idled, wgt, na.rm = TRUE),
              .by = year_new) 

write.csv(lf_stats, "outputs/lf_stats.csv", row.names = FALSE) 