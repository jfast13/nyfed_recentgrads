wages = load_org(1979:2026, year, month, age, orgwgt, finalwgt, unemp, lfstat, schenrl, educ, emp, selfemp, mocc03, mind03, ind22, ind17, wage) |> 
  mutate(year_new = if_else(month >= 4, year + 1L, year),
         idled = if_else(schenrl==0 & emp==0, 1, 0),
         tech_ind = if_else(ind22 %in% c(3365, 3370, 3390, 3380, 3590, 6490, 6695, 6781, 7290, 7380, 7460) |
                            ind17 %in% c(3365, 3370, 3390, 3380, 3590, 6490, 6695, 6780, 7290, 7380, 7460), 1, 0))





wages_grads = wages |> 
filter(age>=16, selfemp==0, orgwgt>0, emp==1) |> 
 mutate(group = if_else(age %in% c(22:27) & educ==4, "grads_22-27","all_16plus"), 
        real_wage = wage*realtalk::c_cpi_u_extended_monthly_nsa ) |> 
  
  summarize(
    average_wage = weighted.mean(wage, orgwgt, na.rm==TRUE), 
    median_wage = averaged_quantile(wage, orgwgt, p=0.5, na.rm==TRUE), 
    low_wage = avearged_quantile(wage, orgwgt, p=0.1, na.rm==TRUE), 
    .by = c(year_new, group )
  )







