table <- function(data){
  bracket <- ")"
  data %>% 
    filter(int %in% c(0,1,2,3,4,6,7,8,9,10)) %>% 
    mutate_if(is.numeric, round,2) %>% 
    unite("riskCI95", c("pd_llim95", "pd_ulim95"), sep = " to ", remove = TRUE) %>% 
    unite("r", c("pd", "riskCI95"), sep = " (", remove = TRUE) %>% 
    mutate(r = glue("{r}{bracket}")) %>% 
    unite("rrCI95", c("RR_llim95", "RR_ulim95"), sep = " to ", remove = TRUE) %>%
    unite("risk_ratio", c("rr", "rrCI95"), sep = " (", remove = TRUE) %>%
    mutate(risk_ratio = glue("{risk_ratio}{bracket}")) %>%
    unite("rdCI95", c("RD_llim95", "RD_ulim95"), sep = " to ", remove = TRUE) %>%
    unite("risk_diff", c("rd", "rdCI95"), sep = " (", remove = TRUE) %>%
    mutate(risk_diff = glue("{risk_diff}{bracket}")) %>%
    mutate(int2 = case_when(
      int == 0 ~ "Natural course",
      int == 1 ~ "Maintaining SBP below 120 mmHg",
      int == 2 ~ "Maintaining SBP below 140 mmHg",
      int == 3 ~ "Reducing SBP by 10% if above 140 mmHg",
      int == 4 ~ "Reducing SBP by 20% if above 140 mmHg",
      int == 6 ~ "Quitting smoking",
      int == 7 ~ "Joint 1 + 5",
      int == 8 ~ "Joint 2 + 5",
      int == 9 ~ "Joint 3 + 5",
      int == 10 ~ "Joint 4 + 5",
      TRUE ~ int2),
      number = case_when(
        int == 6 ~ 5,
        int == 7 ~ 6,
        int == 8 ~ 7,
        int == 9 ~ 8,
        int == 10 ~ 9,
        TRUE ~ int)) %>% 
    select(number, int2, r, risk_ratio, risk_diff, intervened) %>%  # averinterv) %>% 
    rename(`No` = number,
           `Intervention` = int2,
           `Absolute Risk (%) CI(95%)` = r,
           `Risk Ratio CI(95%)` = risk_ratio,
           `Risk Difference CI(95%)` = risk_diff,
           `Total Intervened (%)` = intervened) %>% #`Aver. Intervened (%)` = averinterv) %>% 
    kable() %>% 
    kable_styling(bootstrap_options = c("hover", "responsive"), full_width = TRUE,
                  font_size = 14) %>% 
    footnote(general = "Results with 500 bootstraps.")
}

