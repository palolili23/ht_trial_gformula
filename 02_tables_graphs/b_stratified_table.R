# Function ----------------------------------------------------------------

table_stratified <- function(data, inta, intb){
  stratified <- reduce(data, bind_rows) %>% 
    filter(int == inta | int == intb)
  bracket <- ")"
  stratified %>% 
    unite("riskCI95", c("pd_llim95", "pd_ulim95"), sep = " to ", remove = TRUE) %>% 
    unite("r", c("pd", "riskCI95"), sep = " (", remove = TRUE) %>% 
    mutate(r = glue("{r}{bracket}")) %>% 
    unite("rrCI95", c("RR_llim95", "RR_ulim95"), sep = " to ", remove = TRUE) %>%
    unite("risk_ratio", c("rr", "rrCI95"), sep = " (", remove = TRUE) %>%
    mutate(risk_ratio= glue("{risk_ratio}{bracket}")) %>%
    unite("rdCI95", c("RD_llim95", "RD_ulim95"), sep = " to ", remove = TRUE) %>%
    unite("risk_diff", c("rd", "rdCI95"), sep = " (", remove = TRUE) %>%
    mutate(risk_diff = glue("{risk_diff}{bracket}")) %>%
    mutate(nc = lag(r)) %>% 
    filter(int!= 0) %>% 
    select(subgroup, nc, r, risk_ratio, risk_diff) %>% 
    rename(Subgroup = subgroup,
           `Risk under natural course` = nc,
           `Risk under intervention` = r,
           `Risk Ratio` = risk_ratio,
           `Risk Difference` = risk_diff)
}

# Stroke ------------------------------------------------------------------

complete <- import("V:/Uitwissel/Paloma/HT trial/gformula/stroke/sbp_smoke/results_str.sas7bdat") %>% 
  mutate(subgroup = "Complete cohort")
data65 <- import("V:/Uitwissel/Paloma/HT trial/gformula/stroke/sbp_smoke/results_str65.sas7bdat") %>% 
  mutate(subgroup = "Age below 65")
data80 <- import("V:/Uitwissel/Paloma/HT trial/gformula/stroke/sbp_smoke/results_str80.sas7bdat") %>% 
  mutate(subgroup = "Age between 65 and 80")
dataw <- import("V:/Uitwissel/Paloma/HT trial/gformula/stroke/sbp_smoke/results_strw.sas7bdat") %>% 
  mutate(subgroup = "Women")
datam <- import("V:/Uitwissel/Paloma/HT trial/gformula/stroke/sbp_smoke/results_strm.sas7bdat") %>% 
  mutate(subgroup = "Men")
data_healthy <- import("V:/Uitwissel/Paloma/HT trial/gformula/stroke/sbp_smoke/results_str_healthy.sas7bdat") %>% 
  mutate(subgroup = "Healthy")
data_hd <- import("V:/Uitwissel/Paloma/HT trial/gformula/stroke/sbp_smoke/results_str_hd.sas7bdat") %>% 
  mutate(subgroup = "Free of heart disease")

binded_stroke<- list(complete, data65, data80, dataw, datam, data_healthy, data_hd)

table_stroke<- table_stratified(binded_stroke, 0, 10)


# dementia ----------------------------------------------------------------

complete <- import("V:/Uitwissel/Paloma/HT trial/gformula/dementia/sbp_smoke/results_dem.sas7bdat") %>% 
  mutate(subgroup = "Complete cohort")
data65 <- import("V:/Uitwissel/Paloma/HT trial/gformula/dementia/sbp_smoke/results_dem65.sas7bdat") %>% 
  mutate(subgroup = "Age below 65")
data80 <- import("V:/Uitwissel/Paloma/HT trial/gformula/dementia/sbp_smoke/results_dem80.sas7bdat") %>% 
  mutate(subgroup = "Age between 65 and 80")
dataw <- import("V:/Uitwissel/Paloma/HT trial/gformula/dementia/sbp_smoke/results_demw.sas7bdat") %>% 
  mutate(subgroup = "Women")
datam <- import("V:/Uitwissel/Paloma/HT trial/gformula/dementia/sbp_smoke/results_demm.sas7bdat") %>% 
  mutate(subgroup = "Men")
data_healthy <- import("V:/Uitwissel/Paloma/HT trial/gformula/dementia/sbp_smoke/results_dem_healthy.sas7bdat") %>% 
  mutate(subgroup = "Healthy")
data_hd <- import("V:/Uitwissel/Paloma/HT trial/gformula/dementia/sbp_smoke/results_dem_hd.sas7bdat") %>% 
  mutate(subgroup = "Free of heart disease")

binded_dementia <- list(complete, data65, data80, dataw, datam, data_healthy, data_hd)

table_dementia <- table_stratified(binded_dementia, 0, 10)

table_stroke%>% 
   kable() %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), font_size = 10)

table_dementia%>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), font_size = 10)
# 
# 
# table_merged <- stroke %>% 
#   left_join(dementia, by = "Subgroup")
# 
# table_merged %>% 
#   kable() %>% 
#   kable_styling(bootstrap_options = c("striped", "hover", "condensed"), font_size = 10)
