# Open libraries ----------------------------------------------------------
library(easypackages)
libraries("dplyr", "forcats", "ggplot2", "rio", "tidyr",
          "here", "janitor", "stringr", "readr", "glue")

here()

# Functions ---------------------------------------------------------------
data_setup <- function(df) {
  df %>% 
  clean_names() %>% 
  select(ends_with("diff"), ends_with("lb"), ends_with("ub")) %>% 
  mutate(time = row_number()) %>% 
  mutate_all(round, 3) %>% 
  gather(variable, value, -time, convert = FALSE) %>% 
  separate(variable, c("variable","stat"), sep = "_(?![^_]*_)") %>% 
  spread(stat, value) 
}
plot_pred_obs <- function(data, outcome){
  outcome <- outcome
  data %>% 
  ggplot(aes(x = time, y = diff)) +
  geom_point() +
  geom_errorbar(aes(ymin = lb, ymax = ub)) +
  geom_abline(intercept = 0, slope = 0, colour="#eb3030") +
  facet_wrap(.~variable) +
  theme_bw(base_family = "serif") +
  labs(
    title = "Difference between mean predicted values and mean observed values",
    subtitle = glue("Outcome: {outcome}"),
    x = "Years",
    y = "Difference"
  )
}
save_plot <- function(outcome) {
  outcome <- outcome
  ggsave(glue("04_graphs/pred_obs/{outcome}.png"), width = 7, height = 6, units = "in")
  }



# Apply functions ---------------------------------------------------------

df <- import("V:/Uitwissel/Paloma/HT trial/gformula/stroke/sbp_smoke/covmean_str.sas7bdat")
data <- data_setup(df)

### Change variable names, not part of the function since it would not be reproducible in other datasets
data <- data %>% 
  mutate(variable = case_when(
    variable == "bmi" ~ "BMI",
    variable == "cancer_v" ~ "Cancer diagnosis",
    variable == "chol_cat" ~ "Cholesterol",
    variable == "dem_v" ~ "Dementia diagnosis",
    variable == "diab_v" ~ "Diabetes diagnosis",
    variable == "hd_v" ~ "Heart disease diagnosis",
    variable == "ht_drug" ~ "Hipertension medication",
    variable == "oh_cat" ~ "Alcohol intake",
    variable == "sbp" ~ "Systolic blood pressure",
    variable == "smoke_cig2" ~ "Cigarrete smoking",
    variable == "v2_bin" ~ "Visit 2",
    variable == "v3_bin" ~ "Visit 3",
    variable == "v4_bin" ~ "Visit 4"
  ))

plot_pred_obs(data, "stroke")
save_plot("stroke")


# dementia ----------------------------------------------------------------


df <- import("V:/Uitwissel/Paloma/HT trial/gformula/dementia/sbp_smoke/covmean_dem.sas7bdat")
data <- data_setup(df)

### Change variable names, not part of the function since it would not be reproducible in other datasets
data <- data %>% 
  mutate(variable = case_when(
    variable == "bmi" ~ "BMI",
    variable == "cancer_v" ~ "Cancer diagnosis",
    variable == "chol_cat" ~ "Cholesterol",
    variable == "stroke_v" ~ "Stroke diagnosis",
    variable == "diab_v" ~ "Diabetes diagnosis",
    variable == "hd_v" ~ "Heart disease diagnosis",
    variable == "ht_drug" ~ "Hipertension medication",
    variable == "oh_cat" ~ "Alcohol intake",
    variable == "sbp" ~ "Systolic blood pressure",
    variable == "smoke_cig2" ~ "Cigarrete smoking",
    variable == "v2_bin" ~ "Visit 2",
    variable == "v3_bin" ~ "Visit 3",
    variable == "v4_bin" ~ "Visit 4"
  ))

plot_pred_obs(data, "dementia")
save_plot("dementia")
