
# Upload packages ---------------------------------------------------------

library(easypackages)
libraries("dplyr", "forcats", "ggplot2", "purrr", "data.table", "rio", 
          "tidyr","here", "janitor", "stringr", "readr", "glue")

here()

# Functions ---------------------------------------------------------------


# Set_long generic --------------------------------------------------------


set_long <- function(df){
df_long <- df %>% 
  clean_names() %>% 
  mutate(risk0 = 0,
         comprisk0 = 0) %>% 
  select(sample, int2, int, risk0, starts_with("risk"), comprisk0, starts_with("comp")) %>% 
  gather("surv_risk", "prob", risk0:comprisk15, -c(sample, int2, int), na.rm = FALSE, convert = TRUE) %>% 
  mutate(risk = str_extract(surv_risk, "[aA-zZ]+"),
         time = parse_number(surv_risk)) %>% 
  select(-surv_risk) %>% 
  spread(risk, prob, convert = TRUE) %>% 
  mutate(risk = round((risk*100), 2),
         comprisk_surv = round(((1 - comprisk)*100), 2),
         comprisk = round((comprisk*100), 2),
         int2 = case_when(
              int == 1 ~ "Drop under 140",
              int == 2 ~ "Drop under 120",
              TRUE ~ int2),
         int = as.factor(int))
df_long_0 <- df_long %>% filter(sample == 0)
df_long_1 <- df_long %>% filter(sample != 0)
df_long_1%>% 
  group_by(int, int2, time) %>% 
  summarise(risk_min = quantile(risk, 0.025),
            risk_max = quantile(risk, 0.975),
            comprisk_min = quantile(comprisk, 0.025),
            comprisk_max = quantile(comprisk, 0.975)) %>% 
  left_join(df_long_0) %>% 
  select(starts_with("int"), time, starts_with("risk"), starts_with("comprisk"))
}


# set_long for paper ------------------------------------------------------

set_long <- function(df){
  df_long <- df %>% 
    clean_names() %>% 
    mutate(risk0 = 0,
           comprisk0 = 0) %>% 
    select(sample, int2, int, risk0, starts_with("risk"), comprisk0, starts_with("comp")) %>% 
    gather("surv_risk", "prob", risk0:comprisk15, -c(sample, int2, int), na.rm = FALSE, convert = TRUE) %>% 
    mutate(risk = str_extract(surv_risk, "[aA-zZ]+"),
           time = parse_number(surv_risk)) %>% 
    select(-surv_risk) %>% 
    spread(risk, prob, convert = TRUE) %>% 
    mutate(risk = round((risk*100), 2),
           comprisk_surv = round(((1 - comprisk)*100), 2),
           comprisk = round((comprisk*100), 2),
           int = as.factor(int),
           int2 = case_when(
             int == 0 ~ "Natural course",
             int == 1 ~ "Drop under 120",
             int == 2 ~ "Drop under 140",
             int == 3 ~ "Drop 10% if above 140",
             int == 4 ~ "Drop 20% if above 140",
             int == 6 ~ "Current to former smoker",
             int == 7 ~ "Joint 1 + 5",
             int == 8 ~ "Joint 2 + 5",
             int == 9 ~ "Joint 3 + 5",
             int == 10 ~ "Joint 4 + 5",
             TRUE ~ int2),
           int2 == as.factor(int2))
  df_long_0 <- df_long %>% filter(sample == 0)
  df_long_1 <- df_long %>% filter(sample != 0)
  df_long_1%>% 
    group_by(int, int2, time) %>% 
    summarise(risk_min = quantile(risk, 0.025),
              risk_max = quantile(risk, 0.975),
              comprisk_min = quantile(comprisk, 0.025),
              comprisk_max = quantile(comprisk, 0.975)) %>% 
    left_join(df_long_0) %>% 
    select(starts_with("int"), time, starts_with("risk"), starts_with("comprisk")) 
}


set_long_comprisk <- function(df){
  df %>% 
  gather("risk_type", "prob", c(risk, comprisk), -c(int2, int, time), na.rm = FALSE, convert = TRUE) %>% 
  mutate(time = as.numeric(time),
         min = ifelse((risk_type == "risk"), risk_min, comprisk_min),
         max = ifelse((risk_type == "risk"), risk_max, comprisk_max))}

surv_graph <- function(df, a, b, c) {
  outcome <- c
  inter_name <- df[df$int == b & df$time == 1, "int2"]
  df %>% 
    filter(int == a| int == b) %>%
    ggplot() +
    aes(x = time, y = risk, group = int) +
    geom_line(aes(x = time, y = risk, color = int2), size = 1) + 
    scale_color_manual(values=c("#235AA7", "#eb3030")) +
    geom_ribbon(aes(ymin = risk_min, ymax = risk_max ), alpha = 0.10) +
    theme_bw(base_size = 14, base_family = "serif") +
    labs(title = glue("Risk of {outcome} in 15 years of follow up"),
         # subtitle = glue("Curves for the Natural Course and Intervention: {inter_name}"),
         x = "Years of follow up",
         y = "Cumulative Incidence (%)",
         color = "Intervention") +
    theme(legend.position = 'bottom')
}

surv_graph_cr <- function(df, a, b, c){
  outcome <- c
  df %>% 
  filter(int == a | int == b) %>%
    mutate(risk_type = case_when(
      risk_type == "risk" ~ glue("A. Risk of {c}"),
      risk_type == "comprisk" ~ "B. Risk of death")) %>% 
      ggplot() +
  aes(x = time, y = prob, group = int2) +
  geom_line(aes(x = time, y = prob, color = int2), size = 1) +
    scale_color_manual(values=c("#eb3030", "#235AA7")) +
  geom_ribbon(aes(ymin = min, ymax = max), alpha = 0.10) +
  labs(title = glue("Risk of {outcome} and death in 15 years of follow up"),
       x = "Years of follow up",
       y = "Cumulative Incidence (%)") +
  facet_wrap(~risk_type) +
    theme_bw(base_size = 14, base_family = "serif") +
    theme(legend.position = 'bottom') +
    labs(color = "Interventions")
}
save_plot <- function(outcome, a, b) {
  outcome <- outcome
  a <- a
  b <- b
  ggsave(glue("04_graphs/surv/{outcome}{a}{b}.png"), width = 11, height = 6, units = "in")
  
}

# Datasets ----------------------------------------------------------------

df_stroke <- import("V:/Uitwissel/Paloma/HT trial/gformula/stroke/sbp_smoke/survdata_str.sas7bdat")
df_dementia <- import("V:/Uitwissel/Paloma/HT trial/gformula/dementia/sbp_smoke/survdata_dem.sas7bdat")
df_death <- import("V:/Uitwissel/Paloma/HT trial/gformula/death/survdata_death.sas7bdat")
df_strdem <- import("V:/Uitwissel/Paloma/HT trial/gformula/strdem/survdata_strdem.sas7bdat")


# Application ------------------------------------------------------

df_long <- set_long(df_stroke)
df_long_cr <- set_long_comprisk(df_long)

surv_graph(df_long, 0, 10, "stroke")
save_plot("stroke", 0, 10)

surv_graph_cr(df_long_cr, 0, 10, "stroke")
save_plot("stroke_cr", 0, 10)



df_long_dem <- set_long(df_dementia)
df_long_cr_dem <- set_long_comprisk(df_long_dem)

surv_graph(df_long_dem, 0, 10, "dementia")
save_plot("dementia", 0, 10)

surv_graph_cr(df_long_cr_dem, 0, 10, "dementia")
save_plot("dementia_cr", 0, 10)




