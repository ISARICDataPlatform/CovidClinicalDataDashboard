library(devtools)

# install_github("ISARICDataPlatform/CovidClinicalDataProcessor", force = TRUE)
library(CovidClinicalDataProcessor)
library(shinydashboard)
library(shinyWidgets)
library(ISOcodes)
library(tidyverse)
library(dtplyr)
library(grid)
library(lubridate)
library(glue)
library(data.table)


epiweek.year <- function(date){
  if(is.na(date)){
    return(NA)
  }
  if(year(date)==2019 & date > ymd("2019-12-28")){
    2020
  } else {
    year(date)
  }
}


outcome.remap <- function(oc, od){
  if(is.na(od) & is.na(oc)){
    "censored"
  } else {
    out <- case_when(is.na(oc) ~ NA_character_,
                     oc == "Death" ~ "death",
                     oc == "Discharged Alive" ~ "discharge")
  }
}

source("plot_functions.R")
example.data <- fread("example_data.csv") %>%
  lazy_dt(immutable = FALSE) %>%
  mutate(outcome.3 = map2_chr(outcome, date_outcome, outcome.remap)) %>%
  select(-outcome) %>%
  rename(outcome = outcome.3) %>%
  mutate(agegp10 = cut(age, right = FALSE, breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 120))) %>%
  mutate(year.admit = map_dbl(date_admit, epiweek.year)) %>%
  mutate(epiweek.admit = epiweek(date_admit)) %>%
  mutate(year.epiweek.admit = glue("{year.admit}-{epiweek.admit}", .envir = .SD)) %>%
  mutate(year.epiweek.admit = replace(year.epiweek.admit, year.epiweek.admit == "NA-NA", NA)) %>%
  mutate(lower.age.bound  = map_dbl(agegp10, extract.age.boundaries, TRUE)) %>%
  mutate(upper.age.bound  = map_dbl(agegp10, extract.age.boundaries, FALSE)) %>%
  mutate(agegp10t = fct_relabel(agegp10, prettify.age.labels)) %>%
  select(-agegp10) %>%
  rename(agegp10 = agegp10t) %>%
  as_tibble()


age.bound.lookup <- tibble(agegp10 = cut(1:100, right = FALSE, breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 120)) %>% unique()) %>%
  mutate(lower.age.bound  = map_dbl(agegp10, extract.age.boundaries, TRUE)) %>%
  mutate(upper.age.bound  = map_dbl(agegp10, extract.age.boundaries, FALSE)) %>%
  mutate(agegp10t = fct_relabel(agegp10, prettify.age.labels)) %>%
  select(lower.age.bound, upper.age.bound, agegp10t) %>%
  rename(agegp10 = agegp10t)

age.levels <- age.bound.lookup %>% pull(agegp10) %>% levels


epiweek.order <- glue("{c(rep(2019,4), rep(2020,max(example.data$epiweek.admit[which(example.data$year.admit == 2020)], na.rm = T)))}-{c(49:52, 1:max(example.data$epiweek.admit[which(example.data$year.admit == 2020)], na.rm = T))}")

# example.data <- example.data %>%
#   mutate(year.epiweek.admit = factor(year.epiweek.admit, levels = epiweek.order))

# age.pyramid.input <- example.data %>%
#   lazy_dt(immutable = TRUE) %>%
#   select(sex, agegp10, country, year.epiweek.admit, outcome, lower.age.bound, upper.age.bound) %>%
#   group_by(sex, outcome, country, year.epiweek.admit, agegp10, lower.age.bound, upper.age.bound) %>%
#   summarise(count = n()) %>%
#   as_tibble()
# 
# write_csv(age.pyramid.input, "age_pyramid_input.csv")

age.pyramid.input <- fread("age_pyramid_input.csv")  %>%
  lazy_dt(immutable = FALSE) %>%
  mutate(agegp10 = factor(agegp10, levels = age.levels)) %>%
  as_tibble()



# outcome.admission.date.input <- example.data %>%
#   lazy_dt(immutable = TRUE) %>%
#   filter(!is.na(year.epiweek.admit) & !is.na(outcome)) %>%
#   select(sex, agegp10, country, year.epiweek.admit, outcome) %>%
#   group_by(sex, outcome, country, year.epiweek.admit, agegp10) %>%
#   summarise(count = n()) %>%
#   as_tibble() %>%
#   complete(sex, agegp10, country, year.epiweek.admit, outcome, fill = list(count = 0)) %>%
#   arrange(year.epiweek.admit) %>%
#   group_by(sex, outcome, country, agegp10) %>%
#   mutate(cum.count = cumsum(count)) %>%
#   left_join(age.bound.lookup, by="agegp10")
# 
# write_csv(outcome.admission.date.input, "outcome_admission_date_input.csv")
outcome.admission.date.input <- fread("outcome_admission_date_input.csv") %>%
  lazy_dt(immutable = FALSE) %>%
  mutate(year.epiweek.admit = factor(year.epiweek.admit, levels = epiweek.order)) %>%
  mutate(agegp10 = factor(agegp10, levels = age.levels)) %>%
  as_tibble()

# symptom.prevalence.input <- example.data %>%
#   select(sex, agegp10, country, year.epiweek.admit, outcome, any_of(starts_with("symptoms")), lower.age.bound, upper.age.bound) %>%
#   select(-`symptoms_covid-19_symptoms`) %>%
#   pivot_longer(any_of(starts_with("symptoms")), names_to = "symptom", values_to = "present") %>%
#   group_by(sex, agegp10, country,year.epiweek.admit,outcome, symptom, lower.age.bound, upper.age.bound) %>%
#   summarise(times.present = sum(present, na.rm = TRUE), times.recorded = sum(!is.na(present)))
# 
# nice.symptom.mapper <- tibble(symptom = unique(symptom.prevalence.input$symptom)) %>%
#   mutate(nice.symptom = map_chr(symptom, function(st){
#     temp <- substr(st, 10, nchar(st)) %>% str_replace_all("_", " ")
#     temp2 <- glue("{toupper(substr(temp, 1, 1))}{substr(temp, 2, nchar(temp))}")
#     temp2
#   })) %>%
#   mutate(nice.symptom = case_when(nice.symptom=="Altered consciousness confusion" ~ "Altered consciousness/confusion",
#                                   nice.symptom=="Cough" ~ "Cough (no sputum)",
#                                   nice.symptom=="Cough bloody sputum haemoptysis" ~ "Cough with bloody sputum/haemoptysis",
#                                   nice.symptom=="Fatigue malaise" ~ "Fatigue/malaise",
#                                   TRUE ~ nice.symptom))
# 
# symptom.prevalence.input <- symptom.prevalence.input %>%
#   left_join(nice.symptom.mapper) %>%
#   left_join(age.bound.lookup, by="agegp10")
# 
# write_csv(symptom.prevalence.input, "symptom_prevalence_input.csv")
symptom.prevalence.input <- fread("symptom_prevalence_input.csv")

rev.symptom.order <- symptom.prevalence.input %>% pull(nice.symptom) %>% unique() %>% sort(decreasing = TRUE)

symptom.prevalence.input <- symptom.prevalence.input %>%
  lazy_dt(immutable = FALSE) %>%
  mutate(nice.symptom = factor(nice.symptom, levels = rev.symptom.order)) %>%
  mutate(agegp10 = factor(agegp10, levels = age.levels)) %>%
  as_tibble()



# comorbidity.prevalence.input <- example.data %>%
#   select(sex, agegp10, country, year.epiweek.admit, outcome, any_of(starts_with("comorb")), lower.age.bound, upper.age.bound) %>%
#   pivot_longer(any_of(starts_with("comorb")), names_to = "comorbidity", values_to = "present") %>%
#   group_by(sex, agegp10, country,year.epiweek.admit,outcome, comorbidity, lower.age.bound, upper.age.bound) %>%
#   summarise(times.present = sum(present, na.rm = TRUE), times.recorded = sum(!is.na(present)))
# 
# nice.comorbidity.mapper <- tibble(comorbidity = unique(comorbidity.prevalence.input$comorbidity)) %>%
#   mutate(nice.comorbidity = map_chr(comorbidity, function(st){
#     temp <- substr(st, 10, nchar(st)) %>% str_replace_all("_", " ")
#     temp2 <- glue("{toupper(substr(temp, 1, 1))}{substr(temp, 2, nchar(temp))}")
#     temp2
#   })) %>%
#   mutate(nice.comorbidity = case_when(comorbidity=="Aids hiv" ~ "HIV/AIDS",
#                                   TRUE ~ nice.comorbidity))
# 
# comorbidity.prevalence.input <- comorbidity.prevalence.input %>%
#   left_join(nice.comorbidity.mapper) %>%
#   left_join(age.bound.lookup, by="agegp10")
# 
# 
# write_csv(comorbidity.prevalence.input, "comorbidity_prevalence_input.csv")

comorbidity.prevalence.input <- fread("comorbidity_prevalence_input.csv")


rev.comorbidity.order <- comorbidity.prevalence.input %>% pull(nice.comorbidity) %>% unique() %>% sort(decreasing = TRUE)

comorbidity.prevalence.input <- comorbidity.prevalence.input %>%
  lazy_dt(immutable = FALSE) %>%
  mutate(nice.comorbidity = factor(nice.comorbidity, levels = rev.comorbidity.order)) %>%
  mutate(agegp10 = factor(agegp10, levels = age.levels)) %>%
  as_tibble()



# treatment.prevalence.input <- example.data %>%
#   select(sex, agegp10, country, year.epiweek.admit, outcome, any_of(starts_with("treat")), lower.age.bound, upper.age.bound) %>%
#   pivot_longer(any_of(starts_with("treat")), names_to = "treatment", values_to = "present") %>%
#   group_by(sex, agegp10, country,year.epiweek.admit,outcome, treatment, lower.age.bound, upper.age.bound) %>%
#   summarise(times.present = sum(present, na.rm = TRUE), times.recorded = sum(!is.na(present)))
# 
# nice.treatment.mapper <- tibble(treatment = unique(treatment.prevalence.input$treatment)) %>%
#   mutate(nice.treatment = map_chr(treatment, function(st){
#     temp <- substr(st, 7, nchar(st)) %>% str_replace_all("_", " ")
#     temp2 <- glue("{toupper(substr(temp, 1, 1))}{substr(temp, 2, nchar(temp))}")
#     temp2
#   }))
# 
# treatment.prevalence.input <- treatment.prevalence.input %>%
#   left_join(nice.treatment.mapper) %>%
#   left_join(age.bound.lookup, by="agegp10")
# 
# write_csv(treatment.prevalence.input, "treatment_prevalence_input.csv")
treatment.use.proportion.input <- fread("treatment_use_proportion_input.csv")


rev.treatment.order <- treatment.use.proportion.input %>% pull(nice.treatment) %>% unique() %>% sort(decreasing = TRUE)

treatment.use.proportion.input <- treatment.use.proportion.input %>%
  lazy_dt(immutable = FALSE) %>%
  mutate(nice.treatment = factor(nice.treatment, levels = rev.treatment.order)) %>%
  mutate(agegp10 = factor(agegp10, levels = age.levels)) %>%
  as_tibble()

countries <- age.pyramid.input %>% pull(country) %>% unique %>% sort



icu.treatment.use.proportion.input <- fread("icu_treatment_use_proportion_input.csv")


rev.treatment.order <- icu.treatment.use.proportion.input %>% pull(nice.treatment) %>% unique() %>% sort(decreasing = TRUE)

icu.treatment.use.proportion.input <- icu.treatment.use.proportion.input %>%
  lazy_dt(immutable = FALSE) %>%
  mutate(nice.treatment = factor(nice.treatment, levels = rev.treatment.order)) %>%
  mutate(agegp10 = factor(agegp10, levels = age.levels)) %>%
  as_tibble()

countries <- age.pyramid.input %>% pull(country) %>% unique %>% sort

current.year <- year(today())
current.month <- month(today())
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
month.options <- c("Dec 2019", glue("{months} {2020}") )



