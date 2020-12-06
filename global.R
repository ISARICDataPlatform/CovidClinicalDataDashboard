library(devtools)
# library(renv)
# if this keeps failing, try turning off caching in shinyapps (the web interface). And then deploy _twice_. The first time the browser will open with an error message. That's expected.
# Then comment out the following line for the second deployment. It's mad, but there you go.
# devtools::install_github("ISARICDataPlatform/CovidClinicalDataProcessor")
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
library(tidyfast)
library(ggupset)
library(binom)


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

age.bound.lookup <- tibble(slider_agegp10 = cut(1:100, right = FALSE, breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 120)) %>% unique()) %>%
  mutate(lower.age.bound  = map_dbl(slider_agegp10, extract.age.boundaries, TRUE)) %>%
  mutate(upper.age.bound  = map_dbl(slider_agegp10, extract.age.boundaries, FALSE)) %>%
  mutate(slider_agegp10t = fct_relabel(slider_agegp10, prettify.age.labels)) %>%
  select(lower.age.bound, upper.age.bound, slider_agegp10t) %>%
  rename(slider_agegp10 = slider_agegp10t)

age.levels <- age.bound.lookup %>% pull(slider_agegp10) %>% levels

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

base::load("age_pyramid_input.rda")  
     



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
base::load("outcome_admission_date_input.rda")

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
base::load("symptom_prevalence_input.rda")

rev.symptom.order <- symptom.prevalence.input %>% pull(nice.symptom) %>% unique() %>% sort(decreasing = TRUE)

symptom.prevalence.input <- symptom.prevalence.input %>%
  mutate(nice.symptom = factor(nice.symptom, levels = rev.symptom.order)) 




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

base::load("comorbidity_prevalence_input.rda")


rev.comorbidity.order <- comorbidity.prevalence.input %>% pull(nice.comorbidity) %>% unique() %>% sort(decreasing = TRUE)

comorbidity.prevalence.input <- comorbidity.prevalence.input %>%
  mutate(nice.comorbidity = factor(nice.comorbidity, levels = rev.comorbidity.order))



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
base::load("treatment_use_proportion_input.rda")


rev.treatment.order <- treatment.use.proportion.input %>% pull(nice.treatment) %>% unique() %>% sort(decreasing = TRUE)

treatment.use.proportion.input <- treatment.use.proportion.input %>%
  mutate(nice.treatment = factor(nice.treatment, levels = rev.treatment.order))

countries <- age.pyramid.input %>% pull(slider_country) %>% unique %>% sort



base::load("icu_treatment_use_proportion_input.rda")


rev.treatment.order <- icu.treatment.use.proportion.input %>% pull(nice.treatment) %>% unique() %>% sort(decreasing = TRUE)

icu.treatment.use.proportion.input <- icu.treatment.use.proportion.input %>%
  mutate(nice.treatment = factor(nice.treatment, levels = rev.treatment.order))

base::load("comorbidity_upset_input.rda")
base::load("symptom_upset_input.rda")
base::load("treatment_upset_input.rda")
base::load("icu_treatment_upset_input.rda")


countries <- age.pyramid.input %>% pull(slider_country) %>% unique %>% sort

current.year <- year(today())
current.month <- month(today())
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
month.options <- c("Dec 2019", glue("{months} {2020}") )



#Load the vs data
base::load("data_plot_vs_resp.rda")
base::load("data_plot_vs_hr.rda")
base::load("data_plot_vs_temp.rda")
base::load("data_plot_vs_sysbp.rda")
base::load("data_plot_vs_oxysat.rda")
#Load the laboratory data
base::load("data_plot_lab_crp.rda")
base::load("data_plot_lab_lym.rda")
base::load("data_plot_lab_neut.rda")
base::load("data_plot_lab_wbc.rda")
base::load("data_plot_lab_urean.rda")
base::load("data_plot_lab_pt.rda")
base::load("data_plot_lab_alt.rda")
base::load("data_plot_lab_aptt.rda")
base::load("data_plot_lab_ast.rda")
base::load("data_plot_lab_bili.rda")
#Load the comorbidty by age plots
base::load("data_plot_comorbid_asthma.rda")
base::load("data_plot_comorbid_malignant_neoplasm.rda")
base::load("data_plot_comorbid_obesity.rda")
base::load("data_plot_comorbid_diabetes.rda")
base::load("data_plot_comorbid_dementia.rda")
base::load("data_plot_comorbid_smoking.rda")
base::load("data_plot_comorbid_hypertension.rda")
#Load the symptoms by age plots
base::load("data_plot_symptoms_history_of_fever.rda")
base::load("data_plot_symptoms_cough.rda")
base::load("data_plot_symptoms_cough_fever.rda")
base::load("data_plot_symptoms_shortness_of_breath.rda")
base::load("data_plot_symptoms_cought_fever_shortness_of_breath.rda")
base::load("data_plot_symptoms_upper_respiratory_tract_symptoms.rda")
base::load("data_plot_symptoms_altered_consciousness_confusion.rda")
base::load("data_plot_symptoms_constitutional.rda")
base::load("data_plot_symptoms_vomiting_nausea.rda")
base::load("data_plot_symptoms_diarrhoea.rda")
base::load("data_plot_symptoms_abdominal_pain.rda")
#Load the heatmap data
base::load("data_plot_heatmap.rda")


















