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
library(ggmap)
library(leaflet)
library(mapview) 
library(collaborator)
library(knitr)
library(viridis)  #using scale_fill_viridis

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

base::load("age_pyramid_input.rda")  
     
base::load("outcome_admission_date_input.rda")

base::load("symptom_prevalence_input.rda")

rev.symptom.order <- symptom.prevalence.input %>% pull(nice.symptom) %>% unique() %>% sort(decreasing = TRUE)

symptom.prevalence.input <- symptom.prevalence.input %>%
  mutate(nice.symptom = factor(nice.symptom, levels = rev.symptom.order)) 

base::load("comorbidity_prevalence_input.rda")


rev.comorbidity.order <- comorbidity.prevalence.input %>% pull(nice.comorbidity) %>% unique() %>% sort(decreasing = TRUE)

comorbidity.prevalence.input <- comorbidity.prevalence.input %>%
  mutate(nice.comorbidity = factor(nice.comorbidity, levels = rev.comorbidity.order))

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
month.options <- c("Dec 2019", glue("{months} {2020}"))



map.data <- read_rds(here::here("map_data.rds"))

knit("markdown/Contributor_listmap.Rmd", output = "markdown/Contributor_listmap.md")

#################load data for hospital stays
base::load("length_of_stay_sex_input.rda")
base::load("length_of_stay_age_input.rda")
base::load("admission_to_icu_input.rda")
base::load("status_by_time_after_admission_input.rda")
