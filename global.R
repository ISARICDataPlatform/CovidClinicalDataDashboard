library(devtools)
library(renv)
# if this keeps failing, try turning off caching in shinyapps (the web interface). And then deploy _twice_. The first time the browser will open with an error message. That's expected.
# Then comment out the following line for the second deployment. It's mad, but there you go.
# devtools::install_github("ISARICDataPlatform/CovidClinicalDataProcessor")
# devtools::install_github("kamclean/collaborator")
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
# library(collaborator)
library(knitr)
library(viridis)  #using scale_fill_viridis
library(scales)

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


#################load data for hospital stays
base::load("length_of_stay_sex_input.rda")
base::load("length_of_stay_age_input.rda")
base::load("admission_to_icu_input.rda")
base::load("status_by_time_after_admission_input.rda")
base::load("length_of_stay_icu_input.rda")

report_auth <- function(df, name, group = NULL, subdivision = NULL, path = NULL,
                        name_sep = ", ", group_brachet = "()",group_sep = "; "){
  
  require(stringr);require(readr);require(dplyr)
  
  
  if(is.null(group)==FALSE&length(group)>1){group <- head(group,1)
  print("More than 1 group supplied - only first value used as group")}
  
  if(is.null(subdivision)==FALSE&length(subdivision)>1){subdivision <- head(subdivision,1)
  print("More than 1 subdivision supplied - only first value used as subdivision")}
  
  group_brachet_L = stringr::str_sub(group_brachet, 1, 1)
  group_brachet_R = stringr::str_sub(group_brachet, 2, 2)
  
  df <- df %>% dplyr::mutate(name = dplyr::pull(., name))
  
  
  # No groups / subdivisions
  if(is.null(group)==TRUE&is.null(subdivision)==TRUE){
    output <- df %>%
      dplyr::summarise(auth_out = paste(name, collapse=name_sep) %>% paste0("."))
    
    if(is.null(path)==F){readr::write_file(output$auth_out, path=path)}}
  
  # Just groups
  if(is.null(group)==FALSE&is.null(subdivision)==TRUE){
    output <- df %>%
      dplyr::mutate(group = dplyr::pull(., group)) %>%
      
      dplyr::group_by(group) %>%
      
      dplyr::summarise(name_list = paste(name, collapse=name_sep)) %>%
      
      dplyr::mutate(name_group = paste0(name_list, " ",group_brachet_L, group, group_brachet_R)) %>%
      
      dplyr::summarise(auth_out = paste(name_group, collapse = group_sep) %>% paste0("."))
    
    if(is.null(path)==F){readr::write_file(output$auth_out, path=path)}}
  
  # Just subdivisions
  if(is.null(group)==TRUE&is.null(subdivision)==FALSE){
    output <- df %>%
      dplyr::mutate(subdivision = dplyr::pull(., subdivision)) %>%
      
      dplyr::group_by(subdivision) %>%
      
      dplyr::summarise(name_list = paste(name, collapse=name_sep) %>% paste0(".")) %>%
      
      dplyr::mutate(auth_out = paste0(subdivision, ": ", name_list)) %>%
      
      dplyr::select(auth_out)
    
    if(is.null(path)==F){readr::write_file(output$auth_out, path=path)}}
  
  
  # Groups and subdivisions
  if(is.null(group)==FALSE&is.null(subdivision)==FALSE){
    output <- df %>%
      dplyr::mutate(group = dplyr::pull(., group),
                    subdivision = dplyr::pull(., subdivision)) %>%
      
      dplyr::select(subdivision, group, name) %>%
      
      dplyr::group_by(subdivision, group) %>%
      dplyr::summarise(name_list = paste(name, collapse=name_sep)) %>%
      
      # add group characteristics
      dplyr::mutate(name_group = paste0(name_list, " ",group_brachet_L, group, group_brachet_R)) %>%
      
      # combine groups (by subdivision)
      dplyr::summarise(auth_out = paste(name_group, collapse = group_sep) %>% paste0(".")) %>%
      
      dplyr::mutate(auth_out = paste0(subdivision, ": ", auth_out)) %>%
      
      dplyr::select(auth_out)
    
    if(is.null(path)==F){readr::write_file(output$auth_out, path=path)}}
  
  
  return(gsub("\n\n", " ", output$auth_out))}

knit("markdown/Contributor_listmap.Rmd", output = "markdown/Contributor_listmap.md")


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

#load patient by country data
base::load("patient_by_country_input.rda")



















