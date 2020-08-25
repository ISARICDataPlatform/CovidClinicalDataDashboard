library(devtools)
#
# install_github("ISARICDataPlatform/CovidClinicalDataProcessor")
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


example.data <- fread("example_data.csv") %>%
  lazy_dt(immutable = FALSE) %>%
  mutate(outcome.3 = map2_chr(outcome, date_outcome, outcome.remap)) %>%
  select(-outcome) %>%
  rename(outcome = outcome.3) %>%
  mutate(agegp5 = cut(age, right = FALSE, breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 120))) %>%
  mutate(year.admit = map_dbl(date_admit, epiweek.year)) %>%
  mutate(epiweek.admit = epiweek(date_admit)) %>%
  mutate(year.epiweek.admit = glue("{year.admit}-{epiweek.admit}", .envir = .SD)) %>%
  mutate(year.epiweek.admit = replace(year.epiweek.admit, year.epiweek.admit == "NA-NA", NA)) %>%
  mutate(lower.age.bound  = map_dbl(agegp5, extract.age.boundaries, TRUE)) %>%
  mutate(upper.age.bound  = map_dbl(agegp5, extract.age.boundaries, FALSE)) %>%
  mutate(agegp5t = fct_relabel(agegp5, prettify.age.labels)) %>%
  select(-agegp5) %>%
  rename(agegp5 = agegp5t) %>%
  as_tibble()


age.bound.lookup <- tibble(agegp5 = cut(example.data$age, right = FALSE, breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 120)) %>% unique()) %>%
  mutate(lower.age.bound  = map_dbl(agegp5, extract.age.boundaries, TRUE)) %>%
  mutate(upper.age.bound  = map_dbl(agegp5, extract.age.boundaries, FALSE)) %>%
  mutate(agegp5t = fct_relabel(agegp5, prettify.age.labels)) %>%
  select(agegp5t, lower.age.bound, upper.age.bound) %>%
  rename(agegp5 = agegp5t)


epiweek.order <- glue("{c(rep(2019,4), rep(2020,max(example.data$epiweek.admit[which(example.data$year.admit == 2020)], na.rm = T)))}-{c(49:52, 1:max(example.data$epiweek.admit[which(example.data$year.admit == 2020)], na.rm = T))}")

example.data <- example.data %>%
  mutate(year.epiweek.admit = factor(year.epiweek.admit, levels = epiweek.order))

source("/Users/mdhall/CovidClinicalDataDashboard/plot_functions.R")


outcome.remap <- function(oc, od){
  if(is.na(od) & is.na(oc)){
    "censored"
  } else {
    out <- case_when(is.na(oc) ~ NA_character_,
                     oc == "Death" ~ "death",
                     oc == "Discharged Alive" ~ "discharge")
  }
}

age.pyramid.input <- example.data %>%
  lazy_dt(immutable = TRUE) %>%
  select(sex, agegp5, country, year.epiweek.admit, outcome, lower.age.bound, upper.age.bound) %>%
  group_by(sex, outcome, country, year.epiweek.admit, agegp5, lower.age.bound, upper.age.bound) %>%
  summarise(count = n()) %>%
  as_tibble()

outcome.admission.date.input <- example.data %>%
  lazy_dt(immutable = TRUE) %>%
  filter(!is.na(year.epiweek.admit)) %>%
  select(sex, agegp5, country, year.epiweek.admit, outcome) %>%
  group_by(sex, outcome, country, year.epiweek.admit, agegp5) %>%
  summarise(count = n()) %>%
  as_tibble() %>%
  complete(sex, agegp5, country, year.epiweek.admit, outcome, fill = list(count = 0)) %>%
  arrange(year.epiweek.admit) %>%
  group_by(sex, outcome, country, agegp5) %>%
  mutate(cum.count = cumsum(count)) %>%
  left_join(age.bound.lookup, by="agegp5")

  
  

countries <- age.pyramid.input %>% pull(country) %>% unique %>% sort

