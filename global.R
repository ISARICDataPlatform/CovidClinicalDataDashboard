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

age.pyramid.input <- tibble(sex = sample(c("Male", "Male", "Female"), 100000, replace = TRUE),
                            outcome = sample(c(rep("discharge", 6), rep("death",2), rep("censored", 3)), 100000, replace = TRUE),
                            country = sample(sample(ISO_3166_1$Name, 10, replace = FALSE), 100000, replace = TRUE),
                            year.epiweek.admit = rep("2020-12", 100000),
                            age = floor(rbeta(100000, 5, 2)*100)) %>%
  lazy_dt(immutable = TRUE) %>%
  mutate(agegp5 = cut(age, right = FALSE, breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 120))) %>%
  # mutate(year.admit = year(date_admit)) %>%
  # mutate(epiweek.admit = epiweek(date_admit)) %>%
  # mutate(year.epiweek.admit = glue("{year.admit}-{epiweek.admit}", .envir = .SD)) %>%
  # mutate(year.epiweek.admit = replace(year.epiweek.admit, year.epiweek.admit == "NA-NA", NA)) %>%
  select(sex, agegp5, country, year.epiweek.admit, outcome) %>%
  group_by(sex, outcome, country, year.epiweek.admit, agegp5) %>%
  summarise(count =n()) %>%
  mutate(lower.ag.bound  = map_dbl(agegp5, extract.age.boundaries, TRUE)) %>%
  mutate(upper.ag.bound  = map_dbl(agegp5, extract.age.boundaries, FALSE)) %>%
  mutate(agegp5t = fct_relabel(agegp5, prettify.age.labels)) %>%
  select(-agegp5) %>%
  rename(agegp5 = agegp5t) %>%
  as_tibble()


countries <- age.pyramid.input %>% pull(country) %>% unique %>% sort

age.pyramid.plot <- function(aggregated.tbl, ...){

  # print(nrow(aggregated.tbl))

  max.count = aggregated.tbl %>% group_by(agegp5, sex) %>% summarise(sac = sum(abs(count))) %>% pull(sac) %>% max()

  order.of.magnitude <- ceiling(log10(max.count))

  if(as.numeric(substr(as.character(max.count), 1, 1)) > 5){
    tick.increment <- 10^(order.of.magnitude-1)
  } else {
    tick.increment <- 10^(order.of.magnitude-1)/2
  }

  plot.breaks <- seq(-(ceiling(max.count/tick.increment)*tick.increment), ceiling(max.count/tick.increment)*tick.increment, by = tick.increment)
  plot.labels <- as.character(c(rev(seq(tick.increment, ceiling(max.count/tick.increment)*tick.increment, by = tick.increment)),
                                0,
                                seq(tick.increment, ceiling(max.count/tick.increment)*tick.increment, by= tick.increment)))

  plt <- ggplot() +
    geom_bar(data = (aggregated.tbl %>% filter(sex == "Male")), aes(x=agegp5, y=-count, fill = outcome), stat = "identity") +
    geom_bar(data = aggregated.tbl %>% filter(sex == "Female"), aes(x=agegp5, y=count, fill = outcome),  stat = "identity") +
    coord_flip(clip = 'off') +
    theme_bw() +
    scale_fill_brewer(palette = 'Set2', name = "Outcome", drop="F") +
    xlab("Age group") +
    ylab("Number of patients") +
    scale_x_discrete(drop = "F") +
    scale_y_continuous(
      # currently in hard-coded increments of 5. @todo make this better
      breaks = plot.breaks,
      labels = plot.labels,
      limits = c(-1.1*max.count, 1.1*max.count)) +
    annotation_custom(
      grob = textGrob(label = "Males", hjust = 0.5, gp = gpar(cex = 1.5)),
      ymin = -max.count/2,
      ymax = -max.count/2,
      xmin = length(levels(aggregated.tbl$agegp5))+1.5 ,
      xmax = length(levels(aggregated.tbl$agegp5))+1.5) +
    annotation_custom(
      grob = textGrob(label = "Females", hjust = 0.4, gp = gpar(cex = 1.5)),
      ymin = max.count/2,
      ymax = max.count/2,
      xmin = length(levels(aggregated.tbl$agegp5))+1.5,
      xmax = length(levels(aggregated.tbl$agegp5))+1.5) +
    theme(plot.margin=unit(c(30,5,5,5.5,5.5),"pt"),
          axis.text.x=element_text(angle = -90, vjust = 0.5))

  plt

}