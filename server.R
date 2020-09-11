#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

privacy.text <- "Apologies, we cannot display graphs of data from\nless than five individuals for reasons of data privacy."

confidentiality.check <- function(data, fn, min.rows = 5, ...){
  args <- list(...)
  if(nrow(data) >= min.rows){
    exec(fn, data, !!!args)
  } else {
    ggplot() + annotate(geom = "text", x=0, y=0, label = privacy.text) + theme_void()
  }
}


slider.filters <- function(tbl, input){

  formatted.dates <- parse_date_time(input$admission_date, orders = "my")
  selected.months <- month(formatted.dates)
  selected.years <- year(formatted.dates)
  selected.my <- glue("{selected.months}-{selected.years}")
  

  selected.my <- map_chr(selected.my, function(my){
    ifelse(nchar(my) == 6, glue("0{my}"),my)
  })
  
  out <- tbl %>% 
    as.data.table() %>%
    lazy_dt(immutable = FALSE) %>%
    filter(country %in% input$country) %>%
    filter(outcome %in% input$outcome) %>%
    filter(sex %in% input$sex) %>%
    filter(icu_ever %in% input$icu_ever) %>%
    filter(monthyear %in% selected.my) %>%
    filter(lower.age.bound >= input$agegp10[1] & upper.age.bound <= input$agegp10[2]) %>%
    as.data.table() %>%
    lazy_dt(immutable = FALSE)
  
  return(out)
}

server <- function(input, output) {

  output$agePyramid <- {
    
    age.pyramid.reactive <- reactive({
      
      fd <- age.pyramid.input %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        group_by(agegp10, sex, outcome) %>%
        summarise(count = sum(count)) %>%
        as_tibble()
    })
    renderPlot(confidentiality.check(age.pyramid.reactive(), age.pyramid.plot), height = 300)
  }
  
  output$outcomesByAdmissionDate <- {
 
    outcomes.by.admission.date.reactive <- reactive({
      
      fd <- outcome.admission.date.input %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        group_by(year.epiweek.admit) %>%
        filter(calendar.year.admit == max(calendar.year.admit)) %>%
        filter(calendar.month.admit == max(calendar.month.admit)) %>%
        ungroup() %>%
        as.tibble() %>%
        complete(sex, 
                 nesting(agegp10, lower.age.bound, upper.age.bound), 
                 country, 
                 nesting(calendar.year.admit, calendar.month.admit, year.epiweek.admit), 
                 outcome, 
                 icu_ever, 
                 fill = list(count = 0)) %>%
        arrange(year.epiweek.admit) %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        group_by(sex, outcome, country, agegp10, lower.age.bound, upper.age.bound, icu_ever) %>%
        mutate(cum.count = cumsum(count)) %>% 
        ungroup() %>%
        filter(cum.count > 0) %>%
        mutate(temp.cum.count = cum.count) %>%
        select(-cum.count) %>%
        group_by(year.epiweek.admit, outcome) %>%
        summarise(cum.count = sum(temp.cum.count)) %>%
        as_tibble()
    })
    renderPlot(confidentiality.check(outcomes.by.admission.date.reactive(), outcomes.by.admission.date.plot), height = 300)
  }
  
  output$symptomPrevalence <- {
    
    symptom.prevalence.reactive <- reactive({
      
      fd <- symptom.prevalence.input %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        group_by(nice.symptom) %>%
        summarise(times.present = sum(times.present), times.recorded = sum(times.recorded)) %>% 
        mutate(p.present = times.present/times.recorded) %>%
        mutate(p.absent = 1-p.present) %>%
        as.data.table() %>%
        dt_pivot_longer(c(p.present, p.absent), names_to = "affected", values_to = "proportion") %>%
        lazy_dt(immutable = FALSE) %>%
        mutate(affected = affected == "p.present") %>%
        filter(!is.nan(proportion)) %>%
        as_tibble() %>%
        mutate(label = glue("{times.present} / {times.recorded}")) 
        
    })
    renderPlot(confidentiality.check(symptom.prevalence.reactive(), symptom.prevalence.plot), height = 500)
  }
  
  output$comorbidityPrevalence <- {
    
    comorbidity.prevalence.reactive <- reactive({

      fd <- comorbidity.prevalence.input %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        group_by(nice.comorbidity) %>%
        summarise(times.present = sum(times.present), times.recorded = sum(times.recorded)) %>%
        mutate(p.present = times.present/times.recorded) %>%
        mutate(p.absent = 1-p.present) %>%
        as.data.table() %>%
        dt_pivot_longer(c(p.present, p.absent), names_to = "affected", values_to = "proportion") %>%
        lazy_dt(immutable = FALSE) %>%
        mutate(affected = affected == "p.present") %>%
        filter(!is.nan(proportion)) %>%
        as_tibble() %>%
        mutate(label = glue("{times.present} / {times.recorded}"))
    })
    renderPlot(confidentiality.check(comorbidity.prevalence.reactive(), comorbidity.prevalence.plot), height = 500)
  }
  
  
  output$treatmentPrevalence <- {
    
    treatment.prevalence.reactive <- reactive({
      
      fd <- treatment.use.proportion.input %>%
        slider.filters(input) %>%
        group_by(nice.treatment) %>%
        summarise(times.present = sum(times.present), times.recorded = sum(times.recorded)) %>%
        mutate(p.present = times.present/times.recorded) %>%
        mutate(p.absent = 1-p.present) %>%
        as.data.table() %>%
        dt_pivot_longer(c(p.present, p.absent), names_to = "affected", values_to = "proportion") %>%
        lazy_dt(immutable = FALSE) %>%
        mutate(affected = affected == "p.present") %>%
        filter(!is.nan(proportion)) %>%
        as_tibble() %>%
        mutate(label = glue("{times.present} / {times.recorded}"))
    })
    renderPlot(confidentiality.check(treatment.prevalence.reactive(), treatment.prevalence.plot, icu = FALSE), height = 500)
  }
  
  output$icuTreatmentPrevalence <- {
    
    icu.treatment.prevalence.reactive <- reactive({
      
      fd <- icu.treatment.use.proportion.input %>%
        slider.filters(input) %>%
        group_by(nice.treatment) %>%
        summarise(times.present = sum(times.present), times.recorded = sum(times.recorded)) %>%
        mutate(p.present = times.present/times.recorded) %>%
        mutate(p.absent = 1-p.present) %>%
        as.data.table() %>%
        dt_pivot_longer(c(p.present, p.absent), names_to = "affected", values_to = "proportion") %>%
        lazy_dt(immutable = FALSE) %>%
        mutate(affected = affected == "p.present") %>%
        filter(!is.nan(proportion)) %>%
        as_tibble() %>%
        mutate(label = glue("{times.present} / {times.recorded}"))
    })
    renderPlot(confidentiality.check(icu.treatment.prevalence.reactive(), treatment.prevalence.plot, icu = TRUE), height = 500)
  }
  
}