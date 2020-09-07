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

server <- function(input, output) {

  output$agePyramid <- {
    
    age.pyramid.reactive <- reactive({
      fd <- age.pyramid.input %>%
        filter(country %in% input$country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(lower.age.bound >= input$agegp10[1] & upper.age.bound <= input$agegp10[2]) %>%
        group_by(agegp10, sex, outcome) %>%
        summarise(count = sum(count))
    })
    renderPlot(confidentiality.check(age.pyramid.reactive(), age.pyramid.plot), height = 300)
  }
  
  output$outcomesByAdmissionDate <- {
    
    outcomes.by.admission.date.reactive <- reactive({
      fd <- outcome.admission.date.input %>%
        filter(country %in% input$country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(lower.age.bound >= input$agegp10[1] & upper.age.bound <= input$agegp10[2]) %>%
        group_by(year.epiweek.admit) %>%
        filter(calendar.year.admit == max(calendar.year.admit)) %>%
        filter(calendar.month.admit == max(calendar.month.admit)) %>%
        ungroup() %>%
        group_by(year.epiweek.admit, cum.count, outcome) %>%
        summarise(cum.count = sum(cum.count))
    })
    renderPlot(confidentiality.check(outcomes.by.admission.date.reactive(), outcomes.by.admission.date.plot), height = 300)
  }
  
  output$symptomPrevalence <- {
    
    symptom.prevalence.reactive <- reactive({
      fd <- symptom.prevalence.input %>%
        filter(country %in% input$country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(lower.age.bound >= input$agegp10[1] & upper.age.bound <= input$agegp10[2]) %>%
        group_by(nice.symptom) %>%
        summarise(times.present = sum(times.present), times.recorded = sum(times.recorded)) %>%
        mutate(p.present = times.present/times.recorded) %>%
        mutate(p.absent = 1-p.present) %>%
        pivot_longer(c(p.present, p.absent), names_to = "affected", values_to = "proportion") %>%
        mutate(affected = map_lgl(affected, function(x) x == "p.present")) %>%
        filter(!is.nan(proportion)) %>%
        mutate(label = glue("{times.present} / {times.recorded}"))
    })
    renderPlot(confidentiality.check(symptom.prevalence.reactive(), symptom.prevalence.plot), height = 500)
  }
  
  output$comorbidityPrevalence <- {
    
    comorbidity.prevalence.reactive <- reactive({
      fd <- comorbidity.prevalence.input %>%
        filter(country %in% input$country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(lower.age.bound >= input$agegp10[1] & upper.age.bound <= input$agegp10[2]) %>%
        group_by(nice.comorbidity) %>%
        summarise(times.present = sum(times.present), times.recorded = sum(times.recorded)) %>%
        mutate(p.present = times.present/times.recorded) %>%
        mutate(p.absent = 1-p.present) %>%
        pivot_longer(c(p.present, p.absent), names_to = "affected", values_to = "proportion") %>%
        mutate(affected = map_lgl(affected, function(x) x == "p.present")) %>%
        filter(!is.nan(proportion)) %>%
        mutate(label = glue("{times.present} / {times.recorded}"))
    })
    renderPlot(confidentiality.check(comorbidity.prevalence.reactive(), comorbidity.prevalence.plot), height = 500)
  }
  
  
  output$treatmentPrevalence <- {
    
    treatment.prevalence.reactive <- reactive({

      fd <- treatment.use.proportion.input %>%
        filter(country %in% input$country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(lower.age.bound >= input$agegp10[1] & upper.age.bound <= input$agegp10[2]) %>%
        group_by(nice.treatment) %>%
        summarise(times.present = sum(times.present), times.recorded = sum(times.recorded)) %>%
        mutate(p.present = times.present/times.recorded) %>%
        mutate(p.absent = 1-p.present) %>%
        pivot_longer(c(p.present, p.absent), names_to = "affected", values_to = "proportion") %>%
        mutate(affected = map_lgl(affected, function(x) x == "p.present")) %>%
        filter(!is.nan(proportion)) %>%
        mutate(label = glue("{times.present} / {times.recorded}"))
    })
    renderPlot(confidentiality.check(treatment.prevalence.reactive(), treatment.prevalence.plot), height = 500)
  }
  
}