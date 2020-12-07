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
    filter(slider_country %in% input$country) %>%
    filter(slider_outcome %in% input$outcome) %>%
    filter(slider_sex %in% input$sex) %>%
    filter(slider_icu_ever %in% input$icu_ever) %>%
    filter(slider_monthyear %in% selected.my) %>%
    filter(lower.age.bound >= input$agegp10[1] & upper.age.bound <= input$agegp10[2]) %>%
    as.data.table() %>%
    lazy_dt(immutable = FALSE)
  
  return(out)
}

server <- function(input, output) {
  
  output$flowchart <- renderGrViz({
    grViz({
      diagram
    })
  })

  output$agePyramid <- {
    
    age.pyramid.reactive <- reactive({
      
      fd <- age.pyramid.input %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        group_by(slider_agegp10, slider_sex, slider_outcome) %>%
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
        complete(slider_sex, 
                 nesting(slider_agegp10, lower.age.bound, upper.age.bound), 
                 slider_country, 
                 nesting(calendar.year.admit, calendar.month.admit, year.epiweek.admit), 
                 slider_outcome, 
                 slider_icu_ever, 
                 fill = list(count = 0)) %>%
        arrange(year.epiweek.admit) %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        group_by(slider_sex, slider_outcome, slider_country, slider_agegp10, lower.age.bound, upper.age.bound, slider_icu_ever) %>%
        mutate(cum.count = cumsum(count)) %>% 
        ungroup() %>%
        filter(cum.count > 0) %>%
        mutate(temp.cum.count = cum.count) %>%
        select(-cum.count) %>%
        group_by(year.epiweek.admit, slider_outcome) %>%
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
  
  output$comorbidityUpset <- {
    
    comorbidity.upset.reactive <- reactive({
      
      fd <- comorbidity.upset.input %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(confidentiality.check(comorbidity.upset.reactive(), upset.plot, which.plot = "comorbidity"), height = 500)
  }
  
  output$symptomUpset <- {
    
   symptom.upset.reactive <- reactive({
      
      fd <- symptom.upset.input %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(confidentiality.check(symptom.upset.reactive(), upset.plot, which.plot = "symptom"), height = 500)
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
  
  
  output$treatmentUpset <- {
    
    treatment.upset.reactive <- reactive({
      
      fd <- treatment.upset.input %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(confidentiality.check(treatment.upset.reactive(), upset.plot, which.plot = "treatment"), height = 500)
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
  
  output$icuTreatmentUpset <- {
    
    icu.treatment.upset.reactive <- reactive({
      
      fd <- icu.treatment.upset.input %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(confidentiality.check(icu.treatment.upset.reactive(), upset.plot, which.plot = "icu.treatment"), height = 500)
  }
  
  output$lengthofstayICU <- {
    length.of.stay.icu.reactive <- reactive({
      fd <- length.of.stay.icu.input %>% 
        as.data.table() %>% 
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(confidentiality.check(length.of.stay.icu.reactive(), length.of.stay.icu.plot), height = 500)
  }
  
  output$contributorsMap <- renderLeaflet({
    
    map.data %>%
      # What sites have valid geolocation?
      dplyr::filter(is.na(location_lat)==F&is.na(location_long)==F) %>%
      leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::addCircleMarkers(lat=~location_lat,
                                lng=~location_long,
                                radius = ~2,
                                popup= ~address_half,
                                fillOpacity = 0.5) %>%
      leaflet::setView(lng = 1, lat = 7.9, zoom=1.7)
  })
  output$lengthofstaySex <- {
    length.of.stay.sex.reactive <- reactive({ 
      fd <- length.of.stay.sex.input %>% 
        as.data.table() %>% 
        lazy_dt(immutable = FALSE) %>% 
        slider.filters(input) %>% 
        as_tibble()
      
    })
    renderPlot(confidentiality.check(length.of.stay.sex.reactive(), length.of.stay.sex.plot), height = 500)
  }
  
  output$lengthofstayAge <- {
    length.of.stay.age.reactive <- reactive({
      fd <- length.of.stay.age.input %>% 
        as.data.table() %>% 
        lazy_dt(immutable = FALSE) %>% 
        slider.filters(input) %>% 
        as_tibble()
    })
    renderPlot(confidentiality.check(length.of.stay.age.reactive(), length.of.stay.age.plot), height = 500)
  }
  
  output$admissiontoICU <- {
    admission.to.icu.reactive <- reactive({
      fd <- admission.to.icu.input %>% 
        as.data.table() %>% 
        lazy_dt(immutable = FALSE) %>% 
        slider.filters(input) %>% 
        as_tibble()
    })
    renderPlot(confidentiality.check(admission.to.icu.reactive(), admission.to.icu.plot), height = 500)
  }
  
  output$StatusbyTime <- {
    status.by.time.after.admission.reactive <- reactive({
      fd <- status.by.time.after.admission.input %>% 
        as.data.table() %>% 
        lazy_dt(immutable = FALSE) %>% 
        slider.filters(input) %>% 
        group_by(day, status, slider_sex, slider_agegp10, slider_country, slider_monthyear) %>% 
        summarise(count=n()) %>% 
        as_tibble()
    })
    renderPlot(confidentiality.check(status.by.time.after.admission.reactive(), status.by.time.after.admission.plot), height = 500)
    
  }


  output$lab_results_lab_crp <- {
    
    data_plot_lab_crp.reactive <- reactive({   
      
    fd <- data_plot_lab_crp %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(confidentiality.check(data_plot_lab_crp.reactive(), p_lab_crp), height = 500)
  }
  
  output$lab_results_lab_lym <- {
    
    data_plot_lab_lym.reactive <- reactive({   
      
      fd <- data_plot_lab_lym %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(confidentiality.check(data_plot_lab_lym.reactive(), p_lab_lym), height = 500)
  }
  
  output$lab_results_lab_neut <- {
    
    data_plot_lab_neut.reactive <- reactive({   
      
      fd <- data_plot_lab_neut %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(confidentiality.check(data_plot_lab_neut.reactive(), p_lab_neut), height = 500)
  }
  
  output$lab_results_lab_wbc <- {
    
    data_plot_lab_wbc.reactive <- reactive({   
      
      fd <- data_plot_lab_wbc %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(confidentiality.check(data_plot_lab_wbc.reactive(), p_lab_wbc), height = 500)
  }
  
  output$lab_results_lab_urean <- {
    
    data_plot_lab_urean.reactive <- reactive({   
      
      fd <- data_plot_lab_urean %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(confidentiality.check(data_plot_lab_urean.reactive(), p_lab_urean), height = 500)
  }
  
  output$lab_results_lab_pt <- {
    
    data_plot_lab_pt.reactive <- reactive({   
      
      fd <- data_plot_lab_pt %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(confidentiality.check(data_plot_lab_pt.reactive(), p_lab_pt), height = 500)
  }
  
  output$lab_results_lab_alt <- {
    
    data_plot_lab_alt.reactive <- reactive({   
      
      fd <- data_plot_lab_alt %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(confidentiality.check(data_plot_lab_alt.reactive(), p_lab_alt), height = 500)
  }
  output$lab_results_lab_aptt <- {
    
    data_plot_lab_aptt.reactive <- reactive({   
      
      fd <- data_plot_lab_aptt %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(confidentiality.check(data_plot_lab_aptt.reactive(), p_lab_aptt), height = 500)
  }
  output$lab_results_lab_bili <- {
    
    data_plot_lab_bili.reactive <- reactive({   
      
      fd <- data_plot_lab_bili %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(confidentiality.check(data_plot_lab_bili.reactive(), p_lab_bili), height = 500)
  }
  output$lab_results_lab_ast <- {
    
    data_plot_lab_ast.reactive <- reactive({   
      
      fd <- data_plot_lab_ast %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(confidentiality.check(data_plot_lab_ast.reactive(), p_lab_ast), height = 500)
  }
  output$clinical_signs_vs_resp <- {
    
    data_plot_vs_resp.reactive <- reactive({   
      
      fd <- data_plot_vs_resp %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(confidentiality.check(data_plot_vs_resp.reactive(), p_resp), height = 500)
  }
  
  output$clinical_signs_vs_hr <- {
    
    data_plot_vs_hr.reactive <- reactive({   
      
      fd <- data_plot_vs_hr %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(confidentiality.check(data_plot_vs_hr.reactive(), p_hr), height = 500)
  }
  
  output$clinical_signs_vs_temp <- {
    
    data_plot_vs_temp.reactive <- reactive({   
      
      fd <- data_plot_vs_temp %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(confidentiality.check(data_plot_vs_temp.reactive(), p_temp), height = 500)
  }
  
  output$clinical_signs_vs_sysbp <- {
    
    data_plot_vs_sysbp.reactive <- reactive({   
      
      fd <- data_plot_vs_sysbp %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(confidentiality.check(data_plot_vs_sysbp.reactive(), p_sysbp), height = 500)
  }
  
  output$clinical_signs_vs_oxysat <- {
    
    data_plot_vs_oxysat.reactive <- reactive({   
      
      fd <- data_plot_vs_oxysat %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(confidentiality.check(data_plot_vs_oxysat.reactive(), p_oxysat), height = 500)
  }
  
  output$PatientbyCountry <- {
    
    patient.by.country.reactive <- reactive({
      
      fd <- patient.by.country.input %>% 
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        mutate(Country=slider_country) %>% 
        group_by(Country) %>%
        summarise(count = n()) %>%
        as_tibble()
    })
    renderPlot(confidentiality.check(patient.by.country.reactive(), patient.by.country.plot), height = 500)
  }


}