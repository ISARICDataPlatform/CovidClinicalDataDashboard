#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

privacy.text <-
  "Apologies, we cannot display graphs of data from\nless than five individuals for reasons of data privacy."

confidentiality.check <- function(data, fn, min.rows = 5, ...) {
  args <- list(...)
  if (nrow(data) >= min.rows | sum(data$count)>= min.rows) {
    exec(fn, data,!!!args)
  } else {
    ggplot() + annotate(
      geom = "text",
      x = 0,
      y = 0,
      label = privacy.text
    ) + theme_void()
  }
}

slider.filters <- function(tbl, input) {
  formatted.dates <-
    parse_date_time(input$admission_date, orders = "my")
  selected.months <- month(formatted.dates)
  selected.years <- year(formatted.dates)
  selected.my <- glue("{selected.months}-{selected.years}")
  
  
  selected.my <- map_chr(selected.my, function(my) {
    ifelse(nchar(my) == 6, glue("0{my}"), my)
  })
  
  out <- tbl %>%
    as.data.table() %>%
    lazy_dt(immutable = FALSE) %>%
    filter(slider_country %in% input$country) %>%
    filter(slider_outcome %in% input$outcome) %>%
    filter(slider_sex %in% input$sex) %>%
    filter(slider_icu_ever %in% input$icu_ever) %>%
    filter(slider_monthyear %in% selected.my) %>%
    filter(lower.age.bound >= input$agegp10[1] &
             upper.age.bound <= input$agegp10[2]) %>%
    as.data.table() %>%
    lazy_dt(immutable = FALSE)
  
  return(out)
}

server <- function(input, output) {
  
  output$contributions_video <- renderUI({
    tags$video(
      src=FILE_CONTRIBUTIONS_VIDEO,
      width='600px',
      height='360px',
      type='video/mp4',
      controls="controls"
    )
  })
  
  output$age_pyramid_gif <- renderImage({
    # load gif
    gif_image <- image_read(FILE_GIF_IMAGE)
    # write to tmp file
    tmpfile <- gif_image %>%
      image_scale("600x360!") %>%
      image_write(tempfile(fileext = 'gif'), format = 'gif')
    # return temp file as list object with specified features to be read by ui
    list(src = tmpfile, contentType = "image/gif")
  }, deleteFile = TRUE
  )
  
  output$agePyramid <- {
    age.pyramid.reactive <- reactive({
      fd <- age.pyramid.input %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    
    renderPlot(confidentiality.check(age.pyramid.reactive(), age.pyramid.plot),
               height = 300)
  }
  
  output$outcomesByAdmissionDate <- {
    outcomes.by.admission.date.reactive <- reactive({
      fd <- outcome_admission_date_input %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) 
    })
    renderPlot(
      confidentiality.check(
        outcomes.by.admission.date.reactive(),
        outcomes.by.admission.date.plot
      ),
      height = 300
    )
  }
  
  output$symptomPrevalence <- {
    symptom.prevalence.reactive <- reactive({
      fd <- symptom.prevalence.input %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input)
      
    })
    renderPlot(
      confidentiality.check(symptom.prevalence.reactive(), symptom.prevalence.plot),
      height = 500
    )
  }
  
  output$symptomUpset <- {
    symptom.upset.reactive <- reactive({
      fd <- symptom.upset.input %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(
      confidentiality.check(symptom.upset.reactive(), upset.plot, which.plot = "symptom"),
      height = 500
    )
  }
  
  output$symptomHeatmap <- {
    renderPlot((heatmap_plot(data_plot_heatmap)), height = 500)
  }
  
  
  
  output$comorbidityPrevalence <- {
    comorbidity.prevalence.reactive <- reactive({
      fd <- comorbidity.prevalence.input %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) 
    })
    renderPlot(
      confidentiality.check(
        comorbidity.prevalence.reactive(),
        comorbidity.prevalence.plot
      ),
      height = 500
    )
  }
  
  output$comorbidityUpset <- {
    comorbidity.upset.reactive <- reactive({
      fd <- comorbidity.upset.input %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(
      confidentiality.check(comorbidity.upset.reactive(), upset.plot, which.plot = "comorbidity"),
      height = 500
    )
  }
  
  
  output$treatmentPrevalence <- {
    treatment.prevalence.reactive <- reactive({
      fd <- treatment.use.proportion.input %>%
        slider.filters(input)
    })
    renderPlot(
      confidentiality.check(
        treatment.prevalence.reactive(),
        treatment.prevalence.plot,
        icu = FALSE
      ),
      height = 500
    )
  }
  
  
  output$treatmentUpset <- {
    treatment.upset.reactive <- reactive({
      fd <- treatment.upset.input %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(
      confidentiality.check(treatment.upset.reactive(), upset.plot, which.plot = "treatment"),
      height = 500
    )
  }
  
  
  output$icuTreatmentPrevalence <- {
    icu.treatment.prevalence.reactive <- reactive({
      fd <- icu.treatment.use.proportion.input %>%
        slider.filters(input) 
    })
    renderPlot(
      confidentiality.check(
        icu.treatment.prevalence.reactive(),
        treatment.prevalence.plot,
        icu = TRUE
      ),
      height = 500
    )
  }
  
  output$icuTreatmentUpset <- {
    icu.treatment.upset.reactive <- reactive({
      fd <- icu.treatment.upset.input %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(
      confidentiality.check(icu.treatment.upset.reactive(), upset.plot, which.plot = "icu.treatment"),
      height = 500
    )
  }
  
  output$lengthofstayICU <- {
    length.of.stay.icu.reactive <- reactive({
      fd <- length.of.stay.icu.input %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(
      confidentiality.check(length.of.stay.icu.reactive(), length.of.stay.icu.plot),
      height = 500
    )
  }
  
  output$contributorsMap <- renderLeaflet({
    map.data %>%
      # What sites have valid geolocation?
      dplyr::filter(is.na(location_lat) == F &
                      is.na(location_long) == F) %>%
      leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::addCircleMarkers(
        lat =  ~ location_lat,
        lng =  ~ location_long,
        radius = ~ 2,
        popup = ~ address_half,
        fillOpacity = 0.5
      ) %>%
      leaflet::setView(lng = 1,
                       lat = 7.9,
                       zoom = 1.7)
  })
  output$lengthofstaySex <- {
    length.of.stay.sex.reactive <- reactive({
      fd <- length.of.stay.sex.input %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
      
    })
    renderPlot(
      confidentiality.check(length.of.stay.sex.reactive(), length.of.stay.sex.plot),
      height = 500
    )
  }
  
  output$lengthofstayAge <- {
    length.of.stay.age.reactive <- reactive({
      fd <- length.of.stay.age.input %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(
      confidentiality.check(length.of.stay.age.reactive(), length.of.stay.age.plot),
      height = 500
    )
  }
  
  output$admissiontoICU <- {
    admission.to.icu.reactive <- reactive({
      fd <- admission.to.icu.input %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(
      confidentiality.check(admission.to.icu.reactive(), admission.to.icu.plot),
      height = 500
    )
  }
  
  output$StatusbyTime <- {
    status.by.time.after.admission.reactive <- reactive({
      fd <- status.by.time.after.admission.input %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(
      confidentiality.check(
        status.by.time.after.admission.reactive(),
        status.by.time.after.admission.plot
      ),
      height = 500
    )
    
  }
  
  
  output$lab_results_lab_crp <- {
    data_plot_lab_crp.reactive <- reactive({
      fd <- data_plot_lab_crp %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(confidentiality.check(data_plot_lab_crp.reactive(), p_lab_crp),
               height = 500)
  }
  
  output$lab_results_lab_lym <- {
    data_plot_lab_lym.reactive <- reactive({
      fd <- data_plot_lab_lym %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(confidentiality.check(data_plot_lab_lym.reactive(), p_lab_lym),
               height = 500)
  }
  
  output$lab_results_lab_neut <- {
    data_plot_lab_neut.reactive <- reactive({
      fd <- data_plot_lab_neut %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(confidentiality.check(data_plot_lab_neut.reactive(), p_lab_neut),
               height = 500)
  }
  
  output$lab_results_lab_wbc <- {
    data_plot_lab_wbc.reactive <- reactive({
      fd <- data_plot_lab_wbc %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(confidentiality.check(data_plot_lab_wbc.reactive(), p_lab_wbc),
               height = 500)
  }
  
  output$lab_results_lab_urean <- {
    data_plot_lab_urean.reactive <- reactive({
      fd <- data_plot_lab_urean %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(confidentiality.check(data_plot_lab_urean.reactive(), p_lab_urean),
               height = 500)
  }
  
  output$lab_results_lab_pt <- {
    data_plot_lab_pt.reactive <- reactive({
      fd <- data_plot_lab_pt %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(confidentiality.check(data_plot_lab_pt.reactive(), p_lab_pt),
               height = 500)
  }
  
  output$lab_results_lab_alt <- {
    data_plot_lab_alt.reactive <- reactive({
      fd <- data_plot_lab_alt %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(confidentiality.check(data_plot_lab_alt.reactive(), p_lab_alt),
               height = 500)
  }
  output$lab_results_lab_aptt <- {
    data_plot_lab_aptt.reactive <- reactive({
      fd <- data_plot_lab_aptt %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(confidentiality.check(data_plot_lab_aptt.reactive(), p_lab_aptt),
               height = 500)
  }
  output$lab_results_lab_bili <- {
    data_plot_lab_bili.reactive <- reactive({
      fd <- data_plot_lab_bili %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(confidentiality.check(data_plot_lab_bili.reactive(), p_lab_bili),
               height = 500)
  }
  output$lab_results_lab_ast <- {
    data_plot_lab_ast.reactive <- reactive({
      fd <- data_plot_lab_ast %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(confidentiality.check(data_plot_lab_ast.reactive(), p_lab_ast),
               height = 500)
  }
  output$clinical_signs_vs_resp <- {
    data_plot_vs_resp.reactive <- reactive({
      fd <- data_plot_vs_resp %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(confidentiality.check(data_plot_vs_resp.reactive(), p_resp),
               height = 500)
  }
  
  output$clinical_signs_vs_hr <- {
    data_plot_vs_hr.reactive <- reactive({
      fd <- data_plot_vs_hr %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(confidentiality.check(data_plot_vs_hr.reactive(), p_hr),
               height = 500)
  }
  
  output$clinical_signs_vs_temp <- {
    data_plot_vs_temp.reactive <- reactive({
      fd <- data_plot_vs_temp %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(confidentiality.check(data_plot_vs_temp.reactive(), p_temp),
               height = 500)
  }
  
  output$clinical_signs_vs_sysbp <- {
    data_plot_vs_sysbp.reactive <- reactive({
      fd <- data_plot_vs_sysbp %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(confidentiality.check(data_plot_vs_sysbp.reactive(), p_sysbp),
               height = 500)
  }
  
  output$clinical_signs_vs_oxysat <- {
    data_plot_vs_oxysat.reactive <- reactive({
      fd <- data_plot_vs_oxysat %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(confidentiality.check(data_plot_vs_oxysat.reactive(), p_oxysat),
               height = 500)
  }
  
  output$PatientbyCountry <- {
    patient.by.country.reactive <- reactive({
      fd <- patient.by.country.input %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(
      confidentiality.check(patient.by.country.reactive(), patient.by.country.plot),
      height = 500
    )
  }
  
  output$age_comorbid_asthma <- {
    data_plot_comorbid_asthma.reactive <- reactive({
      fd <- data_plot_comorbid_asthma %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(
      confidentiality.check(
        data_plot_comorbid_asthma.reactive(),
        plot.prop.by.age_comorbid_asthma
      ),
      height = 350
    )
  }
  
  output$age_comorbid_malignant_neoplasm <- {
    data_plot_comorbid_malignant_neoplasm.reactive <- reactive({
      fd <- data_plot_comorbid_asthma %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(
      confidentiality.check(
        data_plot_comorbid_malignant_neoplasm.reactive(),
        plot.prop.by.age_comorbid_malignant_neoplasm
      ),
      height = 350
    )
  }
  
  output$age_comorbid_obesity <- {
    data_plot_comorbid_obesity.reactive <- reactive({
      fd <- data_plot_comorbid_obesity %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(
      confidentiality.check(
        data_plot_comorbid_obesity.reactive(),
        plot.prop.by.age_comorbid_obesity
      ),
      height = 350
    )
  }
  
  output$age_comorbid_diabetes <- {
    data_plot_comorbid_diabetes.reactive <- reactive({
      fd <- data_plot_comorbid_diabetes %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(
      confidentiality.check(
        data_plot_comorbid_diabetes.reactive(),
        plot.prop.by.age_comorbid_diabetes
      ),
      height = 350
    )
  }
  
  output$age_comorbid_dementia <- {
    data_plot_comorbid_dementia.reactive <- reactive({
      fd <- data_plot_comorbid_dementia %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(
      confidentiality.check(
        data_plot_comorbid_dementia.reactive(),
        plot.prop.by.age_comorbid_dementia
      ),
      height = 350
    )
  }
  
  output$age_comorbid_smoking <- {
    data_plot_comorbid_smoking.reactive <- reactive({
      fd <- data_plot_comorbid_smoking %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(
      confidentiality.check(
        data_plot_comorbid_smoking.reactive(),
        plot.prop.by.age_comorbid_smoking
      ),
      height = 350
    )
  }
  
  output$age_comorbid_hypertension <- {
    data_plot_comorbid_hypertension.reactive <- reactive({
      fd <- data_plot_comorbid_hypertension %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(
      confidentiality.check(
        data_plot_comorbid_hypertension.reactive(),
        plot.prop.by.age_comorbid_hypertension
      ),
      height = 350
    )
  }
  
  output$age_symptoms_history_of_fever <- {
    data_plot_symptoms_history_of_fever.reactive <- reactive({
      fd <- data_plot_symptoms_history_of_fever %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(
      confidentiality.check(
        data_plot_symptoms_history_of_fever.reactive(),
        plot.prop.by.age_symptoms_history_of_fever
      ),
      height = 350
    )
  }
  
  output$age_symptoms_cough <- {
    data_plot_symptoms_cough.reactive <- reactive({
      fd <- data_plot_symptoms_cough %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(
      confidentiality.check(
        data_plot_symptoms_cough.reactive(),
        plot.prop.by.age_symptoms_cough
      ),
      height = 350
    )
  }
  
  output$age_symptoms_cough_fever <- {
    data_plot_symptoms_cough_fever.reactive <- reactive({
      fd <- data_plot_symptoms_cough_fever %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(
      confidentiality.check(
        data_plot_symptoms_cough_fever.reactive(),
        plot.prop.by.age_symptoms_cough_fever
      ),
      height = 350
    )
  }
  
  output$age_symptoms_shortness_of_breath <- {
    data_plot_symptoms_shortness_of_breath.reactive <- reactive({
      fd <- data_plot_symptoms_shortness_of_breath %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(
      confidentiality.check(
        data_plot_symptoms_shortness_of_breath.reactive(),
        plot.prop.by.age_symptoms_shortness_of_breath
      ),
      height = 350
    )
  }
  
  output$age_symptoms_cought_fever_shortness_of_breath <- {
    data_plot_symptoms_cought_fever_shortness_of_breath.reactive <-
      reactive({
        fd <- data_plot_symptoms_cought_fever_shortness_of_breath %>%
          as.data.table() %>%
          lazy_dt(immutable = FALSE) %>%
          slider.filters(input) %>%
          as_tibble()
      })
    renderPlot(
      confidentiality.check(
        data_plot_symptoms_cought_fever_shortness_of_breath.reactive(),
        plot.prop.by.age_symptoms_cought_fever_shortness_of_breath
      ),
      height = 350
    )
  }
  
  output$age_symptoms_upper_respiratory_tract_symptoms <- {
    data_plot_symptoms_upper_respiratory_tract_symptoms.reactive <-
      reactive({
        fd <- data_plot_symptoms_upper_respiratory_tract_symptoms %>%
          as.data.table() %>%
          lazy_dt(immutable = FALSE) %>%
          slider.filters(input) %>%
          as_tibble()
      })
    renderPlot(
      confidentiality.check(
        data_plot_symptoms_upper_respiratory_tract_symptoms.reactive(),
        plot.prop.by.age_symptoms_upper_respiratory_tract_symptoms
      ),
      height = 350
    )
  }
  
  output$age_symptoms_altered_consciousness_confusion <- {
    data_plot_symptoms_altered_consciousness_confusion.reactive <-
      reactive({
        fd <- data_plot_symptoms_altered_consciousness_confusion %>%
          as.data.table() %>%
          lazy_dt(immutable = FALSE) %>%
          slider.filters(input) %>%
          as_tibble()
      })
    renderPlot(
      confidentiality.check(
        data_plot_symptoms_altered_consciousness_confusion.reactive(),
        plot.prop.by.age_symptoms_altered_consciousness_confusion
      ),
      height = 350
    )
  }
  
  output$age_symptoms_constitutional <- {
    data_plot_symptoms_constitutional.reactive <- reactive({
      fd <- data_plot_symptoms_constitutional %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(
      confidentiality.check(
        data_plot_symptoms_constitutional.reactive(),
        plot.prop.by.age_symptoms_constitutional
      ),
      height = 350
    )
  }
  
  output$age_symptoms_vomiting_nausea <- {
    data_plot_symptoms_vomiting_nausea.reactive <- reactive({
      fd <- data_plot_symptoms_vomiting_nausea %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(
      confidentiality.check(
        data_plot_symptoms_vomiting_nausea.reactive(),
        plot.prop.by.age_symptoms_vomiting_nausea
      ),
      height = 350
    )
  }
  
  output$age_symptoms_diarrhoea <- {
    data_plot_symptoms_diarrhoea.reactive <- reactive({
      fd <- data_plot_symptoms_diarrhoea %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(
      confidentiality.check(
        data_plot_symptoms_diarrhoea.reactive(),
        plot.prop.by.age_symptoms_diarrhoea
      ),
      height = 350
    )
  }
  
  output$age_symptoms_abdominal_pain <- {
    data_plot_symptoms_abdominal_pain.reactive <- reactive({
      fd <- data_plot_symptoms_abdominal_pain %>%
        as.data.table() %>%
        lazy_dt(immutable = FALSE) %>%
        slider.filters(input) %>%
        as_tibble()
    })
    renderPlot(
      confidentiality.check(
        data_plot_symptoms_abdominal_pain.reactive(),
        plot.prop.by.age_symptoms_abdominal_pain
      ),
      height = 350
    )
  }
  
  
  output$table_patient_characteristic <- renderUI({
    return(tables_suplementary(table_sup = patient.characteristic.table, title_table_1 = "Table 1: Patient Characteristics.",
                               title_table_2 = "Proportions are presented in parentheses. Proportions have been rounded to two decimal places.")%>%
             htmltools_value() 
    )
    
  })
  output$table_outcome_age_sex <- renderUI({
    return(tables_suplementary(table_sup = outcome.age.sex.table, title_table_1 = "Table 2: Outcome by age and sex.",
                               title_table_2 = "Proportions are calculated using the column total as the denominator.")%>%
             htmltools_value() 
    )
    
  })
  output$table_symptoms <- renderUI({
    return(tables_suplementary(table_sup = symptoms.table, title_table_1 = "Table 3: Prevalence of Symptoms.", title_table_2 = "")%>%
             htmltools_value() 
    )
    
  })
  output$table_comorbidity <- renderUI({
    return(tables_suplementary(table_sup = comorbidity.table, title_table_1 = "Table 4: Prevalence of Comorbidities.", title_table_2 = "")%>%
             htmltools_value() 
    )
    
  })
  output$table_treatment <- renderUI({
    return(tables_suplementary(table_sup = treatments.table, title_table_1 = "Table 5: Prevalence of Treatments.",
                               title_table_2 ="The counts presented for treatments include all cases, not only cases with complete details of treatments (as expressed in the summary).")%>%
             htmltools_value() 
    )
    
  })
  output$table_key_times <- renderUI({
    return(tables_suplementary(table_sup = key.times.table, title_table_1 = "Table 6: Key time variables.",
                               title_table_2 = "SD: Standard deviation; IQR: Interquartile range. Outliers (values greater than 120) were excluded prior to the computation of estimates.")%>%
             htmltools_value() 
    )
    
  })
}
