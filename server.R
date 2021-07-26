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
  
  output$downloadReport <- downloadHandler(
    filename =  'Report.pdf',
    content = function(file) {
      
      withProgress(message = 'Rendering, please wait!', {
        tempReport <- file.path(tempdir(), "Static_report.Rmd")
        file.copy("markdown/Static_report.Rmd", tempReport, overwrite = TRUE)
        
        params <- list(rendered_by_shiny = TRUE)
        
        rmarkdown::render("markdown/Static_report.Rmd", output_file = file,
                          envir = new.env(parent = globalenv())
        )
      })
    }
  )
  
  output$static_country <- {
    renderPlot({
      p<-patient.by.country.plot(summary_input_overall)
      plot(p)
    },height=500)
  }
  
  output$contributions_video <- renderUI({
    tags$video(
      src=FILE_CONTRIBUTIONS_VIDEO,
      width='600px',
      height='360px',
      type='video/mp4',
      controls="controls"
    )
  })
  
  output$age_pyramid_video <- renderUI({
    tags$video(
      src=FILE_AGE_PYRAMID_VIDEO,
      width='600px',
      height='360px',
      type='video/mp4',
      controls="controls"
    )
  })
  
  # output$age_pyramid_gif <- renderImage({
  #   # load gif
  #   gif_image <- image_read(FILE_GIF_IMAGE)
  #   # write to tmp file
  #   tmpfile <- gif_image %>%
  #     image_scale("600x360!") %>%
  #     image_write(tempfile(fileext = 'gif'), format = 'gif')
  #   # return temp file as list object with specified features to be read by ui
  #   list(src = tmpfile, contentType = "image/gif")
  # }, deleteFile = TRUE
  # )
  # 
  output$case_def <- {
    renderPlot(
      plot_case_def(case.def.input),
      height = 400
    )
  }
  
  output$agePyramid <- {
    renderPlot(age.pyramid.plot(age.pyramid.input),
               height = 300)
  }
  
  output$outcomesByAdmissionDate <- {
    renderPlot(
      outcomes.by.admission.date.plot(outcome_admission_date_input),
      height = 400
    )
  }
  
  output$symptomPrevalence <- {
    renderPlot(
      symptom.prevalence.plot(symptom.prevalence.input),
      height = 500
    )
  }
  
  output$symptomUpset <- {
    renderCachedPlot(
      upset.plot(symptom.upset.input, which.plot = "symptom"),
      input$sidebarcollapsed,
      sizeGrowthRatio(height = 500)
    )
  }
  output$symptomHeatmap <- {
    renderPlot(heatmap_plot(data_plot_heatmap), height = 500)
  }
  
  
  
  output$comorbidityPrevalence <- {
    renderPlot(
      comorbidity.prevalence.plot(comorbidity.prevalence.input),
      height = 500
    )
  }
  
  output$comorbidityUpset <- {
    renderCachedPlot(
      upset.plot(comorbidity.upset.input, which.plot = "comorbidity"),
      input$sidebarcollapsed,
      sizeGrowthRatio(height = 500)
    )
  }
  
  
  output$treatmentPrevalence <- {
    renderPlot(
      treatment.prevalence.plot(treatment.use.proportion.input, icu = FALSE),
      height = 500
    )
  }
  
  
  output$treatmentUpset <- {
    renderCachedPlot(
      upset.plot(treatment.upset.input, which.plot = "treatment"),
      input$sidebarcollapsed,
      sizeGrowthRatio(height = 500)
    )
  }
  
  
  output$icuTreatmentPrevalence <- {
    renderPlot(
      treatment.prevalence.plot(icu.treatment.use.proportion.input, icu = TRUE),
      height = 500
    )
  }
  
  output$icuTreatmentUpset <- {
    renderCachedPlot(
      upset.plot(icu.treatment.upset.input, which.plot = "icu.treatment"),
      input$sidebarcollapsed,
      sizeGrowthRatio(height = 500)
    )
  }
  
  output$lengthofstayICU <- {
    renderPlot(
      length.of.stay.icu.plot(length.of.stay.icu.input),
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
    renderPlot(
      length.of.stay.sex.plot(length.of.stay.sex.input) ,
      height = 500
    )
  }
  
  output$lengthofstayAge <- {
    renderPlot(
      length.of.stay.age.plot(length.of.stay.age.input),
      height = 500
    )
  }
  
  output$admissiontoICU <- {
    renderPlot(
      admission.to.icu.plot(admission.to.icu.input),
      height = 500
    )
  }
  
  output$StatusbyTime <- {
    renderPlot(
      status.by.time.after.admission.plot(status.by.time.after.admission.input),
      height = 500
    )
    
  }
  
  
  output$lab_results_lab_crp <- {
    renderPlot(p_lab_crp(data_plot_lab_crp)) %>% bindCache(input$sidebarcollapsed)
  } 
  
  output$lab_results_lab_lym <- {
    renderPlot(p_lab_lym(data_plot_lab_lym)) %>% bindCache(input$sidebarcollapsed)
  }
  
  output$lab_results_lab_neut <- {
    renderPlot(p_lab_neut(data_plot_lab_neut)) %>% bindCache(input$sidebarcollapsed)
  }
  
  output$lab_results_lab_wbc <- {
    renderPlot(p_lab_wbc(data_plot_lab_wbc)) %>% bindCache(input$sidebarcollapsed)
  }
  
  output$lab_results_lab_urean <- {
    renderPlot(p_lab_urean(data_plot_lab_urean)) %>% bindCache(input$sidebarcollapsed)
  }
  
  output$lab_results_lab_pt <- {
    renderPlot(p_lab_pt(data_plot_lab_pt)) %>% bindCache(input$sidebarcollapsed)
  }
  
  output$lab_results_lab_alt <- {
    renderPlot(p_lab_alt(data_plot_lab_alt)) %>% bindCache(input$sidebarcollapsed)
  }

  output$lab_results_lab_bili <- {
    renderPlot(p_lab_bili(data_plot_lab_bili)) %>% bindCache(input$sidebarcollapsed)
  }
  output$lab_results_lab_ast <- {
    renderPlot(p_lab_ast(data_plot_lab_ast)) %>% bindCache(input$sidebarcollapsed)
  }
  output$clinical_signs_vs_resp <- {
    renderPlot(p_resp(data_plot_vs_resp)) %>% bindCache(input$sidebarcollapsed)
  }
  
  output$clinical_signs_vs_hr <- {
    renderPlot(p_hr(data_plot_vs_hr)) %>% bindCache(input$sidebarcollapsed)
  }
  
  output$clinical_signs_vs_temp <- {
    renderPlot(p_temp(data_plot_vs_temp)) %>% bindCache(input$sidebarcollapsed)
  }
  
  output$clinical_signs_vs_sysbp <- {
    renderPlot(p_sysbp(data_plot_vs_sysbp)) %>% bindCache(input$sidebarcollapsed)
  }
  
  output$clinical_signs_vs_oxysat <- {
    renderPlot(p_oxysat(data_plot_vs_oxysat)) %>% bindCache(input$sidebarcollapsed)
  }
  
  output$clinical_signs_vs_oxysat_therapy <- {
    renderPlot(p_oxysat_therapy(data_plot_vs_oxysat_therapy)) %>% bindCache(input$sidebarcollapsed)
  }
  
  output$PatientbyCountry <- {
    renderPlot(
      patient.by.country.plot(patient.by.country.input),
      height = 500
    )
  }
  
  output$age_comorbid_asthma <- {
    renderPlotly(plot.prop.by.age_comorbid_asthma(data_plot_comorbid_asthma, FALSE)) %>% bindCache(input$sidebarcollapsed)
  }
  
  output$age_comorbid_malignant_neoplasm <- {
    renderPlotly(plot.prop.by.age_comorbid_malignant_neoplasm(data_plot_comorbid_malignant_neoplasm, FALSE)) %>% bindCache(input$sidebarcollapsed)
  }
  
  output$age_comorbid_obesity <- {
    renderPlotly(plot.prop.by.age_comorbid_obesity(data_plot_comorbid_obesity, FALSE)) %>% bindCache(input$sidebarcollapsed)
  }
  
  output$age_comorbid_diabetes <- {
    renderPlotly(plot.prop.by.age_comorbid_diabetes(data_plot_comorbid_diabetes, FALSE)) %>% bindCache(input$sidebarcollapsed)
  }
  
  output$age_comorbid_dementia <- {
    renderPlotly(plot.prop.by.age_comorbid_dementia(data_plot_comorbid_dementia, FALSE)) %>% bindCache(input$sidebarcollapsed)
  }
  
  output$age_comorbid_smoking <- {
    renderPlotly(plot.prop.by.age_comorbid_smoking(data_plot_comorbid_smoking, FALSE)) %>% bindCache(input$sidebarcollapsed)
  }
  
  output$age_comorbid_hypertension <- {
    renderPlotly(plot.prop.by.age_comorbid_hypertension(data_plot_comorbid_hypertension, FALSE)) %>% bindCache(input$sidebarcollapsed)
  }
  
  output$age_symptoms_history_of_fever <- {
    renderPlotly(plot.prop.by.age_symptoms_history_of_fever(data_plot_symptoms_history_of_fever, FALSE)) %>% bindCache(input$sidebarcollapsed)
  }
  
  output$age_symptoms_cough <- {
    renderPlotly(plot.prop.by.age_symptoms_cough(data_plot_symptoms_cough, FALSE)) %>% bindCache(input$sidebarcollapsed)
  }
  
  output$age_symptoms_cough_fever <- {
    renderPlotly(plot.prop.by.age_symptoms_cough_fever(data_plot_symptoms_cough_fever, FALSE)) %>% bindCache(input$sidebarcollapsed)
  }
  
  output$age_symptoms_shortness_of_breath <- {
    renderPlotly(plot.prop.by.age_symptoms_shortness_of_breath(data_plot_symptoms_shortness_of_breath, FALSE)) %>% bindCache(input$sidebarcollapsed)
  }
  
  output$age_symptoms_cought_fever_shortness_of_breath <- {
    renderPlotly(plot.prop.by.age_symptoms_cought_fever_shortness_of_breath(data_plot_symptoms_cought_fever_shortness_of_breath, FALSE)) %>% bindCache(input$sidebarcollapsed)
  }
  
  output$age_symptoms_upper_respiratory_tract_symptoms <- {
    renderPlotly(plot.prop.by.age_symptoms_upper_respiratory_tract_symptoms(data_plot_symptoms_upper_respiratory_tract_symptoms, FALSE)) %>% bindCache(input$sidebarcollapsed)
  }
  
  output$age_symptoms_altered_consciousness_confusion <- {
    renderPlotly(plot.prop.by.age_symptoms_altered_consciousness_confusion(data_plot_symptoms_altered_consciousness_confusion, FALSE)) %>% bindCache(input$sidebarcollapsed)
  }
  
  output$age_symptoms_constitutional <- {
    renderPlotly(plot.prop.by.age_symptoms_constitutional(data_plot_symptoms_constitutional, FALSE)) %>% bindCache(input$sidebarcollapsed)
  }
  
  output$age_symptoms_vomiting_nausea <- {
    renderPlotly(plot.prop.by.age_symptoms_vomiting_nausea(data_plot_symptoms_vomiting_nausea, FALSE)) %>% bindCache(input$sidebarcollapsed)
  }
  
  output$age_symptoms_diarrhoea <- {
    renderPlotly(plot.prop.by.age_symptoms_diarrhoea(data_plot_symptoms_diarrhoea, FALSE)) %>% bindCache(input$sidebarcollapsed)
  }
  
  output$age_symptoms_abdominal_pain <- {
    renderPlotly(plot.prop.by.age_symptoms_abdominal_pain(data_plot_symptoms_abdominal_pain, FALSE)) %>% bindCache(input$sidebarcollapsed)
  }
  
  
  output$table_patient_characteristic <- renderUI({
    return(tables_supplementary(table_sup = patient.characteristic.table, title_table_1 = "Table 1: Patient Characteristics.",
                               title_table_2 = "Proportions are presented in parentheses. Proportions have been rounded to two decimal places.")%>%
             htmltools_value() 
    )
    
  })
  output$table_outcome_age_sex <- renderUI({
    return(tables_supplementary(table_sup = outcome.age.sex.table, title_table_1 = "Table 2: Outcome by age and sex.",
                               title_table_2 = "Proportions are calculated using the column total as the denominator.")%>%
             htmltools_value() 
    )
    
  })
  output$table_symptoms <- renderUI({
    return(tables_supplementary(table_sup = symptoms.table, title_table_1 = "Table 3: Prevalence of Symptoms.", title_table_2 = "")%>%
             htmltools_value() 
    )
    
  })
  output$table_comorbidity <- renderUI({
    return(tables_supplementary(table_sup = comorbidity.table, title_table_1 = "Table 4: Prevalence of Comorbidities.", title_table_2 = "")%>%
             htmltools_value() 
    )
    
  })
  output$table_treatment <- renderUI({
    return(tables_supplementary(table_sup = treatments.table, title_table_1 = "Table 5: Prevalence of Treatments.",
                               title_table_2 ="The counts presented for treatments include all cases, not only cases with complete details of treatments (as expressed in the summary).")%>%
             htmltools_value() 
    )
    
  })
  output$table_key_times <- renderUI({
    return(tables_supplementary(table_sup = key.times.table, title_table_1 = "Table 6: Key time variables.",
                               title_table_2 = "SD: Standard deviation; IQR: Interquartile range. Outliers (values greater than 120) were excluded prior to the computation of estimates.")%>%
             htmltools_value() 
    )
    
  })
}
