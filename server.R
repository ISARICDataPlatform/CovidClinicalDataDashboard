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
  
  output$case_def <- {
    renderPlot(
      case_def,
      height = 400
    )
  }
  
  output$agePyramid <- {
    renderPlot(age_pyramid,
               height = 400)
  }
  
  output$outcomesByAdmissionDate <- {
    renderPlot(
      outcomes_by_admission,
      height = 400
    )
  }
  
  output$symptomPrevalence <- {
    renderPlot(
      symptom_prevalence,
      height = 500
    )
  }
  
  output$symptomUpset <- {
    renderPlot(
      symptom_upset,
      height = 500
    )
  }
  
  output$symptomHeatmap <- {
    renderPlot(symptom_heatmap, height = 500)
  }
  
  
  
  output$comorbidityPrevalence <- {
    renderPlot(
      comorbidity_prevalence,
      height = 500
    )
  }
  
  output$comorbidityUpset <- {
    renderPlot(
      comorbidity_upset,
      height = 500
    )
  }
  
  
  output$treatmentPrevalence <- {
    renderPlot(
      treatment_prevalence,
      height = 500
    )
  }
  
  
  output$treatmentUpset <- {
    renderPlot(
      treatment_upset,
      height = 500
    )
  }
  
  
  output$icuTreatmentPrevalence <- {
    renderPlot(
      treatment_prevalence,
      height = 500
    )
  }
  
  output$icuTreatmentUpset <- {
    renderPlot(
      treatment_upset_icu,
      height = 500
    )
  }
  
  output$lengthofstayICU <- {
    renderPlot(
      length_of_stay_icu,
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
      length_of_stay_sex,
      height = 500
    )
  }
  
  output$lengthofstayAge <- {
    renderPlot(
      length_of_stay_age,
      height = 500
    )
  }
  
  output$admissiontoICU <- {
    renderPlot(
      admission_to_icu,
      height = 500
    )
  }
  
  output$StatusbyTime <- {
    renderPlot(
      status_by_time,
      height = 500
    )
    
  }
  
  
  output$lab_results_lab_crp <- {
    renderPlotly(crp)
  }
  
  output$lab_results_lab_lym <- {
    renderPlotly(lym)
  }
  
  output$lab_results_lab_neut <- {
    renderPlotly(neut)
  }
  
  output$lab_results_lab_wbc <- {
    renderPlotly(wbc)
  }
  
  output$lab_results_lab_urean <- {
    renderPlotly(urean)
  }
  
  output$lab_results_lab_pt <- {
    renderPlotly(pt)
  }
  
  output$lab_results_lab_alt <- {
    renderPlotly(alt)
  }

  output$lab_results_lab_bili <- {
    renderPlotly(bili)
  }
  output$lab_results_lab_ast <- {
    renderPlotly(ast)
  }
  output$clinical_signs_vs_resp <- {
    renderPlotly(resp)
  }
  
  output$clinical_signs_vs_hr <- {
    renderPlotly(hr)
  }
  
  output$clinical_signs_vs_temp <- {
    renderPlotly(temp)
  }
  
  output$clinical_signs_vs_sysbp <- {
    renderPlotly(sysbp)
  }
  
  output$clinical_signs_vs_oxysat <- {
    renderPlotly(oxysat)
  }
  
  output$PatientbyCountry <- {
    renderPlot(
      patient_by_country,
      height = 500
    )
  }
  
  output$age_comorbid_asthma <- {
    renderPlotly(asthma)
  }
  
  output$age_comorbid_malignant_neoplasm <- {
    renderPlotly(neoplasm)
  }
  
  output$age_comorbid_obesity <- {
    renderPlotly(obesity)
  }
  
  output$age_comorbid_diabetes <- {
    renderPlotly(diabetes)
  }
  
  output$age_comorbid_dementia <- {
    renderPlotly(dementia)
  }
  
  output$age_comorbid_smoking <- {
    renderPlotly(smoking)
  }
  
  output$age_comorbid_hypertension <- {
    renderPlotly(hypertension)
  }
  
  output$age_symptoms_history_of_fever <- {
    renderPlotly(history_of_fever)
  }
  
  output$age_symptoms_cough <- {
    renderPlotly(cough)
  }
  
  output$age_symptoms_cough_fever <- {
    renderPlotly(cough_fever)
  }
  
  output$age_symptoms_shortness_of_breath <- {
    renderPlotly(shortness_of_breath)
  }
  
  output$age_symptoms_cought_fever_shortness_of_breath <- {
    renderPlotly(shortness_of_breath)
  }
  
  output$age_symptoms_upper_respiratory_tract_symptoms <- {
    renderPlotly(upper_respiratory_tract_symptoms)
  }
  
  output$age_symptoms_altered_consciousness_confusion <- {
    renderPlotly(altered_consciousness_confusion)
  }
  
  output$age_symptoms_constitutional <- {
    renderPlotly(constitutional)
  }
  
  output$age_symptoms_vomiting_nausea <- {
    renderPlotly(vomiting_nausea)
  }
  
  output$age_symptoms_diarrhoea <- {
    renderPlotly(diarrhoea)
  }
  
  output$age_symptoms_abdominal_pain <- {
    renderPlotly(abdominal_pain)
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
