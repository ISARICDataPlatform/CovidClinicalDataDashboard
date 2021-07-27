#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

dbHeader <- dashboardHeader(title = "COVID-19 Analysis Report",
                            tags$li(
                              a(
                                href = 'http://isaric.tghn.org/',
                                img(
                                  src = 'ISARIClogoCream.png',
                                  title = "Company Home",
                                  height = "40px"
                                ),
                                style = "padding-top:5px; padding-bottom:5px;"
                              ),
                              class = "dropdown"
                              # tags$style(".main-header {max-height: 40px}"),
                              # tags$style(".main-header .logo {height: 40px;}"),
                              # tags$style(".sidebar-toggle {height: 40px; padding-top: 1px !important;}"),
                              # tags$style(".navbar {min-height:40px !important}")
                            ))

month.option.list <- as.list(c(NA, month.options))
names(month.option.list) <- c("Unknown", month.options)

# Define UI for application that draws a histogram
dashboardPage(
  skin = "black",
  dbHeader,
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Home",
        tabName = "home",
        icon = icon("home")),
      menuItem(
        "Overview", 
        tabName = "flowchart", 
        icon = icon("book")),
      menuItem(
        "Country distribution",
        tabName = "country",
        icon = icon("globe")
      ),
      menuItem(
        "Patient characteristics",
        tabName = "patients",
        icon = icon("bed")
      ),
      menuItem(
        "Symptoms at admission",
        tabName = "symptoms",
        icon = icon("stethoscope")
      ),
      menuItem(
        "Symptoms by age",
        tabName = "symptoms_by_age",
        icon = icon("chart-bar")
      ),
      menuItem(
        "Comorbidities",
        tabName = "comorbidities",
        icon = icon("notes-medical")
      ),
      menuItem(
        "Comorbidities by age",
        tabName = "comorbidities_by_age",
        icon = icon("chart-bar")
      ),
      menuItem(
        "Vital signs",
        tabName = "vital_signs",
        icon = icon("file-medical-alt")
      ),
      menuItem(
        "Laboratory results",
        tabName = "lab_results",
        icon = icon("vial")
      ),
      menuItem(
        "Treatment",
        tabName = "treatment",
        icon = icon("pills")),
      menuItem(
        "ICU Treatment",
        tabName = "icu_treatment",
        icon = icon("heartbeat")
      ),
      menuItem(
        "Hospital stays",
        tabName = "hospital_stays",
        icon = icon("hospital")
      ),
      menuItem(
        "Summary tables",
        tabName = "tables",
        icon = icon("table")
      ),
      menuItem(
        "Data contributors",
        tabName = "contributors",
        icon = icon("users")
      )
    ),
  
    hr(),
    fluidRow(column(3, verbatimTextOutput("value")))
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$style(
        HTML(
          '.btn-custom {background-color: #df0442; color:  #FFFFFF;}
                 .skin-black .main-header .logo {color: #df0442; font-weight: bold; background-color: #FFFFEA;}
                 .skin-black .main-header .logo:hover {color: #df0442; font-weight: bold; background-color: #FFFFEA;}
                 .skin-black .main-header .navbar { background-color: #FFFFEA;}
                 .skin-black .main-header .navbar .nav > li > a:hover { background-color: #FFFFEA;}
                 .skin-black .main-header .navbar .sidebar-toggle:hover {color: #999999; background: #f7b538;}
                 .skin-black .main-sidebar {color: #df0442; border-left-color: #df0442; background-color: #4f6272; font-family: Roboto Condensed}
                 .skin-black .main-sidebar .sidebar .sidebar-menu .active > a {color: #ffffff; border-left-color: #9e145f; background-color: #009cad;}
                 .skin-black .main-sidebar .sidebar .sidebar-menu li:hover  > a {color: #df0442; border-left-color: #df0442; background-color: #f7b538;}
                 .irs-bar, .irs-bar-edge, .irs-single, .irs-to, .irs-from, .irs-grid-pol {background: #df0442; border-color: #df0442;}
                 .content-wrapper, .right-side { background-color: #FFFFFF;}
                 .box-header h3.box-title {font-family: Roboto},
                 .box {font-family: Roboto}'
        )
      )
    ),
    hr(),
    tabItems(
      tabItem(tabName = "home", 
                  includeMarkdown("markdown/welcome.md")
              ,
              fluidRow(
                box(
                  uiOutput("contributions_video", align = "center"),
                  solidHeader = T,
                  title = "Contributions of patient records"
                  ),
                box(
                  uiOutput("age_pyramid_video", align = "center"),
                  solidHeader = T,
                  title = "Outcomes by age and gender"
                  )
              ),
              includeMarkdown("markdown/welcome_caveats.md")
      ),
      tabItem(tabName = "patients",
              fluidRow(
                box(
                  plotOutput("agePyramid", height = "300px"),
                  "Bar fills are outcome (death/discharge/LTFU) at the time of report.",
                  width = 6,
                  height = 400,
                  title = 'Age and sex distribution of patients'
                )),
                fluidRow(
                box(
                  plotOutput("outcomesByAdmissionDate", height = "400px"),
                  "Bar fills are outcome (death/discharge/LTFU) at the time of report.",
                  width = 10,
                  height = 500,
                  title = 'Cumulative patient count by admission date'
                )
              )),
      tabItem(tabName = "symptoms",
              fluidRow(
                box(
                  plotOutput("symptomPrevalence", height = "500px"),
                  "Bars are labelled with the fraction of patients presenting with the symptom to the number of patients with data on the symptom recorded",
                  width = 6,
                  height = 600,
                  solidHeader = T,
                  title = 'Prevalance of symptoms at admission'
                ),
                box(
                  plotOutput("symptomUpset", height = "500px"),
                  width = 6,
                  height = 600,
                  solidHeader = T,
                  title = 'Combinations of the five most common symptoms upon admission'
                ),
                box(
                  plotOutput("symptomHeatmap", height = "500px"),
                  width = 6,
                  height = 600,
                  solidHeader = T,
                  title = 'Heatmap of symptoms at admission'
                ),
                box(
                  plotOutput("case_def"),
                  height = 500,
                  width=10,
                  title = "Proportion of patients that meet the 4 most common COVID-19 symptom case definitions by age"
                ),
              ),
              ),
      tabItem(tabName = "comorbidities",
              fluidRow(
                box(
                  plotOutput("comorbidityPrevalence", height = "500px"),
                  "Bars are labelled with the fraction of patients presenting with the comorbidity to the number of patients with data on the comorbidity recorded",
                  width = 6,
                  height = 600,
                  solidHeader = T,
                  title = 'Prevalance of comorbidities'
                ),
                box(
                  plotOutput("comorbidityUpset", height = "500px"),
                  width = 6,
                  height = 600,
                  solidHeader = T,
                  title = 'Combinations of the five most common comorbidities'
                )
              )),
      tabItem(tabName = "treatment",
              fluidRow(
                box(
                  plotOutput("treatmentPrevalence", height = "500px"),
                  "Bars are labelled with the fraction of patients given the treatment to the number of patients with data on the treatment recorded",
                  width = 6,
                  height = 600,
                  solidHeader = T,
                  title = 'Treatments given'
                ),
                box(
                  plotOutput("treatmentUpset", height = "500px"),
                  width = 6,
                  height = 600,
                  solidHeader = T,
                  title = 'Combinations of the five most common treatments given'
                )
              )),
      tabItem(tabName = "icu_treatment",
              fluidRow(
                box(
                  plotOutput("icuTreatmentPrevalence", height = "500px"),
                  "Bars are labelled with the fraction of patients given the treatment to the number of patients with data on the treatment recorded",
                  width = 6,
                  height = 600,
                  solidHeader = T,
                  title = 'Treatments given'
                ),
                box(
                  plotOutput("icuTreatmentUpset", height = "500px"),
                  width = 6,
                  height = 600,
                  solidHeader = T,
                  title = 'Combinations of the five most common treatments given in the ICU'
                ),
                box(
                  plotOutput("lengthofstayICU", height = "500px"),
                  "This only includes cases with reported outcomes. The coloured areas indicate the kernel probability density of the observed data and the box plots show the median and interquartile range of the variable of interest. The upper value was truncated at 97.5%. The number below is the total number in each group.",
                  width = 6,
                  height = 600,
                  solidHeader = T,
                  title = 'Distribution of length of stay for patients admitted into Intensive Care Unit (ICU)'
                )
              )),
      tabItem(tabName = "contributors",
              fluidRow(
                box(
                  leafletOutput("contributorsMap"),
                  title = "Contributors Map",
                  width = 12
                )
              ),
              fluidRow(
                box(
                  includeMarkdown("markdown/Contributor_listmap.md"),
                  title = "Contributing Site List",
                  width = 12
                )
              )),
      tabItem(tabName = "hospital_stays",
              fluidRow(
                box(
                  plotOutput("lengthofstaySex", height = "500px"),
                  "This only includes cases with reported outcomes. The coloured areas indicate the kernel probability density of the observed data and the box plots show the median and interquartile range of the variable of interest. The upper value was truncated at 97.5%. The number below is the total number in each sex group.",
                  width = 6,
                  height = 620,
                  solidHeader = T,
                  title = "Distribution of length of hospital stay, according to sex"
                ),
                box(
                  plotOutput("lengthofstayAge", height = "500px"),
                  "This only includes cases with reported outcomes. The coloured areas indicate the kernel probability density of the observed data and the box plots show the median and interquartile range of the variable of interest. The upper value was truncated at 97.5%. The number below is the total number in each age group.",
                  width = 6,
                  height = 620,
                  solidHeader = T,
                  title = "Distribution of length of hospital stay, according to patient age group"
                )
              ),
              fluidRow(
                box(
                  plotOutput("admissiontoICU", height = "500px"),
                  "The figure displays data on only those cases with a reported ICU start date",
                  width = 6,
                  height = 600,
                  solidHeader = T,
                  title = "Distribution of time (in days) from hospital admission to ICU admission"
                ),
                box(
                  plotOutput("StatusbyTime", height = "500px"),
                  width = 6,
                  height = 600,
                  solidHeader = T,
                  title = "Distribution of patient status by number of days after admission"
                )
              )),
      tabItem(tabName = "vital_signs",
              fluidRow(
                box(
                  plotOutput("clinical_signs_vs_resp", height = "500px"),
                  width = 6,
                  height = 600,
                  solidHeader = T,
                  title = 'Respiratory rate (min)'
                ),
                box(
                  plotOutput("clinical_signs_vs_hr", height = "500px"),
                  width = 6,
                  height = 600,
                  solidHeader = T,
                  title = 'Heart rate (min)'
                ),
                box(
                  plotOutput("clinical_signs_vs_temp", height = "500px"),
                  width = 6,
                  height = 600,
                  solidHeader = T,
                  title = 'Temperature (Celsius)'
                ),
                box(
                  plotOutput("clinical_signs_vs_sysbp", height = "500px"),
                  width = 6,
                  height = 600,
                  solidHeader = T,
                  title = 'Systolic blood pressure (mmHg)'
                ),
                box(
                  plotOutput("clinical_signs_vs_oxysat", height = "500px"),
                  width = 6,
                  height = 600,
                  solidHeader = T,
                  title = 'Oxygen saturation on room air (%)'
                ),
                box(
                  plotOutput("clinical_signs_vs_oxysat_therapy", height = "500px"),
                  width = 6,
                  height = 600,
                  solidHeader = T,
                  title = 'Oxygen saturation on oxygen therapy (%)'
                )
              )),
      tabItem(
        tabName = "lab_results",
        fluidRow(
          box(
            plotOutput("lab_results_lab_crp", height = "500px"),
            width = 6,
            height = 600,
            solidHeader = T,
            title = 'C-Reactive Protein(CRP) (mg/L)'
          ),
          box(
            plotOutput("lab_results_lab_lym", height = "500px"),
            width = 6,
            height = 600,
            solidHeader = T,
            title = 'Lymphocytes (10^9/L)'
          ),
          box(
            plotOutput("lab_results_lab_neut", height = "500px"),
            width = 6,
            height = 600,
            solidHeader = T,
            title = 'Neutrophils (10^9/L)'
          ),
          box(
            plotOutput("lab_results_lab_wbc", height = "500px"),
            width = 6,
            height = 600,
            solidHeader = T,
            title = 'White Blood Cell(WBC) (10^9/L)'
          ),
          box(
            plotOutput("lab_results_lab_urean", height = "500px"),
            width = 6,
            height = 600,
            solidHeader = T,
            title = 'Urea (mmol/L)'
          ),
          box(
            plotOutput("lab_results_lab_pt", height = "500px"),
            width = 6,
            height = 600,
            solidHeader = T,
            title = 'Protrombin (s)'
          ),
          box(
            plotOutput("lab_results_lab_alt", height = "500px"),
            width = 6,
            height = 600,
            solidHeader = T,
            title = 'Alanine transaminase(ALT) (units/L)'
          ),
          box(
            plotOutput("lab_results_lab_bili", height = "500px"),
            width = 6,
            height = 600,
            solidHeader = T,
            title = 'Bilirubin (mmol/L)'
          ),
          box(
            plotOutput("lab_results_lab_ast", height = "500px"),
            width = 6,
            height = 600,
            solidHeader = T,
            title = 'Aspartate Transaminase(AST) (units/L)'
          )
        )
      ),
      tabItem(tabName = "country",
              fluidRow(
                box(
                  plotOutput("PatientbyCountry", height = "500px"),
                  "This reflects data on only those countries that are contributing data on patients who satisfy the inclusion criteria outlined in the summary section.",
                  width = 12,
                  height = 1000,
                  solidHeader = T,
                  title = "Distribution of patients by country"
                )
              )
      ),
      tabItem(tabName = "flowchart",
              fluidRow(
                box(includeMarkdown("markdown/Summary_dashboard_top.md"),
                    width = 12
                )
              ),
              fluidRow(
                box(flowchart(),
                    title = "Patients inclusion flowchart",
                    width = 12
                )
              ),
              fluidRow(
                box(includeMarkdown("markdown/Summary_dashboard_bottom.md"),
                    width = 12
                )
              )
      ),
      tabItem(tabName = "comorbidities_by_age",
              fluidRow(
                box(
                  img(src="example2.PNG",height=350),
                  height=450,
                  title = 'Example'
                )
              ),
              fluidRow(
                box(
                  plotlyOutput("age_comorbid_asthma", height = "500px"),
                  width = 6,
                  height = 400,
                  solidHeader = T,
                  title = "Proportion with asthma"
                ),
                box(
                  plotlyOutput("age_comorbid_malignant_neoplasm", height = "500px"),
                  width = 6,
                  height = 400,
                  solidHeader = T,
                  title = "Proportion with malignant neoplasma"
                ),
                box(
                  plotlyOutput("age_comorbid_obesity", height = "500px"),
                  width = 6,
                  height = 400,
                  solidHeader = T,
                  title = "Proportion with obesity"
                ),
                box(
                  plotlyOutput("age_comorbid_diabetes", height = "500px"),
                  width = 6,
                  height = 400,
                  solidHeader = T,
                  title = "Proportion with diabetes"
                ),
                box(
                  plotlyOutput("age_comorbid_dementia", height = "500px"),
                  width = 6,
                  height = 400,
                  solidHeader = T,
                  title = "Proportion with dementia"
                ),
                box(
                  plotlyOutput("age_comorbid_smoking", height = "500px"),
                  width = 6,
                  height = 400,
                  solidHeader = T,
                  title = "Proportion of smokers"
                ),
                box(
                  plotlyOutput("age_comorbid_hypertension", height = "500px"),
                  width = 6,
                  height = 400,
                  solidHeader = T,
                  title = "Proportion with hypertension"
                )
              )
      ),
      tabItem(
        tabName = "symptoms_by_age",
        fluidRow(
          box(
            img(src="example2.PNG",height=350),
            height=450,
            title = 'Example'
          )
        ),
        fluidRow(
          box(
            plotlyOutput("age_symptoms_history_of_fever", height = "500px"),
            width = 4,
            height = 400,
            solidHeader = T,
            title = "Proportion with fever"
          ),
          box(
            plotlyOutput("age_symptoms_cough", height = "500px"),
            width = 4,
            height = 400,
            solidHeader = T,
            title = "Proportion with cough"
          ),
          box(
            plotlyOutput("age_symptoms_cough_fever", height = "500px"),
            width = 4,
            height = 400,
            solidHeader = T,
            title = "Proportion with cough or fever"
          ),
          box(
            plotlyOutput("age_symptoms_shortness_of_breath", height = "500px"),
            width = 4,
            height = 400,
            solidHeader = T,
            title = "Proportion with shortness of breath"
          ),
          box(
            plotlyOutput("age_symptoms_cought_fever_shortness_of_breath", height = "500px"),
            width = 4,
            height = 400,
            solidHeader = T,
            title = "Proportion with cough, fever, or shortness of breath"
          ),
          box(
            plotlyOutput("age_symptoms_upper_respiratory_tract_symptoms", height = "500px"),
            width = 4,
            height = 400,
            solidHeader = T,
            title = "Proportion with upper respiratory tract symptoms"
          ),
          box(
            plotlyOutput("age_symptoms_altered_consciousness_confusion", height = "500px"),
            width = 4,
            height = 400,
            solidHeader = T,
            title = "Proportion with confusion"
          ),
          box(
            plotlyOutput("age_symptoms_constitutional", height = "500px"),
            width = 4,
            height = 400,
            solidHeader = T,
            title = "Proportion with constitutional symptoms"
          ),
          box(
            plotlyOutput("age_symptoms_vomiting_nausea", height = "500px"),
            width = 4,
            height = 400,
            solidHeader = T,
            title = "Proportion with vomiting or nausea"
          ),
          box(
            plotlyOutput("age_symptoms_diarrhoea", height = "500px"),
            width = 4,
            height = 400,
            solidHeader = T,
            title = "Proportion with diarrhoea"
          ),
          box(
            plotlyOutput("age_symptoms_abdominal_pain", height = "500px"),
            width = 4,
            height = 400,
            solidHeader = T,
            title = "Proportion with abdominal pain"
          )
        )
      ),
      tabItem(tabName = "tables",
              fluidRow(
                box(uiOutput("table_patient_characteristic"), height = 670),
                box(uiOutput("table_outcome_age_sex"), height = 670),
                box(uiOutput("table_symptoms"), height = 700),
                box(uiOutput("table_comorbidity"), height = 700),
                box(uiOutput("table_treatment")),
                box(uiOutput("table_key_times"))
              )
      )
    )
  )
)  




