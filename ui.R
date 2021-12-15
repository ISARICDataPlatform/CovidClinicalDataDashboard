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
        "Symptoms by region",
        tabName = "symptoms_by_region",
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
        "Comorbidities by region",
        tabName = "comorbidities_by_region",
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
      # menuItem(
      #   "Vaccinations",
      #   tabName = "vaccination",
      #   icon = icon("syringe")
      # ),
      menuItem(
        "Summary tables",
        tabName = "tables",
        icon = icon("table")
      ),
      menuItem(
        "Contributors",
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
                  "Animated map highlighting the growth of patient records chronologically from 1 February 2020 to 20 September 2021.",
                  solidHeader = T,
                  title = "Contributions of patient records"
                  ),
                box(
                  uiOutput("age_pyramid_video", align = "center"),
                  "Animated graphic showing the chronological patient outcomes according to their gender and age.",
                  solidHeader = T,
                  title = "Outcomes by age and gender"
                  )
              ),
              includeMarkdown("markdown/welcome_caveats.md")
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
                    "Graph shows the patient inclusion flowchart, among 706764 patients, 614497 (87%) were included. 88319 (14%) were admitted to ICU/HDU. ",
                    width = 12
                )
              ),
              fluidRow(
                box(includeMarkdown("markdown/Summary_dashboard_bottom.md"),
                    width = 12
                )
              )
      ),
      tabItem(tabName = "country",
              fluidRow(
                box(
                  plotOutput("PatientbyCountry", height = "500px"),
                  "Graph shows the distribution of patients by country. This reflects data on only those countries that are contributing data on patients who satisfy the inclusion criteria outlined in the summary section.",
                  width = 12,
                  height = 600,
                  solidHeader = T,
                  title = "Distribution of patients by country"
                ),
                box(
                  plotOutput("number_by_region", height = "500px"),
                  "Graph shows the number of participants recruited in each country is shown in the y-axis, which uses on the log-scale. Countries were grouped in different regions, and within regions were ordered according to the number of participants recruited.",
                  width = 12,
                  height = 600,
                  solidHeader = T,
                  title = "Distribution of patients by region"
                ),
                box(
                  plotOutput("date_by_region", height = "500px"),
                  "Graph shows the number of participants recruited since the beginning of pandemic is presented by calendar time. Region-specific numbers are represented by different colours. The vertical dashed line represents Different from the previous figure, the y-axis is on linear scale.",
                  width = 12,
                  height = 600,
                  solidHeader = T,
                  title = "Distribution of patients by region and time"
                )
              )
      ),
      tabItem(tabName = "patients",
              fluidRow(
                box(
                  plotOutput("agePyramid", height = "300px"),
                  "Graph shows the distribution of patients by age group and sex. Bar fills are outcomes at the time of report (green: death, orange: discharge, purple: lost-to-follow-up). The median age is 60 years ranging from 0 to 119 years. One-third of the patients are 70 years old and above.",
                  width = 6,
                  height = 450,
                  title = 'Age and sex distribution of patients'
                )),
                fluidRow(
                box(
                  plotOutput("outcomesByAdmissionDate", height = "400px"),
                  "Graph shows the cumulative patient count from the 1st epidemiological week of 2020 to the 38th epidemiological week of 2021. Bar fills are outcomes at the time of report (green: death, orange: discharge, purple: lost-to-follow-up).",
                  width = 10,
                  height = 500,
                  title = 'Cumulative patient count by admission date'
                )
              )),
      tabItem(tabName = "symptoms",
              fluidRow(
                box(
                  plotOutput("symptomPrevalence", height = "500px"),
                  "Graph shows the prevalence of symptoms at admission. The five most common symptoms at admission were shortness of breath, cough, history of fever, fatigue/malaise, and altered consciousness/confusion, accounting for 58.9%, 57.1%, 53.2%, 38.3%, 19.4% of patients with symptom recorded. Bars are labelled with the fraction of patients presenting with the symptom to the number of patients with data on the symptom recorded.",
                  width = 6,
                  height = 650,
                  solidHeader = T,
                  title = 'Prevalance of symptoms at admission'
                ),
                box(
                  plotOutput("symptomUpset", height = "500px"),
                  "Graph shows the distribution of combinations of the five most common symptoms, amongst all patients for whom these data were recorded. Filled and empty circles below the x-axis indicate the presence (filled) or absence (empty) of each symptom. The most common symptom combination is shortness of breath, cough, and history of fever (12.5%).",
                  width = 6,
                  height = 650,
                  solidHeader = T,
                  title = 'Combinations of the five most common symptoms upon admission'
                ),
                box(
                  plotOutput("symptomHeatmap", height = "500px"),
                  "Graph shows the correlation between symptoms. Fill colour is the phi correlation coefficient for each pair of symptoms, calculated amongst patients with recorded presence or absence of both.",
                  width = 6,
                  height = 600,
                  solidHeader = T,
                  title = 'Heatmap of symptoms at admission'
                ),
                box(
                  plotOutput("case_def"),
                  "Graph shows the proportion of 4 most common case definitions met by patients in our dataset, which varies by age.",
                  height = 500,
                  width=10,
                  title = "Proportion of patients that meet the 4 most common COVID-19 symptom case definitions by age"
                ),
              ),
              ),
      tabItem(
        tabName = "symptoms_by_age",
        fluidRow(
          box(
            img(src="example2.PNG",height=350),
            "Graph shows an example of how proportions are distributed amongst different age groups.",
            width = 5,
            title = 'Example'
          )
        ),
        fluidRow(
          box(
            plotlyOutput("age_symptoms_history_of_fever"),
            "Graph shows proportions for 207919 patients with fever at presentation by age group. The incidence of fever increases from 41.8% in children aged 10-19 years, to 63.6% in adults aged 50-59 years, then gradually decreases to 37.6% in adults aged 90 and above.",
            width = 4,
            height=550,
            solidHeader = T,
            title = "Proportion with fever"
          ),
          box(
            plotlyOutput("age_symptoms_cough"),
            "Graph shows proportions for 208021 patients with cough at presentation by age group. The incidence of cough increases from 31.7% in children aged 10-19 years, to 68.5% in adults aged 50-59 years, then gradually decreases to 42.1% in adults aged 90 and above.",
            width = 4,
            height=550,
            solidHeader = T,
            title = "Proportion with cough"
          ),
          box(
            plotlyOutput("age_symptoms_cough_fever"),
            "Graph shows proportions for 212882 patients with cough or fever at presentation by age group. The incidence of cough or fever increases from 51.3% in children aged 10-19 years, to 79.6% in adults aged 50-59 years, then gradually decreases to 56.0% in adults aged 90 and above.",
            width = 4,
            height=550,
            solidHeader = T,
            title = "Proportion with cough or fever"
          ),
          box(
            plotlyOutput("age_symptoms_shortness_of_breath"),
            "Graph shows proportions for 208740 patients with shortness of breath at presentation by age group. The incidence of shortness of breath increases from 23.5% in children aged 10-19 years, to 70.3% in adults aged 50-59 years, then gradually decreases to 46.0% in adults aged 90 and above years.",
            width = 4,
            height=580,
            solidHeader = T,
            title = "Proportion with shortness of breath"
          ),
          box(
            plotlyOutput("age_symptoms_cought_fever_shortness_of_breath"),
            "Graph shows proportions for 215051 patients with cough, fever, or shortness of breath at presentation by age group. The incidence of cough, fever, or shortness of breath increases from 54.7% in children aged 10-19 years, to 86.4% in adults aged 50-59 years, then gradually decreases to 66.3% in adults aged 90 and above.",
            width = 4,
            height=580,
            solidHeader = T,
            title = "Proportion with cough, fever, or shortness of breath"
          ),
          box(
            plotlyOutput("age_symptoms_upper_respiratory_tract_symptoms"),
            "Graph shows proportions for 181192 patients with upper respiratory tract symptoms at presentation by age group. The incidence of upper respiratory tract symptoms decreases from 16.1% in children aged 10-19 years, to 11.7% in adults aged 50-59 years, then further decreases to 3.2% in adults aged 90 and above.",
            width = 4,
            height=580,
            solidHeader = T,
            title = "Proportion with upper respiratory tract symptoms"
          ),
          box(
            plotlyOutput("age_symptoms_altered_consciousness_confusion"),
            "Graph shows proportions for 191747 patients with confusion at presentation by age group. The incidence of confusion increases from 7.5% in children aged 10-19 years, to 8.1% in adults aged 50-59 years, and further increases to 40.2% in adults aged 90 and above.",
            width = 4,
            height=550,
            solidHeader = T,
            title = "Proportion with confusion"
          ),
          box(
            plotlyOutput("age_symptoms_constitutional"),
            "Graph shows proportions for 197970 patients with constitutional symptoms at presentation by age group. The incidence of constitutional symptoms increases from 31.9% in children aged 10-19 years, to 53.4% in adults aged 50-59 years, then decreases to 35.4% in adults aged 90 and above.",
            width = 4,
            height=550,
            solidHeader = T,
            title = "Proportion with constitutional symptoms"
          ),
          box(
            plotlyOutput("age_symptoms_vomiting_nausea"),
            "Graph shows proportions for 195712 patients with vomiting or nausea at presentation by age group. The incidence of vomiting or nausea decreases from 23.5% in children aged 10-19 years, to 20.1% in adults aged 50-59 years, then decreases to 10.7% in adults aged 90 and above.",
            width = 4,
            height=550,
            solidHeader = T,
            title = "Proportion with vomiting or nausea"
          ),
          box(
            plotlyOutput("age_symptoms_diarrhoea"),
            "Graph shows proportions for 196108 patients with diarrhoea at presentation by age group. The incidence of diarrhoea increases from 10.3% in children aged 10-19 years, to 19.7% in adults aged 50-59 years, then decreases to 9.2% in adults aged 90 and above.",
            width = 4,
            height=550,
            solidHeader = T,
            title = "Proportion with diarrhoea"
          ),
          box(
            plotlyOutput("age_symptoms_abdominal_pain"),
            "Graph shows proportions for 191485 patients with abdominal pain at presentation by age group. The incidence of abdominal pain decreases from 19.1% in children aged 10-19 years, to 9.1% in adults aged 50-59 years, and further decreases to 6.4% in adults aged 90 and above.",
            width = 4,
            height=550,
            solidHeader = T,
            title = "Proportion with abdominal pain"
          )
        )
      ),
      tabItem(tabName = "symptoms_by_region",
              "This page shows the frequencies of most common symptoms in patients in the dataset. Each panel corresponds to a different region, and within region, symptoms that were present more often correspond to higher y-axis coordinates. To improve visualization, transparency decreases with increasing frequency. ",
              fluidRow(
                box(
                  plotOutput("symptom_1"),
                  width = 6,
                  height=550,
                  solidHeader = T
                ),
                box(
                  plotOutput("symptom_2"),
                  width = 6,
                  height=550,
                  solidHeader = T
                ),
                box(
                  plotOutput("symptom_3"),
                  width = 6,
                  height=550,
                  solidHeader = T
                ),
                box(
                  plotOutput("symptom_4"),
                  width = 6,
                  height=550,
                  solidHeader = T
                ),
                box(
                  plotOutput("symptom_5"),
                  width =6,
                  height=550,
                  solidHeader = T
                ),
                box(
                  plotOutput("symptom_6"),
                  width = 6,
                  height=550,
                  solidHeader = T
                ),
                box(
                  plotOutput("symptom_7"),
                  width =6,
                  height=550,
                  solidHeader = T
                )
              )
      ),
      tabItem(tabName = "comorbidities",
              fluidRow(
                box(
                  plotOutput("comorbidityPrevalence", height = "500px"),
                  "Graph shows the prevalence of comorbidities. Conditions present in at least 10% of cases are hypertension (31% of those reported), diabetes (22%), cardiac chronic disease (11%). Bars are labelled with the fraction of patients presenting with the comorbidity to the number of patients with data on the comorbidity recorded.",
                  width = 6,
                  height = 650,
                  solidHeader = T,
                  title = 'Prevalance of comorbidities'
                ),
                box(
                  plotOutput("comorbidityUpset", height = "500px"),
                  "Graph shows the distribution of combinations of the five most common such conditions, amongst all patients for whom these data were recorded. Filled and empty circles below the x-axis indicate the presence or absence of each comorbidity.",
                  width = 6,
                  height = 650,
                  solidHeader = T,
                  title = 'Combinations of the five most common comorbidities'
                )
              )),
      tabItem(tabName = "comorbidities_by_age",
              fluidRow(
                box(
                  img(src="example2.PNG",
                      height=350),
                  "Graph shows an example of how proportions are distributed amongst different age groups.",
                  width = 5,
                  title = 'Example'
                )
              ),
              fluidRow(
                box(
                  plotlyOutput("age_comorbid_asthma"),
                  "Graph shows proportions for 474580 patients with asthma at presentation by age group. The incidence of asthma increases from 6.0% in children aged 10-19 years, to 8.8% in adults aged 50-59 years, and decreases to 8.3% in adults aged 90 and above.",
                  width = 4,
                  height=550,
                  solidHeader = T,
                  title = "Proportion with asthma"
                ),
                box(
                  plotlyOutput("age_comorbid_malignant_neoplasm"),
                  "Graph shows proportions for 470966 patients with malignant neoplasma at presentation by age group. The incidence of malignant neoplasma increases from 1.2% in children aged 10-19 years, to 2.3% in adults aged 50-59 years, and increases to 11.1% in adults aged 90 and above.",
                  width = 4,
                  height=550,
                  solidHeader = T,
                  title = "Proportion with malignant neoplasma"
                ),
                box(
                  plotlyOutput("age_comorbid_obesity"),
                  "Graph shows proportion for 278556 patients with obesity at presentation by age group. The incidence of obesity increases from 5.1% in children aged 10-19 years, to 21.6% in adults aged 50-59 years, then decreases to 3.5% in adults aged 90 and above.",
                  width = 4,
                  height=550,
                  solidHeader = T,
                  title = "Proportion with obesity"
                ),
                box(
                  plotlyOutput("age_comorbid_diabetes"),
                  "Graph shows proportion for 491791 patients with diabetes at presentation by age group. The incidence of diabetes increases from 5.6% in children aged 10-19 years, to 30.1% in adults aged 50-59 years, then decreases to 22.6% in adults aged 90 and above.",
                  width = 4,
                  height=550,
                  solidHeader = T,
                  title = "Proportion with diabetes"
                ),
                box(
                  plotlyOutput("age_comorbid_dementia"),
                  "Graph shows proportions for 211562 patients with dementia at presentation by age group. The incidence of dementia increases from 0% in children aged 10-19 years, to 0.8% in adults aged 50-59 years, then further increases to 33.5% in adults aged 90 and above.",
                  width = 4,
                  height=550,
                  solidHeader = T,
                  title = "Proportion with dementia"
                ),
                box(
                  plotlyOutput("age_comorbid_smoking"),
                  "Graph shows proportions for 181050 smokers at presentation by age group. The incidence of smokers increases from 6.9% in children aged 10-19 years, to 27.3% in adults aged 50-59 years, then further increases to 34.7% in adults aged 90 and above.",
                  width = 4,
                  height=550,
                  solidHeader = T,
                  title = "Proportion of smokers"
                ),
                box(
                  plotlyOutput("age_comorbid_hypertension"),
                  "Graph shows proportions for 471096 patients with hypertension at presentation by age group. The incidence of hypertension increases from 2.3% in children aged 10-19 years, to 39.9% in adults aged 50-59 years, then further increases to 60.6% in adults aged 90 and above.",
                  width = 4,
                  height=550,
                  solidHeader = T,
                  title = "Proportion with hypertension"
                )
              )
      ),
      tabItem(tabName = "comorbidities_by_region",
              "This page shows the frequencies of most common comorbidities in patients in the dataset. Each panel corresponds to a different region, and within region, more frequent comorbidities correspond to higher y-axis coordinates. To improve visualization, transparency decreases with increasing frequency. ",
              fluidRow(
                box(
                  plotOutput("comorb_1"),
                  width = 6,
                  height=550,
                  solidHeader = T
                ),
                box(
                  plotOutput("comorb_2"),
                  width = 6,
                  height=550,
                  solidHeader = T
                ),
                box(
                  plotOutput("comorb_3"),
                  width = 6,
                  height=550,
                  solidHeader = T
                ),
                box(
                  plotOutput("comorb_4"),
                  width = 6,
                  height=550,
                  solidHeader = T
                ),
                box(
                  plotOutput("comorb_5"),
                  width =6,
                  height=550,
                  solidHeader = T
                ),
                box(
                  plotOutput("comorb_6"),
                  width = 6,
                  height=550,
                  solidHeader = T
                ),
                box(
                  plotOutput("comorb_7"),
                  width =6,
                  height=550,
                  solidHeader = T
                )
              )
      ),
      tabItem(tabName = "vital_signs",
              fluidRow(
                box(
                  plotOutput("clinical_signs_vs_resp", height = "500px"),
                  "Graph shows median respiratory rates for 201231 patients at presentation by age group. The median level was 35 breaths per minute in children under 10, and 20 breaths per minute in children 10-19, increases to 22 breaths per minute in adults aged 50-59 years, and decreases to 20 breaths per minute in adults aged 90 and above.",
                  width = 6,
                  height = 650,
                  solidHeader = T,
                  title = 'Respiratory rate (min)'
                ),
                box(
                  plotOutput("clinical_signs_vs_hr", height = "500px"),
                  "Graph shows median heart rates for 200478 patients at presentation by age group. The median level decreases from 140 beats per minute in children under 10 years, to 93 beats per minute in adults aged 50-59 years, and further decreases to 83 beats per minute in adults aged 90 and above.",
                  width = 6,
                  height = 650,
                  solidHeader = T,
                  title = 'Heart rate (min)'
                ),
                box(
                  plotOutput("clinical_signs_vs_temp", height = "500px"),
                  "Graph shows median temperature levels for 208807 patients at presentation by age group. The median level increases from 36.9 celsius in children aged 10-19 years, to 37.2 celsius in adults aged 50-59 years, then decreases to 36.8 celsius in adults aged 90 and above.",
                  width = 6,
                  height = 650,
                  solidHeader = T,
                  title = 'Temperature (Celsius)'
                ),
                box(
                  plotOutput("clinical_signs_vs_sysbp", height = "500px"),
                  "Graph shows median systolic blood pressure levels for 207559 patients at presentation by age group. The median level increases from 119 mmHg in children aged 10-19 years, to 129 mmHg in adults aged 50-59 years, and further increases to 134 mmHg in adults aged 90 and above.",
                  width = 6,
                  height = 650,
                  solidHeader = T,
                  title = 'Systolic blood pressure (mmHg)'
                ),
                box(
                  plotOutput("clinical_signs_vs_oxysat", height = "500px"),
                  "Graph shows oxygen saturation on room air levels for 132492 patients at presentation by age group. The median level decreases from 98% in children aged 10-19 years, to 95% in adults aged 50-59 years, and further increases to 96% in adults aged 90 and above.",
                  width = 6,
                  height = 650,
                  solidHeader = T,
                  title = 'Oxygen saturation on room air (%)'
                ),
                box(
                  plotOutput("clinical_signs_vs_oxysat_therapy", height = "500px"),
                  "Graph shows oxygen saturation on oxygen therapy levels for 67316 patients at presentation by age group. The median level decreases from 96% in children aged 10-19 years, to 94% in adults aged 60-79 years, then increases to 95% in adults aged 80 and above.",
                  width = 6,
                  height = 650,
                  solidHeader = T,
                  title = 'Oxygen saturation on oxygen therapy (%)'
                )
              )),
      tabItem(
        tabName = "lab_results",
        fluidRow(
          box(
            plotOutput("lab_results_lab_crp", height = "500px"),
            "Graph shows C-Reactive Protein levels in 13291 patients at presentation by age group. The median level is 9.2 mg/L in children under 10 years, 7.7 mg/L in children aged 10-19 years, and around 7 mg/L in adults aged 20 and above.",
            width = 6,
            height = 650,
            solidHeader = T,
            title = 'C-Reactive Protein(CRP) (mg/L)'
          ),
          box(
            plotOutput("lab_results_lab_lym", height = "500px"),
            "Graph shows lymphocyte levels in 124926 patients at presentation by age group. The median level is 2.9 *10^9/L in children under 10 years, 1.5 *10^9/L in children aged 10-19 years, and decreases to 0.8 *10^9/L in adults aged 70 and above.",
            width = 6,
            height = 650,
            solidHeader = T,
            title = 'Lymphocytes (10^9/L)'
          ),
          box(
            plotOutput("lab_results_lab_neut", height = "500px"),
            "Graph shows neutrophil levels in 124042 patients at presentation by age group. The median level increases from 3.8 *10^9/L in children under 10 to 6 *10^9/L in adults aged 90 and above.",
            width = 6,
            height = 650,
            solidHeader = T,
            title = 'Neutrophils (10^9/L)'
          ),
          box(
            plotOutput("lab_results_lab_wbc", height = "500px"),
            "Graph shows white blood cell levels in 132491 patients at presentation by age group. The median level decreases from 9.2 *10^9/L in children under 10 to 7 *10^9/L in adults aged 30-60 years, and increases to 7.7 *10^9/L in adults aged 90 and above.",
            width = 6,
            height = 650,
            solidHeader = T,
            title = 'White Blood Cell(WBC) (10^9/L)'
          ),
          box(
            plotOutput("lab_results_lab_urean", height = "500px"),
            "Graph shows urea levels in 119362 patients at presentation by age group. The median level increases from 3.5 mmol/L in children under 10 to 10 mmol/L in adults aged 90 and above.",
            width = 6,
            height = 650,
            solidHeader = T,
            title = 'Urea (mmol/L)'
          ),
          box(
            plotOutput("lab_results_lab_pt", height = "500px"),
            "Graph shows protrombin time in 68563 patients at presentation by age group. The median time is around 12-13 s across all age groups.",
            width = 6,
            height = 650,
            solidHeader = T,
            title = 'Protrombin (s)'
          ),
          box(
            plotOutput("lab_results_lab_alt", height = "500px"),
            "Graph shows ALT levels in 82808 patients at presentation by age group. The median level is 24 units/L in children under 10, 19 units/L in children aged 10-19, increases to 37 units/L in adults aged 40-49, and then decreases to 18 units/L in adults aged 90 and above.",
            width = 6,
            height = 650,
            solidHeader = T,
            title = 'Alanine transaminase(ALT) (units/L)'
          ),
          box(
            plotOutput("lab_results_lab_bili", height = "500px"),
            "Graph shows bilirubin levels in 86564 patients at presentation by age group. The median level is 8 mmol/L for adults under 49 years, 9 mmol/L for adults aged 50-69 years, and 10 mmol/L for adults aged 70 and above.",
            width = 6,
            height = 650,
            solidHeader = T,
            title = 'Bilirubin (mmol/L)'
          ),
          box(
            plotOutput("lab_results_lab_ast", height = "500px"),
            "Graph shows AST levels in 19210 patients at presentation by age group. The median level is 39 units/L for children under 10, 23 units/L for children aged 10-19, increases to 43 units/L for adults aged 60-69, then decreases to 35 units/L for adults aged 90 and above.",
            width = 6,
            height = 650,
            solidHeader = T,
            title = 'Aspartate Transaminase(AST) (units/L)'
          )
        )
      ),
      tabItem(tabName = "treatment",
              fluidRow(
                box(
                  plotOutput("treatmentPrevalence", height = "500px"),
                  "Graph shows the proportion of patients receiving each treatment. Antibiotics were given to 56.4% of patients. Bars are labelled with the fraction of patients given the treatment to the number of patients with data on the treatment recorded. Oxygen therapy variable includes high flow nasal cannula, non-invasive ventilation, and invasive ventilation.",
                  width = 6,
                  height = 650,
                  solidHeader = T,
                  title = 'Treatments given'
                ),
                box(
                  plotOutput("treatmentUpset", height = "500px"),
                  "Graph shows the distribution of combinations of 5 most common treatments, across all patients with completed hospital stay and recorded treatment data. Filled and empty circles below the x-axis indicate treatments that were and were not administered respectively. Oxygen therapy variable includes high flow nasal cannula, non-invasive ventilation, and invasive ventilation.",
                  width = 6,
                  height = 650,
                  solidHeader = T,
                  title = 'Combinations of the five most common treatments given'
                )
              )),
      tabItem(tabName = "icu_treatment",
              fluidRow(
                box(
                  plotOutput("icuTreatmentPrevalence", height = "500px"),
                  "Graph shows the proportion of treatments receiving each treatment in ICU. 91.6% received antibiotics and 37.4% antivirals. 95.4% received some degree of oxygen supplementation, of which, 48.2% received NIV and 60.5% IMV. Bars are labelled with the fraction of patients given the treatment to the number of patients with data on the treatment recorded. Oxygen therapy variable includes high flow nasal cannula, non-invasive ventilation, and invasive ventilation.",
                  width = 6,
                  height = 650,
                  solidHeader = T,
                  title = 'Treatments given'
                ),
                box(
                  plotOutput("icuTreatmentUpset", height = "500px"),
                  "Graph shows the distribution of combinations of 5 most common treatments in ICU. Filled and empty circles below the x-axis indicate treatments that were and were not administered respectively. Oxygen therapy variable includes high flow nasal cannula, non-invasive ventilation, and invasive ventilation.",
                  width = 6,
                  height = 650,
                  solidHeader = T,
                  title = 'Combinations of the five most common treatments given in the ICU'
                ),
                box(
                  plotOutput("lengthofstayICU", height = "500px"),
                  "Graph shows the distribution of length of stay for patients admitted into ICU. The duration of stay in ICU had a mean of 10 days and a median of 7 days. This only includes cases with reported outcomes. The coloured areas indicate the kernel probability density of the observed data and the box plots show the median and interquartile range of the variable of interest. The upper value was truncated at 97.5%. The number below is the total number in each group.",
                  width = 6,
                  height = 650,
                  solidHeader = T,
                  title = 'Distribution of length of stay for patients admitted into Intensive Care Unit (ICU)'
                )
              )),
      tabItem(tabName = "hospital_stays",
              fluidRow(
                box(
                  plotOutput("lengthofstaySex", height = "500px"),
                  "Graph shows the distribution of length of hospital stay by sex. This only includes cases with reported outcomes. The coloured areas indicate the kernel probability density of the observed data and the box plots show the median and interquartile range of the variable of interest. The upper value was truncated at 97.5%. The number below is the total number in each sex group.",
                  width = 6,
                  height = 650,
                  solidHeader = T,
                  title = "Distribution of length of hospital stay, according to sex"
                ),
                box(
                  plotOutput("lengthofstayAge", height = "500px"),
                  "Graph shows the distribution of length of hospital stay by age group. This only includes cases with reported outcomes. The coloured areas indicate the kernel probability density of the observed data and the box plots show the median and interquartile range of the variable of interest. The upper value was truncated at 97.5%. The number below is the total number in each age group.",
                  width = 6,
                  height = 650,
                  solidHeader = T,
                  title = "Distribution of length of hospital stay, according to patient age group"
                )
              ),
              fluidRow(
                box(
                  plotOutput("admissiontoICU", height = "500px"),
                  "Graph shows the distribution of time from hospital admission to ICU admission. The figure displays data on only those cases with a reported ICU start date. 55.4% of ICU admissions occur within the first day at the hospital.",
                  width = 6,
                  height = 650,
                  solidHeader = T,
                  title = "Distribution of time (in days) from hospital admission to ICU admission"
                ),
                box(
                  plotOutput("StatusbyTime", height = "500px"),
                  "Graph shows the distribution of patient status by number of days after admission. The overall estimated case fatality ratio (CFR) is 25.3% (95%CI 25.1-25.4) and is 37.8% (95%CI 37.5-38.2) for patients admitted to ICU. The observed median duration from hospital admission to outcome (death or discharge) was 8 days.",
                  width = 6,
                  height = 650,
                  solidHeader = T,
                  title = "Distribution of patient status by number of days after admission"
                )
              )),
      tabItem(tabName = "tables",
              fluidRow(
                box(uiOutput("table_patient_characteristic"), height = 670),
                box(uiOutput("table_outcome_age_sex"), height = 670),
                box(uiOutput("table_symptoms"), height = 700),
                box(uiOutput("table_comorbidity"), height = 700),
                box(uiOutput("table_treatment")),
                box(uiOutput("table_key_times"))
              )
      ),
      tabItem(tabName = "contributors",
              fluidRow(
                box(
                  tags$iframe(seamless="seamless",src="https://www.google.com/maps/d/u/2/embed?mid=1m6gbS4Iqb_4G6EX-YuEJnjF3KxYd38fd&ll=22.763700700551322%2C-118.77549999999997&z=2",height=500,width=1400),
                  width = 12
                )
              ),
              fluidRow(
                box(
                  includeMarkdown("markdown/Contributor_listmap.md"),
                  title = "Contributing Site List",
                  width = 12
                )
              ))
    )
  )
)  




