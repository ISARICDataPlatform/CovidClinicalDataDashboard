#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#




dbHeader <- dashboardHeader(title = "COVID-19 Analysis Report",
                            tags$li(a(href = 'http://isaric.tghn.org/',
                                      img(src = 'ISARIClogoCream.png',
                                          title = "Company Home", height = "40px"),
                                      style = "padding-top:5px; padding-bottom:5px;"),
                                    class = "dropdown"
                                    # tags$style(".main-header {max-height: 40px}"),
                                    # tags$style(".main-header .logo {height: 40px;}"),
                                    # tags$style(".sidebar-toggle {height: 40px; padding-top: 1px !important;}"),
                                    # tags$style(".navbar {min-height:40px !important}")
                            ))

month.option.list <- as.list(c(NA, month.options))
names(month.option.list) <- c("Unknown", month.options)

# Define UI for application that draws a histogram
dashboardPage(skin = "black",
              dbHeader,
              dashboardSidebar(
                sidebarMenu(
                  menuItem("Overview", tabName = "flowchart", icon = icon("book")),
                  menuItem("Patient Characteristics", tabName = "patients", icon = icon("bed")),
                  menuItem("Symptoms at admission", tabName = "symptoms", icon = icon("stethoscope")),
                  menuItem("Comorbidities", tabName = "comorbidities", icon = icon("notes-medical")),
                  menuItem("Treatment", tabName = "treatment", icon = icon("pills")),
                  menuItem("ICU Treatment", tabName = "icu_treatment", icon = icon("heartbeat")),
                  menuItem("Hospital stays", tabName = "hospital_stays", icon = icon("hospital")),
                  menuItem("Vital signs", tabName = "vital_signs", icon = icon("file-medical-alt")),
                  menuItem("Laboratory results", tabName = "lab_results", icon = icon("vial")),
                  menuItem("Country Comparison", tabName = "country", icon = icon("globe")),
                  menuItem("Contributors", tabName = "contributors", icon = icon("users"))
                ),
                
                hr(),
                fluidRow(column(3, verbatimTextOutput("value")))
              ),
              dashboardBody(
                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
                  tags$style(HTML('
                 .btn-custom {background-color: #df0442; color:  #FFFFFF;}
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
                  ))),
                
                dropdown(
                  inputId = "controls",
                  icon = icon("gear"),
                  size = "sm",
                  status = "custom",
                  tooltip = tooltipOptions(title = "Click for data settings"),
                  options = list(`style` = "btm-custom"),
                  tags$h3("Controls"),
                  awesomeCheckboxGroup(
                    inputId = "sex", label = "Gender", status = "custom",
                    choices = list("Male" = "Male", "Female" = "Female", "Unknown" = NA),
                    selected = c("Male","Female",NA)
                  ),
                  sliderInput(inputId = "agegp10", label = "Age group",
                              min = 0, max = 120, step = 10, value = c(0,120), dragRange = T),
                  
                  pickerInput(
                    inputId = "country",
                    label = "Country",
                    choices = countries,
                    selected = countries,
                    options = list(
                      `actions-box` = TRUE),
                    multiple = TRUE
                  ),
                  awesomeCheckboxGroup(
                    inputId = "outcome", label = "Outcome", status = 'custom',
                    choices = list("Death" = "death", "Censored" = "censored", "Discharge" = "discharge"),
                    selected = c("death","censored","discharge")
                  ),
                  awesomeCheckboxGroup(
                    inputId = "icu_ever", label = "ICU admission", status = 'custom',
                    choices = list("Yes" = TRUE, "No" = FALSE),
                    selected = c(TRUE, FALSE)
                  ),
                  pickerInput(
                    inputId = "admission_date",
                    label = "Month of admission",
                    choices = month.option.list,
                    selected = c(NA, month.options),
                    options = list(
                      `actions-box` = TRUE),
                    multiple = TRUE
                  ),
                ),
                hr(),
                tabItems(
                  tabItem(tabName = "flowchart",
                          fluidRow(
                            box(grVizOutput("flowchart", height = "500px"),
                                width = 10, height = 600, solidHeader = T, title = 'Overview of cohort and outcomes'),
                           )
                  ),
                  tabItem(tabName = "patients",
                          fluidRow(
                            box(plotOutput("agePyramid", height = "300px"),
                                "Bar fills are outcome (death/discharge/censored) at the time of report.",
                                width = 6, height = 400, solidHeader = T, title = 'Age and sex distribution of patients'),
                            box(plotOutput("outcomesByAdmissionDate", height = "300px"),
                                "Bar fills are outcome (death/discharge/censored) at the time of report.",
                                width = 6, height = 400, solidHeader = T, title = 'Cumulative patient count by admission date')
                          )
                  ),
                  tabItem(tabName = "symptoms",
                          fluidRow(
                            box(plotOutput("symptomPrevalence", height = "500px"),
                                "Bars are labelled with the fraction of patients presenting with the symptom to the number of patients with data on the symptom recorded",
                                width = 6, height = 600, solidHeader = T, title = 'Prevalance of symptoms at admission'),
                            box(plotOutput("symptomUpset", height = "500px"),
                                width = 6, height = 600, solidHeader = T, title = 'Combinations of the five most common symptoms upon admission')
                          )
                  ),
                  tabItem(tabName = "comorbidities",
                          fluidRow(
                            box(plotOutput("comorbidityPrevalence", height = "500px"),
                                "Bars are labelled with the fraction of patients presenting with the comorbidity to the number of patients with data on the comorbidity recorded",
                                width = 6, height = 600, solidHeader = T, title = 'Prevalance of comorbidities'),
                            box(plotOutput("comorbidityUpset", height = "500px"),
                                width = 6, height = 600, solidHeader = T, title = 'Combinations of the five most common comorbidities')
                          )
                  ),
                  tabItem(tabName = "treatment",
                          fluidRow(
                            box(plotOutput("treatmentPrevalence", height = "500px"),
                                "Bars are labelled with the fraction of patients given the treatment to the number of patients with data on the treatment recorded",
                                width = 6, height = 600, solidHeader = T, title = 'Treatments given'),
                            box(plotOutput("treatmentUpset", height = "500px"),
                                width = 6, height = 600, solidHeader = T, title = 'Combinations of the five most common treatments given')
                          )
                  ),
                  tabItem(tabName = "icu_treatment",
                          fluidRow(
                            box(plotOutput("icuTreatmentPrevalence", height = "500px"),
                                "Bars are labelled with the fraction of patients given the treatment to the number of patients with data on the treatment recorded",
                                width = 6, height = 600, solidHeader = T, title = 'Treatments given'),
                            box(plotOutput("icuTreatmentUpset", height = "500px"),
                                width = 6, height = 600, solidHeader = T, title = 'Combinations of the five most common treatments given in the ICU'),
                            box(plotOutput("lengthofstayICU", height = "500px"),
                                "This only includes cases with reported outcomes. The coloured areas indicate the kernel probability density of the observed data and the box plots show the median and interquartile range of the variable of interest.",
                                width = 6, height = 600, solidHeader = T, title = 'Distribution of length of stay for patients admitted into Intensive Care Unit (ICU)')
                          )
                  ),
                  
                  tabItem(tabName = "contributors",
                          fluidRow(
                            box(leafletOutput("contributorsMap"), title = "Contributors Map", width = 12)
                          ),
                          fluidRow(
                            box(includeMarkdown("markdown/Contributor_listmap.md"), title = "Contributing Site List", width = 12)
                          ),
                  ),
                  tabItem(tabName = "hospital_stays",
                          fluidRow(
                            box(plotOutput("lengthofstaySex", height = "500px"),
                                "This only includes cases with reported outcomes. The coloured areas indicate the kernel probability density of the observed data and the box plots show the median and interquartile range of the variable of interest.",
                                width = 6, height=620, solidHeader = T, title = "Distribution of length of hospital stay, according to sex"),
                            box(plotOutput("lengthofstayAge", height = "500px"),
                                "This only includes cases with reported outcomes. The coloured areas indicate the kernel probability density of the observed data and the box plots show the median and interquartile range of the variable of interest.",
                                width = 6, height=620, solidHeader = T, title = "Distribution of length of hospital stay, according to patient age group")
                          ),
                          fluidRow(
                            box(plotOutput("admissiontoICU", height = "500px"),
                                "The figure displays data on only those cases with a reported ICU start date",
                                width = 6, height = 600, solidHeader = T, title = "Distribution of time (in days) from hospital admission to ICU admission"),
                            box(plotOutput("StatusbyTime", height = "500px"),
                                width = 6, height = 600, solidHeader = T, title = "Distribution of patient status by number of days after admission")
                          )
                  ),
                  tabItem(tabName = "vital_signs",
                          fluidRow(
                            box(plotOutput("clinical_signs_vs_resp", height = "500px"),
                                width = 6, height = 600, solidHeader = T, title = 'Respiratory rate (min)'),
                            box(plotOutput("clinical_signs_vs_hr", height = "500px"),
                                width = 6, height = 600, solidHeader = T, title = 'Heart rate (min)'),
                            box(plotOutput("clinical_signs_vs_temp", height = "500px"),
                                width = 6, height = 600, solidHeader = T, title = 'Temperature (Celsius)'),
                            box(plotOutput("clinical_signs_vs_sysbp", height = "500px"),
                                width = 6, height = 600, solidHeader = T, title = 'Systolic blood pressure (mmHg)'),
                            box(plotOutput("clinical_signs_vs_oxysat", height = "500px"),
                                width = 6, height = 600, solidHeader = T, title = 'Oxygen saturation in room air (%)')
                          )
                  )
                  ,
                  tabItem(tabName = "lab_results",
                          fluidRow(
                            box(plotOutput("lab_results_lab_crp", height = "500px"),
                                width = 6, height = 600, solidHeader = T, title = 'CRP (mg/L)'),
                            box(plotOutput("lab_results_lab_lym", height = "500px"),
                                width = 6, height = 600, solidHeader = T, title = 'Lymphocytes (10^9/L)'),
                            box(plotOutput("lab_results_lab_neut", height = "500px"),
                                width = 6, height = 600, solidHeader = T, title = 'Neutrophils (10^9/L)'),
                            box(plotOutput("lab_results_lab_wbc", height = "500px"),
                                width = 6, height = 600, solidHeader = T, title = 'WBC (10^9/L)'),
                            box(plotOutput("lab_results_lab_urean", height = "500px"),
                                width = 6, height = 600, solidHeader = T, title = 'Urea (mmol/L)'),
                            box(plotOutput("lab_results_lab_pt", height = "500px"),
                                width = 6, height = 600, solidHeader = T, title = 'Protrombin time (s)'),
                            box(plotOutput("lab_results_lab_alt", height = "500px"),
                                width = 6, height = 600, solidHeader = T, title = 'ALT (units/L)'),
                            box(plotOutput("lab_results_lab_aptt", height = "500px"),
                                width = 6, height = 600, solidHeader = T, title = 'APTT (s)'),
                            box(plotOutput("lab_results_lab_bili", height = "500px"),
                                width = 6, height = 600, solidHeader = T, title = 'Bilirubin (mmol/L)'),
                            box(plotOutput("lab_results_lab_ast", height = "500px"),
                                width = 6, height = 600, solidHeader = T, title = 'AST (units/L)')
                          )
                  ),
                  tabItem(tabName = "country",
                          fluidRow(
                            box(plotOutput("PatientbyCountry", height = "500px"),
                                "This reflects data on only those countries that are contributing data on patients who satisfy the inclusion criteria outlined in the summary section.",
                                width = 6, height = 600, solidHeader = T, title = "Distribution of patients by country")
                          )
                  )
                  
                ))
)

