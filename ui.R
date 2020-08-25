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
                                      img(src = 'ISARIClogo.png',
                                          title = "Company Home", height = "40px"),
                                      style = "padding-top:5px; padding-bottom:5px;"),
                                    class = "dropdown"
                                    # tags$style(".main-header {max-height: 40px}"),
                                    # tags$style(".main-header .logo {height: 40px;}"),
                                    # tags$style(".sidebar-toggle {height: 40px; padding-top: 1px !important;}"),
                                    # tags$style(".navbar {min-height:40px !important}")
                            ))

# Define UI for application that draws a histogram
dashboardPage(skin = "black",
              dbHeader,
              dashboardSidebar(
                sidebarMenu(
                  menuItem("Patient Characteristics", tabName = "patients", icon = icon("bed"))
                ),
                
                hr(),
                fluidRow(column(3, verbatimTextOutput("value")))
              ),
              dashboardBody(
                tags$head(tags$style(HTML('
                 .btn-custom {background-color: #F70656; color:  #FFFFFF;}
                 .skin-black .main-header .logo {color: #F70656; font-weight: bold;}
                 .skin-black .main-sidebar .sidebar .sidebar-menu .active a {color: #F70656; border-left-color: #F70656;}
                 .irs-bar, .irs-bar-edge, .irs-single, .irs-to, .irs-from, .irs-grid-pol {background: #F70656; border-color: #F70656;}'
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
                  sliderInput(inputId = "agegp5", label = "Age group",
                              min = 0, max = 90, step = 5, value = c(0,120), dragRange = T),
                  
                  
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
                  )
                ),
                hr(),
                tabItem(tabName = "patients",
                        fluidRow(
                          box(plotOutput("agePyramid", height = "300px"),
                              "Bar fills are outcome (death/discharge/censored) at the time of report.",
                              width = 6, height = 400, solidHeader = T, title = 'Age and sex distribution of patients'),
                          box(plotOutput("outcomesByAdmissionDate", height = "300px"),
                              "Bar fills are outcome (death/discharge/censored) at the time of report.",
                              width = 6, height = 400, solidHeader = T, title = 'Cumulative patient count by admission date')
                        )
                )
              )
)

