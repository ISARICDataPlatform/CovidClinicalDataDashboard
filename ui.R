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

# Define UI for application that draws a histogram
dashboardPage(skin = "black",
              dbHeader,
              dashboardSidebar(
                sidebarMenu(
                  menuItem("Patient Characteristics", tabName = "patients", icon = icon("bed"))
                ),
                sidebarMenu(
                  menuItem("Symptoms at admission", tabName = "symptoms", icon = icon("stethoscope"))
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
                 .skin-black .main-sidebar .sidebar .sidebar-menu .active > a {color: #9e145f; border-left-color: #9e145f; background-color: #009cad;}
                 .skin-black .main-sidebar .sidebar .sidebar-menu li:hover  > a {color: #df0442; border-left-color: #df0442; background-color: #f7b538;}
                 .irs-bar, .irs-bar-edge, .irs-single, .irs-to, .irs-from, .irs-grid-pol {background: #df0442; border-color: #df0442;}
                 .content-wrapper, .right-side { background-color: #FFFFEA;}
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
                ),
                tabItem(tabName = "symptoms"
                )
              )
)

