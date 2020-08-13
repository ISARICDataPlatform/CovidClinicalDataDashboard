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
        filter(lower.ag.bound >= input$agegp5[1] & upper.ag.bound <= input$agegp5[2])
    })
    renderPlot(confidentiality.check(age.pyramid.reactive(), age.pyramid.plot), height = 300)
  }
  # output$tree <- renderTree({
  #   list(  'I lorem impsum'= list(
  #     'I.1 lorem impsum'   =  structure(list('I.1.1 lorem impsum'='1', 'I.1.2 lorem impsum'='2'),stselected=TRUE),
  #     'I.2 lorem impsum'   =  structure(list('I.2.1 lorem impsum'='3'), stselected=TRUE)))
  # })
}