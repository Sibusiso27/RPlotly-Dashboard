server <- function(session, input, output) {
  # renderPlotly() also understands ggplot2 objects!

  #autoInvalidate <- reactiveTimer(10000)
  #jobDetails <- reactive({#Compute FRN PV
  #    autoInvalidate()
  #    jobsFrame()
  #})
#
  #output$jobTable <- renderTable({
  #  jobDetails()
  #})  

  output$barPlot <- renderPlotly({
    barChart
  })

  output$piePlot <- renderPlotly({
    pieChart
  })
}