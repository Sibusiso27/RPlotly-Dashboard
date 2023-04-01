library(shiny)
library(dplyr)
library(plotly)
library(shinyWidgets)
library(shinydashboard)

options(shiny.trace = F, warn = -1)

source("plots/plots.R")

############### ----- HEADER + SIDEBAR ----- ############
header <- dashboardHeader(disable = F)
sidebar <- dashboardSidebar(disable = T)

############### ----- BODY ----- ############
#Formatting
css <- gsub('[\n\r]', "", readChar("style/formatting.css", file.info("style/formatting.css")$size))

#Body
body <- dashboardBody( 
  tags$head(
    tags$style(css),
    tags$link(href="https://fonts.googleapis.com/css?family=Montserrat", rel="stylesheet")),
  div(h2("Overnight Process Data Analysis", style="padding: 5px;font-weight: 600;"), id = "headTitle"),

  #Main Candle plot
  fluidRow(id = "row1", div(style = "height:600px;margin-left:-20px;padding-top: 10px;color: white;", 
               
               #Text1
               div(style = "height: auto;left: 48%;top: 9.7%;position: absolute;z-index: 10000;background: #ffffff00;padding: 2px;", 
                   div(style = "text-align: center;font-size: 11px;", "Runtime averages"),
                   div(style = "text-align: center;font-size: 16px;font-weight: bold;", 
                       textOutput("durationStats"))),
              
               #Text2
               div(style = "height: auto;left: 15%;top: 9.7%;position: absolute;z-index: 10000;width: 160px;background: #ffffff00;padding: 2px;", 
                   div(style = "text-align: center;font-size: 11px;", "This data is"),
                   div(style = "text-align: center;font-size: 11px;float: left;padding-top: 6px;", "from"),
                   div(style = "text-align: center;font-size: 16px;font-weight: bold;", textOutput("range1")),
                   div(style = "text-align: center;font-size: 11px;float: left;padding-top: 6px;", "to"),
                   div(style = "text-align: center;font-size: 16px;font-weight: bold;", textOutput("range2"))),

               #Text3
               div(style = "height: auto;left: 75%;top: 9.6%;position: absolute;z-index: 10000;width: 170px;background: #ffffff00;padding: 2px;", 
                   div(style = "text-align: center;font-size: 11px;", "On average the overnight process"),
                   div(style = "text-align: center;font-size: 11px;float: left;padding-top: 6px;", "starts at"),
                   div(style = "text-align: center;font-size: 16px;font-weight: bold;", textOutput("timeRange1")),
                   div(style = "text-align: center;font-size: 11px;float: left;padding-top: 6px;", "and ends at"),
                   div(style = "text-align: center;font-size: 16px;font-weight: bold;", textOutput("timeRange2")),
                   div(style = "text-align: center;font-size: 11px;", p("the following day"))),
              
               #Candle Plot
               plotlyOutput("barPlot"))),
  
  #Table result
  fluidRow(column(12, 
                  div(style = "height:240px;margin-left:-20px;padding-top: 10px;", 
                      plotlyOutput("startEndTableOut")
                      ),
                  div(style = "text-align: left;font-size: 11px;position: absolute;padding-left: 50px;", 
                      "*The following day"))),
  
  
  #Reports vs System availability
  fluidRow(
    column(7, div(style = "height:400px;margin-left:-20px;padding-top: 10px;",
                  #Reports Availability text 
                  div(id = "reports1",
                      div(style = "text-align: center;font-size: 11px;", "Reports are available"),
                      div(style = "text-align: center;font-size: 11px;", "on average from"),
                      textOutput("reportAvalTime"),
                      div(style = "text-align: center;font-size: 11px;", p("during the week"))),

                  #System availability text
                  div(id = "system1",
                      div(style = "text-align: center;font-size: 11px;", "System becomes available"),
                      div(style = "text-align: center;font-size: 11px;", "for usage on average from"),
                      textOutput("systemAvalTime"),
                      div(style = "text-align: center;font-size: 11px;", p("during the week"))),
                  plotlyOutput("reportSystemPlot"))),
    
    #Reports vs System availability Histograms
    column(5, div(style = "height:400px;margin-left:-20px;padding-top: 10px;", 
                  div(
                    style = "height: auto;left: 60%;top: 42%;position: absolute;z-index: 10000;width: 170px;color:#FF7F0E;background: #ffffff57;padding: 2px;", 
                      div(style = "text-align: center;font-size: 11px;", "Reports availability"),
                      div(style = "text-align: center;font-size: 11px;", "ranges between"),
                      textOutput("reportAvalRange"),
                      div(style = "text-align: center;font-size: 11px;", p("on weekdays"))),
                  div(
                    style = "height: auto;left: 20%;top: 22%;position: absolute;z-index: 10000;width: 170px;color:#2E71A3;background: #ffffff57;padding: 2px;", 
                      div(style = "text-align: center;font-size: 11px;", "System availability"),
                      div(style = "text-align: center;font-size: 11px;", "ranges between"),
                      textOutput("systemAvalRange"),
                      div(style = "text-align: center;font-size: 11px;", p("on weekdays"))),
                  plotlyOutput("reportSystemHistogram")))),
  
  #Main Weekday and Friday Pie charts
  fluidRow(column(6, 
                  div(style = "height:400px;margin-left:-20px;", 
                               div(style = "height: 100px;left: 29%;top: 32%;position: absolute;z-index: 10000;", 
                                   div(style = "text-align: center;font-size: 11px;", "The process averages"),
                                   textOutput("weekDayPieText"), 
                                   div(style = "text-align: center;font-size: 11px;", p("during the week"))), 
                               plotlyOutput("weekDayPie"))),
           column(6, 
                  div(style = "height:400px;margin-left:-10px;", 
                      div(style = "height: 100px;left: 29%;top: 32%;position: absolute;z-index: 10000;", 
                          div(style = "text-align: center;font-size: 11px;", "The process averages"),
                          textOutput("weekEndPieText"), 
                          div(style = "text-align: center;font-size: 11px;", p("during weekends"))), 
                      plotlyOutput("weekEndPie")))),
  
  #Each weekday Pie charts
  fluidRow(column(12, 
                  div(style = "height:350px;margin-left:-30px;float: left;", 
                      plotlyOutput("monDayPie")),
                  div(style = "height:350px;float: left;", 
                      plotlyOutput("tueDayPie")),
                  div(style = "height:350px;float: left;", 
                      plotlyOutput("wedDayPie")),
                  div(style = "height:350px;float: left;", 
                      plotlyOutput("thuDayPie")))
           ),
  
  #Phases line charts
  fluidRow(column(6,
                  div(class = "phasesPlots", plotlyOutput("reportPrepWeekDayChart")),
                  div(class = "phasesPlots", plotlyOutput("computWeekDayChart")),
                  div(class = "phasesPlots", plotlyOutput("reportRunWeekDayChart")),
                  div(class = "phasesPlots", plotlyOutput("controlWeekDayChart")),
                  div(class = "phasesPlots", plotlyOutput("initWeekDayChart")),
                  div(class = "phasesPlots", plotlyOutput("planWeekDayChart")),
                  div(class = "phasesPlots", plotlyOutput("maintenWeekDayChart")),
                  div(class = "phasesPlots", plotlyOutput("techWeekDayChart")),
                  div(class = "phasesPlots", plotlyOutput("houseWeekDayChart"))),
           column(6,
                  div(class = "phasesPlots", plotlyOutput("reportPrepWeekEndChart")),
                  div(class = "phasesPlots", plotlyOutput("computWeekEndChart")),
                  div(class = "phasesPlots", plotlyOutput("reportRunWeekEndChart")),
                  div(class = "phasesPlots", plotlyOutput("controlWeekEndChart")),
                  div(class = "phasesPlots", plotlyOutput("initWeekEndChart")),
                  div(class = "phasesPlots", plotlyOutput("planWeekEndChart")),
                  div(class = "phasesPlots", plotlyOutput("maintenWeekEndChart")),
                  div(class = "phasesPlots", plotlyOutput("techWeekEndChart")),
                  div(class = "phasesPlots", plotlyOutput("houseWeekEndChart")))),

  #Weekday Gantt
  fluidRow(div(style = "height:900px;margin-left:-20px;padding-top: 10px;", 
               plotlyOutput("weekDayScriptGanttPlot")
               )),
  
  #Weekend Gantt
  fluidRow(div(style = "height:900px;margin-left:-20px;padding-top: 10px;", 
               plotlyOutput("weekEndScriptGanttPlot")
               )),
  #Bubble
  fluidRow(column(12,
                  div(style = "height:400px;margin-left:-20px;padding-top: 10px;margin-bottom: 10px;", plotlyOutput("bubbleChart"),
                      img(id = "legend3", src='legend.png', align = "center"))))
  #fluidRow(column(12,
  #              div(style = "height:400px;margin-left:-20px;padding-top: 10px;margin-bottom: 10px;", jobsTreeMap)))
  
  #fluidRow(div(style = "height:500px;margin-left:-20px;padding-top: 10px;", plotlyOutput("boxWhiskerOutPlot"))),

  #fluidRow(div(style = "height:400px;margin-left:-20px;padding-top: 10px;", plotlyOutput("barChartPlot"))),
  
  #fluidRow(column(12, valueBox("", "", width = 12, color = "purple")))
  #          column(3, valueBox(format(Sys.Date(), "%d %b %y"), "Latest COB Results", width = 12, color = "yellow")),
  #          column(6, valueBox(format(Sys.Date(), "%d %b %y"), "Latest COB Results", width = 12, color = "red")))
)

############### ----- UI ----- ############
ui <- dashboardPage(
  header = header, 
  sidebar = sidebar, 
  body = body, 
  title = "Dashboard")

############### ----- SERVER ----- ############
server <- function(session, input, output) {
  output$barPlot <- renderPlotly({
    candlestick
  })
  
  #Table
  output$startEndTableOut <- renderPlotly({
    startEndTable
  })

  
  #Pie plots
  output$weekDayPie <- renderPlotly({
    weekDayPieChart
  })
  
  output$weekEndPie <- renderPlotly({
    weekendPieChart
  })
  
  #Weekday pie plots
  output$monDayPie <- renderPlotly({
    monDayPieChart
  })
  
  output$tueDayPie <- renderPlotly({
    tueDayPieChart
  })
  
  output$wedDayPie <- renderPlotly({
    wedDayPieChart
  })
  
  output$thuDayPie <- renderPlotly({
    thuDayPieChart
  })
  
  #output$friDayPie <- renderPlotly({
  #  friDayPieChart
  #})
  
  #histograms
  output$reportSystemPlot <- renderPlotly({
    reportsReady
  })
  
  output$reportSystemHistogram <- renderPlotly({
    reportsReadyHisto
  })
  
  #Gantt plots
  output$weekDayScriptGanttPlot <- renderPlotly({
    processGanttWeekDay
  })
  
  output$weekEndScriptGanttPlot <- renderPlotly({
    processGanttWeekDay
  })
  
  output$boxWhiskerOutPlot <- renderPlotly({
    boxWhiskerPlot
  })
  
  output$barChartPlot <- renderPlotly({
    barChart2
  })
  
  #WeekDay Phases
  output$initWeekDayChart <- renderPlotly({
    initWeekDayPlot
  })
  
  output$planWeekDayChart <- renderPlotly({
    planWeekDayPlot
  })
  
  output$computWeekDayChart <- renderPlotly({
    computWeekDayPlot
  })
  
  output$reportPrepWeekDayChart <- renderPlotly({
    reportPrepWeekDayPlot
  })
  
  output$reportRunWeekDayChart <- renderPlotly({
    reportRunWeekDayPlot
  })
  
  output$controlWeekDayChart <- renderPlotly({
    controlWeekDayPlot
  })
  
  output$maintenWeekDayChart <- renderPlotly({
    maintenWeekDayPlot
  })
  
  output$techWeekDayChart <- renderPlotly({
    techWeekDayPlot
  })
  
  output$houseWeekDayChart <- renderPlotly({
    houseWeekDayPlot
  })
  
  #WeekEnd Phases
  output$initWeekEndChart <- renderPlotly({
    initWeekEndPlot
  })
  
  output$planWeekEndChart <- renderPlotly({
    planWeekEndPlot
  })
  
  output$computWeekEndChart <- renderPlotly({
    computWeekEndPlot
  })
  
  output$reportPrepWeekEndChart <- renderPlotly({
    reportPrepWeekEndPlot
  })
  
  output$reportRunWeekEndChart <- renderPlotly({
    reportRunWeekEndPlot
  })
  
  output$controlWeekEndChart <- renderPlotly({
    controlWeekEndPlot
  })
  
  output$maintenWeekEndChart <- renderPlotly({
    maintenWeekEndPlot
  })
  
  output$techWeekEndChart <- renderPlotly({
    techWeekEndPlot
  })
  
  output$houseWeekEndChart <- renderPlotly({
    houseWeekEndPlot
  })

  #individual phaseses plots
  output$reportPrepJobsPlotChart <- renderPlotly({
    reportPrepJobsPlot
  })
  
  output$feedJobsChart <- renderPlotly({
    feedJobsPlot
  })

  output$reportJobsChart <- renderPlotly({
    reportJobsPlot
  })

  output$extractJobsChart <- renderPlotly({
    extractJobsPlot
  })

  output$bubbleChart <- renderPlotly({
    bubblePlot
  })

  # output$marketDataChart <- renderPlotly({
  #   marketDataPlot
  # })

  #output$jobsTreeMapChart <- renderTreeMap({
  #  jobsTreeMap
  #})
  
  output$weekDayPieText <- renderText({
    toTime2(weekDayAverage, T)
  })
  
  output$weekEndPieText <- renderText({
    toTime2(weekEndAverage, T)
  })
  
  output$durationStats <- renderText({
    toTime2(round(mean(endTime$.) - mean(startTime$START_TIME_MIDNIGHT), 2))
  })
  
  output$range1 <- renderText({
    cobDateRange[1]
  })
  
  output$range2 <- renderText({
    cobDateRange[2]
  })
  
  output$timeRange1 <- renderText({
    toTime(1245 + mean(startTime$START_TIME_MIDNIGHT) * 60)
  })
  
  output$timeRange2 <- renderText({
    toTime(mean(endTime$.) * 60 - 195)
  })
  
  output$reportAvalTime <- renderText({
    toTime(mean(reports$TOTAL_DURATION)*60 - 195)
  })
  
  output$systemAvalTime <- renderText({
    toTime(1245 + mean(system$TOTAL_DURATION)*60)
  })
  
  output$reportAvalRange <- renderText({
    paste(toTime(range(reports$TOTAL_DURATION)*60 - c(0, 195)), collapse = " and ")
  })

  output$systemAvalRange <- renderText({
    paste(toTime(range(system$TOTAL_DURATION)*60 - c(-1245, 195)), collapse = " and ")
  })
  
  output$tableChart <- renderTable({
    tableDat
  })
  
}

shinyApp(ui, server)