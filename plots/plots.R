
################################## Package Import #################################

library(plotly)
library(colorRamps)
library(reshape2)
library(treemap)
library(zoo)
library(dplyr)

############################ Data + Colors + Helpers ##############################

#Helpers
source(file = "misc/helpers.R")

#Colors
phaseColors <- c("#00A2E8", "#D62728", "#7F7F7F", "#A349A4", "#3F48CC", "#880015", "#FF7F27", "#22B14C", "#BF5B16")
names(phaseColors) <- c("Computation", "Control", "Housekeep", "Initiation", "Maintenance", "Planning", 
                                                  "Report Prep", "Report Run", "Technical")
#Data
#source(file = "data/readClean.R")
dat <- read.csv("data/datBasicClean.csv", header = T, sep = ",", stringsAsFactors = F)

#Convert date time string to date time types
dat$COB_DATE <- as.Date(dat$COB_DATE)
dat$START_TIME <- as.POSIXct(dat$START_TIME, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
dat$END_TIME <- as.POSIXct(dat$END_TIME, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")


##################################### Candle Plot #################################

#Entire date range
cobDateRange <- format(range(dat$COB_DATE), "%b %Y")

#Only consider start time from 20:45 and convert to hours 
#Start time for each day
startTime <- dat[which(dat$PROCESS_NAME == 'START_01'), c("COB_DATE", "TOTAL_DURATION")]
#startTime$TOTAL_DURATION <- (startTime$TOTAL_DURATION - 74700)/3600

monthLabel <- unique(format(startTime$COB_DATE,"%b %Y"))
n1 <- length(monthLabel)
n2 <- length(startTime$COB_DATE)

monthSource <- as.vector(sapply(
  split(startTime$COB_DATE, rep(1:(n2/(n2/n1)), each=round(n2/n1), length.out=n2)), function(i) i[ceiling(length(i)/2)]))

monthSource <- format(as.Date(monthSource), "%d %b %Y")

startTime$COB_DATE <- format(as.Date(startTime$COB_DATE), "%d %b %Y")

startTime$COB_DATE <- factor(startTime$COB_DATE,
                             levels = startTime$COB_DATE[order(as.Date(startTime$COB_DATE, "%d %b %Y"),decreasing = FALSE)])

startTime$color <- blue2green(nrow(startTime))

endTime <- dcast(data = dat, formula = COB_DATE ~ ., fun.aggregate = max, value.var = "TOTAL_DURATION")
endTime$COB_DATE <- format(as.Date(endTime$COB_DATE), "%d %b %Y")
endTime$COB_DATE <- factor(endTime$COB_DATE,
                           levels = endTime$COB_DATE[order(as.Date(endTime$COB_DATE, "%d %b %Y"),decreasing = FALSE)])

nendTime <- 1:nrow(endTime)
nstartTime <- 1:nrow(startTime)

candlestick <- plot_ly(x = dat$COB_DATE, height = 600, hoverlabel = list(font = list(family = "Montserrat, sans-serif", size = 9)))  %>%
  add_trace(x = endTime$COB_DATE, y = endTime$., type = 'bar', name = "End", marker = list(color = '#1f90b4')) %>%
  add_trace(x = startTime$COB_DATE, y = startTime$TOTAL_DURATION, type = 'bar', name = "Start", marker = list(color = '#ca2341')) %>%
  add_lines(x = endTime$COB_DATE, y = stats::filter(endTime$., rep(1/10, 10), side=2), 
           line=list(shape = 'spline', smoothing = 1.5, dash = "solid", width = 3, color = "#37c837"), showlegend = T, 
           name = "<i>end moving average</i>") %>%
  add_lines(x = startTime$COB_DATE, y = stats::filter(startTime$TOTAL_DURATION, rep(1/10, 10), side=2), 
            line=list(shape = 'spline', smoothing = 1.5, dash = "solid", width = 1, color = "#b8de41"), showlegend = T,
            name = "<i>start moving average</i>") %>%
  add_lines(x = endTime$COB_DATE, 
            y = predict(lm(endTime$. ~ nendTime + I(nendTime^2) + I(nendTime^3) + I(nendTime^4) + I(nendTime^5))), 
            line = list(dash = "solid", width = 5, color = "#a02ca0"), showlegend = T, name = "<i>quintic trend</i>") %>%
  add_lines(x = endTime$COB_DATE, 
            y = mean(endTime$.), 
            line = list(dash = "solid", width = 4, color = "#FFA000"), showlegend = T, name = "<i>end average</i>") %>%
  # add_lines(x = startTime$COB_DATE, 
  #           y = mean(startTime$TOTAL_DURATION), 
  #           line = list(dash = "solid", width = 1, color = "#c0c0c0"), showlegend = T, name = "<i>start average</i>") %>%
  layout(barmode='overlay',
         title = "<b>Overnight Process Total Duration</b>",
         titlefont = list(family = "Montserrat, sans-serif", size = 16, color = 'rgb(0, 0, 0)'),
         xaxis = list(title = "Date", tickfont = list(family = "Montserrat, sans-serif", size = 12, color = 'rgb(0, 0, 0)'),
                      titlefont = list(family = "Montserrat, sans-serif", size = 14, color = 'rgb(0, 0, 0)'),
                      rangeslider = list(visible = FALSE), showline = T, nticks = n1, showgrid = T,
                      tickvals = monthSource,
                      ticktext = monthLabel),
         yaxis = list(title = "Time", showline = T, side = "left",
                      tickfont = list(family = "Montserrat, sans-serif", size = 12, color = 'rgb(0, 0, 0)'),
                      titlefont = list(family = "Montserrat, sans-serif", size = 14, color = 'rgb(0, 0, 0)'), 
                      zeroline = F, nticks = 13, tickvals = seq(0, 25, 2), ticktext = toTime(1245 + seq(0, 25, 2)*60, T)),
         legend = list(bgcolor = "rgba(255, 255, 255, 0.8)", orientation = "h", x = 0.2, y = 0.98, 
                       font = list(size = 14, family = "Montserrat, sans-serif")),
         bargap = 0, bargroupgap = 0)
         

###################### COB Average Start and End time per day #######################

cobDate <- unique(cbind(dat$COB_DATE, dat$DAY))

#EndTime
endTimeCOB <- dcast(data = dat, formula = COB_DATE ~ ., fun.aggregate = max, value.var = "TOTAL_DURATION")
endTimeCOB$DAY <- cobDate[,2]
names(endTimeCOB) <- c("COB_DATE", "TIME", "DAY")
endAverage <- apply(dcast(data = endTimeCOB, formula = COB_DATE ~ DAY, 
                          fun.aggregate = sum, value.var = "TIME")[,-1], 2, sum)
endAverage <- endAverage/table(endTimeCOB$DAY)

endTimeRange <- dcast(data = endTimeCOB, formula = COB_DATE ~ DAY, 
                      fun.aggregate = sum, value.var = "TIME")[,-1]
endTimeRange[endTimeRange < 1] <- NA
endTimeRange <- apply(endTimeRange, 2, range, na.rm = T)
endTimeRange <- apply(endTimeRange*60  - 195, 2, toTime, day = T)
endTimeRange <- apply(endTimeRange, 2, function(x) paste0(x[1], " / ", x[2]))

#StartTime
startTimeCOB <- dcast(data = dat[which(dat$PROCESS_NAME == 'START_01'),], formula = COB_DATE ~ DAY, 
                      fun.aggregate = sum, value.var = "START_TIME_MIDNIGHT")[,-1]
startTimeCOB[startTimeCOB < 74700  & startTimeCOB <= 86400] <- NA
startAverage <- apply(startTimeCOB, 2, mean, na.rm=TRUE)
startAverage <- (startAverage - 74700)/3600

startTimeRange <- apply(startTimeCOB, 2, range, na.rm = T)
startTimeRange <- apply(1245 + 60*(startTimeRange - 74700)/3600, 2, toTime, day = T)
startTimeRange <- apply(startTimeRange, 2, function(x) paste0(x[1], " / ", x[2]))

#Reorder columns
endAverage <- endAverage[c(2,4,5,3,1)]
startAverage <- startAverage[c(2,4,5,3,1)]
startTimeRange <- startTimeRange[c(2,4,5,3,1)]
endTimeRange <- endTimeRange[c(2,4,5,3,1)]

startEndTable <- plot_ly(
    type = 'table',
    header = list(
      values = c('',paste0(rep('<b>', 5), paste0(names(endAverage), rep('<b>', 5)))),
      line = list(color = '#1A2229'),
      fill = list(color = '#1F77B4'),
      align = rep('center', 6),
      height = 40,
      font = list(color = 'white', size = 12)
    ),
    cells = list(
      values = t(cbind(
        c('Average Start Time', 'Start Time min / max', 'Average End Time*', 
          'End Time min / max*', 'Average Duration'),
        rbind(
          toTime(1245 + startAverage*60,T), #Average start time
          startTimeRange,
          toTime(endAverage*60 - 195, T), #Average end time
          endTimeRange, 
          toTime2(round(endAverage - startAverage,2))))),
      line = list(color = '#1A2229'),
      fill = list(color = c('#EE9F15', 'white')),
      align = rep('center', 6),
      height = 30,
      font = list(color = c('white', '#1B2127'), size = 12)
    )) %>%
  layout( title = "<b>Overnight Process Start and End times per day (with min and max times)</b>",
          titlefont = list(family = "Montserrat, sans-serif", size = 16, color = 'rgb(0, 0, 0)'))


################################### Pie Plots #####################################

dat$COB_DATE <- as.Date(dat$COB_DATE)
cobDateRange <- format(range(dat$COB_DATE), "%d %b %Y")
weekDay <- apply(dcast(data = dat[which(dat$DAY != 'FRIDAY'),], formula = COB_DATE ~ PROCESS_PHASE, 
                 fun.aggregate = sum, value.var = "PROCESS_DURATION")[,-1], 2, mean)
weekEnd <- apply(dcast(data = dat[which(dat$DAY == 'FRIDAY'),], formula = COB_DATE ~ PROCESS_PHASE, 
                 fun.aggregate = sum, value.var = "PROCESS_DURATION")[,-1], 2, mean)

weekDayAverage <- mean((endAverage - startAverage)[1:4])
weekEndAverage <- (endAverage - startAverage)[5]

weekDay <- weekDay/sum(weekDay)*weekDayAverage
weekEnd <- weekEnd/sum(weekEnd)*weekEndAverage

weekEnd <- weekEnd[-which(names(weekEnd) == "Technical")]
weekDay <- weekDay[-which(names(weekDay) == "Technical")]

weekDayPieChart <- plot_ly(labels = names(weekDay), values = weekDay, type = "pie", height = 410,
                           #textinfo= "text",
                           automargin = T,
                           text = paste0("<b>", toTime2(weekDay, T), "</b>"),
                           domain = list(x = c(0, 1), y = c(0, 1)), hole = 0.5,
                           sort = T, pull = 0.02, textposition = 'outside', showlegend = T,
                           outsidetextfont = list(size = 14, family = "Montserrat, sans-serif", 
                                                  color = phaseColors[1:length(weekDay)]),
                           hoverlabel = list(font = list(family = "Montserrat, sans-serif", size = 9)),
                           marker = list(colors = phaseColors[1:length(weekDay)])) %>%
  layout(title = '<b>WeekDay Average Runtime per Phase</b>', showlegend = T, 
         titlefont = list(family = "Montserrat, sans-serif", size = 16, color = 'rgb(0, 0, 0)'),
         xaxis = list(showgrid = FALSE, zeroline = F, showticklabels = T),
         yaxis = list(showgrid = FALSE, zeroline = F, showticklabels = T),
         legend = list(bgcolor = "rgba(255, 255, 255, 0.8)", x = 1.2, y = 0.5, 
                       font = list(size = 14, family = "Montserrat, sans-serif")))

weekendPieChart <- plot_ly(labels = names(weekEnd), values = weekEnd, type = "pie", height = 400,
                           domain = list(x = c(0, 1), y = c(0, 1)), hole = 0.5,
                           automargin = T,
                           textposition ="outside",
                           #textinfo= "text",
                           text = paste0("<b>", round(weekEnd, 2), " hrs</b>"),
                           sort = T, pull = 0.02, showlegend = T,
                           insidetextfont = list(size = 14, family = "Montserrat, sans-serif", color = "#FFFFFF"),
                           outsidetextfont = list(size = 14, family = "Montserrat, sans-serif", 
                                                  color = phaseColors[1:length(weekEnd)]),
                           hoverlabel = list(font = list(family = "Montserrat, sans-serif", size = 9)),
                           marker = list(colors = phaseColors[1:length(weekEnd)])) %>%
  layout(title = '<b>Friday Average Runtime per Phase</b>', showlegend = T,
         titlefont = list(family = "Montserrat, sans-serif", size = 16, color = 'rgb(0, 0, 0)'),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         legend = list(bgcolor = "rgba(255, 255, 255, 0.8)", x = 1.2, y = 0.5, 
                       font = list(size = 14, family = "Montserrat, sans-serif"))) 


############################ COB Per day per phase ##################################

sortedNames <- names(sort(weekDay, decreasing = T))
sortedColors <- phaseColors[1:length(weekDay)]

piePlot <- function(day = "MONDAY", showLegend = F, w = 100) {
  dayData <- apply(dcast(data = dat[which(dat$DAY == day),], formula = COB_DATE ~ PROCESS_PHASE, 
                        fun.aggregate = sum, value.var = "PROCESS_DURATION")[,-1], 2, mean)
  dayData <- dayData/sum(dayData)*endAverage[day]
  dayData <- dayData[-which(names(dayData) == "Technical")]
  dayData <- dayData[order(factor(names(dayData), levels=sortedNames))]
  plot_ly(labels = names(dayData), values = dayData, type = "pie", height = 360, width = w,
          #textinfo= "text",
          automargin = T,
          legendheight = 100,
          text = paste0("<b>", round(dayData, 2), " hrs</b>"),
          domain = list(x = c(0, 1), y = c(0, 1)), hole = 0.2,
          sort = F, pull = 0.02, textposition = 'outside', showlegend = showLegend,
          outsidetextfont = list(size = 14, family = "Montserrat, sans-serif", 
                                 color = phaseColors[names(dayData)]),
          hoverlabel = list(font = list(family = "Montserrat, sans-serif", size = 9)),
          marker = list(colors = phaseColors[names(dayData)])) %>%
    layout(title = paste0('<b>',day,'</b>'), showlegend = showLegend, 
           titlefont = list(family = "Montserrat, sans-serif", size = 16, color = 'rgb(0, 0, 0)'),
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = T),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = T),
           legend = list(bgcolor = "rgba(255, 255, 255, 0.8)", x = -1.5, y = 0.5, 
                         font = list(size = 14, family = "Montserrat, sans-serif")))
}

#width = 1240
monDayPieChart <- piePlot("MONDAY", T, 450)
tueDayPieChart <- piePlot("TUESDAY", F, 270)
wedDayPieChart <- piePlot("WEDNESDAY", F, 270)
thuDayPieChart <- piePlot("THURSDAY", F, 270)
#friDayPieChart <- piePlot("FRIDAY")


########################## Reports ready vs System ready ############################

#Reports ready 
reports <- dat[which(dat$PROCESS_NAME == "EXECUTION_23" & dat$DAY != "FRIDAY"),c("COB_DATE", "TOTAL_DURATION")]

system <- dat[which(dat$PROCESS_NAME == "REPORT_PREP_18" & dat$DAY != "FRIDAY"),c("COB_DATE", "TOTAL_DURATION")]

reportsReady <- plot_ly(x = dat$COB_DATE, y = 0, mode = 'lines', height = 400, showlegend = T,
                            hoverlabel = list(font = list(family = "Montserrat, sans-serif", size = 9))) %>%
  add_trace(x = system$COB_DATE, y = system$TOTAL_DURATION,  mode = "lines", name = "System Ready", 
            line = list(shape = 'spline', smoothing = 0.5)) %>%
  add_trace(x = reports$COB_DATE, y = reports$TOTAL_DURATION,  mode = "lines", name = "Reports Ready",
            line = list(shape = 'spline', smoothing = 0.5)) %>%
  layout(
    shapes = list(
      list(type='line', x0 = first(reports$COB_DATE), x1 = last(reports$COB_DATE), 
           y0 = mean(reports$TOTAL_DURATION), y1 = mean(reports$TOTAL_DURATION), line = list(width = 1, color = "#ff7e0e9b")),
      list(type='line', x0 = first(system$COB_DATE), x1 = last(system$COB_DATE), 
           y0 = mean(system$TOTAL_DURATION), y1 = mean(system$TOTAL_DURATION), line = list(width = 1, color = "#1f76b47c"))
      ),
    title = "<b>System Ready vs. Report Ready</b>",
    titlefont = list(family = "Montserrat, sans-serif", size = 16, color = 'rgb(0, 0, 0)'),
    xaxis = list(title = "Date", tickfont = list(family = "Montserrat, sans-serif", size = 12, color = 'rgb(0, 0, 0)'),
                 titlefont = list(family = "Montserrat, sans-serif", size = 14, color = '#000')),
    yaxis = list(title = 'Time', 
                 tickfont = list(family = "Montserrat, sans-serif", size = 12, color = 'rgb(0, 0, 0)'),
                 titlefont = list(family = "Montserrat, sans-serif", size = 14, color = '#000'),
                 tickvals = seq(2, 24, 2), ticktext = toTime(1245 + seq(2, 24, 2)*60)),
    legend = list(bgcolor = "rgba(255, 255, 255, 0.8)", x = 0.01, y = 0.95, orientation = "v", font = list(size = 14, family = "Montserrat, sans-serif")))


#Histogram
systemDensity <- density(system$TOTAL_DURATION)
reportsDensity <- density(reports$TOTAL_DURATION)

systemHist <- hist(system$TOTAL_DURATION, plot = F, breaks = 12)
reportsHist <- hist(reports$TOTAL_DURATION, plot = F, breaks = 12)

reportsReadyHisto <- plot_ly(hoverlabel = list(font = list(family = "Montserrat, sans-serif", size = 9)),
                                 height = 400) %>%
    add_trace(alpha = 0.4, x = systemHist$breaks, y = c(systemHist$counts, 0), type = 'bar', name = "System Ready",
              marker = list(color = '#2E71A3')) %>%
    add_trace(x = reportsHist$breaks, y = c(reportsHist$counts, 0), type = 'bar', name = "Reports Ready", 
            marker = list(color = '#EC852B')) %>%
    add_lines(x = systemDensity$x, y = systemDensity$y / max(systemDensity$y) * max(systemHist$counts),  
              name = "Density", line = list(dash = "solid", width = 1, color = "#2e71a338"), 
              fill = 'tozeroy', fillcolor = "#2e71a338", showlegend = F) %>% 
    add_lines(x = reportsDensity$x, y = reportsDensity$y / max(reportsDensity$y) * max(reportsHist$counts),  
            name = "Density", line = list(dash = "solid", width = 1, color = "#ec852b3d"), fill = 'tozeroy', 
            fillcolor = "#ec852b3d", showlegend = F) %>% 
  add_lines(x = mean(reports$TOTAL_DURATION), 
            y = -2:max(c(systemHist$counts, reportsHist$counts))+1, 
            line = list(dash = "solid", width = 2, color = "#ff7e0e9b"), showlegend = T, name = "<i>reports average</i>") %>%
  add_lines(x = mean(system$TOTAL_DURATION), 
            y = -2:max(c(systemHist$counts, reportsHist$counts))+1, 
            line = list(dash = "solid", width = 2, color = "#1f76b47c"), showlegend = T, name = "<i>system average</i>") %>%
    layout(barmode = "overlay",
           title = "<b>System Ready vs. Report Ready - Distributions</b>",
           titlefont = list(family = "Montserrat, sans-serif", size = 16, color = 'rgb(0, 0, 0)'),
           xaxis = list(title = "Time", tickfont = list(family = "Montserrat, sans-serif", size = 12, color = 'rgb(0, 0, 0)'),
                        titlefont = list(family = "Montserrat, sans-serif", size = 14, color = '#000'),
                        tickvals = seq(4, 24, 4), ticktext = toTime(1245 + seq(4, 24, 4)*60)),
           yaxis = list(title = 'Count', overlaying = "y",
                        tickfont = list(family = "Montserrat, sans-serif", size = 12, color = 'rgb(0, 0, 0)'),
                        titlefont = list(family = "Montserrat, sans-serif", size = 14, color = '#000')),
           legend = list(bgcolor = "rgba(255, 255, 255, 0.8)", x = 0.6, y = 0.95, orientation = "v", 
                         font = list(size = 14, family = "Montserrat, sans-serif")),
           bargap = 0)

################################### Box Plots #####################################

#Box plots
boxDat <- dat[-which(dat$PROCESS_PHASE == "Maintenance"),]
boxDat <- boxDat[-which(boxDat$PROCESS_DURATION < 0.25),]
boxDat <- boxDat[-which(boxDat$PROCESS_DURATION > 4),]

boxWhiskerPlot <- plot_ly(boxDat, x = ~PROCESS_PHASE, y = ~PROCESS_DURATION, color = ~DAY, type = "box", 
                          height = 500,
                          hoverlabel = list(font = list(family = "Montserrat, sans-serif", size = 9))) %>%
  layout(
    boxmode = "group",
    title = "<b>Process Phase Runtime Distribution</b>",
    titlefont = list(family = "Montserrat, sans-serif", size = 16, color = 'rgb(0, 0, 0)'),
    xaxis = list(title = "", tickfont = list(family = "Montserrat, sans-serif", size = 12, color = 'rgb(0, 0, 0)'),
                 titlefont = list(family = "Montserrat, sans-serif", size = 14, color = '#000')),
    yaxis = list(title = 'Hours', 
                 titlefont = list(family = "Montserrat, sans-serif", size = 14, color = '#000')),
    legend = list(orientation = "h", xanchor = "center", x = 0.45, font = list(size = 14, family = "Montserrat, sans-serif")))


################################### Gantt chart - Weekday ###################################

weekDayGantt <- dat[which(dat$COB_DATE == as.Date('2019-06-27')), ]
weekDayGantt <- weekDayGantt[-which(weekDayGantt$PROCESS_DURATION <= 1/60),]
weekDayGantt$color <- as.vector(phaseColors[match(weekDayGantt$PROCESS_PHASE, names(phaseColors))])

processGanttWeekDay <- plot_ly(height = 500)

for(i in 1:(nrow(weekDayGantt) - 1)){
  processGanttWeekDay <- add_trace(processGanttWeekDay, type = "scatter",
                 x = c(weekDayGantt$START_TIME[i] - 7200, weekDayGantt$END_TIME[i] - 7200),  # x0, x1
                 y = c(i, i),  # y0, y1
                 mode = "lines",
                 line = list(
                   #color = paste0("rgba(", sample(1:255, 1),",", sample(1:255,1), ",", sample(1:255,1), ",1)"), width = 20),
                   color = weekDayGantt$color[i], width = 25),
                 name = weekDayGantt$PROCESS_NAME[i],
                 showlegend = F,
                 hoverinfo = "text",
                 legend = list(font = list(size = 10, family = "Montserrat, sans-serif")),
                 hoverlabel = list(font = list(family = "Montserrat, sans-serif", size = 9)),
                 text = paste("Process: ", weekDayGantt$PROCESS_NAME[i], "<br>",
                              "Start: ", weekDayGantt$START_TIME[i], "<br>",
                              "End: ", weekDayGantt$END_TIME[i], "<br>",
                              "Duration: ", weekDayGantt$PROCESS_DURATION[i], "Hours<br>",
                              "Phase: ", weekDayGantt$PROCESS_PHASE[i])#,
                 #evaluate = T
  )
}

processGanttWeekDay <- layout(processGanttWeekDay,
                      title = paste0("<b>Weekday Process Gantt chart - ",  paste(c(substring(weekDayGantt$DAY[1], 1, 1), tolower(substring(weekDayGantt$DAY[1], 2))), collapse=''), 
                      ", ", format(as.Date(weekDayGantt$COB_DATE[1]), "%d %b %Y"), "</b>"),
                      titlefont = list(family = "Montserrat, sans-serif", size = 16, color = 'rgb(0, 0, 0)'),
            xaxis = list(title = "", showgrid = T, tickfont = list(family = "Montserrat, sans-serif", size = 12, color = 'rgb(0, 0, 0)')),
            yaxis = list(showgrid = T, tickfont = list(family = "Montserrat, sans-serif", size = 12, color = weekDayGantt$color),
                         showticklabels = F),
            legend = list(font = list(size = 10, family = "Montserrat, sans-serif")))


#################################### Gantt chart - Friday ####################################

weekEndGantt <- dat[which(dat$COB_DATE == as.Date('2019-07-05')), ]
weekEndGantt <- weekEndGantt[-which(weekEndGantt$PROCESS_DURATION <= 4/60),]
weekEndGantt$color <- as.vector(phaseColors[match(weekEndGantt$PROCESS_PHASE, names(phaseColors))])

processGanttWeekEnd <- plot_ly(height = 500)
for(i in 1:(nrow(weekEndGantt) - 1)){
  processGanttWeekEnd <- add_trace(processGanttWeekEnd, type = "scatter",
                 x = c(weekEndGantt$START_TIME[i] - 7200, weekEndGantt$END_TIME[i] - 7200),  # x0, x1
                 y = c(i, i),  # y0, y1
                 mode = "lines",
                 line = list(color = weekEndGantt$color[i], width = 25),
                 name = weekEndGantt$PROCESS_NAME[i],
                 showlegend = F,
                 hoverinfo = "text",
                 legend = list(font = list(size = 10, family = "Montserrat, sans-serif")),
                 hoverlabel = list(font = list(family = "Montserrat, sans-serif", size = 9)),
                 text = paste("Process: ", weekEndGantt$PROCESS_NAME[i], "<br>",
                              "Start: ", weekEndGantt$START_TIME[i], "<br>",
                              "End: ", weekEndGantt$END_TIME[i], "<br>",
                              "Duration: ", weekEndGantt$PROCESS_DURATION[i], "Hours<br>",
                              "Phase: ", weekEndGantt$PROCESS_PHASE[i])#,
                              #evaluate = T
  )
}

processGanttWeekEnd <- layout(processGanttWeekEnd,
                              title = paste0("<b>Friday Process Duration - ",  
                                             paste(c(substring(weekEndGantt$DAY[1], 1, 1), tolower(substring(weekEndGantt$DAY[1], 2))), collapse=''), 
                      ", ", format(as.Date(weekEndGantt$COB_DATE[1]), "%d %b %Y"), "</b>"),
                      titlefont = list(family = "Montserrat, sans-serif", size = 16, color = 'rgb(0, 0, 0)'),
                      xaxis = list(title = "", showgrid = T, tickfont = list(family = "Montserrat, sans-serif", size = 12, color = 'rgb(0, 0, 0)')),
                      yaxis = list(showgrid = T, tickfont = list(family = "Montserrat, sans-serif", size = 12, color = weekEndGantt$color),
                                   showticklabels = F),
                      legend = list(font = list(size = 10, family = "Montserrat, sans-serif")))


################################## Phases Plots ################################

phases <- aggregate(dat$PROCESS_DURATION, by=list(Phase = dat$PROCESS_PHASE, Date = dat$COB_DATE, Day = dat$DAY), FUN = sum)

phasePlot <- function(phase = "Initiation", weekend = F, showLegend = T, h = 200, trendColor = "#22B14C") {
  
  days <- ifelse(weekend, "FRIDAY", c("MONDAY", "TUESDAY", "WEDNESDAY", "THURSDAY"))
  
  phasesResult <- phases[which(phases$Phase == phase & phases$Day %in% days),]
  phasesResult <- phasesResult[order(phasesResult$Date, decreasing = F),]
  
  nphasesResult <- 1:nrow(phasesResult)
  
  plot_ly(x = phasesResult$Date, y = phasesResult$x, type = "scatter", mode = "lines",  
          line = list(shape = 'spline', smoothing = 0.5, color = phaseColors[phase]), fillcolor = '#ffffff00', height = h,
          hoverlabel = list(font = list(family = "Montserrat, sans-serif", size = 9)), name = paste0("<i>", phase, "</i>"),
          visible = 'legendonly') %>%
    add_lines(x = phasesResult$Date, 
              y = predict(lm(phasesResult$x ~ nphasesResult + I(nphasesResult^2) + I(nphasesResult^3))), 
              line = list(dash = "solid", width = 2, color = sample(phaseColors, 1)), name = "<i>trend</i>",
              visible = TRUE) %>%
    layout(
      showlegend = showLegend,
      legend = list(bgcolor = "rgba(255, 255, 255, 0.5)", x = 0.7, y = 1, 
                    font = list(size = 12, family = "Montserrat, sans-serif")),
      title = paste0("<b>", phase, ifelse(weekend, " (Friday)", " (Weekday)"), "</b>"),
      titlefont = list(family = "Montserrat, sans-serif", size = 16, color = 'rgb(0, 0, 0)'),
      xaxis = list(title = "", tickfont = list(family = "Montserrat, sans-serif", size = 12, color = 'rgb(0, 0, 0)'),
                   titlefont = list(family = "Montserrat, sans-serif", size = 14, color = '#000')),
      yaxis = list(title = 'Hours', tickfont = list(family = "Montserrat, sans-serif", size = 12, color = 'rgb(0, 0, 0)'),
                   titlefont = list(family = "Montserrat, sans-serif", size = 14, color = '#000')))
}

#Weekday plots
initWeekDayPlot <- phasePlot("Initiation", F, T, 200, "#22B14C")
planWeekDayPlot <- phasePlot("Planning", F, T, 200, "#22B14C")
computWeekDayPlot <- phasePlot("Computation", F, T, 200, "#d62801")
reportPrepWeekDayPlot <- phasePlot("Report Prep", F, T, 200, "#22B14C")
reportRunWeekDayPlot <- phasePlot("Report Run", F, T, 200, "#d62801")
controlWeekDayPlot <- phasePlot("Control", F, T, 200, "#22B14C")
maintenWeekDayPlot <- phasePlot("Maintenance", F, T, 200, "#d62801")
techWeekDayPlot <- phasePlot("Technical", F, T, 200, "#22B14C")
houseWeekDayPlot <- phasePlot("Housekeep", F, T, 200, "#d62801")

#Weekend plots
initWeekEndPlot <- phasePlot("Initiation", T, T, 200, "#22B14C")
planWeekEndPlot <- phasePlot("Planning", T, T, 200, "#22B14C")
computWeekEndPlot <- phasePlot("Computation", T, T, 200, "#d62801")
reportPrepWeekEndPlot <- phasePlot("Report Prep", T, T, 200, "#22B14C")
reportRunWeekEndPlot <- phasePlot("Report Run", T, T, 200, "#d62801")
controlWeekEndPlot <- phasePlot("Control", T, T, 200, "#22B14C")
maintenWeekEndPlot <- phasePlot("Maintenance", T, T, 200, "#d62801")
techWeekEndPlot <- phasePlot("Technical", T, T, 200, "#22B14C")
houseWeekEndPlot <- phasePlot("Housekeep", T, T, 200, "#d62801")


######################### Top 10 Process Plots per phase ###########################

initProcesses <- dat[which(dat$PROCESS_PHASE == "Initiation"), c("PROCESS_NAME", "PROCESS_DURATION") ]
initProcesses <- aggregate(initProcesses$PROCESS_DURATION, by=list(Process = initProcesses$PROCESS_NAME), FUN = mean)
initProcesses <- initProcesses[order(initProcesses$x, decreasing = T)[1:7],]
initProcesses$Process<- factor(initProcesses$Process, levels = initProcesses$Process[order(initProcesses$x, decreasing = T)])

initProcessesPlot <- plot_ly(initProcesses, height = 200,
                             hoverlabel = list(font = list(family = "Montserrat, sans-serif", size = 9)),
                             x = ~x,
                             text = ~Process, textposition = 'right', 
                             insidetextanchor = 'outside',
                             textfont = list(family = "Montserrat, sans-serif", size = 11, color = 'white'),
                             y = ~Process,
                             name = "Initiation Processess",
                             type = "bar",orientation = "h",
                             marker = list(color = '#1F77B4', width = 1.5)) %>%
  layout(title = "<b>Bottom 10 Initiation Processes</b>",
         titlefont = list(family = "Montserrat, sans-serif", size = 16, color = 'rgb(0, 0, 0)'),
         xaxis = list(title = "Average Hours", showgrid = F, showticklabels = T, 
                      tickfont = list(family = "Montserrat, sans-serif", size = 12, color = 'rgb(0, 0, 0)')), 
         yaxis = list(title = "", showgrid = F, showticklabels = FALSE),
         bargap = 0.02)

planProcesses <- dat[which(dat$PROCESS_PHASE == "Planning"), c("PROCESS_NAME", "PROCESS_DURATION") ]
planProcesses <- aggregate(planProcesses$PROCESS_DURATION, by=list(Process = planProcesses$PROCESS_NAME), FUN = mean)
planProcesses <- planProcesses[order(planProcesses$x, decreasing = T)[1:7],]
planProcesses$Process<- factor(planProcesses$Process, levels = planProcesses$Process[order(planProcesses$x, decreasing = T)])

planProcessesPlot <- plot_ly(planProcesses, height = 200,
                             hoverlabel = list(font = list(family = "Montserrat, sans-serif", size = 9)),
                             x = ~x,
                             text = ~Process, textposition = 'right', 
                             insidetextanchor = 'outside',
                             textfont = list(family = "Montserrat, sans-serif", size = 11, color = 'white'),
                             y = ~Process,
                             name = "Planning Processess",
                             type = "bar",orientation = "h",
                             marker = list(color = '#1F77B4', width = 1.5)) %>%
  layout(title = "<b>Bottom 10 Planning Processes</b>",
         titlefont = list(family = "Montserrat, sans-serif", size = 16, color = 'rgb(0, 0, 0)'),
         xaxis = list(title = "Average Hours", showgrid = F, showticklabels = T, 
                      tickfont = list(family = "Montserrat, sans-serif", size = 12, color = 'rgb(0, 0, 0)')), 
         yaxis = list(title = "", showgrid = F, showticklabels = FALSE),
         bargap = 0.02)

computProcesses <- dat[which(dat$PROCESS_PHASE == "Computation"), c("PROCESS_NAME", "PROCESS_DURATION") ]
computProcesses <- aggregate(computProcesses$PROCESS_DURATION, by=list(Process = computProcesses$PROCESS_NAME), FUN = mean)
computProcesses <- computProcesses[order(computProcesses$x, decreasing = T)[1:10],]
computProcesses$Process<- factor(computProcesses$Process, levels = computProcesses$Process[order(computProcesses$x, decreasing = T)])

computProcessesPlot <- plot_ly(computProcesses, height = 200,
                               hoverlabel = list(font = list(family = "Montserrat, sans-serif", size = 9)),
                               x = ~x,
                               text = ~Process, textposition = 'right', 
                               insidetextanchor = 'outside',
                               textfont = list(family = "Montserrat, sans-serif", size = 11, color = 'white'),
                               y = ~Process,
                               name = "Computation Processess",
                               type = "bar",orientation = "h",
                               marker = list(color = '#1F77B4', width = 1.5)) %>%
  layout(title = "<b>Bottom 10 Computation Processes</b>",
         titlefont = list(family = "Montserrat, sans-serif", size = 16, color = 'rgb(0, 0, 0)'),
         xaxis = list(title = "Average Hours", showgrid = F, showticklabels = T, 
                      tickfont = list(family = "Montserrat, sans-serif", size = 12, color = 'rgb(0, 0, 0)')), 
         yaxis = list(title = "", showgrid = F, showticklabels = FALSE),
         bargap = 0.02)

reportPrepProcesses <- dat[which(dat$PROCESS_PHASE == "Report Prep"), c("PROCESS_NAME", "PROCESS_DURATION") ]
reportPrepProcesses <- aggregate(reportPrepProcesses$PROCESS_DURATION, by=list(Process = reportPrepProcesses$PROCESS_NAME), FUN = mean)
reportPrepProcesses <- reportPrepProcesses[order(reportPrepProcesses$x, decreasing = T)[1:7],]
reportPrepProcesses$Process<- factor(reportPrepProcesses$Process, levels = reportPrepProcesses$Process[order(reportPrepProcesses$x, decreasing = T)])

reportPrepProcessesPlot <- plot_ly(reportPrepProcesses, height = 200,
                                   hoverlabel = list(font = list(family = "Montserrat, sans-serif", size = 9)),
                                   x = ~x,
                                   text = ~Process, textposition = 'right', 
                                   insidetextanchor = 'outside',
                                   textfont = list(family = "Montserrat, sans-serif", size = 11, color = 'white'),
                                   y = ~Process,
                                   name = "Report Prep Processess",
                                   type = "bar",orientation = "h",
                                   marker = list(color = '#1F77B4', width = 1.5)) %>%
  layout(title = "<b>Bottom 10 Report Prep Processes</b>",
         titlefont = list(family = "Montserrat, sans-serif", size = 16, color = 'rgb(0, 0, 0)'),
         xaxis = list(title = "Average Hours", showgrid = F, showticklabels = T, 
                      tickfont = list(family = "Montserrat, sans-serif", size = 12, color = 'rgb(0, 0, 0)')), 
         yaxis = list(title = "", showgrid = F, showticklabels = FALSE),
         bargap = 0.02)

reportRunProcesses <- dat[which(dat$PROCESS_PHASE == "Report Run"), c("PROCESS_NAME", "PROCESS_DURATION") ]
reportRunProcesses <- aggregate(reportRunProcesses$PROCESS_DURATION, by=list(Process = reportRunProcesses$PROCESS_NAME), FUN = mean)
reportRunProcesses <- reportRunProcesses[order(reportRunProcesses$x, decreasing = T)[1:7],]
reportRunProcesses$Process<- factor(reportRunProcesses$Process, levels = reportRunProcesses$Process[order(reportRunProcesses$x, decreasing = T)])

reportRunProcessesPlot <- plot_ly(reportRunProcesses, height = 200,
                                  hoverlabel = list(font = list(family = "Montserrat, sans-serif", size = 9)),
                                  x = ~x,
                                  text = ~Process, textposition = 'right', 
                                  insidetextanchor = 'outside',
                                  textfont = list(family = "Montserrat, sans-serif", size = 11, color = 'white'),
                                  y = ~Process,
                                  name = "Report Run Processess",
                                  type = "bar",orientation = "h",
                                  marker = list(color = '#1F77B4', width = 1.5)) %>%
  layout(title = "<b>Bottom 10 Report Run Processes</b>",
         titlefont = list(family = "Montserrat, sans-serif", size = 16, color = 'rgb(0, 0, 0)'),
         xaxis = list(title = "Average Hours", showgrid = F, showticklabels = T, 
                      tickfont = list(family = "Montserrat, sans-serif", size = 12, color = 'rgb(0, 0, 0)')), 
         yaxis = list(title = "", showgrid = F, showticklabels = FALSE),
         bargap = 0.02)

controlProcesses <- dat[which(dat$PROCESS_PHASE == "Control"), c("PROCESS_NAME", "PROCESS_DURATION") ]
controlProcesses <- aggregate(controlProcesses$PROCESS_DURATION, by=list(Process = controlProcesses$PROCESS_NAME), FUN = mean)
controlProcesses <- controlProcesses[order(controlProcesses$x, decreasing = T)[1:7],]
controlProcesses$Process<- factor(controlProcesses$Process, levels = controlProcesses$Process[order(controlProcesses$x, decreasing = T)])

controlProcessesPlot <- plot_ly(controlProcesses, height = 200,
                                hoverlabel = list(font = list(family = "Montserrat, sans-serif", size = 9)),
                                x = ~x,
                                text = ~Process, textposition = 'right', 
                                insidetextanchor = 'outside',
                                textfont = list(family = "Montserrat, sans-serif", size = 11, color = 'white'),
                                y = ~Process,
                                name = "Control Processess",
                                type = "bar",orientation = "h",
                                marker = list(color = '#1F77B4', width = 1.5)) %>%
  layout(title = "<b>Bottom 10 Control Processes</b>",
         titlefont = list(family = "Montserrat, sans-serif", size = 16, color = 'rgb(0, 0, 0)'),
         xaxis = list(title = "Average Hours", showgrid = F, showticklabels = T, 
                      tickfont = list(family = "Montserrat, sans-serif", size = 12, color = 'rgb(0, 0, 0)')), 
         yaxis = list(title = "", showgrid = F, showticklabels = FALSE),
         bargap = 0.02)


maintenProcesses <- dat[which(dat$PROCESS_PHASE == "Maintenance"), c("PROCESS_NAME", "PROCESS_DURATION") ]
maintenProcesses <- aggregate(maintenProcesses$PROCESS_DURATION, by=list(Process = maintenProcesses$PROCESS_NAME), FUN = mean)
maintenProcesses <- maintenProcesses[order(maintenProcesses$x, decreasing = T)[1:7],]
maintenProcesses$Process<- factor(maintenProcesses$Process, levels = maintenProcesses$Process[order(maintenProcesses$x, decreasing = T)])

maintenProcessesPlot <- plot_ly(maintenProcesses, height = 200,
                                hoverlabel = list(font = list(family = "Montserrat, sans-serif", size = 9)),
                                x = ~x,
                                text = ~Process, textposition = 'right', 
                                insidetextanchor = 'outside',
                                textfont = list(family = "Montserrat, sans-serif", size = 11, color = 'white'),
                                y = ~Process,
                                name = "Maintenance Processess",
                                type = "bar",orientation = "h",
                                marker = list(color = '#1F77B4', width = 1.5)) %>%
  layout(title = "<b>Bottom 10 Maintenance Processes</b>",
         titlefont = list(family = "Montserrat, sans-serif", size = 16, color = 'rgb(0, 0, 0)'),
         xaxis = list(title = "Average Hours", showgrid = F, showticklabels = T, 
                      tickfont = list(family = "Montserrat, sans-serif", size = 12, color = 'rgb(0, 0, 0)')), 
         yaxis = list(title = "", showgrid = F, showticklabels = FALSE),
         bargap = 0.02)

techProcesses <- dat[which(dat$PROCESS_PHASE == "Technical"), c("PROCESS_NAME", "PROCESS_DURATION") ]
techProcesses <- aggregate(techProcesses$PROCESS_DURATION, by=list(Process = techProcesses$PROCESS_NAME), FUN = mean)
techProcesses <- techProcesses[order(techProcesses$x, decreasing = T)[1:7],]
techProcesses$Process<- factor(techProcesses$Process, levels = techProcesses$Process[order(techProcesses$x, decreasing = T)])

techProcessesPlot <- plot_ly(techProcesses, height = 200,
                             hoverlabel = list(font = list(family = "Montserrat, sans-serif", size = 9)),
                             x = ~x,
                             text = ~Process, textposition = 'right', 
                             insidetextanchor = 'outside',
                             textfont = list(family = "Montserrat, sans-serif", size = 11, color = 'white'),
                             y = ~Process,
                             name = "Technical Processess",
                             type = "bar",orientation = "h",
                             marker = list(color = '#1F77B4', width = 1.5)) %>%
  layout(title = "<b>Bottom 10 Technical Processes</b>",
         titlefont = list(family = "Montserrat, sans-serif", size = 16, color = 'rgb(0, 0, 0)'),
         xaxis = list(title = "Average Hours", showgrid = F, showticklabels = T, 
                      tickfont = list(family = "Montserrat, sans-serif", size = 12, color = 'rgb(0, 0, 0)')), 
         yaxis = list(title = "", showgrid = F, showticklabels = FALSE),
         bargap = 0.02)

houseProcesses <- dat[which(dat$PROCESS_PHASE == "Housekeep"), c("PROCESS_NAME", "PROCESS_DURATION") ]
houseProcesses <- aggregate(houseProcesses$PROCESS_DURATION, by=list(Process = houseProcesses$PROCESS_NAME), FUN = mean)
houseProcesses <- houseProcesses[order(houseProcesses$x, decreasing = T)[1:7],]
houseProcesses$Process<- factor(houseProcesses$Process, levels = houseProcesses$Process[order(houseProcesses$x, decreasing = T)])

houseProcessesPlot <- plot_ly(houseProcesses, height = 200,
                              hoverlabel = list(font = list(family = "Montserrat, sans-serif", size = 9)),
                              x = ~x,
                              text = ~Process, textposition = 'right', 
                              insidetextanchor = 'outside',
                              textfont = list(family = "Montserrat, sans-serif", size = 11, color = 'white'),
                              y = ~Process,
                              name = "Housekeep Processess",
                              type = "bar",orientation = "h",
                              marker = list(color = '#1F77B4', width = 1.5)) %>%
  layout(title = "<b>Bottom 10 Housekeep Processes</b>",
         titlefont = list(family = "Montserrat, sans-serif", size = 16, color = 'rgb(0, 0, 0)'),
         xaxis = list(title = "Average Hours", showgrid = F, showticklabels = T, 
                      tickfont = list(family = "Montserrat, sans-serif", size = 12, color = 'rgb(0, 0, 0)')), 
         yaxis = list(title = "", showgrid = F, showticklabels = FALSE),
         bargap = 0.02)

################################ Individual Process ################################

##################################### Tree Map ####################################

# Create data
group = c("group-1","group-2","group-3")
value = c(13,5,22)
data = data.frame(group,value)

# treemap
jobsTreeMap <- treemap(data,
        index = "group",
        vSize = "value",
        type = "index", 
        title = "", 
        fontfamily.title = 'Montserrat, sans-serif'
)


##################################### Bubble Plot ##################################

bubbleDat <- aggregate(dat$PROCESS_DURATION, by=list(Process = dat$PROCESS_NAME, Phase = dat$PROCESS_PHASE), FUN = mean)
bubbleDat <- bubbleDat[-which(bubbleDat$x < 1e-1),]
bubbleDat$color <- as.vector(phaseColors[match(bubbleDat$Phase, names(phaseColors))])

bubbleDat$y <- sample(1:nrow(bubbleDat))
bubbleDat$Process <- ifelse(bubbleDat$x < 0.5, "", bubbleDat$Process)

bubblePlot <- plot_ly(bubbleDat, x = ~y, text=~Process, y = ~x, type = 'scatter', mode = 'markers', 
        textposition = 'middle right', height = 400,
        hoverlabel = list(font = list(family = "Montserrat, sans-serif", size = 9)),
        marker = list(size = ~x*80, opacity = 0.8, color = ~color, sizemode = 'diameter', 
                      line = list(width = 2, color = "#FFFFFF")), showlegend = F) %>%
  add_text(textfont = list(family = "Montserrat, sans-serif", size = 8, color = '#ffffff'), textposition = "middle center") %>%
  layout(
         title = "<b>Phase Bubble Scatter Plot</b>",
         titlefont = list(family = "Montserrat, sans-serif", size = 16, color = 'rgb(0, 0, 0)'),
         xaxis = list(showgrid = F, showline = FALSE, showticklabels = FALSE, zeroline = FALSE, title = ""),
         yaxis = list(showgrid = F, showline = FALSE, showticklabels = FALSE, zeroline = TRUE, title = ""),
         showlegend = F)

