#Convert seconds to time 12-hour (am pm)
toTime <- function(t, day = T) {
  t <- ifelse(t <= 1440, t, t - 1440)
  h <- floor(t%%3600/60)
  m <- floor(t%%60)
  s <- t%%60
  if (day) {
    p <- ifelse(h < 12, ' am', ' pm')
    h <- ifelse(h > 12, h - 12, h)
  }
  else {
    p <- ""
  }
  finalTime <- paste0(
    ifelse(nchar(h) == 1, paste0("0", h),  h), ":", ifelse(nchar(m) == 1, paste0("0", m),  m), p)
  return(finalTime)
}

#Convert hours to H hrs M mins (remove zero padding)
toTime2 <- function(hours = 12.5, short = F) {
  h <- floor(hours)
  m <- round((hours - h) * 60)
  f1 <- ifelse(short, "%Hh %Mm", "%H hrs %M mins")
  f2 <- ifelse(short, "%Mm", "%M mins")
  return(
    gsub("0(.)","\\1",
         ifelse(hours >= 1, 
                format(as.POSIXct(paste(h, m, "00", sep=":"), format="%H:%M:%S"), format=f1),
                format(as.POSIXct(paste(h, m, "00", sep=":"), format="%H:%M:%S"), format=f2)
         )
    )
  )
}