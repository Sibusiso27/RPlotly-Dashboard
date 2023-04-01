###################################################################################
#                                                                                 #
#                               db Connection + Query                             #
#                                                                                 #
###################################################################################

dbaseConnection <- function(devName = "ATHENA", startDate = '20/MAR/2018', endDate = '31/MAR/2023', resultType = "BASIC") {
  
  #Library for connections to Oracle database
  library(RJDBC)
  
  sqlQuery <- function(inceptionDate = '20/MAR/2018', endDate = '31/MAR/2023', result = "BASIC", schema = "ATHENA") {

    if (result == "ADVANCED") {
      query <- paste0("SELECT * FROM (SELECT TO_CHAR(SYSTEM_DATE,'YYYY-MM-DD') AS SYSTEM_DATE, SUM((SUBSTR(TIME_ELAPSED, 1, 2) * 3600 + SUBSTR(TIME_ELAPSED, 4, 2) * 60 + SUBSTR(TIME_ELAPSED, 7, 2))) AS SECONDS, TRIM(PROCESS_PHASE) AS PROCESS_PHASE 
      FROM ", schema, ".PROCESS_TABLE WHERE COMP_DATE >= '", inceptionDate, "' AND COMP_DATE <= '", endDate,"' GROUP BY TO_CHAR(SYSTEM_DATE,'YYYY-MM-DD'), TRIM(PROCESS_NAME)) PIVOT (SUM(SECONDS/3600) FOR PROCESS_PHASE IN ('Initiation' AS Initiation, 'Planning' AS Planning, 'Computation' AS Computation, 'Report Prep' AS ReportPrep, 'Reporting Run' AS ReportingRun, 'Control' AS Control, 'Maintenance' AS Maintenance, 'Technical' AS Technical, 'Housekeep' AS Housekeep)) ORDER BY SYSTEM_DATE ASC")}
    
    else if (result == "PIVOT") {
      query <- paste0("SELECT * FROM (SELECT TO_CHAR(SYSTEM_DATE,'YYYY-MM-DD') AS SYSTEM_DATE, TRIM(PROCESS_NAME) AS PROCESS_NAME,SUM((SUBSTR(TIME_ELAPSED, 1, 2) * 3600 + SUBSTR(TIME_ELAPSED, 4, 2) * 60 + SUBSTR(TIME_ELAPSED, 7, 2))) AS SECONDS FROM ", schema, ".PROCESS_TABLE WHERE  COMP_DATE >= '", inceptionDate, "' AND COMP_DATE <= '", endDate,"' GROUP BY TO_CHAR(SYSTEM_DATE,'YYYY-MM-DD'), TRIM(PROCESS_NAME)) PIVOT (SUM(SECONDS/3600) FOR PROCESS_NAME IN (...)) ORDER BY SYSTEM_DATE ASC")
    }
    
    else if (result == "BASIC") {
      query <- paste0("SELECT ROWNUM AS ROW_NUM, TO_CHAR(COB_DATE, 'YYYY-MM-DD') AS COB_DATE, TRIM(TO_CHAR(COB_DATE, 'DAY')) AS DAY, TRIM(PROCESS_NAME) AS PROCESS_NAME, CASE WHEN COMP_TIME - TIME_ELASP < 0 THEN TO_CHAR(COMP_DATE - 1, 'YYYY-MM-DD') || ' ' || TO_CHAR(TRUNC((COMP_TIME - TIME_ELASP + 86400)/3600),'FM9900') || ':' || TO_CHAR(TRUNC(MOD((COMP_TIME - TIME_ELASP + 43200),3600)/60),'FM00') || ':' || TO_CHAR(MOD((COMP_TIME - TIME_ELASP + 43200),60),'FM00') ELSE TO_CHAR(COMP_DATE, 'YYYY-MM-DD') || ' ' || TO_CHAR(TRUNC((COMP_TIME - TIME_ELASP)/3600),'FM9900') || ':' || TO_CHAR(TRUNC(MOD((COMP_TIME - TIME_ELASP),3600)/60),'FM00') || ':' || TO_CHAR(MOD((COMP_TIME - TIME_ELASP),60),'FM00') END AS START_TIME, COMP_TIME AS END_TIME_MIDNIGHT, CASE WHEN COMP_TIME - TIME_ELASP < 0 THEN COMP_TIME - TIME_ELASP + 86400 ELSE COMP_TIME - TIME_ELASP END AS START_TIME_MIDNIGHT, TO_CHAR(COMP_DATE, 'YYYY-MM-DD') || ' ' || TO_CHAR(TRUNC(COMP_TIME/3600),'FM9900') || ':' || TO_CHAR(TRUNC(MOD(COMP_TIME,3600)/60),'FM00') || ':' || TO_CHAR(MOD(COMP_TIME,60),'FM00') AS END_TIME, TO_CHAR(TRUNC(TIME_ELASP/3600),'FM9900') || ':' || TO_CHAR(TRUNC(MOD(TIME_ELASP,3600)/60),'FM00') || ':' || TO_CHAR(MOD(TIME_ELASP,60),'FM00') AS PROCESS_DURATION, TO_CHAR(TRUNC(TOTAL_ELAPSED/3600),'FM9900') || ':' || TO_CHAR(TRUNC(MOD(TOTAL_ELAPSED,3600)/60),'FM00') || ':' || TO_CHAR(MOD(TOTAL_ELAPSED,60),'FM00') AS COB_DURATION, PHASE FROM (SELECT CASE WHEN MOD(TO_CHAR(DATE_CMP, 'J'), 7) + 1 = 6 THEN DATE_CMP - 1 WHEN MOD(TO_CHAR(DATE_CMP, 'J'), 7) + 1 = 7 THEN DATE_CMP - 2 WHEN TIME_CMP < 74700 THEN DATE_CMP - 1 ELSE DATE_CMP END AS COB_DATE, DATE_CMP AS COMP_DATE, (SUBSTR(TIME_ELAPSED, 1, 2) * 3600 + SUBSTR(TIME_ELAPSED, 4, 2) * 60 + SUBSTR(TIME_ELAPSED, 7, 2)) AS TIME_ELASP, TIME_CMP AS COMP_TIME, DATE_CMP, PROCESS_NAME, CASE WHEN TIME_CMP - 74700 < 0 THEN TIME_CMP + 11700 ELSE TIME_CMP - 74700 END AS TOTAL_ELAPSED, TRIM(PROCESS_PHASE) AS PROCESS_PHASE FROM ", schema, ".PROCESS_TABLE WHERE COMP_DATE >= '", inceptionDate, "' AND COMP_DATE <= '", endDate,"' ORDER BY ID) WHERE TRIM(TO_CHAR(COB_DATE, 'DAY')) <> 'SUNDAY' AND TO_CHAR(COB_DATE, 'DD-MM-YYYY') NOT IN ('01-01-2014','21-03-2014','18-04-2014','21-04-2014','28-04-2014','01-05-2014')")
    }

    else {
      query <- paste0("SELECT * FROM (SELECT TO_CHAR(SYSTEM_DATE,'YYYY-MM-DD') AS SYSTEM_DATE, TRIM(PROCESS_NAME) AS PROCESS_NAME, TRIM(PROCESS_PHASE) AS PROCESS_PHASE, SUM((SUBSTR(TIME_ELAPSED, 1, 2) * 3600 + SUBSTR(TIME_ELAPSED, 4, 2) * 60 + SUBSTR(TIME_ELAPSED, 7, 2))) AS SECONDS FROM ", schema, ".PROCESS_TABLE WHERE COMP_DATE >= '", inceptionDate, "' AND COMP_DATE <= '", endDate,"' GROUP BY TO_CHAR(SYSTEM_DATE,'YYYY-MM-DD'), TRIM(PROCESS_NAME)) ORDER BY SYSTEM_DATE, PHASE, PROCESS_NAME ASC")
    }

    return(query)
  }
  

  jdbcDriver = JDBC("oracle.jdbc.OracleDriver",classPath="C:/Program Files/OracleTools/ojdbc6.jar")

  if (devName == "APOLLO") {schema <- name <- pass <- "APOLLO"; host = ""; sid = "APOLLOX"}
  else if (devName == "ARTEMIS") {schema <- name <- pass <- "ARTEMIS"; host = ""; sid = "ARTEMISX"} 
  else if (devName == "ATHENA") {schema <- name <- pass <- "ATHENA"; host = ""; sid = "ATHENAX"}
  else if (devName == "ZEUS") {schema = "ZEUS"; name = "ZEUS"; pass = "ZEUS"; host = ""; sid = "ZEUSX"}
  else {schema <- name <- "ARES"; pass = "ARES"; host = ""; sid = "ARESX"}
  credentials = paste0("jdbc:oracle:thin:@//", host, ":1521/", sid)
  mxConn = dbConnect(jdbcDriver, credentials, name, pass)
  query = sqlQuery(inceptionDate = startDate, endDate = endDate, result = resultType, schema = schema)
  elapsed = system.time(results <- dbGetQuery(mxConn, query))
  disconnected = dbDisconnect(mxConn)
  if (disconnected) print(paste0("DONE... in ", as.character(round(elapsed[3],3)), " seconds"))
  return(results)
}
