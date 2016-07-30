parseCountryName <- function(place) {
  location <- as.character(place)
  country <- strsplit(location, "-")[[1]][1]
  exceptionBrazilList = c("nordeste", "sudeste", "sul", "centro", "norte")
  exceptionUSList = c("united_states_virgin_islands")
  if (tolower(country) %in% exceptionBrazilList) {
    return("Brazil")
  }
  if(tolower(country) %in% exceptionUSList) {
    return("United_States_Virgin_Islands")
  }
  return (country)
}

extractCountryNameList <- function(zikaData) {
  countryList <- c()
  for (idx in 1:nrow(zikaData)) {
    rowData <- zikaData[idx,]
    country <- parseCountryName(rowData$location)
    if(country %in% countryList) {
      next
    }
    if (is.na(country)) {
      next
    }
    countryList <- c(countryList, country)
  }
  return (countryList);
}

createMatrixZikaCountry <- function(countryNameList) {
  #zika cases by country matrix
  colNames <- c(countryNameList)
  rowNames <- c("zikaCases")
  rowNumber <- length(rowNames)
  colNumber <- length(colNames)
  zikaCasesByCountryTable <- matrix(nrow=rowNumber, ncol=colNumber, dimnames=list(rowNames,colNames))
  #init matrix values
  for(row in rowNames) {
    for(col in colNames) {
      zikaCasesByCountryTable[row, col] <- 0
    }
  }  
  return (zikaCasesByCountryTable)
}

findZikaDataByFilters <- function(zikaData, country, dataFieldList, locationType, reportDate, countryUsingStartsWith) {
  if(is.null(dataFieldList)) {
    if(countryUsingStartsWith) {
      zikaSubset <- subset(zikaData,  substr(zikaData$location, 1, nchar(country)) == country & zikaData$report_date == reportDate & 
                             as.character(zikaData$location_type) == locationType 
                           & length(as.character(zikaData$value)) != 0L & is.na(zikaData$value) == FALSE );  
    } else {
      zikaSubset <- subset(zikaData,  as.character(zikaData$location) == country & zikaData$report_date == reportDate & 
                             as.character(zikaData$location_type) == locationType 
                           & length(as.character(zikaData$value)) != 0L & is.na(zikaData$value) == FALSE );  
    }
  } else {
    if(countryUsingStartsWith) {
      zikaSubset <- subset(zikaData,  substr(zikaData$location, 1, nchar(country)) == country & zikaData$report_date == reportDate & 
                             as.character(zikaData$location_type) == locationType & as.character(zikaData$data_field) %in% dataFieldList
                           & length(as.character(zikaData$value)) != 0L & is.na(zikaData$value) == FALSE )
    } else {
      zikaSubset <- subset(zikaData,  as.character(zikaData$location) == country & zikaData$report_date == reportDate & 
                             as.character(zikaData$location_type) == locationType & as.character(zikaData$data_field) %in% dataFieldList
                           & length(as.character(zikaData$value)) != 0L & is.na(zikaData$value) == FALSE )
    }
  }
  return (zikaSubset)
}



fillMatrixZikaCountry <- function(zikaData, zikaCasesByCountryTable, countryNameList) {
  for(country in countryNameList) {
    zikaSubset <- NULL
    lastDate <- NULL
    if(country == "Brazil") {
      lastDate <- findMaxDate(zikaData, country, NULL)
      zikaSubset <- findZikaDataByFilters(zikaData, country, c("zika_reported"), "state", lastDate, FALSE)
    } else if(country == "Argentina") {
      lastDate <- findMaxDate(zikaData, country, NULL)
      zikaSubset <- findZikaDataByFilters(zikaData, country, c("cumulative_confirmed_local_cases", "cumulative_probable_local_cases", "cumulative_confirmed_imported_cases", "cumulative_probable_imported_cases"), "province", lastDate, TRUE)
    } else if(country == "Colombia") {
      lastDate <- findMaxDate(zikaData, country, NULL)
      zikaSubset <- findZikaDataByFilters(zikaData, country, c("zika_confirmed_laboratory", "zika_confirmed_clinic", "zika_suspected", "zika_suspected_clinic"), "municipality", lastDate, TRUE)
    } else if(country == "Dominican_Republic") {
      lastDate <- findMaxDate(zikaData, country, NULL)
      zikaSubset <- findZikaDataByFilters(zikaData, country, c("zika_suspected_cumulative", "zika_confirmed_pcr_cumulative"), "country", lastDate, TRUE)
    } else if(country == "Ecuador") {
      lastDate <- findMaxDate(zikaData, country, NULL)
      zikaSubset <- findZikaDataByFilters(zikaData, country, NULL, "country", lastDate, TRUE)
    } else if(country == "El_Salvador") {
      lastDate <- findMaxDate(zikaData, country, NULL)
      zikaSubset <- findZikaDataByFilters(zikaData, country, c("cumulative_confirmed", "cumulative_suspected_total"), "country", lastDate, TRUE)
      zikaSubset <- zikaSubset[ !duplicated(zikaSubset$data_field,fromLast=TRUE),]
    } else if(country == "Guatemala") {
      lastDate <- findMaxDate(zikaData, country, NULL)
      zikaSubset <- findZikaDataByFilters(zikaData, country, c("total_zika_suspected_cumulative"), "country", lastDate, TRUE)
    } else if(country == "Haiti") {
      lastDate <- findMaxDate(zikaData, country, NULL)
      zikaSubset <- findZikaDataByFilters(zikaData, country, c("total_zika_new_suspected_cumulative"), "country", lastDate, TRUE)
    } else if(country == "Mexico") {
      lastDate <- findMaxDate(zikaData, country, NULL)
      zikaSubset <- findZikaDataByFilters(zikaData, country, c("yearly_cumulative_female", "yearly_cumulative_male"), "state", lastDate, TRUE)
    } else if(country == "Nicaragua") {
      lastDate <- findMaxDate(zikaData, country, c("total_zika_confirmed_cumulative"))
      zikaSubset <- findZikaDataByFilters(zikaData, country, c("total_zika_confirmed_cumulative"), "country", lastDate, TRUE)
    } else if(country == "Panama") {
      lastDate <- findMaxDate(zikaData, country, NULL)
      zikaSubset <- findZikaDataByFilters(zikaData, country, c("Zika_confirmed_F", "Zika_confirmed_M"), "country", lastDate, TRUE)
    } else if(country == "United_States") {
      lastDate2 <- findMaxDate(zikaData, country, c("yearly_reported_travel_cases"))
      zikaSubset <- findZikaDataByFilters(zikaData, country, c("yearly_reported_travel_cases"), "state", lastDate2, TRUE) 
    } else if(country == "United_States_Virgin_Islands") {
      lastDate <- findMaxDate(zikaData, country, c("zika_reported"))
      zikaSubset <- findZikaDataByFilters(zikaData, country, c("zika_reported"), "territory", lastDate, TRUE) 
    } else if(country == "Puerto_Rico") { 
      lastDate <- findMaxDate(zikaData, country, NULL)
      zikaSubset <- findZikaDataByFilters(zikaData, country, c("zika_confirmed_cumulative_2015-2016"), "territory", lastDate, TRUE) 
    } else {      
      next
    }
    for (idx in 1:nrow(zikaSubset)) {
      rowData <- zikaSubset[idx,]      
      zikaCasesByCountryTable["zikaCases", country] <- zikaCasesByCountryTable["zikaCases", country] + as.numeric(as.character(rowData$value))
    }
  }
  return(zikaCasesByCountryTable)
}
plotBar <- function(zikaCasesByCountryTable, countryNameList) {
  counts <- c()
  for(country in countryNameList) {
    counts <- c(counts, zikaCasesByCountryTable["zikaCases", country]) 
  }
  nameList <- countryNameList
  nameList[nameList == "United_States_Virgin_Islands"] <- "US Virgin_Islands"
  barplot(counts, names.arg=substr(nameList, 1, 5), cex.names=0.8, main="Zika Cases by Country" ) 
}

findMaxDate <- function(zikaData, country, dataFieldList) {
  if(is.null(dataFieldList)) {
    zikaSubset <- subset(zikaData, substr(zikaData$location, 1, nchar(country)) == country)  
  } else {
    zikaSubset <- subset(zikaData, substr(zikaData$location, 1, nchar(country)) == country & as.character(zikaData$data_field) %in% dataFieldList)
  }
  if("Puerto_Rico" == country) {
    dateList <- as.Date(zikaSubset$report_date, '%Y_%m-%d')
  } else {
    dateList <- as.Date(zikaSubset$report_date, '%Y-%m-%d')  
  }
  order(dateList)
  lastdate = as.character(dateList[length(dateList)])
  if("Puerto_Rico" == country) {
    lastdate <- paste(substr(lastdate, 1, 4), substr(lastdate, 6, nchar(lastdate)), sep='_')
  }
  return (lastdate)
}

createMatrixZikaCountryOverTime <- function(zikaData, country) {
  #zika cases by country matrix over time
  zikaSubset <- subset(zikaData, substr(zikaData$location, 1, nchar(country)) == country)  
  if("Puerto_Rico" == country) {
    dateList <- as.Date(zikaSubset$report_date, '%Y_%m-%d')
  } else {
    dateList <- as.Date(zikaSubset$report_date, '%Y-%m-%d')  
  }
  order(dateList)
  colNames <- c(country)
  rowNames <- unique(as.character(dateList))
  if("Puerto_Rico" == country) {
    rowNames <- paste(substr(rowNames, 1, 4), substr(rowNames, 6, nchar(rowNames)), sep='_')
    rowNames <- rowNames[rowNames != "NA_NA"]
  }
  order(rowNames)
  rowNumber <- length(rowNames)
  colNumber <- length(colNames)
  zikaCasesByCountryOverTimeTable <- matrix(nrow=rowNumber, ncol=colNumber, dimnames=list(rowNames,colNames))
  #init matrix values
  for(row in rowNames) {
    for(col in colNames) {
      zikaCasesByCountryOverTimeTable[row, col] <- 0
    }
  }  
  return (zikaCasesByCountryOverTimeTable)
}

fillMatrixZikaCountryOverTime <- function(zikaData, zikaCasesByCountryOverTime, country) {
  rowNames <- rownames(zikaCasesByCountryOverTime)
  for(date in rowNames) {
    if(country == "Brazil") {
      zikaSubset <- findZikaDataByFilters(zikaData, country, c("zika_reported"), "state", date, FALSE)
    } else if(country == "Argentina") {
      zikaSubset <- findZikaDataByFilters(zikaData, country, c("cumulative_confirmed_local_cases", "cumulative_probable_local_cases", "cumulative_confirmed_imported_cases", "cumulative_probable_imported_cases"), "province", date, TRUE)
    } else if(country == "Colombia") {
      zikaSubset <- findZikaDataByFilters(zikaData, country, c("zika_confirmed_laboratory", "zika_confirmed_clinic", "zika_suspected", "zika_suspected_clinic"), "municipality", date, TRUE)
    } else if(country == "Dominican_Republic") {
      zikaSubset <- findZikaDataByFilters(zikaData, country, c("zika_suspected_cumulative", "zika_confirmed_pcr_cumulative"), "country", date, TRUE)
    } else if(country == "Ecuador") {
      zikaSubset <- findZikaDataByFilters(zikaData, country, NULL, "country", date, TRUE)
    } else if(country == "El_Salvador") {
      zikaSubset <- findZikaDataByFilters(zikaData, country, c("cumulative_confirmed", "cumulative_suspected_total"), "country", date, TRUE)
      zikaSubset <- zikaSubset[ !duplicated(zikaSubset$data_field,fromLast=TRUE),]
    } else if(country == "Guatemala") {
      zikaSubset <- findZikaDataByFilters(zikaData, country, c("total_zika_suspected_cumulative"), "country", date, TRUE)
    } else if(country == "Haiti") {
      zikaSubset <- findZikaDataByFilters(zikaData, country, c("total_zika_new_suspected_cumulative"), "country", date, TRUE)
    } else if(country == "Mexico") {
      zikaSubset <- findZikaDataByFilters(zikaData, country, c("yearly_cumulative_female", "yearly_cumulative_male"), "state", date, TRUE)
    } else if(country == "Nicaragua") {
      zikaSubset <- findZikaDataByFilters(zikaData, country, c("total_zika_confirmed_cumulative"), "country", date, TRUE)
    } else if(country == "Panama") {
      zikaSubset <- findZikaDataByFilters(zikaData, country, c("Zika_confirmed_F", "Zika_confirmed_M"), "country", date, TRUE)
    } else if(country == "United_States") {
      zikaSubset <- findZikaDataByFilters(zikaData, country, c("yearly_reported_travel_cases"), "state", date, TRUE) 
    } else if(country == "United_States_Virgin_Islands") {
      zikaSubset <- findZikaDataByFilters(zikaData, country, c("zika_reported"), "territory", date, TRUE) 
    } else if(country == "Puerto_Rico") { 
      zikaSubset <- findZikaDataByFilters(zikaData, country, c("zika_confirmed_cumulative_2015-2016"), "territory", date, TRUE) 
    } else {      
      next
    }
    if(nrow(zikaSubset) <= 0) {
      next
    }
    for (idx in 1:nrow(zikaSubset)) {
      rowData <- zikaSubset[idx,]      
      zikaCasesByCountryOverTime[date, country] <- as.numeric(as.character(rowData$value)) +  zikaCasesByCountryOverTime[date, country]
    }
  }
  return (zikaCasesByCountryOverTime)
}


#main program
zikaData <- read.csv(file = "../input/cdc_zika.csv", header = TRUE, sep = ",")
countryNameList <- extractCountryNameList(zikaData)
zikaCasesByCountryTable <- createMatrixZikaCountry(countryNameList)
zikaCasesByCountryTable <- fillMatrixZikaCountry(zikaData, zikaCasesByCountryTable, countryNameList)
plotBar(zikaCasesByCountryTable, countryNameList)
#print(zikaCasesByCountryTable)


for(country in countryNameList) {
  if(country == "Haiti" || country == "Panama") {
    next
  }
  zikaCasesByCountryOverTime <-createMatrixZikaCountryOverTime(zikaData, country)
  zikaCasesByCountryOverTime <- fillMatrixZikaCountryOverTime(zikaData, zikaCasesByCountryOverTime, country) 
  #print(zikaCasesByCountryOverTime)
  zikaCasesByCountryOverTime <- zikaCasesByCountryOverTime[!apply(is.na(zikaCasesByCountryOverTime) | zikaCasesByCountryOverTime == 0, 1, all),]
  barplot(zikaCasesByCountryOverTime, names.arg=rownames(zikaCasesByCountryOverTime), cex.names=0.8, main=paste("Zika cases over time in ", country, sep = "") ) 
}








