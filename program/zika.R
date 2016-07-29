parseCountryName <- function(place) {
  location <- as.character(place)
  country <- strsplit(location, "-")[[1]][1]
  exceptionBrazilList = c("nordeste", "sudeste", "sul", "centro", "norte")
  exceptionUSList = c("united_states_virgin_islands")
  if (tolower(country) %in% exceptionBrazilList) {
    return("Brazil")
  }
  if(tolower(country) %in% exceptionUSList) {
    return("United_States")
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


fillMatrix <- function(zikaData, zikaCasesByCountryTable, countryNameList) {
  for(country in countryNameList) {
    zikaSubset <- NULL
    lastDate <- NULL
    if(country == "Brazil") {
      lastDate <- findMaxDate(zikaData, country, NULL)
      zikaSubset <- subset(zikaData,  as.character(zikaData$location) == country & zikaData$report_date == lastDate & 
                             as.character(zikaData$location_type) == "state" & as.character(zikaData$data_field) == "zika_reported"
                           & length(as.character(zikaData$value)) != 0L & is.na(zikaData$value) == FALSE)
    } else if(country == "Argentina") {
      lastDate <- findMaxDate(zikaData, country, NULL)
      zikaSubset <- subset(zikaData, substr(zikaData$location, 1, nchar(country)) == country & zikaData$report_date == lastDate 
                           & (as.character(zikaData$data_field) == "cumulative_confirmed_local_cases" |
                                as.character(zikaData$data_field) == "cumulative_probable_local_cases" | 
                                as.character(zikaData$data_field) == "cumulative_confirmed_imported_cases" |
                                as.character(zikaData$data_field) == "cumulative_probable_imported_cases")
                           & as.character(zikaData$location_type) == "province" 
                           & length(as.character(zikaData$value)) != 0L & is.na(zikaData$value) == FALSE)   
    } else if(country == "Colombia") {
      lastDate <- findMaxDate(zikaData, country, NULL)
      zikaSubset <- subset(zikaData, substr(zikaData$location, 1, nchar(country)) == country & zikaData$report_date == lastDate 
                           & (as.character(zikaData$data_field) == "zika_confirmed_laboratory" |
                                as.character(zikaData$data_field) == "zika_confirmed_clinic" | 
                                as.character(zikaData$data_field) == "zika_suspected" |
                                as.character(zikaData$data_field) == "zika_suspected_clinic")
                           & as.character(zikaData$location_type) == "municipality" 
                           & length(as.character(zikaData$value)) != 0L & is.na(zikaData$value) == FALSE)  
    } else if(country == "Dominican_Republic") {
      lastDate <- findMaxDate(zikaData, country, NULL)
      zikaSubset <- subset(zikaData, substr(zikaData$location, 1, nchar(country)) == country & zikaData$report_date == lastDate 
                           & (as.character(zikaData$data_field) == "zika_suspected_cumulative" |
                                as.character(zikaData$data_field) == "zika_confirmed_pcr_cumulative")
                           & as.character(zikaData$location_type) == "country" 
                           & length(as.character(zikaData$value)) != 0L & is.na(zikaData$value) == FALSE)  
    } else if(country == "Ecuador") {
      lastDate <- findMaxDate(zikaData, country, NULL)
      zikaSubset <- subset(zikaData, substr(zikaData$location, 1, nchar(country)) == country & zikaData$report_date == lastDate 
                           & as.character(zikaData$location_type) == "country" 
                           & length(as.character(zikaData$value)) != 0L & is.na(zikaData$value) == FALSE)
      
      
    } else if(country == "El_Salvador") {
      lastDate <- findMaxDate(zikaData, country, NULL)
      zikaSubset <- subset(zikaData, substr(zikaData$location, 1, nchar(country)) == country & zikaData$report_date == lastDate 
                           & (as.character(zikaData$data_field) == "cumulative_confirmed" |
                                as.character(zikaData$data_field) == "cumulative_suspected_total")
                           & as.character(zikaData$location_type) == "country" 
                           & length(as.character(zikaData$value)) != 0L & is.na(zikaData$value) == FALSE)  
      zikaSubset <- zikaSubset[ !duplicated(zikaSubset$data_field,fromLast=TRUE),]
    } else if(country == "Guatemala") {
      lastDate <- findMaxDate(zikaData, country, NULL)
      zikaSubset <- subset(zikaData, substr(zikaData$location, 1, nchar(country)) == country & zikaData$report_date == lastDate 
                           & (as.character(zikaData$data_field) == "total_zika_suspected_cumulative" )
                           & as.character(zikaData$location_type) == "country" 
                           & length(as.character(zikaData$value)) != 0L & is.na(zikaData$value) == FALSE)  
    } else if(country == "Haiti") {
      lastDate <- findMaxDate(zikaData, country, NULL)
      zikaSubset <- subset(zikaData, substr(zikaData$location, 1, nchar(country)) == country & zikaData$report_date == lastDate 
                           & (as.character(zikaData$data_field) == "total_zika_new_suspected_cumulative" )
                           & as.character(zikaData$location_type) == "country" 
                           & length(as.character(zikaData$value)) != 0L & is.na(zikaData$value) == FALSE)  
    } else if(country == "Mexico") {
      lastDate <- findMaxDate(zikaData, country, NULL)
      zikaSubset <- subset(zikaData, substr(zikaData$location, 1, nchar(country)) == country & zikaData$report_date == lastDate 
                           & (as.character(zikaData$data_field) == "yearly_cumulative_female" | 
                                as.character(zikaData$data_field) == "yearly_cumulative_male")
                           & as.character(zikaData$location_type) == "state" 
                           & length(as.character(zikaData$value)) != 0L & is.na(zikaData$value) == FALSE)
    } else if(country == "Nicaragua") {
      lastDate <- findMaxDate(zikaData, country, c("total_zika_confirmed_cumulative"))
      zikaSubset <- subset(zikaData, substr(zikaData$location, 1, nchar(country)) == country & zikaData$report_date == lastDate
                           & (as.character(zikaData$data_field) == "total_zika_confirmed_cumulative")
                           & as.character(zikaData$location_type) == "country" 
                           & length(as.character(zikaData$value)) != 0L & is.na(zikaData$value) == FALSE)
    } else if(country == "Panama") {
      lastDate <- findMaxDate(zikaData, country, NULL)
      zikaSubset <- subset(zikaData, substr(zikaData$location, 1, nchar(country)) == country & zikaData$report_date == lastDate
                           & (as.character(zikaData$data_field) == "Zika_confirmed_F" |
                                as.character(zikaData$data_field) == "Zika_confirmed_M")
                           & as.character(zikaData$location_type) == "country" 
                           & length(as.character(zikaData$value)) != 0L & is.na(zikaData$value) == FALSE)
    } else if(country == "United_States") {
      lastDate <- findMaxDate(zikaData, country, c("zika_reported"))
      lastDate2 <- findMaxDate(zikaData, country, c("yearly_reported_travel_cases"))
      
      zikaSubset1 <- subset(zikaData, substr(zikaData$location, 1, nchar(country)) == country & zikaData$report_date == lastDate
                           & (as.character(zikaData$data_field) == "zika_reported")
                           & as.character(zikaData$location_type) == "territory" 
                           & length(as.character(zikaData$value)) != 0L & is.na(zikaData$value) == FALSE)
      zikaSubset2 <- subset(zikaData, substr(zikaData$location, 1, nchar(country)) == country & zikaData$report_date == lastDate2
                           & (as.character(zikaData$data_field) == "yearly_reported_travel_cases")
                           & as.character(zikaData$location_type) == "state" 
                           & length(as.character(zikaData$value)) != 0L & is.na(zikaData$value) == FALSE)
      
      zikaSubset <- rbind(zikaSubset1[,], zikaSubset2[,])
    
    } else if(country == "Puerto_Rico") { 
      lastDate <- findMaxDate(zikaData, country, NULL)
      zikaSubset <- subset(zikaData, substr(zikaData$location, 1, nchar(country)) == country & zikaData$report_date == lastDate
                           & (as.character(zikaData$data_field) == "zika_confirmed_cumulative_2015-2016")
                           & as.character(zikaData$location_type) == "territory" 
                           & length(as.character(zikaData$value)) != 0L & is.na(zikaData$value) == FALSE)
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
  barplot(counts, names.arg=substr(countryNameList, 1, 5), cex.names=0.8, main="Zika Cases by Country" ) 
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

#main program
zikaData <- read.csv(file = "../input/cdc_zika.csv", header = TRUE, sep = ",")
countryNameList <- extractCountryNameList(zikaData)
zikaCasesByCountryTable <- createMatrixZikaCountry(countryNameList)
zikaCasesByCountryTable <- fillMatrix(zikaData, zikaCasesByCountryTable, countryNameList)
plotBar(zikaCasesByCountryTable, countryNameList)
#print(zikaCasesByCountryTable)






