## R Programming Project (Flying Saucers and Beyond)
## Based on the free data found at the NUFORC website: 
## (http://www.nuforc.org/webreports.html)
## The National UFO Reporting Center Online Database 
## Original Author: Danny Korves (a.k.a. AcousticMonster)
## Original Author Date: 10-16-2014

##########################################################
## NURFORC UFO Reports - Webpage Scraper Function       ##
##########################################################

## This function is used to scrape the NURFORC UFO reporting data
## from their various website pages (format in html tables).
## 1. Simply install function 
## 2. Run using Year/Month input format. Examples:
##      - NURFORCreports("201409")
##      - NURFORCreports(c("201409","201410")) 

## Packages to install and use 
## install.packages("XML")
## install.packages("rJava")
## install.packages("xlsx")
## install.packages("xlsxjars")
## install.packages("dplyr")
## install.packages("reshape")

## Package Notes
## If you are running 64bit Windows Operating System you must install 64bit Java
## before installing the rJava and xlsx packages.

## Load the needed libraries
library(XML)
library(rJava)
library(xlsx)
library(dplyr)
library(reshape)

## Install the Function
NURFORCreports <- function(YearMonth) {

      ## Create a list from inputed arguments (ie. "201409" or c("201409","201410")) 
      x <-  as.list(YearMonth)      
      
      ## Determine the number of inputed Month/Year arguments
      y <- 1:length(YearMonth)
      
      ## Create a list of workbook names (for final printed message)
      WorkbookNames <- NULL
      
      ## Loop through each inputed Month/Year, build the data, and output to an Excel file (.xlsx)
      ## Output files will be located in your R working directory
      for(i in y) {
                      
                      ## Create new xlsx (Excel) workbook
                      ufoWB <- createWorkbook()
                      
                      ## Create the unique workbook file name 
                      ufoWBname <- gsub(" ","", paste("NURFORC_UFO_Report_Data_", x[i], ".xlsx")) 
                      
                      ## Create the worksheets within the workbook
                      USANURFORCraw <- createSheet(wb = ufoWB, sheetName = "USA NURFORC Raw Data")
                      CanadaNURFORCraw <- createSheet(wb = ufoWB, sheetName = "Canada NURFORC Raw Data")
                      OtherNURFORCraw <- createSheet(wb = ufoWB, sheetName = "Other NURFORC Raw Data")
                      
                      USAShapeCount <- createSheet(wb = ufoWB, sheetName = "USA UFO Shapes Count")
                      CanadaShapeCount <- createSheet(wb = ufoWB, sheetName = "Canada UFO Shapes Count")
                      OtherShapeCount <- createSheet(wb = ufoWB, sheetName = "Other UFO Shapes Count")
                      
                      USALengthSeen <- createSheet(wb = ufoWB, sheetName = "USA UFO Shapes Seconds")
                      CanadaLengthSeen <- createSheet(wb = ufoWB, sheetName = "Canada UFO Shapes Seconds")
                      OtherLengthSeen <- createSheet(wb = ufoWB, sheetName = "Other UFO Shapes Seconds")
                      
                      
                      ## Create the unique url website link to scrape data from
                      url <-  gsub(" ","", paste("http://www.nuforc.org/webreports/ndxe", x[i], ".html"))
                      
                      ## Build the initial dataset from the selected website
                      ## Retrieve the data
                      NURFORCdata <- readHTMLTable(url, as.data.frame=TRUE)
                      ## Convert list to data frame
                      NURFORCtable <- as.data.frame(NURFORCdata)
                      
                      ## Cleanup Column Names
                      names(NURFORCtable) <- c("Reported", "City", "State", "UFO_Shape", "Duration", "Summary", "Posted")
                      
                      ## Split the data into three seperate data tables (USA, Canada, and Other)
                      ## Note: the "Other" data table includes UFO reports from outside the USA
                      ## and Canada territories.
                      
                      ## Build USA States and Canada Provinces
                      Canada <- c("AB", "BC", "MB", "NB", "NL", "NT", "NS", "NU", "ON", "PE", "QC", "SK", "YT")
                      
                      USA <- c("AA","AE","AP","AL","AK","AS","AZ","AR","CA","CO","CT","DE","DC","FM","FL","GA",
                               "GU","HI","ID","IL","IN","IA","KS","KY","LA","ME","MH","MD","MA","MI","MN","MS",
                               "MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","MP","OH","OK","OR","PW","PA",
                               "PR","RI","SC","SD","TN","TX","UT","VT","VI","VA","WA","WV","WI","WY")
                      
                      
                     
                      ## Subset the data into USA, Canada, and all other countries                      
                      USASightings <- NURFORCtable[NURFORCtable$State %in% USA,]                      
                      USASightings$Reported <- as.Date(USASightings$Reported, format = "%m/%d/%y")
                      
                      CanadaSightings <- NURFORCtable[NURFORCtable$State %in% Canada,]
                      CanadaSightings$Reported <- as.Date(CanadaSightings$Reported, format = "%m/%d/%y")
                                            
                      OtherSightings <- NURFORCtable[NURFORCtable$State == "" & NURFORCtable$City != "",]
                      OtherSightings$Reported <- as.Date(OtherSightings$Reported, format = "%m/%d/%y")
 
                      
                      
                      ###############################
                      ## USASightings Data Reports ##
                      ####################################################################################################
                      
                      ## Count Number of UFO Shapes grouped by States                      
                      ## subset data to the three columns needed and rename the columns
                      USAShapesCount <- subset(USASightings[, c(1, 3, 4)])
                      names(USAShapesCount) <- c("Reported", "State", "Shape")
                       
                      ## Replace blank/Null shapes with "Unknown"
                      USAShapesCount$Shape <- replace(USAShapesCount$Shape, USAShapesCount$Shape =="", "Unknown")
                      
                      ## Reformat Reported datetime to a date format 
                      USAShapesCount$Reported <- format(USAShapesCount$Reported,format='%b %Y')
                      
                      
                      ## Group and Count the shapes by State
                      USAShapes <- USAShapesCount %>% group_by(Reported, State, Shape) %>% summarize(NumberSeen = length(Shape))
                      
                      ## pivot USAShapes dataframe data and replace generated NA with 0
                      USAShapesFinalCount <- cast(USAShapes, Reported + State ~ Shape, value="NumberSeen") 
                      USAShapesFinalCount <- replace(USAShapesFinalCount, is.na(USAShapesFinalCount), "0")
                      
                      
                      ##----- Next Tab ------
                      
                      
                      ## Calculate the number of seconds seen by State and Shape Types
                      
                      ## Reformat Reported datetime to a date format 
                      USASightings$Reported <- format(USASightings$Reported,format='%b %Y')
                      
                      ## Split Duration to find seconds, minutes, hours, unusable types
                      USASightings$DurationType <- ifelse(grepl("minute", USASightings$Duration), "minutes", 
                                                ifelse(grepl("second", USASightings$Duration), "seconds", 
                                                ifelse(grepl("hour", USASightings$Duration), "hours", "unusable")))
                      
                      ## find numeric value in the string for time calculation
                      USASightings$DurationAmount <- suppressWarnings(as.numeric(gsub(" .*$|.*-|.*~|.*>", "", USASightings$Duration)))
                      ## gsub filters 
                      ## "-.*$" Finds number before the "-"
                      ## ".*-" Finds number after the "-"
                      ## " .*$" Finds before a space
                      ## "|" is equal to an OR
                      
                      ## Replace the values NA with 0
                      USASightings <- replace(USASightings, is.na(USASightings), "0")
                      
                      ## Convert (seconds, minutes, hours) to seconds
                      USASightings$DurationSeconds <- ifelse(USASightings$DurationType == "seconds", as.numeric(USASightings$DurationAmount), 
                                                ifelse(USASightings$DurationType == "minutes", as.numeric(USASightings$DurationAmount) * 60, 
                                                ifelse(USASightings$DurationType == "hours", as.numeric(USASightings$DurationAmount) * 3600, 0)))
                      
                      
                      ## Group the data and calculate the time summaries
                      USASightingsTime <- USASightings %>% group_by(Reported, State, UFO_Shape) %>% 
                              summarize(NumberSeen = length(UFO_Shape), 
                                        DurationInSeconds = sum(DurationSeconds), 
                                        DurationSecondsMin = min(DurationSeconds),
                                        DurationSecondsMax = max(DurationSeconds), 
                                        DurationSecondsMean = mean(DurationSeconds),
                                        DurationSecondsMedian = median(DurationSeconds),
                                        DurationSecondsStandardDeviation = sd(DurationSeconds)) 
                      
                      USASightingsTimeFinal <- as.data.frame(USASightingsTime)
                      
                      ####################################################################################################

                      
                      
                      ##################################                      
                      ## CanadaSightings Data Reports ##
                      ####################################################################################################
                      
                      ## Count Number of UFO Shapes grouped by Province                      
                      ## subset data to the three columns needed and rename the columns
                      CanadaShapesCount <- subset(CanadaSightings[, c(1, 3, 4)])
                      names(CanadaShapesCount) <- c("Reported", "Province", "Shape")
                      
                      ## Replace blank/Null shapes with "Unknown"
                      CanadaShapesCount$Shape <- replace(CanadaShapesCount$Shape, CanadaShapesCount$Shape =="", "Unknown")
                      
                      ## Reformat Reported datetime to a date format 
                      CanadaShapesCount$Reported <- format(CanadaShapesCount$Reported,format='%b %Y')
                      
                      ## Group and Count the shapes by Province
                      CanadaShapes <- CanadaShapesCount %>% group_by(Reported, Province, Shape) %>% summarize(NumberSeen = length(Shape))
                      
                      ## pivot CanadaShapes dataframe data and replace generated NA with 0
                      CanadaShapesFinalCount <- cast(CanadaShapes, Reported + Province ~ Shape, value="NumberSeen") 
                      CanadaShapesFinalCount <- replace(CanadaShapesFinalCount, is.na(CanadaShapesFinalCount), "0")
                                            
                      ##----- Next Tab ------
                      
                      
                      ## Calculate the number of seconds seen by Province and Shape Types
                      
                      ## Reformat Reported datetime to a date format 
                      CanadaSightings$Reported <- format(CanadaSightings$Reported,format='%b %Y')
                      
                      ## Split Duration to find seconds, minutes, hours, unusable types
                      CanadaSightings$DurationType <- ifelse(grepl("minute", CanadaSightings$Duration), "minutes", 
                                                          ifelse(grepl("second", CanadaSightings$Duration), "seconds", 
                                                                 ifelse(grepl("hour", CanadaSightings$Duration), "hours", "unusable")))
                      
                      ## find numeric value in the string for time calculation
                      CanadaSightings$DurationAmount <- suppressWarnings(as.numeric(gsub(" .*$|.*-|.*~|.*>", "", CanadaSightings$Duration)))
                      ## gsub filters 
                      ## "-.*$" Finds number before the "-"
                      ## ".*-" Finds number after the "-"
                      ## " .*$" Finds before a space
                      ## "|" is equal to an OR
                      
                      ## Replace the values NA with 0
                      CanadaSightings <- replace(CanadaSightings, is.na(CanadaSightings), "0")
                      
                      ## Convert (seconds, minutes, hours) to seconds
                      CanadaSightings$DurationSeconds <- ifelse(CanadaSightings$DurationType == "seconds", as.numeric(CanadaSightings$DurationAmount), 
                                                        ifelse(CanadaSightings$DurationType == "minutes", as.numeric(CanadaSightings$DurationAmount) * 60, 
                                                        ifelse(CanadaSightings$DurationType == "hours", as.numeric(CanadaSightings$DurationAmount) * 3600, 0)))                   
                      
                      ## Group the data and calculate the time summaries
                      CanadaSightingsTime <- CanadaSightings %>% group_by(Reported, State, UFO_Shape) %>% 
                              summarize(NumberSeen = length(UFO_Shape), 
                                        DurationInSeconds = sum(DurationSeconds), 
                                        DurationSecondsMin = min(DurationSeconds),
                                        DurationSecondsMax = max(DurationSeconds), 
                                        DurationSecondsMean = mean(DurationSeconds),
                                        DurationSecondsMedian = median(DurationSeconds),
                                        DurationSecondsStandardDeviation = sd(DurationSeconds)) 
                      
                      CanadaSightingsTimeFinal <- as.data.frame(CanadaSightingsTime)
                      
                      names(CanadaSightingsTimeFinal)[2] <- "Province"                 
                      
                      ####################################################################################################
                      
                     
                      
                      #################################
                      ## OtherSightings Data Reports ##
                      ####################################################################################################
                      
                      ## Count Number of UFO Shapes grouped by OtherLocation                     
                      ## subset data to the three columns needed and rename the columns
                      OtherShapesCount <- subset(OtherSightings[, c(1, 2, 4)])
                      names(OtherShapesCount) <- c("Reported", "OtherLocation", "Shape")
                      
                      ## Replace blank/Null shapes with "Unknown"
                      OtherShapesCount$Shape <- replace(OtherShapesCount$Shape, OtherShapesCount$Shape =="", "Unknown")
                      
                      ## Reformat Reported datetime to a date format 
                      OtherShapesCount$Reported <- format(OtherShapesCount$Reported,format='%b %Y')
                      
                      
                      ## Group and Count the shapes by OtherLocation
                      OtherShapes <- OtherShapesCount %>% group_by(Reported, OtherLocation, Shape) %>% summarize(NumberSeen = length(Shape))
                      
                      ## pivot OtherShapes dataframe data and replace generated NA with 0
                      OtherShapesFinalCount <- cast(OtherShapes, Reported + OtherLocation ~ Shape, value="NumberSeen") 
                      OtherShapesFinalCount <- replace(OtherShapesFinalCount, is.na(OtherShapesFinalCount), "0")
                      
                      ##----- Next Tab ------
                      
                      
                      ## Calculate the number of seconds seen by Other Countries and Shape Types
                      
                      ## Reformat Reported datetime to a date format 
                      OtherSightings$Reported <- format(OtherSightings$Reported,format='%b %Y')
                      
                      ## Split Duration to find seconds, minutes, hours, unusable types
                      OtherSightings$DurationType <- ifelse(grepl("minute", OtherSightings$Duration), "minutes", 
                                                          ifelse(grepl("second", OtherSightings$Duration), "seconds", 
                                                                 ifelse(grepl("hour", OtherSightings$Duration), "hours", "unusable")))
                      
                      ## find numeric value in the string for time calculation
                      OtherSightings$DurationAmount <- suppressWarnings(as.numeric(gsub(" .*$|.*-|.*~|.*>", "", OtherSightings$Duration)))
                      ## gsub filters 
                      ## "-.*$" Finds number before the "-"
                      ## ".*-" Finds number after the "-"
                      ## " .*$" Finds before a space
                      ## "|" is equal to an OR
                      
                      ## Replace the values NA with 0
                      OtherSightings <- replace(OtherSightings, is.na(OtherSightings), "0")
                      
                      ## Convert (seconds, minutes, hours) to seconds
                      OtherSightings$DurationSeconds <- ifelse(OtherSightings$DurationType == "seconds", as.numeric(OtherSightings$DurationAmount), 
                                                             ifelse(OtherSightings$DurationType == "minutes", as.numeric(OtherSightings$DurationAmount) * 60, 
                                                                    ifelse(OtherSightings$DurationType == "hours", as.numeric(OtherSightings$DurationAmount) * 3600, 0)))
                      
                      
                      ## Group the data and calculate the time summaries
                      OtherSightingsTime <- OtherSightings %>% group_by(Reported, City, UFO_Shape) %>% 
                              summarize(NumberSeen = length(UFO_Shape), 
                                        DurationInSeconds = sum(DurationSeconds), 
                                        DurationSecondsMin = min(DurationSeconds),
                                        DurationSecondsMax = max(DurationSeconds), 
                                        DurationSecondsMean = mean(DurationSeconds),
                                        DurationSecondsMedian = median(DurationSeconds),
                                        DurationSecondsStandardDeviation = sd(DurationSeconds)) 
                      
                      OtherSightingsTimeFinal <- as.data.frame(OtherSightingsTime)
                      
                      names(OtherSightingsTimeFinal)[2] <- "Location"
                      
                      ####################################################################################################
                      
                      
                      ## Output the Data to the Xcel Workbook
                      
                      ## Add NURFORCtable data to the Raw Data worksheets
                      addDataFrame(x=USASightings, sheet=USANURFORCraw, row.names=FALSE)
                      addDataFrame(x=CanadaSightings, sheet=CanadaNURFORCraw, row.names=FALSE)
                      addDataFrame(x=OtherSightings, sheet=OtherNURFORCraw, row.names=FALSE)
                      
                      ## Add NURFORCtable data to the Shape Count worksheets
                      addDataFrame(x=USAShapesFinalCount, sheet=USAShapeCount, row.names=FALSE)
                      addDataFrame(x=CanadaShapesFinalCount, sheet=CanadaShapeCount, row.names=FALSE)
                      addDataFrame(x=OtherShapesFinalCount, sheet=OtherShapeCount, row.names=FALSE)
                      
                      ## Add NURFORCtable data to the Seconds Seen worksheets
                      addDataFrame(x=USASightingsTimeFinal, sheet=USALengthSeen, row.names=FALSE) 
                      addDataFrame(x=CanadaSightingsTimeFinal, sheet=CanadaLengthSeen, row.names=FALSE) 
                      addDataFrame(x=OtherSightingsTimeFinal, sheet=OtherLengthSeen, row.names=FALSE) 
                      
                      
                      
                      ## Save the workbook to output folder (R Directory)
                      saveWorkbook(ufoWB, ufoWBname)
                      
                      ## Save current workbook name to list
                      WorkbookNames <- rbind(WorkbookNames, ufoWBname)
                      
                } ## End Loop

#         OutputTabs <- c("USA NURFORC Raw Data", "Canada NURFORC Raw Data", "Other NURFORC Raw Data", "USA UFO Shapes Count", 
#             "Canada UFO Shapes Count", "Other UFO Shapes Count", "USA UFO Shapes Seconds", "Canada UFO Shapes Seconds", 
#             "Other UFO Shapes Seconds")
#         
#         FinishMessage <- paste("Created the following Excel worksheets:", OutputTabs, " ", "In the following Excel workbooks:", WorkbookNames[,1])
#        FinishMessage   

}


## Test the Function
NURFORCreports(c("201407","201408")) 
