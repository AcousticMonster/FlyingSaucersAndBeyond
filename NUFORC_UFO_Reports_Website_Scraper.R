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

NURFORCreports(c("201407","201408")) 

## Install the Function
NURFORCreports <- function(YearMonth) {

      ## Create a list from inputed arguments (ie. "201409" or c("201409","201410")) 
      x <-  as.list(YearMonth)      
      
      ## Determine the number of inputed Month/Year arguments
      y <- 1:length(YearMonth)
      
      
      ## Loop through each inputed Month/Year, build the data, and output to a CSV file
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
                      CanadaSightings <- NURFORCtable[NURFORCtable$State %in% Canada,]
                      OtherSightings <- NURFORCtable[NURFORCtable$State == "" & NURFORCtable$City != "",]
                     
                      ##---------------------------------------------------------------------------------
                      
                      ## USASightings Data Reports
                      
                      ## Count Number of UFO Shapes grouped by States
                      USAShapesCount <- subset(USASightings[, c(1, 3, 4)])
                      names(USAShapesCount) <- c("Reported", "State", "Shape")
                      
                      USAShapesCount$Shape <- replace(USAShapesCount$Shape, USAShapesCount$Shape =="", "Unknown")
                      USAShapesCount$ReportedMonth <- format(as.POSIXct(USAShapesCount$Reported,format='%m/%d/%Y'),format='%m')
                      USAShapesCount$ReportedYear <- format(as.POSIXct(USAShapesCount$Reported,format='%m/%d/%Y'),format='%Y')
                      USAShapesCount$Reported <- format(as.POSIXct(USAShapesCount$Reported,format='%m/%d/%Y'),format='%b %Y')
                                           
                      ## Counts
                      USAShapes <- USAShapesCount %>% group_by(Reported, State, Shape) %>% summarize(NumberSeen = length(Shape))
                      
                      # pivot USAShapes dataframe data and replace generated NA with 0
                      USAShapesFinalCount <- cast(USAShapes, Reported + State ~ Shape, value="NumberSeen") 
                      USAShapesFinalCount <- replace(USAShapesFinalCount, is.na(USAShapesFinalCount), "0")
                      
                      ##---------------------------------------------------------------------------------
                      
                      
                      ## CanadaSightings Data Reports
                      
                      
                      
                      ## OtherSightings Data Reports
                      
                      
                      
                      
                      ## Add NURFORCtable data to the NURFORCraw worksheet
                      addDataFrame(x=USASightings, sheet=USANURFORCraw, row.names=FALSE)
                      addDataFrame(x=CanadaSightings, sheet=CanadaNURFORCraw, row.names=FALSE)
                      addDataFrame(x=OtherSightings, sheet=OtherNURFORCraw, row.names=FALSE)
                      
                      addDataFrame(x=USAShapesFinalCount, sheet=USAShapeCount, row.names=FALSE)
                      
                      
                      
                      ## Save the workbook to output folder (R Directory)
                      saveWorkbook(ufoWB, ufoWBname)
 
                } ## End Loop

}






