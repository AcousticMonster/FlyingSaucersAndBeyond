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
## install.packages("dplyr") 

## Load the needed libraries
library(XML)
library(dplyr)

## Install the Function
NURFORCreports <- function(YearMonth) {

      ## Create a list from inputed arguments (ie. "201409" or c("201409","201410")) 
      x <-  as.list(YearMonth)      
      
      ## Determine the number of inputed Month/Year arguments
      y <- 1:length(YearMonth)
      
      ## Loop through each inputed Month/Year, build the data, and output to a CSV file
      ## Output files will be located in your R working directory
      for(i in y) {
              
              url <-  gsub(" ","", paste("http://www.nuforc.org/webreports/ndxe", x[i], ".html"))
              NURFORCtable <- readHTMLTable(url)
              
              fileName <- gsub(" ","", paste(x[i], ".csv"))        
              write.csv(NURFORCtable, file = fileName, row.names=FALSE)              

              
      }

}

NURFORCreports(c("201409","201410")) 








