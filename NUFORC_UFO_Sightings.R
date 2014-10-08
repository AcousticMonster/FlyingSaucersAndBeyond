## R Programming Project (Flying Saucers and Beyond)
## Based on the free data found at the NUFORC website: 
## (http://www.nuforc.org/webreports.html)
## The National UFO Reporting Center Online Database 
## Original Author: Danny Korves (a.k.a. AcousticMonster)
## Original Author Date: 9-25-2014

## Note: must install the dplyr package "install.packages("dplyr")"


## Function to sum UFO Sightings Data for either "USA", "Canada", or "Foreign"
## From R directory "CSV Raw UFO Files" and output to new CSV file called "xfiles.csv"
## example: ufofiles("CSV Raw UFO Files", "USA")


## Uses the dplyr library
## load library
library("dplyr")



ufofiles <- function(directory, country) {
        
        ## Uses the dplyr library
        ## load library
        library("dplyr")
        
        ## Build USA States, Canada Provinces
        Canada <- c("AB", "BC", "MB", "NB", "NL", "NT", "NS", "NU", "ON", "PE", "QC", "SK", "YT")
        USA <- c("AA","AE","AP","AL","AK","AS","AZ","AR","CA","CO","CT","DE","DC","FM","FL","GA",
        "GU","HI","ID","IL","IN","IA","KS","KY","LA","ME","MH","MD","MA","MI","MN","MS",
        "MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","MP","OH","OK","OR","PW","PA",
        "PR","RI","SC","SD","TN","TX","UT","VT","VI","VA","WA","WV","WI","WY")
        
        ## Create list of file names in directory "UFO CSV Files"
        NUFORC_files <- list.files(directory, full.names=TRUE)        
        
        ## Determine the number of files in the directory folder
        id = 1:length(NUFORC_files)
        
        ## Create empty data frame for the row bind loop
        ufodata <- data.frame() 
        
        for (i in id) { 
                
                ## get the csv data from current file name in loop
                initial_ufodata <- read.csv(NUFORC_files[i], colClasses = "character")
                
                ## check to see which country was selected
                if(country == "USA") {
                        ufodata <- rbind(ufodata, initial_ufodata[initial_ufodata$State %in% USA, ])
                }
                else if (country == "Canada") {
                        ufodata <- rbind(ufodata, initial_ufodata[initial_ufodata$State %in% Canada, ])
                }
                else if (country == "Foreign") {
                        ufodata <- rbind(ufodata, initial_ufodata[initial_ufodata$State == "" & initial_ufodata$City != "", ])                                       
                }
                
        }
        
        ## Cleanup Column Names
        names(ufodata) <- c("Reported", "City", "State", "UFO_Shape", "Duration", "Summary", "Posted")
        
        ## Add Reported Month/Year columns for group purposes
        ufodata$ReportedMonth <- format(as.POSIXct(ufodata$Reported,format='%m/%d/%Y'),format='%m')
        ufodata$ReportedYear <- format(as.POSIXct(ufodata$Reported,format='%m/%d/%Y'),format='%Y')
        
        ## Split Duration to find seconds, minutes, hours, unusable types
        ufodata$DurationType <- ifelse(grepl("minute", ufodata$Duration), "minutes", 
        ifelse(grepl("second", ufodata$Duration), "seconds", 
        ifelse(grepl("hour", ufodata$Duration), "hours", "unusable")))
                
        ## find numeric value in the string for time calculation
        ufodata$DurationAmount <- suppressWarnings(as.numeric(gsub(" .*$|.*-|.*~|.*>", "", ufodata$Duration)))
        ## gsub filters 
        ## "-.*$" Finds number before the "-"
        ## ".*-" Finds number after the "-"
        ## " .*$" Finds before a space
        ## "|" is equal to an OR
              
        ## Replace the values NA with 0
        ufodata <- replace(ufodata, is.na(ufodata), "0")
        
        ## Convert (seconds, minutes, hours) to seconds
        ufodata$DurationSeconds <- ifelse(ufodata$DurationType == "seconds", as.numeric(ufodata$DurationAmount), 
        ifelse(ufodata$DurationType == "minutes", as.numeric(ufodata$DurationAmount) * 60, 
        ifelse(ufodata$DurationType == "hours", as.numeric(ufodata$DurationAmount) * 3600, 0)))
        
        ## Group the data and export to "xfiles.csv" file 
        xfiles <- ufodata %>% group_by(ReportedYear, ReportedMonth, State, UFO_Shape) %>% 
        summarize(NumberSeen = length(UFO_Shape), 
        DurationInSeconds = sum(DurationSeconds), 
        DurationSecondsMin = min(DurationSeconds),
        DurationSecondsMax = max(DurationSeconds), 
        DurationSecondsMean = mean(DurationSeconds),
        DurationSecondsMedian = median(DurationSeconds),
        DurationSecondsStandardDeviation = sd(DurationSeconds))  

        csvfilename <- paste("CSV Final Output Files/", country, " xfiles ", Sys.Date(), ".csv", sep="")
        
        write.csv(xfiles, file = csvfilename, row.names = FALSE)
   

}

##-----------------------------------------------------------------------------------------------

## gsub filters 
## "-.*$" Finds number before the "-"
## ".*-" Finds number after the "-"
## " .*$" Finds before a space
## "|" is equal to an OR

## Workout function details above
library("dplyr")
xfactor <- read.csv("CSV Raw UFO Files/NUFORC_UFO_SIGHTINGS_09-2014.csv", colClasses = "character")
xfactor <- xfactor[5]
xfactor$DurationType <- ifelse(grepl("minute", xfactor$Duration), "minutes", ifelse(grepl("second", xfactor$Duration), "seconds", ifelse(grepl("hour", xfactor$Duration), "hours", "unusable")))
xfactor$DurationAmount <- suppressWarnings(as.numeric(gsub(" .*$|.*-|.*~|.*>", "", xfactor$Duration)))
xfactor <- replace(xfactor, is.na(xfactor), "0")
xfactor
head(xfactor)

##------------------------------------------------------------------------------------------------

## Practice Pivot (using table and xtabs method)
## TV Series X-Files

USA <- c("AA","AE","AP","AL","AK","AS","AZ","AR","CA","CO","CT","DE","DC","FM","FL","GA",
         "GU","HI","ID","IL","IN","IA","KS","KY","LA","ME","MH","MD","MA","MI","MN","MS",
         "MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","MP","OH","OK","OR","PW","PA",
         "PR","RI","SC","SD","TN","TX","UT","VT","VI","VA","WA","WV","WI","WY")


## read csv data file, subset data to State and Shape columns
smokingman <- read.csv("CSV Raw UFO Files/NUFORC_UFO_SIGHTINGS_09-2014.csv", colClasses = "character")
mulder <- subset(smokingman[, c(3, 4)], smokingman$State %in% USA)

## For blank UFO Shape types reclassify as "Unknown"
mulder$Shape <- replace(mulder$Shape, mulder$Shape =="", "Unknown")

## create table with mulder dataframe data (for margin, prop tabling below)
scully <- table(mulder$State, mulder$Shape)

## Pivot data and count Shape types per States
xfiles <- xtabs(~ State + Shape, mulder)

## Export xfiles to CSV file
svfilename <- paste("CSV Final Output Files/xfiles USA ", Sys.Date(), ".csv", sep="")
write.csv(xfiles, file = svfilename, row.names=TRUE)


## Additional Play
## Get Marginal Fequencies
margin.table(scully, 1) ## Row marginal fequencies
margin.table(scully, 2) ## Column marginal fequencies

## Get Proportions
## With rounding to get just 2 decimal places
round(prop.table(scully), 2) ## Cell Percentages
round(prop.table(scully, 1), 2) ## Row Percentages
round(prop.table(scully, 2), 2) ## Column Percentages


## Pearson's Chi-squared test 
## (x-squared, df = degrees of freedom, p-value = probablity value)
## statistical significants p-value should be tested by .05
chisq.test(scully) 

##------------------------------------------------------------------------------------------------

## Practice Pivot (using "reshape" package)
## TV Series Kolchak: The Night Stalker
## Install package for use of reshape library
##install.packages("reshape")
library("reshape")
library("dplyr")

## Get List of State Abreviations
USA <- c("AA","AE","AP","AL","AK","AS","AZ","AR","CA","CO","CT","DE","DC","FM","FL","GA",
         "GU","HI","ID","IL","IN","IA","KS","KY","LA","ME","MH","MD","MA","MI","MN","MS",
         "MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","MP","OH","OK","OR","PW","PA",
         "PR","RI","SC","SD","TN","TX","UT","VT","VI","VA","WA","WV","WI","WY")

## Create list of file names in directory "UFO CSV Files"
NUFORC_files <- list.files("CSV Raw UFO Files", full.names=TRUE)      

## Determine the number of files in the directory folder
id = 1:length(NUFORC_files)

## Create empty data frame for the row bind loop
updyke <- data.frame() 

## Loop through each CSV file in the directory folder
for (i in id) { 
        
        ## read csv data file, subset data to State and Shape columns
        vincenzo <- read.csv(NUFORC_files[i], colClasses = "character")        
        updyke <- rbind(updyke, vincenzo[vincenzo$State %in% USA, ])

}


## Subset Data, Cleanup Column Names, 2. Change blank Shapes to Unknown, 3. Add Month/Year Columns for grouping
updyke <- subset(updyke[, c(1, 3, 4)])
names(updyke) <- c("Reported", "State", "Shape")
updyke$Shape <- replace(updyke$Shape, updyke$Shape =="", "Unknown")
updyke$ReportedMonth <- format(as.POSIXct(updyke$Reported,format='%m/%d/%Y'),format='%m')
updyke$ReportedYear <- format(as.POSIXct(updyke$Reported,format='%m/%d/%Y'),format='%Y')
updyke$Reported <- format(as.POSIXct(updyke$Reported,format='%m/%d/%Y'),format='%b %Y')


## Count Number of UFO Shapes Per State
INS <- updyke %>% group_by(Reported, State, Shape) %>% summarize(NumberSeen = length(Shape))

# pivot INS dataframe data and replace generated NA with 0
kolchak <- cast(INS, Reported + State ~ Shape, value="NumberSeen") 
kolchak <- replace(kolchak, is.na(kolchak), "0")

## Export xfiles to CSV file
svfilename <- paste("CSV Final Output Files/INS Daily News USA Edition ", Sys.Date(), ".csv", sep="")
write.csv(kolchak, file = svfilename, row.names=FALSE)


                 
                 
                 
                 
                 
                 
                 
                 
                 
                 