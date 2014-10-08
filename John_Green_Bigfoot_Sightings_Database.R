## R Programming Project (Flying Saucers and Beyond)
## Based on free data from Bigfoot Researcher John Green
## Found at website: (http://www.sasquatchdatabase.com/)
## Original Author: Danny Korves (a.k.a. AcousticMonster)
## Original Author Date: 10-08-2014

## Notes: 
## John Green's original data was extremely dirty.
## Bad formatting, poor data structure, combined keys, etc.
## I took liberty in cleaning what I could, but some data
## was unusable. The reported data in this program is a 
## percentage of the original data.


##-----------------------------------------------------------------------

## Hypothesis: Has Bigfoot become an Fat American?
## Guesstimating Bigfoots Body Mass Index (BMI) 

## Notes: 
## 1. John Green Database (table = tbl_creature).
## 2. Fields used c_height, c_build.
## 3. Only 558 of 3636 records contain numeric measurements in Feet and 
##    a linkable observation date (15.35%).
## 4. Incases of estimated heights (ex. 7-9ft), the higher number was used.
## 5. No numeric weight measurements given, only descriptions (very thin, 
##    thin, medium, heavy, very heavy).
## 6. I'm using the Extrapolated Weight Estimates table (Table 1) from the
##    website (http://www.bigfootencounters.com/biology/bfphysics.htm) to
##    estimate the numeric weight measurements.
## 7. BMI calculations based on the CDC formulas located at the following
##    website (http://www.cdc.gov/healthyweight/assessing/bmi/adult_bmi/index.html?s_cid=tw_ob064)
## 8. My raw data extract (Bigfoot_Height_Build_Raw_Data 10-8-2014.xls)

## Directory name "CSV Raw Bigfoot Files"
## Example bigfootfitness("CSV Raw Bigfoot Files")

       
## Uses the dplyr library
## load library
library("dplyr")

## Load raw usable John Green data (Observation Date, Height, Build)
bigfootcsv <- read.csv("CSV Raw Bigfoot Files/Bigfoot_Height_Build_Raw_Data 10-8-2014.csv", colClasses = "character")
bigfootFitness <- tbl_df(bigfootcsv)

## find numeric value in the Height string, convert to Feet
bigfootFitness <- mutate(bigfootFitness, HeightFeet = suppressWarnings(as.numeric(gsub(" .*$|.*-|over ", "", Height))))
## gsub filters 
## ".*-" Finds number after the "-"
## " .*$" Finds before a space
## "over " Finds number after the word "over"
## "|" = OR

## Convert Feet to Inches
bigfootFitness <- mutate(bigfootFitness, HeightInches = HeightFeet * 12)


## Guesstimate Bigfoot Weight based on the "Extrapolated Weight Estimates - Table 1" 
## Notes:
## 1. There are 7 weight measures used in the Extrapolated table (300 lbs, 381 lbs,
##    476 lbs, 529 lbs, 586 lbs, 711 lbs, 1012 lbs).
## 2. There are only 5 build types in the John Green Data (very thin, thin, medium,
##    heavy, very heavy)
## 3. Guesstimating numbers used: 
##      very thin = 300 lbs
##      thin = 428.5 lbs (median of 381 & 476)
##      medium = 529 lbs1
##      heavy = 648.5 lbs (median of 586 & 711)
##      very heavy = 1012 lbs
## 4. See "avgbfoot.csv" file in "CSV Raw Bigfoot Files"

bigfootFitness <- mutate(bigfootFitness, EstimatedWeightPounds = ifelse(Build == "very thin", 300, 
                                                                ifelse(Build == "thin", 428.5, 
                                                                ifelse(Build == "medium", 529, 
                                                                ifelse(Build == "heavy", 648.5, 
                                                                ifelse(Build == "very heavy", 1012, 0))))))


## Calculate Bigfoot BMI base on CDC human standards
bigfootFitness <- mutate(bigfootFitness, BMI = EstimatedWeightPounds / (HeightInches^2) * 703)

## Add CDC BMI Weight Status Class (based on human standards)
bigfootFitness <- mutate(bigfootFitness, CDCWeightStatus = ifelse(BMI < 18.5, "Underweight", 
                                                                 ifelse(BMI >= 18.5 & BMI <= 24.9,"Normal",
                                                                 ifelse(BMI >= 25.0 & BMI <= 29.9, "Overweight", 
                                                                 ifelse(BMI > 30.0, "Obese", "Unknown")))))

## Export bigfootFitness to CSV file
svfilename <- paste("CSV Final Output Files/Guesstimated Bigfoot Body Mass Index ", Sys.Date(), ".csv", sep="")
write.csv(bigfootFitness, file = svfilename, row.names=FALSE)    

##-----------------------------------------------------------------------

## Play with dplyr functions
select(bigfootFitness, ObservationDate, HeightInches, EstimatedWeightPounds, BMI, CDCWeightStatus)

filter(bigfootFitness, EstimatedWeightPounds == 300)

arrange(bigfootFitness, desc(HeightInches))

summarize(bigfootFitness, AverageWeight = mean(EstimatedWeightPounds))

       
##-----------------------------------------------------------------------