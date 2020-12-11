remove(list = ls())
install.packages("dplyr")
install.packages("cansim")
install.packages("openxlsx", dependencies = TRUE)
library(openxlsx)
library(cansim)
library(dplyr)
#=========================================================================================================
#Question 1: Do millennials actually make less money than boomers?

#Year ranges to use: 1964 - 1975 (Baby boomers would have been 18-29 years old)
#                    2001 - 2012 (20-31 for millenials because that's the only available data)

#First we read data off of the Historical Statistics of Canada website from StatCan, which comes
#in the form of csvs. Then it's just a matter of filtering the text to get the values we want
#We can't use the regular statcan data because it doesn't go back far enough.
#THe reason we use weekly wage data is because that's the only readily available comparable
#data we could find - we also had to make some reasonable assumptions as far as the
#industries go.

oldwages <- read.csv("https://www150.statcan.gc.ca/n1/pub/11-516-x/sectione/E86_103-eng.csv")
oldwages2 <- filter(oldwages, Series.E86.103. %in% 1964:1975)
oldwages3 <- oldwages2[, c(2, 3, 5, 9, 12, 14, 20, 22)]
colnames(oldwages3) <- c("Year", "Forestry", "Mining", "Manufacturing", "Construction", "Transportation
                        &Storage", "Utilities", "Finance&Insurance")
oldwages3 <- oldwages3[order(nrow(oldwages2):1), ]


#Same thing here for the updated data for millenials, except we can use Cansim to get the updated
#values for millennials.
newwages <- get_cansim("14-10-0204-01")
newwages2 <- newwages[, c(1, 6, 13)]
newwages3 <- filter(newwages2, REF_DATE %in% (2001:2012))
newwages4 <- aggregate(list(newwages3$VALUE), by = list(
  newwages3$REF_DATE, newwages3
  $`North American Industry Classification System (NAICS)`), mean, na.rm = TRUE)
colnames(newwages4) <- c("1", "2", "3")
newwagesactual <- data.frame(unique(newwages4[,1]))
industries <- c("Forestry, logging and support [11N]", "Mining and quarrying (except oil and gas) [212]"
                , "Manufacturing [31-33]", "Construction [23]", "Transportation and warehousing [48-49]",
                "Utilities [22,221]", "Finance and insurance [52]")
for (i in 1:7){
  newwagesactual[, i+1] <- data.frame(filter(newwages4, `2` == industries[i])[, 3])
}
colnames(newwagesactual) <- c("Year", industries)


#Then we get the CPI data, also using cansim, and use it to calculate inflation over the
#period.
cpidata <- get_cansim("18-10-0005-01")
oldcpi <- numeric()
for(i in 1964:1975){
  oldcpi <- rbind(oldcpi, filter(cpidata, cpidata$REF_DATE == i & cpidata$GEO == 'Canada' & 
                               cpidata$`Products and product groups` == "All-items")[11])
}

newcpi <- numeric()
for(i in 2001:2012){
  newcpi <- rbind(newcpi, filter(cpidata, cpidata$REF_DATE == i & cpidata$GEO == 'Canada' & 
                                   cpidata$`Products and product groups` == "All-items")[11])  
}
#By creating a list of CPI multipliers and multiplying it into the oldwages dataframe, we
#get the actual wages boomers would have made, but in 2001:2012 dollars instead. This allows
#us to make a fair comparison between them.
cpis <- cbind(newcpi, oldcpi)
cpimultipliers <- (newcpi - oldcpi)/oldcpi
oldwagesactual <- 1964:1975
for (i in 2:8){
  oldwagesactual <- cbind(oldwagesactual, as.numeric(as.character(oldwages3[ , i]))*cpimultipliers)
}
colnames(oldwagesactual) <- c("Year", "Forestry", "Mining", "Manufacturing", "Construction", 
"Transportation&Storage", "Utilities", "Finance&Insurance")

#=========================================================================================================
#Q2 Do millennials actually have a harder time finding jobs than boomers?

#Year ranges to use: 1964 - 1975 (Baby boomers would have been 18-29 years old)
#                    2001 - 2012 (20-31 for millenials because that's the only available data)

#First, we'll retrieve all the data in the same way as above, using the Historical Statistics
#of Canada CSV files and get_cansim.

oldunemployment <- read.csv("https://www150.statcan.gc.ca/n1/pub/11-516-x/sectiond/D190_204-eng.csv")
oldenrolment <- read.csv("https://www150.statcan.gc.ca/n1/pub/11-516-x/sectionw/W307_339-eng.csv")
oldpopulation <- read.csv("https://www150.statcan.gc.ca/n1/pub/11-516-x/sectiona/A1-eng.csv")
oldemployment <- read.csv("https://www150.statcan.gc.ca/n1/pub/11-516-x/sectiond/D290_317-eng.csv")
oldvacancies <- read.csv ("https://www150.statcan.gc.ca/n1/pub/11-516-x/sectiond/D522_527-eng.csv")
newenrolment <- get_cansim("37-10-0018-01")
newpopulation <- get_cansim("17-10-0005-01")
newemployment <- get_cansim("14-10-0023-01")
newvacancies <- get_cansim("14-10-0225-01")

#Then we'll use filter() to extract all the columns of data we want - some of them are in units of
#thousands, which is why we multiply by 1000, and others simply need to be changed
#to numeric so we can perform analysis on them later.
#For the old dataframes, they come in descending order of year, which is what the last line
#of code in each section is for - it flips it to be in ascending order of year.
oldunemployment2 <- filter(oldunemployment, Series.D190.204. %in% 1964:1975)[, c(2, 4)]
oldunemployment2[, 2] <- as.numeric(as.character(oldunemployment2[, 2])) * 1000
colnames(oldunemployment2) <- c("Year", "Unemployment")
oldunemployment2 <- oldunemployment2[order(nrow(oldunemployment2):1), ]

oldenrolment2 <- filter(oldenrolment, Series.W307.339. %in% 1964:1975)[, c(2, 7)]
oldenrolment2[, 2] <- as.numeric(sub(",", "", as.character(oldenrolment2[, 2]), fixed = TRUE)) * 1
oldenrolment2 <- oldenrolment2[c(1:12), ]
colnames(oldenrolment2) <- c("Year", "Enrolment")
oldenrolment2 <- oldenrolment2[order(nrow(oldenrolment2):1), ]

oldpopulation2 <- filter(oldpopulation, Series.A1. %in% 1964:1975)[, c(2, 3)]
oldpopulation2[, 2] <- as.numeric(sub(",", "", as.character(oldpopulation2[, 2]), fixed = TRUE)) * 1000
colnames(oldpopulation2) <- c("Year", "Population")
oldpopulation2 <- oldpopulation2[order(nrow(oldpopulation2):1), ]

oldemployment2 <- filter(oldemployment, Series.D290.317. %in% 1964:1975)[, c(2, 3)]
oldemployment2[, 2] <- as.numeric(sub(",", "", as.character(oldemployment2[, 2]), fixed = TRUE)) * 1000
oldemployment2 <- oldemployment2[c(1:12), ]
colnames(oldemployment2) <- c("Year", "Employment")
oldemployment2 <- oldemployment2[order(nrow(oldemployment2):1), ]

oldvacancies2 <- filter(oldvacancies, X.7 %in% 1964:1975)[, c(10, 11)]
oldvacancies2[, 2] <- as.numeric(sub(",", "", as.character(oldvacancies2[, 2]), fixed = TRUE)) * 1
colnames(oldvacancies2) <- c("Year", "Vacancies")
oldvacancies2 <- oldvacancies2[order(nrow(oldvacancies2):1), ]


#Creating a summary of all the data adjusted for proportion of population. Vacancies are purposely 
#not made a proportion of population - it should really be a proportion of the total number of
#available jobs, but that data wasn't available. Other data is included for later use in regression
#as well.
oldsummary <- data.frame(1964:1975)
oldsummary[, 2] <- (oldunemployment2[, 2] / oldpopulation2[, 2]) * 100
oldsummary[, 3] <- (oldenrolment2[, 2] / oldpopulation2[, 2]) * 100
oldsummary[, 4] <- (oldemployment2[, 2] / oldpopulation2[, 2]) * 100
oldsummary[, 5] <- (oldvacancies2[, 2])
oldsummary[, 6] <- rowMeans(oldwagesactual[, c(2:8)])
oldsummary[, 7] <- oldcpi
oldsummary[, 8] <- oldpopulation2[, 2]
oldsummary[, 9] <- as.numeric(oldtuition2[, 2])
colnames(oldsummary) <- c("Year", "% of unemployed persons", "% of persons enrolled in post-secondary", 
                          "% of persons employed", "Estimated number of job vacancies", 
                          "Average wages across all industries", "CPI", "Population",
                          "Tuition in 2020 dollars")


 #Here we're doing the same thing with the new data, except we have to use a series of for() loops
#and an extended dplyr filter() to get the data we want.
newunemployment <- numeric()
for(i in 2001:2012){
  newunemployment <- rbind(newunemployment, filter(newemployment, newemployment$REF_DATE == i & 
                             newemployment$GEO == 'Canada' & 
                             newemployment$`Labour force characteristics` == "Unemployment" & 
                             newemployment$`North American Industry Classification System (NAICS)` == 
                             "Total, all industries" & newemployment$Sex == "Both sexes" & 
                             newemployment$`Age group` == "15 years and over")[, c(1, 14)])
}
newunemployment[, 2] <- newunemployment[, 2] * 1000
colnames(newunemployment) <- c("Year", "Unemployment")


newenrolment2 <- numeric()
for(i in 2001:2012){
  newenrolment2 <- rbind(newenrolment2, filter(newenrolment, 
                   newenrolment$REF_DATE == paste0(i, "/", i+1) & 
                   newenrolment$GEO == "Canada" &
                   newenrolment$`International Standard Classification of Education (ISCED)` == 
                   'Total, International Standard Classification of Education (ISCED)' & 
                   newenrolment$`Institution type` == 'Total, institution type' & 
                   newenrolment$`Registration status` == 'Total, registration status' & 
                   newenrolment$`Field of study` == 'Total, field of study' &
                   newenrolment$Gender == 'Total, gender' &
                   newenrolment$`Status of student in Canada` == 'Total, status of student in Canada' &
                   newenrolment$`Selected statistics` == 'Number'
                        )[, c(1, 17)])
}
colnames(newenrolment2) <- c("Year", "Enrolment")

newpopulation2 <- numeric()
for(i in 2001:2012){
  newpopulation2 <- rbind(newpopulation2, filter(newpopulation, newpopulation$REF_DATE == i & 
                                                   newpopulation$GEO == 'Canada' & 
                                                   newpopulation$Sex == "Both sexes" & 
                                                   newpopulation$`Age group` == "All ages")[, c(1, 12)])
}
colnames(newpopulation2) <- c("Year", "Population")


newemployment2 <- numeric()
for(i in 2001:2012){
  newemployment2 <- rbind(newemployment2, filter(newemployment, newemployment$REF_DATE == i & 
                             newemployment$GEO == 'Canada' & 
                             newemployment$`Labour force characteristics` == "Employment" & 
                             newemployment$`North American Industry Classification System (NAICS)` == 
                             "Total, all industries" & newemployment$Sex == "Both sexes" & 
                             newemployment$`Age group` == "15 years and over")[, c(1, 14)])
}
newemployment2[, 2] <- newemployment2[, 2] * 1000
colnames(newemployment2) <- c("Year", "Employment")

#The new data for vacancies here is added as a comparison to the old one, but you'll notice that we
#use a different year range. This is because that's the only data that was 
#readily available (You can find statistics on vacancies from 1975-2010, but its not
#in electronic format and has to be checked out of the StatCan library), although it does
#have the added benefit of providing us with data that's more relevant to today.
newvacancies2 <- numeric()
for(i in 2011:2018){
  newvacancies2 <- rbind(newvacancies2, filter(newvacancies, newvacancies$REF_DATE == i & 
                   newvacancies$GEO == 'Canada' & 
                   newvacancies$`Job vacancy statistics` == "Number of job vacancies" & 
                   newvacancies$`North American Industry Classification System (NAICS)` == 
                   "Industrial aggregate excluding unclassified businesses [11-91N]")[, c(1, 12)])
}
newvacancies2[, 2] <- newvacancies2[, 2] * 1000
newvacancies2[c(9:12), 2] <- 0
colnames(newvacancies2) <- c("Year", "Vacancies")

#Same summary as before - vacancies are still here to be used later for curiosity's sake, even though
#they use a different date range.
newsummary <- data.frame(2001:2012)
newsummary[, 2] <- (newunemployment[, 2] / newpopulation2[, 2]) * 100
newsummary[, 3] <- (newenrolment2[, 2] / newpopulation2[, 2]) * 100
newsummary[, 4] <- (newemployment2[, 2] / newpopulation2[, 2]) * 100
newsummary[, 5] <- newvacancies2[, 2]
newsummary[, 6] <- rowMeans(newwagesactual[, c(2:8)])
newsummary[, 7] <- newcpi
newsummary[, 8] <- newpopulation2[, 2]
newsummary[, 9] <- newtuition2[, 2]
colnames(newsummary) <- c("Year", "% of unemployed persons", "% of persons enrolled in post-secondary", 
                          "% of persons employed", "Estimated number of job vacancies", 
                          "Average wages across all industries", "CPI", "Population",
                          "Tuition in 2020 dollars")

#=========================================================================================================
#Q3: Do millennials actually have a harder time getting credentials/training than boomers?

#Again, we are restricted to using different year ranges for this question due to the availability
#of data. This does give us the opportunity to show a statistic in 2020 dollars though, since
#both datasets use current prices.
#Year ranges for tuition only: 1972 - 1983, 2009 - 2020

#Here we're reading the old tuition data off of the Queens University website using read.xlsx()
oldtuition <- read.xlsx('http://library.queensu.ca/madgic/free/education_tables/Tuition-data_revised/historical/Table%208E_Combined.xlsx')
oldtuition2 <- filter(oldtuition, oldtuition[, 1] %in% paste0(1972:1983, "-", 1973:1984))[, c(1, 2)]
colnames(oldtuition2) <- c("Year", "Tuition in 2020 Dollars")

newtuition <- get_cansim("37-10-0003-01")
newtuition2 <- numeric()
for(i in 2009:2020){
  newtuition2 <- rbind(newtuition2, filter(newtuition, 
             newtuition$REF_DATE == paste0(i, "/", i+1) & 
               newtuition$GEO == "Canada" &
               newtuition$`Field of study` == "Total, field of study"
  )[, c(1, 11)])
}
colnames(newtuition2) <- c("Year", "Tuition in 2020 Dollars")

#=========================================================================================================
#Regression Analysis/Follow-Up Questions

#Null Q1: There is no difference in average pay rate between millennials and boomers.
t.test(newsummary$`Average wages across all industries`, oldsummary$`Average wages across all industries`
       , alternative = "greater")
#You can tell from the summary, but the T test makes it apparent. No, millennials actually make
#more money!

#Null Q2: There is no difference in unemployment/employment/job vacancy rates between 
#         these two generations.
t.test(newsummary$`% of unemployed persons`, oldsummary$`% of unemployed persons`
       , alternative = "greater")

t.test(newsummary$`% of persons employed`, oldsummary$`% of persons employed`
       , alternative = "greater")

#Again you can tell from the summary, but there is definitely more 
#unemployment, employment, and job vacancy (Couldn't do a T test for vacancy due to missing data)


#Null Q3: For equivalent credentials, millennials have to pay the same tuition as boomers did
t.test(newsummary$`Tuition in 2020 dollars`, oldsummary$`Tuition in 2020 dollars`
       , alternative = "greater")

#Once again, we have to pay more tuition than boomers did


#Q1.1: For both generations, what are the most statistically significant predictors of wages
#including %employment, %unemployment, %enrollment, wages, CPI, and population?

#We're omitting vacancies and tuition for the first part here just because the year ranges are different.

summary(lm(oldsummary$`Average wages across all industries` ~ oldsummary$`% of unemployed persons` + 
                       oldsummary$`% of persons enrolled in post-secondary` + 
                       oldsummary$`% of persons employed` + 
                       oldsummary$CPI + 
                       oldsummary$Population, data = oldsummary))

summary(lm(newsummary$`Average wages across all industries` ~ newsummary$`% of unemployed persons` + 
                       newsummary$`% of persons enrolled in post-secondary` + 
                       newsummary$`% of persons employed` + 
                       newsummary$CPI + 
                       newsummary$Population, data = newsummary))
#As can clearly be seen, the most significant predictors for boomers were unemployment, employment,
#and CPI, meaning that at that time, you could safely predict that the average wage would go up or
#down based on these percentage factors. For millennials, however, none of the variables are
#significant predictors, meaning wages are much more unpredictable.



#Q2.1: For both generations, what are the most statistically significant predictors of the % of 
#the population that is employed including %unemployment, %enrollment, wages, CPI, and population?

summary(lm(oldsummary$`% of persons employed` ~ oldsummary$`% of unemployed persons` + 
                      oldsummary$`% of persons enrolled in post-secondary` + 
                      oldsummary$`Average wages across all industries` + 
                      oldsummary$CPI + 
                      oldsummary$Population, data = oldsummary))

summary(lm(newsummary$`% of persons employed` ~ newsummary$`% of unemployed persons` + 
                      newsummary$`% of persons enrolled in post-secondary` + 
                      newsummary$`Average wages across all industries` + 
                      newsummary$CPI + 
                      newsummary$Population, data = newsummary))
#In this case, unemployment, wages, and CPI are significant predictors for boomers, and there are still
#no significant predictors for millennials.


#Q3.1: For both generations, what are the most statistically significant predictors of the % of 
#the population that is employed and wages including all of the previous predictors, and now
#tuition?

#We're omitting vacancies here just because the year ranges are different - we also know this isn't
#exactly an accurate assessment due to the year range for tuition also being different, but we felt
#it was an interesting comparison nonetheless.

summary(lm(oldsummary$`% of persons employed` ~ oldsummary$`% of unemployed persons` + 
             oldsummary$`% of persons enrolled in post-secondary` + 
             oldsummary$`Average wages across all industries` + 
             oldsummary$CPI + 
             oldsummary$Population + 
             oldsummary$`Tuition in 2020 dollars`,
             data = oldsummary))

summary(lm(newsummary$`% of persons employed` ~ newsummary$`% of unemployed persons` + 
             newsummary$`% of persons enrolled in post-secondary` + 
             newsummary$`Average wages across all industries` + 
             newsummary$CPI + 
             newsummary$Population +
             newsummary$`Tuition in 2020 dollars`,
             data = newsummary))

summary(lm(oldsummary$`Average wages across all industries` ~ oldsummary$`% of unemployed persons` + 
             oldsummary$`% of persons enrolled in post-secondary` + 
             oldsummary$`% of persons employed` + 
             oldsummary$CPI + 
             oldsummary$Population + 
             oldsummary$`Tuition in 2020 dollars`,
             data = oldsummary))

summary(lm(newsummary$`Average wages across all industries` ~ newsummary$`% of unemployed persons` + 
             newsummary$`% of persons enrolled in post-secondary` + 
             newsummary$`% of persons employed` + 
             newsummary$CPI + 
             newsummary$Population +
             newsummary$`Tuition in 2020 dollars`,
             data = newsummary))

#In this case, we see that tuition is a significant predictor of employment % for millennials, and of
#wages for boomers. This could indicate that while higher tuition might have reliably boosted the
#wages of boomers, it also helps millennials find jobs (When considering all of the other variables,
#of course)

