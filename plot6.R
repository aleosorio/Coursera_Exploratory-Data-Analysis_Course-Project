# PROBLEM 6
## Compare emissions from motor vehicle sources in Baltimore City with emissions
## from motor vehicle sources in Los Angeles County, California (fips == "06037").
## Which city has seen greater changes over time in motor vehicle emissions?

## Answer: LA has seen greater changes over time, specially regarding the rise of
## Diesel Heavy Duty and Gasoline Light Duty Vehicles' Emissions

# CALLING LIBRARIES
library(tidyverse)
library(stringr)

# SETTING WORKING DIRECTORY
setwd("C:/Users/lenovo/Documents/MAO/Aprendizaje/Data Science/Exploratory Data Analysis_Coursera/Course Project/Data")

# READING RAW DATA FROM WORKING DIRECTORY IN PC
sumdat <- readRDS("summarySCC_PM25.rds")
sccdat <- readRDS("Source_Classification_Code.rds")

# GENERATING DATASET
        ## Converting SCC variable from factor to character
        sccdat$SCC <- as.character(sccdat$SCC)

        ## Joining sccdat with sumdat (key = "SCC") into unique dataset
        unidat <- left_join(sumdat, sccdat, by = c("SCC", "SCC"))
        unidat$EI.Sector <- as.character(unidat$EI.Sector)
        
        ## Subsetting and reshaping dataset into meandot to plot
        findat <- filter(unidat, str_detect(unidat$EI.Sector, "Vehicles") &
                                 (unidat$fips == "24510" | unidat$fips == "06037")) # filtering by "EI.Sector" (with string "Vehicles") and Baltimore
        meandat <- select(findat, fips, year, EI.Sector, Emissions) # selecting required variables
        meandat1 <- filter(meandat, fips == "24510") # filtering Baltimore's data
        meandat2 <- filter(meandat, fips == "06037") # filtering LA's data
        meandat1 <- split(meandat1, meandat1$EI.Sector) # splitting by EI.Sector (Baltimore)
        meandat2 <- split(meandat2, meandat2$EI.Sector) # splitting by EI.Sector (LA)
        meandat1 <- sapply(meandat1, function(X) { tapply(X$Emissions, X$year, mean) })# calculating Avg yearly Emissions, by EI.Sector (Baltimore)
        meandat2 <- sapply(meandat2, function(X) { tapply(X$Emissions, X$year, mean) })# calculating Avg yearly Emissions, by EI.Sector (LA)
        meandot1 <- melt(meandat1, id = colnames(meandat1), measure.vars = meandat1) # melting into dataframe with only one "EI.Sector" variable (Baltimore)
        meandot2 <- melt(meandat2, id = colnames(meandat2), measure.vars = meandat2) # melting into dataframe with only one "EI.Sector" variable (LA)
        meandot1 <- mutate(meandot1, city = "Baltimore City") # adding city variable
        meandot2 <- mutate(meandot2, city = "Los Angeles County") # adding city variable
        meandot <- bind_rows(meandot1, meandot2) # joining into final dataset
        meandot[,2] <- as.character(meandot[,2])
        names(meandot) <- c("year", "Source", "Emissions", "city")

        # PLOTTING AND SAVING INTO FILE
        ## Opening PNG device
        png(filename = "plot6.png")
        
        ## Plotting
        ggplot(meandot, aes(year, Emissions)) +
                geom_point(aes(color = Source)) +
                geom_smooth(aes(color = Source), method = "lm", se = FALSE) +
                labs(title = "Avg Yearly PM2.5 Emissions, by City and Source") +
                facet_wrap(~ city, nrow = 2)

        ## Closing PNG device
        dev.off()