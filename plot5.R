# PROBLEM 5
## How have emissions from motor vehicle sources changed from 1999–2008 in
## Baltimore City?

## Answer: All 4 vehicle sources' emissions have gone down.

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
        findat <- filter(unidat, str_detect(unidat$EI.Sector, "Vehicles") & unidat$fips == "24510") # filtering by "EI.Sector" (with string "Vehicles") and Baltimore
        meandat <- select(findat, year, EI.Sector, Emissions) # selecting required variables
        meandat <- split(meandat, meandat$EI.Sector) # splitting by EI.Sector
        meandat <- sapply(meandat, function(X) { tapply(X$Emissions, X$year, mean) })# calculating Avg yearly Emissions, by EI.Sector
        meandot <- melt(meandat, id = colnames(meandat), measure.vars = meandat) # melting into dataframe with only one "EI.Sector" variable
        meandot[,2] <- as.character(meandot[,2])
        names(meandot) <- c("year", "EI.Sector", "Emissions")

        # PLOTTING AND SAVING INTO FILE
        ## Opening PNG device
        png(filename = "plot5.png")
        
        ## Plotting
        ggplot(meandot, aes(year, Emissions)) +
                geom_point(aes(color = EI.Sector)) +
                geom_smooth(aes(color = EI.Sector), method = "lm", se = FALSE) +
                labs(title = "Avg Yearly PM2.5 Emissions, by EI.Sector")

        ## Closing PNG device
        dev.off()