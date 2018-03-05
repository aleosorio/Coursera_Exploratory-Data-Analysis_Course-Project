# PROBLEM 3
## Of the four types of sources indicated by the type (point, nonpoint, onroad,
## nonroad) variable, which of these four sources have seen decreases in emissions
## from 1999–2008 for Baltimore City? Which have seen increases in emissions from
## 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.

# CALLING LIBRARIES
library(tidyverse)

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
        
        ## Subsetting and reshaping dataset into meandot to plot
        findat <- filter(unidat, unidat$fips == "24510") # filtering by Baltimore's fip
        meandat <- select(findat, year, type, Emissions) # selecting required variables
        meandat <- split(meandat, meandat$type) # splitting by type
        meandat <- sapply(meandat, function(X) { tapply(X$Emissions, X$year, mean) })# calculating Avg yearly Emissions, by type
        meandot <- melt(meandat, id = colnames(meandat), measure.vars = meandat) # melting into dataframe with only one "type" variable
        meandot[,2] <- as.character(meandot[,2])
        names(meandot) <- c("year", "type", "Emissions")

        # PLOTTING AND SAVING INTO FILE
        ## Opening PNG device
        png(filename = "plot3.png")
        
        ## Plotting
        ggplot(meandot, aes(year, Emissions)) + geom_point(aes(color = type)) + geom_smooth(aes(color = type), method = "lm", se = FALSE) + labs(title = "Avg Yearly PM2.5 Emissions, by Type")

        ## Closing PNG device
        dev.off()