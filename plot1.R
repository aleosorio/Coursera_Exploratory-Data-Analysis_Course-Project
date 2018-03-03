# PROBLEM 1
## Have total emissions from PM2.5 decreased in the United States
## from 1999 to 2008? Using the base plotting system, make a plot
## showing the total PM2.5 emission from all sources for each of
## the years 1999, 2002, 2005, and 2008.

## ANSWER: Indeed, from a yearly mean of 6.6 in 1999, down to 1.8
## in 2008.  It can also be seen through linear modeling, on the plot.

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

        ## Subsetting required dataset into meandot to plot
        meandat <- select(unidat, year, Emissions) # selecting required variables
        meandot <- tapply(meandat$Emissions, meandat$year, mean) # vector w/yearly emissions' means
        meandot <- data.frame(names = as.integer(row.names(meandot)), emissions = meandot) # dataframe ready to plot

# PLOTTING AND SAVING INTO FILE
        ## Opening PNG device to create "plot1.png"
        png(filename = "plot1.png")

        ## Plotting
        par(mar = c(4,4,2,1), mfcol = c(1, 1))
        with(meandot, plot(names, emissions, pch = 19 , xlab = "Year", ylab = "Yearly Avg Emissions"))
        with(meandot, abline(lm(emissions ~ names), lwd = 2)) # linear yearly emissions' means

        ## Closing PNG device
        dev.off()