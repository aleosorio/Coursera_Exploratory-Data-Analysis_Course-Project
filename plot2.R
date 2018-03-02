# PROBLEM 2
## Have total emissions from PM2.5 decreased in the Baltimore
## City, Maryland (fips == "24510") from 1999 to 2008? Use the
## base plotting system to make a plot answering this question.

## ANSWER: Indeed, from a yearly mean of 10.2 in 1999, down to 2.7
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
        findat <- subset(unidat, unidat$fips == "24510") # filtering by Baltimore's fip
        meandat <- cbind(findat$year, findat$Emissions) # 2 column matrix w/year and emissions
        meandot <- tapply(meandat[,2], meandat[,1], mean) # vector w/yearly emissions' means
        meandot <- data.frame(names = as.integer(row.names(meandot)), emissions = meandot) # dataframe ready to plot

# PLOTTING AND SAVING INTO FILE
        ## Opening PNG device to create "plot1.png"
        png(filename = "plot2.png")

        ## Plotting
        par(mar = c(4,4,2,1), mfcol = c(1, 1))
        with(meandot, plot(names, emissions, pch = 19 , main = "Baltimore's Yearly Avg. Emissions", xlab = "Year", ylab = "Avg Emissions"))
        with(meandot, abline(lm(emissions ~ names), lwd = 2)) # linear yearly emissions' means

        ## Closing PNG device
        dev.off()