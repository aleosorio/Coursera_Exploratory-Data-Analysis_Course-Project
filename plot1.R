# PROBLEM 1
## Have total emissions from PM2.5 decreased in the United States
## from 1999 to 2008? Using the base plotting system, make a plot
## showing the total PM2.5 emission from all sources for each of
## the years 1999, 2002, 2005, and 2008.

## ANSWER: Indeed, from a yearly total of MM7.3 tons in 1999, down to MM3.5
## tons in 2008.  It can also be seen through linear modeling, on the plot.

# CALLING LIBRARIES
library(tidyverse)

# SETTING WORKING DIRECTORY
setwd("C:/Users/lenovo/Documents/MAO/Aprendizaje/Data Science/Exploratory Data Analysis_Coursera/Course Project/Data")

# READING RAW DATA FROM WORKING DIRECTORY IN PC
sumdat <- readRDS("summarySCC_PM25.rds") # no extra data required for this problem

# GENERATING DATASET
## Subsetting, grouping and summaryzing into final dataset (findat)
        findat <- sumdat %>%
                filter(.$year %in% c(1999, 2002, 2005, 2008)) %>% # required years
                select(year, Emissions) %>% # selecting required variables
                group_by(year) %>%
                summarize(Emissions_total = sum(Emissions))

# PLOTTING AND SAVING INTO FILE
        ## Opening PNG device to create "plot1.png"
        png(filename = "plot1.png")

        ## Plotting
        par(mar = c(4,4,2,1), mfcol = c(1, 1))
        with(findat, plot(year, Emissions_total, pch = 19 , main = "National Yearly Total Emissions", xlab = "Year", ylab = "PM2.5 Emissions in tons."))
        with(findat, abline(lm(Emissions_total ~ year), lwd = 2)) # linear yearly emissions' totals

        ## Closing PNG device
        dev.off()