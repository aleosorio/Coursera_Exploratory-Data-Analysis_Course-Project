# PROBLEM 2
## Have total emissions from PM2.5 decreased in the Baltimore
## City, Maryland (fips == "24510") from 1999 to 2008? Use the
## base plotting system to make a plot answering this question.

## ANSWER: Indeed, from a yearly total of 3.3 KTons in 1999, down to 1.9
## KTons in 2008.  Clearly seen through linear modeling, on the plot.

# CALLING LIBRARIES
library(tidyverse)

# SETTING WORKING DIRECTORY
setwd("C:/Users/lenovo/Documents/MAO/Aprendizaje/Data Science/Exploratory Data Analysis_Coursera/Course Project/Data")

# READING RAW DATA FROM WORKING DIRECTORY IN PC
sumdat <- readRDS("summarySCC_PM25.rds") # no extra data required for this problem

# GENERATING DATASET
        ## Subsetting, grouping and summaryzing into final dataset (findat)
        findat <- sumdat %>%
                filter(.$fips == "24510") %>% # only Baltimore
                select(year, Emissions) %>% # selecting required variables
                group_by(year) %>%
                summarize(Emissions_total = sum(Emissions))

# PLOTTING AND SAVING INTO FILE
        ## Opening PNG device
        png(filename = "plot2.png")

        ## Plotting
        par(mar = c(4,4,2,1), mfcol = c(1, 1))
        with(findat, plot(year, Emissions_total, pch = 19 , main = "Baltimore's Yearly Total Emissions", xlab = "Year", ylab = "PM2.5 Emissions in tons."))
        with(findat, abline(lm(Emissions_total ~ year), lwd = 2)) # linear yearly emissions' totals

        ## Closing PNG device
        dev.off()