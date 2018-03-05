# PROBLEM 3
## Of the four types of sources indicated by the type (point, nonpoint, onroad,
## nonroad) variable, which of these four sources have seen decreases in emissions
## from 1999–2008 for Baltimore City? Which have seen increases in emissions from
## 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.

## Answer: All four sources have seen decreases in emissions, specially the NONPOINT type.

# CALLING LIBRARIES
library(tidyverse)

# SETTING WORKING DIRECTORY
setwd("C:/Users/lenovo/Documents/MAO/Aprendizaje/Data Science/Exploratory Data Analysis_Coursera/Course Project/Data")

# READING RAW DATA FROM WORKING DIRECTORY IN PC
sumdat <- readRDS("summarySCC_PM25.rds") # no extra data required for this problem

# GENERATING DATASET
        ## Subsetting and reshaping dataset into meandot to plot
        findat <- sumdat %>%
                filter(.$fips == "24510") %>% # filtering by Baltimore's fip
                select(year, type, Emissions) %>% # selecting required variables
                group_by(year, type) %>%
                summarize(Emissions_mean = mean(Emissions))

        # PLOTTING AND SAVING INTO FILE
        ## Opening PNG device
        png(filename = "plot3.png")
        
        ## Plotting
        ggplot(findat, aes(year, Emissions_mean)) +
                geom_point(aes(color = type)) +
                geom_smooth(aes(color = type), method = "lm", se = FALSE) +
                labs(title = "Avg Yearly PM2.5 Emissions, by Type")

        ## Closing PNG device
        dev.off()