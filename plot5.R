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
        
        ## Subsetting, grouping and summaryzing into final dataset (findat)
        findat <- unidat %>%
                filter(str_detect(.$EI.Sector, "Vehicles") & .$fips == "24510") %>% # filtering by string "Vehicles" in "EI.Sector" and Baltimore
                select(year, EI.Sector, Emissions) %>% # selecting required variables
                group_by(year, EI.Sector) %>%
                summarize(Emissions_mean = mean(Emissions))

# PLOTTING AND SAVING INTO FILE
        ## Opening PNG device
        png(filename = "plot5.png")
        
        ## Plotting
        ggplot(findat, aes(year, Emissions_mean)) +
                geom_point(aes(color = EI.Sector)) +
                geom_smooth(aes(color = EI.Sector), method = "lm", se = FALSE) +
                labs(title = "Avg Yearly PM2.5 Emissions, by EI.Sector")

        ## Closing PNG device
        dev.off()