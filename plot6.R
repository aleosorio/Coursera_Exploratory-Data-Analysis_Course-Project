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
        
        ## Subsetting, grouping and summaryzing into final dataset (findat)
        findat <- unidat %>%
                filter(str_detect(.$EI.Sector, "Vehicles") &
                                 (.$fips == "24510" | .$fips == "06037")) %>% # filtering by string "Vehicles" in "EI.Sector" and (Baltimore or LA)
                select(fips, year, EI.Sector, Emissions) %>% # selecting required variables
                group_by(year, fips, EI.Sector) %>%
                summarize(Emissions_mean = mean(Emissions))

        ## Converting fips into factor with city names
        findat$fips <- factor(findat$fips, labels = c("Los Angeles County", "Baltimore City"))

# PLOTTING AND SAVING INTO FILE
        ## Opening PNG device
        png(filename = "plot6.png")
        
        ## Plotting
        ggplot(findat, aes(year, Emissions_mean)) +
                geom_point(aes(color = EI.Sector)) +
                geom_smooth(aes(color = EI.Sector), method = "lm", se = FALSE) +
                labs(title = "Avg Yearly PM2.5 Emissions, by City and Source") +
                facet_wrap(~ fips, nrow = 2)

        ## Closing PNG device
        dev.off()