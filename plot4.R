# PROBLEM 4
## Across the United States, how have emissions from coal combustion-related
## sources changed from 1999–2008?

## Answer: There are 3 types of emissions related to coal.
## Electric Generation: Went down from 937.2 to 223.1 (main source of coal by 2
##                      orders of magnitude in relation to other coal sources)
## Industrial Boilers, ICEs: Went up from 7.9 to 10.14, having peaked at around 14.8
## Comm/Institutional: Went down from 3.9 to 1.5


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
                filter(str_detect(.$EI.Sector, "Coal")) %>% # filtering by string "Coal" in "EI.Sector"
                select(year, EI.Sector, Emissions) %>% # selecting required variables
                group_by(year, EI.Sector) %>%
                summarize(Emissions_mean = mean(Emissions))

# PLOTTING AND SAVING INTO FILE
        ## Opening PNG device
        png(filename = "plot4.png")
        
        ## Plotting
        ggplot(findat, aes(year, Emissions_mean)) +
                geom_point(aes(color = EI.Sector)) +
                geom_smooth(aes(color = EI.Sector), method = "lm", se = FALSE) +
                labs(title = "Avg Yearly PM2.5 Emissions, by EI.Sector")

        ## Closing PNG device
        dev.off()