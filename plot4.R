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
        
        ## Subsetting and reshaping dataset into meandot to plot
        findat <- filter(unidat, str_detect(unidat$EI.Sector, "Coal")) # filtering by variable "EI.Sector" with values that include string "Coal"
        meandat <- select(findat, year, EI.Sector, Emissions) # selecting required variables
        meandat <- split(meandat, meandat$EI.Sector) # splitting by EI.Sector
        meandat <- sapply(meandat, function(X) { tapply(X$Emissions, X$year, mean) })# calculating Avg yearly Emissions, by type
        meandot <- melt(meandat, id = colnames(meandat), measure.vars = meandat) # melting into dataframe with only one "type" variable
        meandot[,2] <- as.character(meandot[,2])
        names(meandot) <- c("year", "EI.Sector", "Emissions")

        # PLOTTING AND SAVING INTO FILE
        ## Opening PNG device to create "plot1.png"
        png(filename = "plot4.png")
        
        ## Plotting
        ggplot(meandot, aes(year, Emissions)) + geom_point(aes(color = EI.Sector)) + geom_smooth(aes(color = EI.Sector), method = "lm", se = FALSE) + labs(title = "Avg Yearly PM2.5 Emissions, by EI.Sector")

        ## Closing PNG device
        dev.off()