# CALLING LIBRARIES TO USE
library(dplyr)
library(lubridate)
library(ggplot2)

# SETTING WORKING DIRECTORY
setwd("C:/Users/lenovo/Documents/MAO/Aprendizaje/Data Science/Exploratory Data Analysis_Coursera/Course Project/Data")

# READING RAW DATA FROM WORKING DIRECTORY IN PC
sumdat <- readRDS("summarySCC_PM25.rds")
sccdat <- readRDS("Source_Classification_Code.rds")

# GENERATING DATASET TO WORK WITH
## Creating unique and enriched dataset by adding sccdat data to sumdat, using SCC variable as key
unidat <- left_join(sumdat, sccdat, by = c("SCC", "SCC"))

