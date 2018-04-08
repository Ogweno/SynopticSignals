# Load Packages
# Ran into some memory issues with XLConnect so I eneded up using a similar package openxlsx
library(rJava)
# Defines how much memory can be used for this product. 1024 is 1.024 GB
options(java.parameters = "-Xmx1024m")
library(dplyr)
library(openxlsx)


setwd("~/SynopticSignals/04.08.18 - Automating Excel Processes")

# Creates list of countries which will be used in the for loop
Countries <- c("Botswana", "Burundi", "Cameroon")

# For Loop
# Cycles through each country, finds the file, pulls in the data, gets rid of rows with blanks, 
# assigns dataset as that country name, clears import from memory
# Will import data as values from formulas and ignores filters



for (Country in Countries) {
  filename <- paste0(Country, " Budget.xlsx")
  CountryDF <- read.xlsx(filename, sheet = "Input", startRow = 1, colNames = TRUE, skipEmptyRows = TRUE)
  CountryDF <- CountryDF[!(is.na(CountryDF$PrimePartner)),]
  CountryDF$OU <- Country
  assign(paste(Country),CountryDF)
  gc()
}

# Appending all of the country files together
Budget_Full <- bind_rows(Botswana, Burundi, Cameroon)

# Exports the dataset as a CSV
write.csv(Budget_Full,"Budget_Full.csv", na = "")

# Export each dataset into Excel
for (Country in Countries) {
  WB <- loadWorkbook("Budget Tool.xlsx")
  CountryDF <- get(Country)
  writeData(WB, sheet = "Output", x = CountryDF)
  filenameXLSX <- paste0(Country, " Budget Tool populated.xlsx")
  saveWorkbook(WB, filenameXLSX)
}
