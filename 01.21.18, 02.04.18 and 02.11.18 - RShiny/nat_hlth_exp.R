# Load packages
library(dplyr)
library(ggplot2)
library(reshape2)

# Load dataset
nat_exp <- read.csv(file="nat_hlth_exp.csv", header = TRUE, sep = ",", strip.white = TRUE )

str(nat_exp)

m_nat_exp <- melt(nat_exp, id = c("First.Level", "Second.Level", "Third.Level", "Fourth.Level", "Fifth.Level"))

str(m_nat_exp)

m_nat_exp$variable <- substring(m_nat_exp$variable, 2)

str(m_nat_exp)

m_nat_exp$variable <- as.numeric(m_nat_exp$variable)

str(m_nat_exp)

m_nat_exp %>%
  filter(First.Level == "Source of Funds" & variable == "X2016") %>%
  summarize(Total = sum(value))

m_nat_exp %>%
  filter(First.Level == "Source of Funds" & variable == "X2016") %>%
  group_by(Second.Level) %>%
  summarize(Total = sum(value))

m_nat_exp %>%
  filter(First.Level == "Type of Service or Product" & variable == "X2016") %>%
  group_by(Fourth.Level) %>%
  summarize(Total = sum(value))

m_nat_exp %>%
  filter(First.Level == "Type of Service or Product" & Fourth.Level == "Hospital Care") %>%
  group_by(variable) %>%
  summarize(Total = sum(value))

m_nat_exp %>%
  filter(First.Level == "Type of Service or Product" & Fourth.Level == "Hospital Care") %>%
  group_by(variable) %>%
  summarize(Total = sum(value)) %>%
  ggplot(., aes(x = variable, y = Total, fill = second.level)) + geom_point(position = "stack")


cols <- c("Hospital Care" = "darkviolet", "Physician and Clinical Services" = "royalblue1", "Dental Services" = "royalblue2", "Other Professional Services" = "royalblue3", "Home Health Care Expenditures" = "green3", "Other Non-Durable Medical Products Expenditures" = "orangered1",
          "Prescription Drug Expenditures" = "orangered2", "Durable Medical Equipment Expenditures" = "orangered3", "Nursing Care Facilities and Continuing Care Retirement Communities" = "yellow2", "Other Health, Residential, and Personal Care Expenditures" = "red2",
          "State and Local  Administration Expenditures" = "slategray1", "Federal Administration Expenditures" = "slategray2", "Net Cost of Health Insurance" = "lightblue2", "Government Public Health Activities" = "orchid1", "Research" = "magenta2", "Structures and Equipment" = "magenta3")

cols2 <- c("Hospital Care", "Physician and Clinical Services", "Dental Services", "Other Professional Services", "Home Health Care Expenditures", "Other Non-Durable Medical Products Expenditures",
          "Prescription Drug Expenditures", "Durable Medical Equipment Expenditures", "Nursing Care Facilities and Continuing Care Retirement Communities", "Other Health, Residential, and Personal Care Expenditures",
          "State and Local  Administration Expenditures", "Federal Administration Expenditures", "Net Cost of Health Insurance", "Government Public Health Activities", "Research", "Structures and Equipment")


m_nat_exp %>%
  filter(First.Level == "Type of Service or Product") %>%
  arrange(Third.Level) %>%
  group_by_(.dots = c("variable", "Third.Level", "Fifth.Level")) %>%
  summarize(Total = sum(value)) %>%
  ggplot(., aes(x = variable, y = Total, fill = Fifth.Level, order = Third.Level)) + geom_area(position = "stack") + scale_fill_manual(values = cols)


m_nat_exp %>%
  filter(First.Level == "Type of Service or Product") %>%
  group_by_(.dots = c("variable", "Fifth.Level")) %>%
  summarize(Total = sum(value)) %>%
  ggplot(., aes(x = variable, y = Total, fill = Fifth.Level)) + geom_area(position = "stack") 


 