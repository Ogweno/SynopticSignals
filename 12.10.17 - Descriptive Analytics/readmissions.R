# Load Packages
library(dplyr)
library(ggplot2)

getwd()

# bring in data
readmin <- read.csv(file="diabetic_data.csv", header = TRUE, sep = ",", strip.white = TRUE )

# Need to understand your dataset, background of your data, and what question you are trying to answer

# Check information on dataset
# we have 101766 observations of 50 variables
str(readmin)

head(readmin, n=20)

# our variables of interest will be patient_nbr (to identify if we have repeated obsevations), encounter_id (make sure unique)
# race, gender, age, weight, time_in_hospital, admission_type_id, readmitted, diabetesMed

summary(readmin)

# frequency tables for our variables
# Can see that yes, some of our patients have multiple encounters. 
patient_nbr <- readmin$patient_nbr
cbind(freq = table(patient_nbr), percentage=prop.table(table(patient_nbr))*100)

race <- readmin$race
cbind(freq = table(race), percentage=prop.table(table(race))*100)

readmin.freq <- function(x, column) {
  cbind(freq = table(x[,column]), percentage=prop.table(table(x[,column]))*100)
}

readmin.freq(readmin, "race")
readmin.freq(readmin, "gender")
readmin.freq(readmin, "age")
readmin.freq(readmin, "weight")
readmin.freq(readmin, "admission_type_id")
readmin.freq(readmin, "readmitted")
readmin.freq(readmin, "diabetesMed")

# Time in Hospital
hist(readmin$time_in_hospital)

ggplot(readmin, aes(x = time_in_hospital)) + geom_histogram()

# Plots
ggplot(data = readmin, aes(x = age)) +
        geom_bar() 

ggplot(data = readmin, aes(x = readmitted)) +
        geom_bar() 

ggplot(data = readmin, aes(x = readmitted)) +
        geom_bar() + facet_grid(~ gender)

ggplot(data = readmin, aes(x = readmitted)) +
        geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])) +
        xlab("Readmitted") +
        scale_y_continuous(labels = scales::percent, name = "Proportion") +
        facet_grid(~ race) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = readmin, aes(x = readmitted)) +
        geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])) +
        xlab("Readmitted") +
        scale_y_continuous(labels = scales::percent, name = "Proportion") +
        facet_grid(~ gender) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(data = readmin, aes(x = readmitted)) +
        geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])) +
        xlab("Readmitted") +
        scale_y_continuous(labels = scales::percent, name = "Proportion") +
        facet_grid(~ age) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = readmin, aes(x = readmitted)) +
        geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])) +
        xlab("Readmitted") +
        scale_y_continuous(labels = scales::percent, name = "Proportion") +
        facet_grid(~ diabetesMed) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = readmin, aes(x = time_in_hospital)) +
        geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])) +
        xlab("Readmitted") +
        scale_y_continuous(labels = scales::percent, name = "Proportion") +
        facet_grid(~ readmitted) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Logistic Regression

# Load Packages
library(dplyr)

# bring in data
readmin <- read.csv(file="diabetic_data.csv", header = TRUE, sep = ",", strip.white = TRUE, stringsAsFactors = FALSE )

# remove duplicates by keeping only last unique observation for patient number
readmin2 <- readmin[!rev(duplicated(rev(readmin$patient_nbr))),]

# check structure
str(readmin2)

# set race "?" as NA
readmin2$race2 <- readmin2$race
readmin2$race2[readmin2$race2=="?"] <- NA

# check to make sure NA worked correctly
readmin.freq(readmin2, "race2")
sum(is.na(readmin2$race2))
sum(is.na(readmin2$race))

# set gender unknown to NA
readmin2$gender2 <- readmin2$gender
readmin2$gender2[readmin2$gender2=="Unknown/Invalid"] <- NA
sum(is.na(readmin2$gender2))

# classify age as above and below 60
readmin2$age2 <- readmin2$age
readmin2$age2[readmin2$age2=="[0-10)"] <- "<60"
readmin2$age2[readmin2$age2=="[10-20)"] <- "<60"
readmin2$age2[readmin2$age2=="[20-30)"] <- "<60"
readmin2$age2[readmin2$age2=="[30-40)"] <- "<60"
readmin2$age2[readmin2$age2=="[40-50)"] <- "<60"
readmin2$age2[readmin2$age2=="[50-60)"] <- "<60"
readmin2$age2[readmin2$age2=="[60-70)"] <- "60+"
readmin2$age2[readmin2$age2=="[70-80)"] <- "60+"
readmin2$age2[readmin2$age2=="[80-90)"] <- "60+"
readmin2$age2[readmin2$age2=="[90-100)"] <- "60+"

#check age 
readmin.freq(readmin2, "age2")

readmin2$readmitted2 <- readmin2$readmitted
readmin2$readmitted2[readmin2$readmitted2=="<30"] <- "YES"
readmin2$readmitted2[readmin2$readmitted2==">30"] <- "YES"

readmin2$readmitted3 <- (as.factor(readmin2$readmitted2))

readmin2$admission_type_id2 <- readmin2$admission_type_id
readmin2$admission_type_id2[readmin2$admission_type_id2==1] <- 1
readmin2$admission_type_id2[readmin2$admission_type_id2==2] <- 0
readmin2$admission_type_id2[readmin2$admission_type_id2==3] <- 0
readmin2$admission_type_id2[readmin2$admission_type_id2==4] <- 0
readmin2$admission_type_id2[readmin2$admission_type_id2==5] <- 0
readmin2$admission_type_id2[readmin2$admission_type_id2==6] <- 0
readmin2$admission_type_id2[readmin2$admission_type_id2==7] <- 0
readmin2$admission_type_id2[readmin2$admission_type_id2==8] <- 0


readmin.freq(readmin2, "admission_type_id")
readmin.freq(readmin2, "admission_type_id2")

model <- glm(readmitted3 ~ admission_type_id2 + age2 + race2 + gender2,family=binomial(link='logit'),data=readmin2)

summary(model)

anova(model, test="Chisq")

exp(cbind(Odd_Ratio = coef(model), confint(model)))

# Predictive analytics

# Separate the data into test and train
train <- readmin2[1:60000,]
test <- readmin2[60001:71518,]

# new model using only train data
model2 <- glm(readmitted3 ~ admission_type_id2 + age2 + race2 + gender2,family=binomial(link='logit'),data=train)

# run summary stats on model
summary(model2)

anova(model2, test="Chisq")

# Predict readmission using developed model
fit.results <- predict(model2,test, type = 'response')
fit.results <- ifelse(fitted.results > 0.255,1,0)
summary(fit.results)

# Convert readmitted to 1, 0 for use in comparison
test$readmitted4 <- ifelse(test$readmitted3=="YES", 1, 0)

# check fitted results with actual results. 
misClass <- mean(fit.results != test$readmitted4, na.rm = TRUE)
print(paste('Accuracy',1-misClass))

# accuracy is 60% which considering 50% is heads/tails, is not very good. Since this was an example, there were many variables we did not include which could improve this model

fitted.results

readmin.freq(test, "readmitted3")
summary(fitted.results)

str(fitted.results)

