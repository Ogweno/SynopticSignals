# Load Packages
library(dplyr)
library(ggplot2)

getwd()

# bring in data
confounding <- read.csv(file="confounding example.csv", header = TRUE, sep = ",", strip.white = TRUE )

names(confounding)[names(confounding)=="?..VarA"] <- "VarA"

str(confounding)

#Confounding Plot
ggplot(confounding, aes(x=VarE, y=VarD, color=VarC)) + geom_point()

#Confounding model without controlling for VarC
model3 <- lm(VarD ~ VarE, data = confounding)
summary(model3)

#Confounding model with VarC
model4 <- lm(VarD ~ VarE + VarC, data = confounding)
summary(model4)

#Interaction Plot
ggplot(confounding, aes(x=VarA, y=VarB, color=VarC)) + geom_point()

#Interaction model without interaction
model1 <- lm(VarB ~ VarA, data= confounding)
print(model1)
summary(model1)

#Interaction model controlling for VarC but without interaction term
model2 <- lm(VarB ~ VarA + VarC, data = confounding)
summary(model2)

#Interaction model with interaction term for VarC
model5 <- lm(VarB ~ VarA + VarC + VarA*VarC, data = confounding)
summary(model5)


