rm(list=ls())
setwd("C:/Users/admin/Desktop")
dt=read.csv("D6.2 Jet Pens historical.csv")
head(dt,10)

##Task 1
fit <- glm(y1~p1,data=dt,family=binomial())
summary(fit)
confint(fit) # 95% CI for the coefficients
exp(coef(fit)) # exponentiated coefficients
exp(confint(fit)) # 95% CI for exponentiated coefficients
predict(fit, type="response") # predicted values
residuals(fit, type="deviance") # residuals
exp(coef(fit))

##########Task 2
dta=read.csv("D6.2 Jet Pens experimental.csv")
head(dta,10)
############Task 3
# Estimate ATE for Condition 0 vs. Condition 1
# Compute the ATE for condition 0 versus condition 1

# Load the data
# Create a new variable called "Condition"
dta$Condition <- NA

# Assign conditions to the new variable
dta$Condition[dta$p1 == 20] <- "Condition 0"
dta$Condition[dta$p1 == 15] <- "Condition 1"
dta$Condition[dta$p1 == 25] <- "Condition 2"
dta$Condition[dta$p1 == 19.99] <- "Condition 3"

# Print the first few rows of the updated data
head(dta,10)

# Compute the ATE for condition 0 versus condition 1
y1_condition_0<-mean(dta$y1[dta$Condition == "Condition 0"])
y1_condition_1<-mean(dta$y1[dta$Condition == "Condition 1"])
ATE_01 <- mean(dta$y1[dta$Condition == "Condition 1"]) - mean(dta$y1[dta$Condition == "Condition 0"])

# Compute the ATE for condition 0 versus condition 2
y1_condition_2<-mean(dta$y1[dta$Condition == "Condition 2"])
ATE_02 <- mean(dta$y1[dta$Condition == "Condition 2"]) - mean(dta$y1[dta$Condition == "Condition 0"])

# Print the results
cat("ATE for condition 0 versus condition 1:", round(ATE_01, 3), "\n")
cat("ATE for condition 0 versus condition 2:", round(ATE_02, 3), "\n")

################Task 4

# Compute the ATE for condition 0 versus condition 1
y2_condition_0<-mean(dta$y2[dta$Condition == "Condition 0"])
y2_condition_1<-mean(dta$y2[dta$Condition == "Condition 1"])
ATE_01 <- mean(dta$y2[dta$Condition == "Condition 1"]) - mean(dta$y2[dta$Condition == "Condition 0"])

# Compute the ATE for condition 0 versus condition 2
y2_condition_2<-mean(dta$y2[dta$Condition == "Condition 2"])
ATE_02 <- mean(dta$y2[dta$Condition == "Condition 2"]) - mean(dta$y2[dta$Condition == "Condition 0"])

# Print the results
cat("ATE for condition 0 versus condition 1:", round(ATE_01, 3), "\n")
cat("ATE for condition 0 versus condition 2:", round(ATE_02, 3), "\n")

##################Task 5

# Compute the ATE for condition 0 versus condition 1
y1_condition_0<-mean(dta$y1[dta$Condition == "Condition 0"])
y1_condition_1<-mean(dta$y1[dta$Condition == "Condition 3"])
ATE_01 <- mean(dta$y1[dta$Condition == "Condition 1"]) - mean(dta$y1[dta$Condition == "Condition 0"])

# Compute the ATE for condition 0 versus condition 3
y1_condition_2<-mean(dta$y1[dta$Condition == "Condition 3"])
ATE_02 <- mean(dta$y1[dta$Condition == "Condition 3"]) - mean(dta$y1[dta$Condition == "Condition 0"])

# Print the results
cat("ATE for condition 0 versus condition 1:", round(ATE_01, 3), "\n")
cat("ATE for condition 0 versus condition 2:", round(ATE_02, 3), "\n")





