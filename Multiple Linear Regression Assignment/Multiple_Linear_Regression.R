require(tidyverse)

#         -- RABE5 3.3 --
##DATASET
exam<-read.table('https://www1.aucegypt.edu/faculty/hadi/RABE6/Data6/Examination.Data.txt',sep='\t',header=T)
head(exam)
names(exam)

##FITTED MODELS

#Intercept Model
fit.null <- lm(F ~ 1, data = exam)
fit.null

#Model 1 (P1 only)
fit.P1 <- lm(F ~ P1, data = exam)
fit.P1

#Model 2 (P2 only)
fit.P2 <- lm(F ~ P2, data = exam)
fit.P2

#Model 3 (P1 and P2)
fit.full <- lm(F ~ ., data = exam)
fit.full

##TESTING THE MODELS

#Model 1 - The intercept is -22.34 with the p-value = .068.
summary(fit.P1)
#          Do not reject the null hypothesis

#Model 2 - The intercept is -1.85 with the p-value = .809.
summary(fit.P2)
#          Do not reject the null hypothesis


#Model 3 - The intercept is -14.50 with the p-value = .133.
summary(fit.full)
#          Do not reject the null hypothesis

##BETTER PREDICTOR OF F

#Model 1 - R^2 = .802, R^2adj = .793 and p-value = 1.78e-08
summary(fit.P1)

#Model 2 - R^2 = .86, R^2adj = .853 and p-value = 5.44e-10
summary(fit.P2)

#Conclusion - fit.P2 is the better predictor due to R^2 and R^2adj
#             being higher and p-value < .05

##BEST MODEL FOR PREDICTION

#Model 3 - R^2 = .886, R^2adj = .87 and p-value = 1.069e-09
summary(fit.full)

#Conclusion - Model 3 is the best model for this prediction due to its high R^2,
#             R^2adj and p-value < .05

#Prediction - Score is predicted to be 80.71 based upon Model 3 (full model)
student <- data.frame(P1 = 78, P2 = 85)
predicted_score <- predict(fit.full, newdata = student)
predicted_score


#         -- RABE5 3.15 --
##DATASET
cig<-read.table('https://www1.aucegypt.edu/faculty/hadi/RABE6/Data6/Cigarette.Consumption.txt',sep='\t',header=T)
head(cig)
names(cig)
view(cig)

##TESTING THE HYPOTHESIS - NO FEMALES

#Full Model
fit.full <- lm(Sales ~ Age + HS + Income + Black + Female + Price, data = cig)
fit.full
summary(fit.full)

#Model without Female Variable
fit.no_female <- lm(Sales ~ Age + HS + Income + Black + Price, data = cig)
fit.no_female
summary(fit.no_female)

#ANOVA test to compare models
anova(fit.no_female, fit.full)

#Conclusion - After the comparison of the Full Model (including female) and
#             Model not including female, there is not significant enough evidence
#             that "Female" is necessary data to make prediction due to the p-value
#             being .85 (p-value > .05) causing us failing to reject the null hypothesis.

##TESTING THE HYPOTHESIS - NO FEMALES OR HS

#Model without Female and HS Variable
fit.no_female_hs <- lm(Sales ~ Age + Income + Black + Price, data = cig)
fit.no_female_hs
summary(fit.no_female_hs)

#ANOVA test to compare models
anova(fit.no_female_hs, fit.full)

#Conclusion - After the comparison of the Full Model (including female and hs) and
#             Model not including female and hs, there is not significant enough evidence
#             that "Female" and "HS" are necessary data to make prediction due to the p-value
#             being .979 (p-value > .05) causing us failing to reject the null hypothesis.

#CONFIDENCE INTERVAL
fit.full
summary(fit.full)
CI.U<-0.01895+qt(.975,44)*.01022
CI.L<-0.01895-qt(.975,44)*.01022
CI.U
CI.L
confint(fit.full, "Income", level = 0.95)

##VARIATION - NO INCOME

#Model without Income Variable
fit.no_income <- lm(Sales ~ Age + HS + Black + Female + Price, data = cig)
summary(fit.no_income)

#R^2 values from Models
r_squared_full <- summary(fit.full)$r.squared
r_squared_full
r_squared_no_income <- summary(fit.no_income)$r.squared
r_squared_no_income
variation_income <- r_squared_full - r_squared_no_income
variation_income

#Conclusion - R^2 of the full model is .321 and R^2 for the no income model is .268
#             This means Income affects 5.31% (.0531) prediction of Sales.
#             Income is an important variable when using our model to predict Sales.

## VARIATION - PRICE, AGE, INCOME

#Model with only Price, Age, and Income Variables
fit.price_age_income <- lm(Sales ~ Price + Age + Income, data = cig)

summary(fit.price_age_income)$r.squared

#Conclusion - R^2 of the Price, Age, and Income model is .303.
#             This means these three variables explain 30.3% prediction of Sales.

##VARIATION - INCOME

#Model with only Income
fit.income <- lm(Sales ~ Income, data = cig)
summary(fit.income)$r.squared

#Conclusion - R^2 of the Income Model is 0.1063 .
#             This means income affects the prediction of sales by 10.63%
