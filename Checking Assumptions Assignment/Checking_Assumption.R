require(tidyverse)
require(GGally)

#Please answer each of the questions below.
df<- read.csv('https://www1.aucegypt.edu/faculty/hadi/RABE6/Data6/Exercise5.12.txt',sep='\t',header=T)
names(df)
head(df)

#fit the full model.
fit<-lm(Y~.,data=df)

#Notice we changed the name of the data frame
#that includes the information about the fitted values,
#the leverage values ect.
df.fit<-df%>%
  mutate(
    fitted=fit$fitted.values, #y_hat values
    res.student = rstudent(fit), #r*_i - studentized residuals
    leverage = hatvalues(fit), #p_ii - the leverage values
    cooks=cooks.distance(fit), #C_i - cook's Distances
    Index = c(1:nrow(df)) #observation numbers
  )
names(df.fit)
head(df.fit)

#Use ggpairs() to comment on the linearity assumptions
# and the assumption about the indepence of the predictor
#variables
ggpairs(df)

##The linearity assumption holds for X1, X2, and X3, but X4, X5, and X6
##show non-linear patterns, possibly requiring transformations.

#Plot a histogram of the studentized residuals.
ggplot(df.fit, aes(x = res.student)) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "forestgreen") +
  labs(title = "Histogram of Studentized Residuals",
       x = "Studentized Residuals",
       y = "Frequency") +
  theme_minimal()

#Does the shape of the distribution of the studentized
#residuals look like it strongly deviates from a normal distribution?

##The "studentized" data appears to be normally distributed due to its symmetry and bell shape.
##There is some slight skewness but not enough to deviate it from normal distribution

#Plot the studentized residuals vs the fitted values and comment on
ggplot(df.fit, aes(x = fitted, y = res.student)) +
  geom_point(color = "forestgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "Studentized Residuals vs. Fitted Values",
       x = "Fitted Values",
       y = "Studentized Residuals") +
  theme_minimal()
# anything that might indicate a violation of assumptions.

#Plot the studentized residuals vs the Index
ggplot(df.fit, aes(x = Index, y = res.student)) +
  geom_point(color = "forestgreen") +
  
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "Studentized Residuals vs. Fitted Values",
       x = "Index",
       y = "Studentized Residuals") +
  theme_minimal()
#Are any of the residuals usually far from 0?

## Yes based upon the plot, there seems to be some residuals far from 0 passing 2 and -2

#Use the which() function to find the studentized residual and its observation number.
outlier_indices <- which(abs(df.fit$res.student) > 2)
outliers <- df.fit[outlier_indices, c("Index", "res.student")]
outliers

#Plot the studentized residuals vs the predictor variable X1
ggplot(df, aes(x = df.fit$X1, y = df.fit$Y)) +
  geom_point(color = "forestgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "Violations?",
       x = "X-Values",
       y = "Y-Values") +
  theme_minimal()

# anything that might indicate a violation of assumptions?

## There are minor violations of assumption. The minor violation is "Outliers". 
## There is evidence of heteroscedasticity.

#Plot Leverage Values vs the Index
ggplot(df.fit, aes(x = Index, y = leverage)) +
  geom_point(color = "forestgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +  # Adding a linear trend line with
  confidence interval
labs(title = "Leverage Values vs. Index",
     x = "Index",
     y = "Leverage Values") +
  theme_minimal()

#Use the which() function to identify any high leverage values
p <- ncol(df) - 1
n <- nrow(df)
high_leverage_threshold <- 2 * (p + 1) / n
high_leverage_indices <- which(df.fit$leverage > high_leverage_threshold)
high_leverage_values <- df.fit$leverage[high_leverage_indices]
data.frame(Index = high_leverage_indices, Leverage = high_leverage_values)

#Plot Cooks Distances vs the Index
ggplot(df, aes(x = df.fit$cooks, y = df.fit$Index)) +
  geom_point(color = "forestgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "Cooks Distances vs Index",
       x = "Cooks Distance",
       y = "Index") +
  theme_minimal()

# Use the which() function to identify any high Cook's Distances
threshold_cooks <- 4 / (n - p - 1)
high_cooks_indices <- which(df.fit$cooks > threshold_cooks)
high_cooks_values <- df.fit$cooks[high_cooks_indices]
data.frame(Index = high_cooks_indices, Cooks_Distance = high_cooks_values)
