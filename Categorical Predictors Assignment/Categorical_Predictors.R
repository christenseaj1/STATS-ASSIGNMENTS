require(tidyverse)

#Let's work on categorical predictors!

#Note the data I'm generating is random data from R
#Your data and your classmates will be slightly different.
#Generate data - MAKE SURE YOU EXICUTE THIS CODE
#Note: This is random data so it will create different
#data each time it is exicuted.
n = 20 #number of observations
X <- c(1:n) #Quantitative Predictor
Cat<-as.factor(c(rep(c("A","B"),n/2)))#Categorical Predictor
E<-c(rep(c(1,0),n/2))
Y <- 2*X+rnorm(n, 0, 4)+E*rnorm(n,5,1)# Quantitative Response

#The data is stored in df.
df<-data.frame(Y=Y,X=X,Cat=Cat) #Data
view(df)

#Note: Check the levels of Cat
levels(as.factor(Cat))

#Q:  First make a boxplot of Y~Cat (Chap 6 notes show this)
ggplot(df, aes(x = Cat, y = Y)) +
  geom_boxplot() +
  labs(title = "Boxplot of Y by Category",
       x = "Category",
       y = "Y") +
  theme_minimal()

#Q: What is your conclusion from the following t-test?
t.test(Y~Cat,data=df)

#We get a p-value of 0.2877 which is greater than .05.
#We fail to reject the null hypothesis.
#This means there is not enough statistical evidence that Y can be explained by the type of Cat

#Q: What is the R2 adj of the following model?
fitX<-lm(Y~X,data=df)
summary(fitX)

#R2 adj is .756 meaning the proportion variance for Y explained by the predictor X
#is approx 75.6% showing somewhat of a strong positive correlation between the variables.

#Q: Visualize the Model using ggplot
ggplot(df, aes(x = X, y = Y, color = Cat)) +
  geom_point(size = 2, alpha = 0.6) +
  stat_smooth(method = 'lm', se=F) +
  labs(title = "X vs Y Explained by Cat Type",
       x = "X",
       y = "Y") +
  theme_minimal()

#Q: Use mutate to add an dummy variable Cat1 to the
#dataframe that is 1 for A and 0 for B. hint: ifelse(Cat=="A",1,0)
df <- df %>%
  mutate(
    Cat = as.factor(Cat),
    Cat1 = ifelse(Cat == "A", 1, 0)
    
  )

#Q: What is the R2 adj of the model with X and Cat1?
fitXCat1<-lm(Y~X+Cat1, data=df)
summary(fitXCat1)

#Note: Your Model above should be the same output as this.
fitXCat<-lm(Y~X+Cat, data=df)
summary(fitXCat)

#The R2 adj is 0.8614 meaning the proportion variance for Y explained by the predictors X
#and Cat1 is approx 86.14% showing a strong positive correlation between the variables.

#Q: What is the p-value testing for the variable Cat1?

#The p-value is 1.966e-08 which is less than .05
#We reject the null hypothesis.
#This means there is significant evidence that Y is explained by the predictors X and Cat1

#Q: Use ggplot to graph the model and data (don't use stat_smooth!)
#look in the notes.  You need to find the fitted values
df <- df %>%
  mutate(fitted = fitted(fitXCat))
ggplot(df, aes(x = X, y = Y, color = Cat)) +
  geom_point(size = 2, alpha = 0.6) +
  geom_line(aes(y = fitted), size = 1) +
  labs(title = "X vs Y by Category",
       x = "X",
       y = "Y") +
  theme_minimal()

#Q: Graph the residuals for level A and B
df <- df %>%
  mutate(residuals = resid(fitXCat))
ggplot(df, aes(x=Cat, y = residuals, color = Cat))+
  geom_point()

#Q: Graph the residuals vs the fitted values.
ggplot(df, aes(x = fitted, y = residuals)) +
  geom_point(size = 1, color = "forestgreen") +
  stat_smooth(method = 'lm', se=F, color = "black") +
  labs(title = "Residuals vs. Fitted ",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

#Note:  Thanks for doing this HW!