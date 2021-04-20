#Simple Linear Regression

#Import Dataset 
dataset = read.csv("Salary_Data.csv")

#Splitting the dataset into traing set and test set
library(caTools)
set.seed(123)
split = sample.split(dataset$Salary,SplitRatio = 2/3)
training_set = subset(dataset, split==TRUE)
testing_set = subset(dataset, split==FALSE)

#Fitting Sample Leniear Regression to training set 
regressor = lm(formula = Salary ~ YearsExperience, data = training_set )

#Predicting the test set result 
Y_pred = predict(regressor, newdata = testing_set)

#Visualize the training_set results
library(ggplot2)
ggplot()+
  geom_point(aes(x = training_set$YearsExperience, y= training_set$Salary),
             color = 'red')+
  geom_line(aes(x=training_set$YearsExperience, y=predict(regressor,newdata = training_set)),
            color = 'blue')+
  ggtitle('Salary vs Experience (Training set)') +
  xlab('Years of experience') +
  ylab('Salary')

# Visualising the Test set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = testing_set$YearsExperience, y = testing_set$Salary),
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Salary vs Experience (Test set)') +
  xlab('Years of experience') +
  ylab('Salary')

