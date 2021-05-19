#simple linear regression
#installing required packages
library(ggplot2)
library(caTools)
#read data 
df = read.csv("LR.csv")
head(df)

#fitting linear regression
model<- lm(Scores~Hours,df)
model

#plotting the model
ggplot(df,aes(x=Hours,y=Scores))+geom_point()

#checking for outliers
outliers::outlier(df)

#plotting the regression fit
plot(df)
abline(model)

#Predict the percentage of an student based on the 9.25 hours of study 
df1<-data.frame(Hours=9.25)
df1

#predicting the model
pred<-predict(model,df1)
print(pred)

summary(model)
library(caTools)


# Simple Linear Regression


# Splitting the dataset into the
# Training set and Test set

split = sample.split(df$Scores, SplitRatio = 0.7)
trainingset = subset(df, split == TRUE)
trainingset
testset = subset(df, split == FALSE)
testset

# Fitting Simple Linear Regression to the Training set
lm= lm(formula = Scores ~ Hours,
         data = trainingset)
coef(lm)

# Predicting the Test set results
ypred = predict(lm, newdata = testset)
ypred

#Predict the percentage of an student based on the 9.25 hours of study 
#created dataframe with hours 9.25
df2<-data.frame(Hours=9.25)
df2
pred = predict(lm,df2)
pred[1]



install.packages("ggplot2")
library(ggplot2)

# VisualiZing the Training set results
ggplot() + geom_point(aes(x = trainingset$Hours,
                          y = trainingset$Scores), colour = 'red') +
  geom_line(aes(x = trainingset$Hours,
                y = predict(lm, newdata = trainingset)), colour = 'blue') +
  
  ggtitle('HOURS VS SCORES (Training set)') +
  xlab('HOURS') +
  ylab('SCORES')

# Visualizing the Test set results
ggplot() +
  geom_point(aes(x = testset$Hours, y = testset$Scores),
             colour = 'red') +
  geom_line(aes(x = trainingset$Hours,
                y = predict(lm, newdata = trainingset)),
            colour = 'blue') +
  ggtitle('HOURS VS SCORES (Test set)') +
  xlab('HOURS') +
  ylab('SCORES')

summary(lm)

df3<- data.frame( actual= testset$Scores, predicted = ypred )
df3

#Finally, our model equation can be written as follow: scores = 4.0229+9.6905Hours 

##model accuracy 

#Rse    #Adjusted-R-squared #Multiple R-squared  #F-statistic:  #p-value
#5.158     0.9542              0.9571              334.7             1.144e-11