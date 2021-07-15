dataset<- read.csv("https://raw.githubusercontent.com/AdiPersonalWorks/Random/master/student_scores%20-%20student_scores.csv")
dataset
typeof(dataset)
dataset=as.data.frame(dataset)

library(dplyr)
library(caTools)
library(rpart)
plot(dataset)

split_values=sample.split(dataset$Scores,SplitRatio=0.7)

train_sample=subset(dataset,split_values==T)
nrow(train_sample)
test_sample=subset(dataset,split_values==F)
nrow(test_sample)

plot(dataset)
y=dataset$Scores
x=dataset$Hours
model=lm(y~x,train_sample)
print(summary(model))

result=predict(model,test_sample)
result

z=data.frame(pred=result,actual=dataset$Scores)
z

mse=sqrt(mean((z$actual-z$pred)^2))
mse

a=data.frame(x=9.25)
final_result=predict(model,a)
final_result

