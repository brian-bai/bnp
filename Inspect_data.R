library(readr)
submission = read_csv("data/sample_submission.csv")
#submission colname
colnames(submission)

test=read_csv("data/test.csv")
str(test)
summary(test)
#remove test to release memory
#rm(test)

train=read_csv("data/train.csv")

str(train)
summary(train)

colnames(train)
#col type
class(train$v125)=="character"

#guess with mean
target_mean<-mean(train$target)
submission$PredictedProb<-target_mean

#All 0.5 Benchmark	0.69315	
#Guess with mean: 0.55021
#write_csv(submission, "data/guess_with_mean.csv")
#detect data case
detect_complete<-function(dt, dtname){
  nrows<-nrow(dt)
  complete_cases<-dt[complete.cases(dt),]
  complete_rate<-nrow(complete_cases)/nrows
  result<-data.frame(dataset=dtname,nrows=nrows, complete_rate=complete_rate)
  return(result)
}
#detect variable
detect_variable<-function(variable, name){
  
}

train_desc<-detect_complete(train, "Train")
test_desc<-detect_complete(test,"Test")

data_desc<-rbind(train_desc,test_desc)
data_desc

complete.train<-train[complete.cases(train),]
complete.test<-test[complete.cases(test),]

nrow(complete.train)/nrow(train)
nrow(complete.test)/nrow(test)

library(dplyr)
library(tidyr)
library(ggplot2)
na_count<-function(x){
  sum(is.na(x))
}
missings<-summarise_each(train,funs(na_count)) %>% gather
stat.missings<-transform(missings, key=reorder(key,value))

stat.missing<-group_by(stat.missings, value) %>% mutate(count=n())

g <- ggplot(stat.missing,aes(x=key,y=value))
g + geom_point(aes(color=count, size=count),stat="identity",fill="gray") +
  coord_flip() + 
  theme(axis.text.y=element_text(size=rel(0.5)))


dplyr::filter(stat.missings, value==0)
dplyr::filter(stat.missings, value==0)
dplyr::filter(stat.missings, value>50000)

#dplyr::count(stat.missings, value)
#stat.missing<-group_by(stat.missings, value) %>% mutate(count=n())

#vane graph to show missing count
