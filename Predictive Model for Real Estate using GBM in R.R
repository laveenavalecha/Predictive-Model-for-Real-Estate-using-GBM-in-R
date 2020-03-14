# Author - Laveena Valecha - valechalaveena21@gmail.com

options(stringsAsFactors =FALSE,scipen = 99)
house_train = read.csv("housing_train.csv", stringsAsFactors = FALSE)
house_test = read.csv("housing_test.csv", stringsAsFactors = FALSE)
head(house_train)

library(dplyr)

names(house_train)
names(house_test)

setdiff(names(house_train), names(house_test))

dim(house_train)
dim(house_test)

house_test$Price = NA

house_train$data = "train"
house_test$data = "test"

house = rbind(house_train, house_test)
dim(house)

str(house)
#View(house)


sapply(house[], function(x)sum(is.na(x)))


replace_median <- function(x){
  x[is.na(x)] <-median(sort(x))
  return(x)
}
house[,c("Bathroom"  , "Car" , "Landsize")]=
  lapply(house[,c("Bathroom"  , "Car" , "Landsize")], replace_median)

## Removing the garbage columns which contains a lot of categories or missing data
house=house %>%
  select(-Suburb,-Address,-Bedroom2,-BuildingArea,-YearBuilt,-CouncilArea)

#View(house)



CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var]) ## getting the table for the variable(any categorical variable)
  t=t[t>freq_cutoff] ## cutoff is the frequency of occurance of a variable default is 0 , but ideally it should be atleast 15-20% of actual data,
  ## so here whatever categories which are less than that cut off frequencies are dropped(no dummes are created for them)
  t=sort(t) ## sort the data
  categories=names(t)[-1] ## pick everything but exclude the first as it has lowest frequency: REmember its n-1
  
  for( cat in categories){
    name=paste(var,cat,sep="_") ## Inside the for loop create a name separated by name of variable and category separeted by "_" underscore
    name=gsub(" ","",name) ## replace any spaces if there is found in categoreis of variables
    name=gsub("-","_",name) ## replace any dash if found in categories to underscropes: e.g. 'Cat-1', 'Cat-2' will be 'Cat_1', 'Cat_2'
    name=gsub("\\?","Q",name) ## any question mark is converted to 'Q'
    name=gsub("<","LT_",name) ## Less than sign is converted to LT_
    name=gsub("\\+","",name) ## + sign is removed
    name=gsub("\\/","_",name) ## "/" is replaced with "_"
    name=gsub(">","GT_",name) ## ">" is replaced with 'GT_'
    name=gsub("=","EQ_",name) ## '=' is replaced with 'EQ_'
    name=gsub(",","",name) ##  ',' is replaced with ''
    data[,name]=as.numeric(data[,var]==cat) ## changing to numeric type
  }
  
  data[,var]=NULL
  return(data)
}


#house  %>% group_by(house$Postcode) %>%  summarise('count'= n()) %>% View()

## picking all the character columns and creating dummies

house=CreateDummies(house,"Type")
house=CreateDummies(house,"Method")
house=CreateDummies(house,"SellerG",100)
house=CreateDummies(house,"Postcode",100)


dim(house)
## separate train and test
house_train=house %>% filter(data=='train') %>% select(-data)
house_test=house %>% filter(data=='test') %>% select(-data,-Price)

##
## Make a train set from ld_train for model validation
set.seed(10)
s=sample(1:nrow(house_train),0.75*nrow(house_train))
house_train1=house_train[s,]
house_train2=house_train[-s,]

dim(house_train)
dim(house_train1)
dim(house_train2)

library(gbm)
library(cvTools)

param=list(interaction.depth=c(1:7),
           n.trees=c(50,100,200,500,700),
           shrinkage=c(.1,.01,.001),
           n.minobsinnode=c(1,2,5,10))

subset_paras=function(full_list_para,n=10){
  all_comb=expand.grid(full_list_para)
  s=sample(1:nrow(all_comb),n)
  subset_para=all_comb[s,]
  return(subset_para)
}

num_trials=10
my_params=subset_paras(param,num_trials)
# Note: A good value for num_trials is around 10-20% of total possible
# combination. It doesnt have to be always 10
myerror=9999999
for(i in 1:num_trials){
  print(paste0('starting iteration:',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
  k=cvTuning(gbm,Price~.,
             data =house_train1,
             tuning =params,
             args = list(distribution="gaussian"),
             folds = cvFolds(nrow(house_train1), K=10, type = "random"),
             seed =2,
             predictArgs = list(n.trees=params$n.trees)
  )
  score.this=k$cv[,2]
  if(score.this<myerror){
    print(params)
    # uncomment the line above to keep track of progress
    myerror=score.this
    print(myerror)
    # uncomment the line above to keep track of progress
    best_params=params
  }
  print('DONE')
  # uncomment the line above to keep track of progress
}

myerror

best_params

#levels(house_train1$Price ) <- make.names(levels(house_train1$Price )) #ADDED later but same error 
#levels(house_train2$Price ) <- make.names(levels(house_train2$Price )) #ADDED later but same error 

house.gbm.final=gbm(Price~.,data=house_train1,
                    n.trees = 700,
                    n.minobsinnode = 5,
                    shrinkage = 0.1,
                    interaction.depth = 4,
                    distribution = "gaussian")

summary(house.gbm.final)

train.pred=predict(house.gbm.final,newdata=house_train1,n.trees = best_params$n.trees)
errors1=house_train1$Price-train.pred
RMSE1=errors1**2 %>% mean() %>% sqrt()
score1 = 212467/RMSE1
score1

val.pred=predict(house.gbm.final,newdata=house_train2,n.trees = best_params$n.trees)
errors2=house_train2$Price-val.pred
RMSE2=errors2**2 %>% mean() %>% sqrt()
score2 = 212467/RMSE2
score2

test.pred=predict(house.gbm.final,newdata=house_test,n.trees = best_params$n.trees)
length(test.pred)
write.csv(test.pred,"Laveena_Valecha_P1_part2.csv",row.names = F)


#paste("Original = ",house_train$Price[0:10],"| Predicted = ",test.pred[0:10])
