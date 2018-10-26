library(data.table)
library(dplyr)
library(randomForest)

#m_data=fread("rf_features.csv")[,-1]
m_data=fread("rf_features_3-5-18.csv")[,-1]
m_data$fraud=as.factor(as.numeric(m_data$fraud))
model_data = m_data

#model_data$fraud=as.factor(model_data$fraud)
#training_1=oot_data
##run a logistic regression

k=5
set.seed(562)
m_data$id=sample(1:k,nrow(m_data),replace=T)

train.fdr=vector()
test.fdr=vector()

list=1:k
#progress.bar=create_progress_bar("text")

for (i in 1:k){
  
  train_d = subset(m_data,id %in% list[-i])[,-16]
  test_d = subset(m_data,id %in% c(i))[,-16]
  ##now run the logistic regression model
  rf.fit = randomForest(fraud~.,data=train_d,ntree=50)
  pred_tr=predict(rf.fit,newdata=train_d,type="prob")[,2]
  pred_te=predict(rf.fit,newdata=test_d,type="prob")[,2]
  train_d$pred=pred_tr
  test_d$pred = pred_te
  train_d$fraud=as.numeric(as.character(train_d$fraud))
  test_d$fraud=as.numeric(as.character(test_d$fraud))
  train_d=train_d%>%
    arrange(-pred)
  test_d=test_d%>%
    arrange(-pred)
  #train.fdr[i]=generate_report(train_d)
  #test.fdr[i]=generate_report(test_d)
  train.fdr[i]=sum(train_d[1:floor(.02*nrow(train_d)),]$fraud)/sum(train_d$fraud)
  test.fdr[i] = sum(test_d[1:floor(.02*nrow(test_d)),]$fraud)/sum(test_d$fraud)
}
train.rf.fdr = mean(train.fdr)
test.rf.fdr = mean(test.fdr)
###run it on oot sample and compute FDR
###now train the logistic model on all data and use that model to predict fraud in oot sample
#oot_data=fread("oot_backward_features_final.csv")[,-1]

oot_data=read.csv("oot_rf_3-5-18.csv")[,-1]


oot_data$fraud=as.factor(oot_data$fraud)
rf.fit=randomForest(fraud~., data=model_data,ntree=50)
oot_data$pred=predict(rf.fit,newdata=oot_data,type="prob")[,2]
oot_data=oot_data%>%
  arrange(-pred)


oot_data$fraud=as.numeric(as.character(oot_data$fraud))
x=oot_data[1:252,]
sum(x$fraud)
sum(oot_data$fraud)
##rearrange the oot_data
###generating FDR
generate_report = function(training_1){
  fdr=data.frame()
  total_records = nrow(training_1)
  num_records_bin = floor(1/200*total_records)
  total_bad_pop = sum(training_1$fraud)
  total_good_pop = total_records-total_bad_pop
  for (i in 1:100){
    bin_l=floor((i-1)/100*total_records)+1
    bin_u=floor(i/100*total_records)
    bin_count=bin_u-bin_l+1
    cumulative_records = floor(i/100*total_records)
    num_good_bin = bin_count-sum(training_1[bin_l:bin_u,]$fraud)
    num_bad_bin = sum(training_1[bin_l:bin_u,]$fraud)
    cumulative_num_good = bin_u-sum(training_1[1:bin_u,]$fraud)
    cumulative_num_bad = sum(training_1[1:bin_u,]$fraud)
    
    fdr[i,"population bin %"]=paste(i,"%",sep="")
    fdr[i,"total number of records"]=num_records_bin
    fdr[i,"# Good"] = num_good_bin
    fdr[i,"# Bad"] = num_bad_bin
    fdr[i,"% Good"] = 100* num_good_bin/(num_good_bin+num_bad_bin)
    fdr[i,"% Bad"]=100*(num_bad_bin/(num_good_bin+num_bad_bin))
    
    fdr[i,"cumulative good"]=cumulative_num_good
    
    
    fdr[i,"cumulative bad"]=cumulative_num_bad
    fdr[i,"% Good out of total Goods in population"]= 100*cumulative_num_good/total_good_pop
    fdr[i,"% Bad (FDR)"]=100*cumulative_num_bad/total_bad_pop
    
    
    if(i==4){
      return(fdr[i,"% Bad (FDR)"])
    }
  }
  
}