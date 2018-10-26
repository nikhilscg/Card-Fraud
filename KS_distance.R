library(dplyr)
library(ggplot2)
library(randomForest)
#ks_data1 = read.csv("train.lowvariables.csv")[,-1]
ks_data1=read.csv("train_sample_3-5-18.csv")[,-1]
###remove the records for january
ks_data1$date=as.character(ks_data1$date)
ks_data1$date=ymd(ks_data1$date)


ks_data1%>%
  filter(date>=ymd("2010-02-01"))-> ks_data1

ks_data = ks_data1[,-c(1,2)]
colnames(ks_data1)
ks.distance = vector()


for (i in 2:length(ks_data)){
  x = data.frame(ks_data%>%
                   filter(fraud == 0))[,i]
  y =  data.frame(ks_data%>%
                    filter(fraud == 1))[,i] 
  ks.distance[i-1] = ks.test(x,y)$statistic
}
#colnames(model_data1)[4:206]

ks_features
names(ks.distance)=colnames(ks_data)[2:length(ks_data)]
names(ks.distance)
ks_features=ks.distance[order(-ks.distance)]
#Select top 40 variables
ks_features=ks_features[1:40]
names(ks_features)
ks_features_final = ks_data[names(ks_data) %in% names(ks_features)]

ks_features_final=cbind(record=ks_data1$record,ks_features_final,fraud=ks_data1$fraud)

#feature_selection=ks_features_final
###write out this dataset
write.csv(ks_features_final,"ks_features_3-5-18.csv")
#write.csv(ks_features_final,"ks.lowvariables.csv")
##read in the dataset
feature_selection = read.csv("ks_features_3-5-18.csv")[,-1]



##to select the features that are most important we can run a backward selection wrapped around a logistic one
##also run a RF to find out the important varaibale
###backward selection
library(leaps)
##change fraud to factor
feature_selection$fraud=as.factor(feature_selection$fraud)

feature_selection=feature_selection[,-1]

## run a forward selection around a logistic function
## Run forward selection using logistic regression
# backward.fit = glm(fraud~.,data = feature_selection,family="binomial")
# #summary(backward.fit)
# formula(backward.fit)
# ###backward fit
# #step(backward.fit,direction="backward")
# summary(backward.fit)
# x=as.data.frame(ks.distance)
# ##forward fit
# fitstart = glm(fraud~1,data = feature_selection,family="binomial")
# summary(fitstart)
# 
# forward_selection_model = step(fitstart,direction="forward",scope=formula(backward.fit))
# forward_selection_model
# x=as.data.frame(cor(feature_selection[,-41]))
# write.csv(x,"x.csv")
################
regfit.bwd = regsubsets(fraud~.,data=feature_selection,nvmax=30,method="backward")
reg.summary=summary(regfit.bwd)

reg.summary$cp

plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC")

which.min(reg.summary$bic)

points(14,reg.summary$bic[14],col="red",cex=2,pch=20)
####22 variable model
?plot.regsubsets
plot(regfit.bwd,scale="bic")
bwd_features=names(coef(regfit.bwd,14))
bwd_features
###select these features from the dataset
selected_features = feature_selection[names(feature_selection)%in% c("fraud",bwd_features)]
###write out this dataset for training features
write.csv(selected_features,"train_bwd_features_3-5-18.csv")
###read in the oot and select the features and write that out too
oot=read.csv("oot_sample_3-5-18.csv")[,-1]
oot_bwd_features = oot[names(oot) %in% names(selected_features)]
write.csv(oot_bwd_features,"oot_bwd_features_3-5-18.csv")

###use RF to find the best variables

rf.fit=randomForest(fraud~.,data=feature_selection,ntree=200)

varImpPlot(rf.fit)

rf_variables = c("card_fraud_r","merch_fraud_r","cardNum_amount_1","cardNum_amount_3","merchnum_amount_1","merchdesc_amount_1","amount","normamountTrans_dow","normamountTrans_dom","merchnum_amount_3","cardNum_amount_10","cardNum_1","merchdesc_amount_3","cardNum_amount_15","fraud")

rf_features = feature_selection[names(feature_selection) %in% rf_variables]
###write out this dataset
write.csv(rf_features,"rf_features_3-5-18.csv")
##use it for the oot sample
oot_rf = oot[names(oot) %in% rf_variables]
##write out this dataset
write.csv(oot_rf,"oot_rf_3-5-18.csv")