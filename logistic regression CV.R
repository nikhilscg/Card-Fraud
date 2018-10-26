library(e1071)
library(data.table)

##read in both the datasets
m_data=fread("train_bwd_features_3-5-18.csv")[,-1]
oot_data = fread("oot_bwd_features_3-5-18.csv")[,-1]
m_data$fraud = as.factor(as.numeric(m_data$fraud))
oot_data$fraud=as.factor(as.numeric(oot_data$fraud))
train = m_data

oot = oot_data
###convert the file to h2o file

k=5
set.seed(562)
train$id=sample(1:k,nrow(train),replace=T)

train.fdr=vector()
test.fdr=vector()

list=1:k
fdr_dataframe=data.frame()
y="fraud"
x=setdiff(names(m_data),y)
a=1
#for (num_trees in c(200,400,600)){
 # for (depth in c(5,6,7)){
  #  for (eta in c(0.01,0.05,0.1,0.2,0.3)){
      for (i in 1:k){
        temp_train = subset(train,id %in% list[-i])[,-16]
        temp_test = subset(train,id %in% c(i))[,-16]
        ###convert to H2o environment
        # train_d=as.h2o(temp_train)
        # test_d = as.h2o(temp_test)
        ##now run the xgboost model ##this will cross validate a particular set of parameters
        log.fit = glm(fraud~.,data=temp_train,family="binomial")
        
        pred_tr=predict(log.fit,newdata=temp_train,type="response")
        pred_te=predict(log.fit,newdata=temp_test,type="response")
        temp_train = cbind(temp_train,pred = pred_tr)
        temp_test = cbind(temp_test,pred=pred_te)
        temp_train$fraud=as.numeric(as.character(temp_train$fraud))
        temp_test$fraud=as.numeric(as.character(temp_test$fraud))
        temp_train=temp_train%>%
          arrange(-pred)
        temp_test=temp_test%>%
          arrange(-pred)
        #train.fdr[i]=generate_report(train_d)
        #test.fdr[i]=generate_report(test_d)
        train.fdr[i]= sum(temp_train[1:floor(.02*nrow(temp_train)),]$fraud)/sum(temp_train$fraud)
        test.fdr[i] = sum(temp_test[1:floor(.02*nrow(temp_test)),]$fraud)/sum(temp_test$fraud)
        
      }
      
      mean_train_fdr=mean(train.fdr)
      mean_test_fdr = mean(test.fdr)
      fdr_dataframe=rbind(fdr_dataframe,c(a,num_trees,num_m,num_l,mean_train_fdr,mean_test_fdr))
      a=a+1
    #}
  #}
#}


colnames(fdr_dataframe)=c("Model#","Ntree","Mtry","Leaf Size","Train_FDR","Test_FDR")
##write this out
write.csv(fdr_dataframe,"FDR_CV_RF")

temp_train=m_data
temp_test=oot_data
