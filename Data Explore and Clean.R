library(dplyr)
library(ggplot2)
library(stringr)
library(xlsx)
library(sqldf)
library(RSQLite)
library(data.table)
library(readxl)
options(scipen=999)


trans_data1 = read_excel("card transactions.xlsx",1)
trans_data=trans_data1
######data exploration
str(trans_data)
summary(trans_data)

x=100-colSums(is.na(trans_data))*100/nrow(trans_data)
names(x)
trans_data$Cardnum=as.factor(trans_data$Cardnum)
trans_data$Merchantnum=as.factor(trans_data$Merchantnum)

trans_data$`Merch Description`=as.factor(trans_data$`Merch Description`)
trans_data$`Merchant State`=as.factor(trans_data$`Merchant State`)
trans_data$`Merchant Zip`=as.factor(trans_data$`Merchant Zip`)
trans_data$Transtype=as.factor(trans_data$Transtype)
trans_data$Fraud=as.factor(trans_data$Fraud)

trans_data%>%
  arrange(-Amount)%>%
  head(1)->nik
nik$Amount
###remove the not rtequired values
trans_data%>%
  filter(!Recordnum==52594) -> trans_data


trans_data%>%
  group_by(Recordnum)%>%
  summarize(distinct=n_distinct(Recordnum))

trans_data%>%
  group_by(Cardnum)%>%
  summarize(distinct=n())%>%
  arrange(-distinct)%>%
  head(10)%>%
  ggplot(aes(x=Cardnum,y=distinct))+
  geom_bar(stat="identity")

###amounts
trans_data%>%
  group_by(Amount)%>%
  summarize(distinct=n())%>%
  arrange(-distinct)%>%
  head(10)%>%
  ggplot(aes(x=Amount,y=distinct))+
  geom_bar(stat="identity")

trans_data%>%
  filter(Amount<5000)%>%
  ggplot(aes(x=Amount),color="blue")+
  geom_histogram(fill="red",color="blue")+
  scale_y_log10()
###remove the transactions with different transtyoe
summary(trans_data)
trans_data%>%
  filter(Transtype=="P")->trans_data

trans_data%>%
  group_by(Date)%>%
  summarize(num_tr = n())%>%
  ggplot(aes(x=Date,y=num_tr))+
  geom_line()

##replace the NAs
summary(trans_data)
trans_data$Transtype=droplevels(trans_data$Transtype)
##remove the trans_type
trans_data=trans_data[,-8]

##for feature creation 
features_data = trans_data
summary(features_data)
features_data%>%
  filter(Fraud==1)%>%
  nrow()
trans_data=features_data
sum(is.na(trans_data$`Merch Description`))
tail(trans_data$`Merch Description`)

##create a variable by number of transactions on a date the record occured, amount of the total transactions that day before the record
##create the variables # of times a cardnum, merchant num,mer des,merchant sta, zip was seen before
##no. of distinct other fields seen for a particular field
##days since the particular field was seen
which.max(trans_data1$Amount)
trans_data1$Amount[52594]
0.054*3102046
##should remove them
mode = function(x, na.rm = TRUE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}


trans_data%>%
  group_by(Merchantnum)%>%
  summarize(mode_data=mode(`Merchant Zip`)) -> merchzip_mode_for_merchnum

###now to replace those values where NAs are there so join this table to the merchant state*zip = NA
trans_data%>%
  group_by(Merchantnum,`Merchant Zip`)%>%
  summarize(count=n()) %>%
  filter(is.na(`Merchant Zip`))-> na_data_merchantnumXzip

temp_data = left_join(na_data_merchantnumXzip,merchzip_mode_for_merchnum)
temp_data=temp_data[,c(1,4)]
trans_data=left_join(trans_data,temp_data)

new_data=trans_data%>%
  mutate(merchzip=if_else(is.na(`Merchant Zip`),mode_data,`Merchant Zip`))



new_data=new_data[,-c(7,10)]
new_data$merchzip=droplevels(new_data$merchzip)
###now on basis of state
new_data%>%
  group_by(`Merchant State`)%>%
  summarize(mode_data=mode(merchzip)) -> merchzip_mode_for_merchstate

new_data%>%
  group_by(`Merchant State`,merchzip)%>%
  summarize(count=n()) %>%
  filter(is.na(merchzip))-> na_data_merchantstateXzip

temp_data = left_join(na_data_merchantstateXzip,merchzip_mode_for_state)
temp_data=temp_data[,c(1,4)]
new_data=left_join(new_data,temp_data)

new_data1=new_data%>%
  mutate(merchzip=if_else(is.na(merchzip),mode_data,merchzip))

sum(is.na(new_data$merchzip))
sum(is.na(new_data1$merchzip))
##brought down NAs from 4300 to 1556
new_data=new_data1[,-10]

new_data$Fraud=as.character(new_data$Fraud)
new_data[is.na(new_data$merchzip),]%>%
  filter(Fraud=="1")
nik=new_data[new_data$`Merch Description`=="IBM INTERNET  01000025",]
##replace the zip with 10504
temp=new_data%>%
  group_by(`Merch Description`)%>%
  summarize(zip = "10504")%>%
  filter(`Merch Description`=="IBM INTERNET  01000025")

new_data=left_join(new_data,temp)

new_data$merchzip=as.character(new_data$merchzip)
new_data=new_data%>%
  mutate(merchzip=if_else(is.na(merchzip),zip,merchzip))
##################
sum(is.na(new_data$merchzip))
###now replace merchzip with "00000" where NA
new_data=new_data%>%
  mutate(merchzip=if_else(is.na(merchzip),"0000",merchzip))

new_data=new_data[,-10]
new_data$merchzip=as.factor(new_data$merchzip)
summary(new_data)

sum(is.na((new_data$`Merchant State`)))
new_data%>%
  filter(is.na(`Merchant State`))%>%
  filter(Fraud=="1")-> nik

###only 3 are fraud
##put in XX in the state
new_data$`Merchant State`=as.character(new_data$`Merchant State`)
new_data=new_data%>%
  mutate(`Merchant State`=if_else(is.na(`Merchant State`),"XX",`Merchant State`))
sum(is.na(new_data$`Merchant State`))
summary(new_data)

###now for merchant num
##by merchatn description

##
new_data%>%
  filter(Fraud=="1")%>%
  filter(is.na(Merchantnum))%>%
  nrow()
##13 frauds
temp_data=new_data
new_data=temp_data
#give all merfchant num a separate num
new_data$Merchantnum=as.character(new_data$Merchantnum)
for (i in 1:nrow(new_data)){
  if(is.na(new_data[i,"Merchantnum"])){
    new_data[i,"Merchantnum"]=paste("NA_",i)
  }
}

new_data$Recordnum=as.factor(new_data$Recordnum)
new_data$Merchantnum=as.factor(new_data$Merchantnum)
new_data$`Merchant State`=as.factor(new_data$`Merchant State`)
new_data$Fraud=as.character(new_data$Fraud)
new_data$Fraud=as.factor(new_data$Fraud)
levels(new_data$Fraud)=c("0","1")
new_data%>%
  mutate(Fraud=if_else(Fraud==1,0,1))->new_data
summary(new_data)
###write out this clean dataset
write.csv(new_data,"filled_values.csv")


new_data[new_data$`Merchant State`=="NY",]
n_distinct(new_data$Recordnum)
n_distinct(new_data$Cardnum)
new_data$Cardnum=droplevels(new_data$Cardnum)
n_distinct(new_data$Merchantnum)
summary(new_data)
n_distinct(new_data$`Merch Description`)
new_data$`Merch Description`=droplevels(new_data$`Merch Description`)
