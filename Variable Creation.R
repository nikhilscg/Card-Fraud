library(dplyr)
library(RSQLite)
library(sqldf)
library(tidyr)
library(data.table)
library(lubridate)
##Check NA values

###checking the values of merchant



#card_data = fread("transaction_data_filled.csv")[,-c(1,9)]
card_data = fread("filled_values.csv")[,-1]

colnames(card_data) = c("record","cardnum","date","merchnum","merchdesc","merchstate","amount","fraud","merchzip")
card_data$record=as.numeric(card_data$record)

card_data%>%
  arrange(record)-> card_data
#card_data$record=as.factor(card_data$record)
card_data$cardnum=as.factor(card_data$cardnum)
card_data$merchnum=as.factor(card_data$merchnum)
card_data$merchdesc=as.factor(card_data$merchdesc)


card_data$merchstate=as.factor(card_data$merchstate)
card_data$merchzip=as.factor(card_data$merchzip)
card_data$date=as.character(card_data$date)

card_data$date=ymd(card_data$date)

card_data=as.data.table(card_data)
card_data%>%
  mutate(dayofw = wday(card_data$date,label=T))-> card_data
card_data%>%
  mutate(dayofm = day(card_data$date)) -> card_data
####create the variables
str(card_data)

sum(is.na(card_data$cardnum))

#sum(is.na(card_data$`Merch Description`))
##change the type

##create a copy of the dataset
card_copy = card_data
###define a function to do the time-interval join

timeWinJoin = function(dt,n,byVar){
  dt1=dt
  dt1$join_ts1 = dt1$date
  dt1$join_ts2 = dt1$date + n
  dt1$join_record = dt1$record
  keys = c(byVar,'join_ts1<=date','join_ts2>=date','record<=record')
  dt2=dt1[dt,on=keys,allow.cartesian=T]
  return(dt2)
  }
#remove(cardNum_1day)
###create CardNum for 30 day
cardJoin_30days = timeWinJoin(card_copy,30,'cardnum')

###just check the card num variables
card_copy%>%
  filter(record<=68499)%>%
  filter(date>=ymd("2010-08-15"))%>%
  filter(cardnum==5142189108)%>%
  summarize(sum(amount))

#str(cardJoin_30days)
###now create the various time count variables
cardVars_30_dt = cardJoin_30days[,.(
  ##variable for num of times card num seen in last 15   days
  cardNum_30 = .N,
  cardNum_15 = sum(join_ts1-date<=15),
  ##card num seen in last 10 days
  cardNum_10 = sum(join_ts1-date<=10),
  ###card num seen in last 3 days
  cardNum_3 = sum(join_ts1-date<=3),
  cardNum_1 = sum(join_ts1-date<=1),
  cardNummerchnum_30 = sum(!duplicated(merchnum)),
  cardNummerchnum_15 = sum(!duplicated(merchnum[join_ts1-date<=15])),
  cardNummerchnum_10 = sum(!duplicated(merchnum[join_ts1-date<=10])),
  cardNummerchnum_3=sum(!duplicated(merchnum[join_ts1-date<=3])),
  cardNummerchnum_1=sum(!duplicated(merchnum[join_ts1-date<=1])),
  cardNummerchstate_30 = sum(!duplicated(merchstate)),
  cardNummerchstate_15 = sum(!duplicated(merchstate[join_ts1-date<=15])),
  cardNummerchstate_10 = sum(!duplicated(merchstate[join_ts1-date<=10])),
  cardNummerchstate_3=sum(!duplicated(merchstate[join_ts1-date<=3])),
  cardNummerchstate_1=sum(!duplicated(merchstate[join_ts1-date<=1])),
  # cardNummerchzip_30 = sum(!duplicated(merchzip)),
  # cardNummerchzip_15 = sum(!duplicated(merchzip[join_ts1-date<=15])),
  # cardNummerchzip_7=sum(!duplicated(merchzip[join_ts1-date<=7])),
  # cardNummerchzip_1=sum(!duplicated(merchzip[join_ts1-date<=1])),
  # cardNummerchdesc_30 = sum(!duplicated(merchdesc)),
  # cardNummerchdesc_15 = sum(!duplicated(merchdesc[join_ts1-date<=15])),
  # cardNummerchdesc_7=sum(!duplicated(merchdesc[join_ts1-date<=7])),
  # cardNummerchdesc_1=sum(!duplicated(merchdesc[join_ts1-date<=1])),
  cardNum_amount_1 = sum(amount[join_ts1-date<=1]),
  cardNum_amount_3 = sum(amount[join_ts1-date<=3]),
  cardNum_amount_10 = sum(amount[join_ts1-date<=10]),
  cardNum_amount_15 = sum(amount[join_ts1-date<=15]),
  cardNum_amount_30 = sum(amount)),by=record]



card_copy%>%
  filter(date<=ymd("2010-01-31"))%>%
  filter(fraud=="1")%>%
  nrow()
# cardVars_30_dt$amount = card_copy$amount
# check=cardVars_30_dt
# cardVars_30_dt$cardNum_1=cardVars_30_dt$cardNum_1-cardVars_30_dt$amount

card_copy%>%
  filter(cardnum==5142257356)%>%
  filter(record<=300)->n
sum(n$amount)

check=cardVars_30_dt[,c(1,22)]
check=cbind(amount=card_copy$amount,check)
# card_data%>%
#   filter(Recordnum<=30)%>%
#   filter(Cardnum==5142148452)%>%
#   nrow()

###create MerchNum for 30 day
merchnumJoin_30days = timeWinJoin(card_copy,30,'merchnum')

###now create the various time count variables
merchnumVars_30_dt = merchnumJoin_30days[,.(
  ##variable for num of times merch num seen in last 30 days
  merchnum_30 = .N,
  ##merch num seen in last 15 days
  merchnum_15 = sum(join_ts1-date<=15),
  
  merchnum_10 = sum(join_ts1-date<=10),
  ###merch num seen in last 7 days
  merchnum_3 = sum(join_ts1-date<=3),
  merchnum_1 = sum(join_ts1-date<=1),
  # merchnumcardnum_30 = sum(!duplicated(cardnum)),
  # merchnumcardnum_15 = sum(!duplicated(cardnum[join_ts1-date<=15])),
  # merchnumcardnum_7 = sum(!duplicated(cardnum[join_ts1-date<=7])),
  # merchnumcardnum_1 = sum(!duplicated(cardnum[join_ts1-date<=1])),
  merchnummerchdesc_30 = sum(!duplicated(merchdesc)),
  merchnummerchdesc_15 = sum(!duplicated(merchdesc[join_ts1-date<=15])),
  merchnummerchdesc_10 = sum(!duplicated(merchdesc[join_ts1-date<=10])),
  merchnummerchdesc_3 = sum(!duplicated(merchdesc[join_ts1-date<=3])),
  merchnummerchdesc_1 = sum(!duplicated(merchdesc[join_ts1-date<=1])),
  # merchnummerchstate_30 = sum(!duplicated(merchstate)),
  # merchnummerchstate_15 = sum(!duplicated(merchstate[join_ts1-date<=15])),
  # merchnummerchstate_7 = sum(!duplicated(merchstate[join_ts1-date<=7])),
  # merchnummerchstate_1 = sum(!duplicated(merchstate[join_ts1-date<=1])),
  merchnummerchzip_30 = sum(!duplicated(merchzip)),
  merchnummerchzip_15 = sum(!duplicated(merchzip[join_ts1-date<=15])),
  merchnummerchzip_10 = sum(!duplicated(merchzip[join_ts1-date<=10])),
  merchnummerchzip_3 = sum(!duplicated(merchzip[join_ts1-date<=3])),
  merchnummerchzip_1 = sum(!duplicated(merchzip[join_ts1-date<=1])),
  merchnum_amount_1 = sum(amount[join_ts1-date<=1]),
  merchnum_amount_3 = sum(amount[join_ts1-date<=3]),
  merchnum_amount_10 = sum(amount[join_ts1-date<=10]),
  merchnum_amount_15 = sum(amount[join_ts1-date<=15]),
  merchnum_aamount_30 = sum(amount)),by=record]

###create MerchDesc for 15 day
merchdescJoin_30days = timeWinJoin(card_copy,30,'merchdesc')
###now create the various time count variables
merchdescVars_30_dt = merchdescJoin_30days[,.(
  ##variable for num of times merch num seen in last 30 days
   merchdesc_30 = .N,
  # ##merch num seen in last 15 days
   merchdesc_15 = sum(join_ts1-date<=15),
  merchdesc_10 = sum(join_ts1-date<=10),
  
  # ###merch num seen in last 7 days
  merchdesc_3 = sum(join_ts1-date<=3),
  merchdesc_1 = sum(join_ts1-date<=1),
  # merchdesccardnum_30 = sum(!duplicated(cardnum)),
  # merchdesccardnum_15 = sum(!duplicated(cardnum[join_ts1-date<=15])),
  # merchdesccardnum_7 = sum(!duplicated(cardnum[join_ts1-date<=7])),
  # merchdesccardnum_1 = sum(!duplicated(cardnum[join_ts1-date<=1])),
  # merchdescmerchnum_30 = sum(!duplicated(merchnum)),
  # merchdescmerchnum_15 = sum(!duplicated(merchnum[join_ts1-date<=15])),
  # merchdescmerchnum_7 = sum(!duplicated(merchnum[join_ts1-date<=7])),
  # merchdescmerchnum_1 = sum(!duplicated(merchnum[join_ts1-date<=1])),
  # merchdescmerchstate_30 = sum(!duplicated(merchstate)),
  # merchdescmerchstate_15 = sum(!duplicated(merchstate[join_ts1-date<=15])),
  # merchdescmerchstate_7 = sum(!duplicated(merchstate[join_ts1-date<=7])),
  # merchdescmerchstate_1 = sum(!duplicated(merchstate[join_ts1-date<=1])),
  # merchdescmerchzip_30 = sum(!duplicated(merchzip)),
  # merchdescmerchzip_15 = sum(!duplicated(merchzip[join_ts1-date<=15])),
  # merchdescmerchzip_7 = sum(!duplicated(merchzip[join_ts1-date<=7])),
  # merchdescmerchzip_1 = sum(!duplicated(merchzip[join_ts1-date<=1])),
  merchdesc_amount_1 = sum(amount[join_ts1-date<=1]),
  merchdesc_amount_3 = sum(amount[join_ts1-date<=3]),
  merchdesc_amount_10 = sum(amount[join_ts1-date<=10]),
  merchdesc_amount_15 = sum(amount[join_ts1-date<=15]),
  merchdesc_amount_30 = sum(amount)),by=record]

###create Merchstate for 15 day
#merchstateJoin_15days = timeWinJoin(card_copy,15,'merchstate')
###now create the various time count variables
#merchstateVars_15_dt = merchstateJoin_15days[,.(
  ##variable for num of times merch num seen in last 30 days
  # merchstate_30 = .N,
  # ##merch num seen in last 15 days
  # merchstate_15 = sum(join_ts1-date<=15),
  # ###merch num seen in last 7 days
  # merchstate_7 = sum(join_ts1-date<=7),
  # merchstate_1 = sum(join_ts1-date<=1),
  # merchstatecardnum_30 = sum(!duplicated(cardnum)),
  # merchstatecardnum_15 = sum(!duplicated(cardnum[join_ts1-date<=15])),
  # merchstatecardnum_7 = sum(!duplicated(cardnum[join_ts1-date<=7])),
  # merchstatecardnum_1 = sum(!duplicated(cardnum[join_ts1-date<=1])),
  # merchstatemerchnum_30 = sum(!duplicated(merchnum)),
  # merchstatemerchnum_15 = sum(!duplicated(merchnum[join_ts1-date<=15])),
  # merchstatemerchnum_7 = sum(!duplicated(merchnum[join_ts1-date<=7])),
  # merchstatemerchnum_1 = sum(!duplicated(merchnum[join_ts1-date<=1])),
  # merchstatemerchdesc_30 = sum(!duplicated(merchdesc)),
  # merchstatemerchdesc_15 = sum(!duplicated(merchdesc[join_ts1-date<=15])),
  # merchstatemerchdesc_7 = sum(!duplicated(merchdesc[join_ts1-date<=7])),
  # merchstatemerchdesc_1 = sum(!duplicated(merchdesc[join_ts1-date<=1])),
  # merchstatemerchzip_30 = sum(!duplicated(merchzip)),
  # merchstatemerchzip_15 = sum(!duplicated(merchzip[join_ts1-date<=15])),
  # merchstatemerchzip_7 = sum(!duplicated(merchzip[join_ts1-date<=7])),
  # merchstatemerchzip_1 = sum(!duplicated(merchzip[join_ts1-date<=1])),
  # merchstate_avgtrans_1 = sum(amount[join_ts1-date<=1])/sum(join_ts1-date<=1),
  # merchstate_avgtrans_7 = sum(amount[join_ts1-date<=7])/sum(join_ts1-date<=7),
  # merchstate_avgtrans_15 = sum(amount[join_ts1-date<=15])/sum(join_ts1-date<=15),
  # merchstate_avgtrans_30 = sum(amount[join_ts1-date<=30])/sum(join_ts1-date<=30),by=record]


###create Merchzip for 30 day
merchzipJoin_30days = timeWinJoin(card_copy,30,'merchzip')
###now create the various time count variables
merchzipVars_30_dt = merchzipJoin_30days[,.(
  ##variable for num of times merch num seen in last 30 days
  # merchzip_30 = .N,
  # ##merch num seen in last 15 days
  # merchzip_15 = sum(join_ts1-date<=15),
  # ###merch num seen in last 7 days
  # merchzip_7 = sum(join_ts1-date<=7),
  # merchzip_1 = sum(join_ts1-date<=1),
  # merchzipcardnum_30 = sum(!duplicated(cardnum)),
  # merchzipcardnum_15 = sum(!duplicated(cardnum[join_ts1-date<=15])),
  # merchzipcardnum_7 = sum(!duplicated(cardnum[join_ts1-date<=7])),
  # merchzipcardnum_1 = sum(!duplicated(cardnum[join_ts1-date<=1])),
  merchzipmerchnum_30 = sum(!duplicated(merchnum)),
  merchzipmerchnum_15 = sum(!duplicated(merchnum[join_ts1-date<=15])),
  merchzipmerchnum_10 = sum(!duplicated(merchnum[join_ts1-date<=10])),
  merchzipmerchnum_3 = sum(!duplicated(merchnum[join_ts1-date<=3])),
  merchzipmerchnum_1 = sum(!duplicated(merchnum[join_ts1-date<=1]))
  # merchzipmerchdesc_30 = sum(!duplicated(merchdesc)),
  # merchzipmerchdesc_15 = sum(!duplicated(merchdesc[join_ts1-date<=15])),
  # merchzipmerchdesc_15 = sum(!duplicated(merchdesc[join_ts1-date<=10])),
  # merchzipmerchdesc_7 = sum(!duplicated(merchdesc[join_ts1-date<=7])),
  # merchzipmerchdesc_1 = sum(!duplicated(merchdesc[join_ts1-date<=1])),
  # merchzipmerchstate_30 = sum(!duplicated(merchstate)),
  # merchzipmerchstate_15 = sum(!duplicated(merchstate[join_ts1-date<=15])),
  # merchzipmerchstate_7 = sum(!duplicated(merchstate[join_ts1-date<=7])),
  # merchzipmerchstate_1 = sum(!duplicated(merchstate[join_ts1-date<=1])),
  # merchzip_amount_1 = sum(amount[join_ts1-date<=1])/sum(join_ts1-date<=1),
  # merchzip_avgtrans_7 = sum(amount[join_ts1-date<=7])/sum(join_ts1-date<=7),
  # merchzip_avgtrans_15 = sum(amount[join_ts1-date<=15])/sum(join_ts1-date<=15),
  # merchzip_avgtrans_30 = sum(amount[join_ts1-date<=30])/sum(join_ts1-date<=30),
  ),by=record]

###separate out the variables from which 1 has to be subtracted and the other amount variables
x1= cardVars_30_dt[,c(2:16)]
x2= cardVars_30_dt[,c(17:21)]
x1=x1-1
x2=as.data.frame(x2)
for (i in 1:length(x2)){
  x2[,i]=x2[,i]-card_copy$amount
}
c=cbind(record=cardVars_30_dt$record,x1,x2)
###merch
x1= merchnumVars_30_dt[,c(2:16)]
x2= merchnumVars_30_dt[,c(17:21)]
x1=x1-1
x2=as.data.frame(x2)
for (i in 1:length(x2)){
  x2[,i]=x2[,i]-card_copy$amount
}
c=cbind(c,x1,x2)

###merch desc

###merch
x1= merchdescVars_30_dt[,c(2:6)]
x2= merchdescVars_30_dt[,c(7:11)]
x1=x1-1
x2=as.data.frame(x2)
for (i in 1:length(x2)){
  x2[,i]=x2[,i]-card_copy$amount
}
c=cbind(c,x1,x2)
###merch zip
x1= merchzipVars_30_dt[,c(2:5)]
x1=x1-1
c=cbind(c,x1)

####variables for average amounts for a card number
###days varibales
# days_cardnum=vector()
# days_merchnum=vector()
# norm_amount_card=vector()
# norm_amount_merch = vector()
# date_begin=ymd("2010-01-01")

# for (i in 1:nrow(card_copy)){
#   print(i)
#   temp=card_copy[i,]
#   # samplecardnum=card_copy[1:i-1,]%>%
#   #   filter(cardnum==temp$cardnum)
#   # if(nrow(samplecardnum)>=1){
#   #   days_cardnum[i] = temp$date - samplecardnum[samplecardnum$record==max(samplecardnum$record),]$date
#   # }
#   # else {
#   #   days_cardnum[i]=temp$date-date_begin
#   # }
#   # 
#   # samplemerch=card_copy[1:i-1,]%>%
#   #   filter(merchnum==temp$merchnum)
#   # if(nrow(samplemerch)>=1){
#   #   days_merchnum[i] = temp$date - samplemerch[samplemerch$record==max(samplemerch$record),]$date
#   # }
#   # else {
#   #   days_merchnum[i]=temp$date-date_begin
#   # }
#   
#   card_copy[1:i-1,]%>%
#     group_by(cardnum)%>%
#     summarize(avg_amount = sum(amount)/n()) -> check1
#   if(length(temp$amount/check1[check1$cardnum==temp$cardnum,]$avg_amount)==0) {
#     norm_amount_card[i] = 1
#   } else {
#     norm_amount_card[i] = temp$amount/check1[check1$cardnum==temp$cardnum,]$avg_amount
#   }
#   card_copy[1:i-1,]%>%
#     group_by(merchnum)%>%
#     summarize(avg_amount = sum(amount)/n()) -> check1
#   if(length(temp$amount/check1[check1$merchnum==temp$merchnum,]$avg_amount)==0) {
#     norm_amount_merch[i] = 1
#   } else {
#     norm_amount_merch[i] = temp$amount/check1[check1$merchnum==temp$merchnum,]$avg_amount
#   } 
#   
#   
# }

card_fraud_r=vector()
merch_fraud_r=vector()
card_isnew = vector()
merchnum_isnew = vector()
#date_begin=ymd("2010-01-01")
##change the type of fraud
card_copy$fraud=as.numeric(card_copy$fraud)
for (i in 1:nrow(card_copy)){
  print(i)
  temp=card_copy[i,]
  samplecardnum=card_copy[1:i-1,]%>%
    filter(cardnum==temp$cardnum)
  # if(nrow(samplecardnum)>=1){
  #   days_cardnum[i] = temp$date - samplecardnum[samplecardnum$record==max(samplecardnum$record),]$date
  # }
  # else {
  #   days_cardnum[i]=temp$date-date_begin
  # }
  
  
  samplemerch=card_copy[1:i-1,]%>%
    filter(merchnum==temp$merchnum)
  # if(nrow(samplemerch)>=1){
  #   days_merchnum[i] = temp$date - samplemerch[samplemerch$record==max(samplemerch$record),]$date
  # }
  # else {
  #   days_merchnum[i]=temp$date-date_begin
  # }
  if(is.na(sum(samplecardnum$fraud)/nrow(samplecardnum))){
    card_fraud_r[i]=0
    card_isnew[i]=1
  }
  
  else {
    card_fraud_r[i]= sum(samplecardnum$fraud)/nrow(samplecardnum)
    card_isnew[i]=0
  }
  
  if(is.na(sum(samplemerch$fraud)/nrow(samplemerch))){
    merch_fraud_r[i]=0
    merchnum_isnew[i]=1
  }
  else{
    merch_fraud_r[i]=sum(samplemerch$fraud)/nrow(samplemerch)
    merchnum_isnew[i]=0
  }
  
  # card_copy[1:i-1,]%>%
  #   group_by(cardnum)%>%
  #   summarize(avg_amount = sum(amount)/n()) -> check1
  # if(length(temp$amount/check1[check1$cardnum==temp$cardnum,]$avg_amount)==0) {
  #   norm_amount_card[i] = 1
  # } else {
  #   norm_amount_card[i] = temp$amount/check1[check1$cardnum==temp$cardnum,]$avg_amount
  # }
  # card_copy[1:i-1,]%>%
  #   group_by(merchnum)%>%
  #   summarize(avg_amount = sum(amount)/n()) -> check1
  # if(length(temp$amount/check1[check1$merchnum==temp$merchnum,]$avg_amount)==0) {
  #   norm_amount_merch[i] = 1
  # } else {
  #   norm_amount_merch[i] = temp$amount/check1[check1$merchnum==temp$merchnum,]$avg_amount
  # } 
  
  
}

##added all time count variables and sum_amount in past x days
card_copy%>%
  left_join(c) -> feature_data
##add the days variable
days_since = read.csv("days_data.csv")[,-1]
###add this data to the feature_data
feature_data%>%
  left_join(days_since) -> feature_data

##now add the variables ratio of frauds and is new variable
a = as.data.frame(cbind(record=card_copy$record,card_fraud_r,merch_fraud_r,card_isnew,merchnum_isnew))


##add this to the feature_selection
feature_data%>%
  left_join(a) -> feature_data
#card_isnew=as.data.frame(card_isnew)

###build the tables for form factor

###calculate the avegrage amount transacted on each day of week or day of monht


#days_data = as.data.frame(cbind(record=card_copy$record,days_cardnum,days_merchnum))

##write out this dataset
# write.csv(days_data,"days_data.csv")
# 
# card_copy%>%
#   group_by(cardnum,dayofw)%>%
#   summarize(n())%>%
#   nrow()
# train%>%
#   group_by(cardnum)%>%
#   summarize(n())%>%
#   nrow()
# oot%>%
#   group_by(cardnum)%>%
#   summarize(n())%>%
#   nrow()

##remove the outof time sample
oot = card_copy%>%
  filter(date>=ymd("2010-11-01"))

train = card_copy%>%
  filter(date>=ymd("2010-01-01") & date<=ymd("2010-10-31"))
##remove the frauds from the train dataset to build the form fasctor table
train_nofraud = train%>%
  filter(fraud==0)
#train_nofraud = card_copy
##build the table with each merchant num and DOW combo
##make another for card num and DOW combo with sum of transactions and number of transactions
train_nofraud%>%
  group_by(dayofw)%>%
  summarize(avg_amount_w = sum(amount)/n())-> dayofwFF

train_nofraud%>%
  group_by(dayofm)%>%
  summarize(avg_amount_m = sum(amount)/n())-> dayofmFF




# train_nofraud%>%
#   filter(dayofw=="Mon")%>%
#   filter(cardnum==5142310768)
# nrow(cardnumFF)
# cardnumFF%>%
#   group_by(cardnum)%>%
#   summarize(avgTrans = sum(numTrans)/7,avgAmount = sum(amountTrans)/7)-> join_cardnum
# cardnumFF = left_join(cardnumFF,join_cardnum)
# cardnumFF%>%
#   mutate(numTransFF_c = avgTrans/numTrans,amountTransFF_c = avgAmount/amountTrans) -> cardnumFF
# 
# train_nofraud%>%
#   group_by(merchnum,dayofw)%>%
#   summarize(numTrans=n(),amountTrans = sum(amount))->merchnumFF
# merchnumFF%>%
#   group_by(merchnum)%>%
#   summarize(avgTrans = sum(numTrans)/7,avgAmount = sum(amountTrans)/7)-> join_merchnum
# merchnumFF = left_join(merchnumFF,join_merchnum)
# merchnumFF%>%
#   mutate(numTransFF_m = avgTrans/numTrans,amountTransFF_m = avgAmount/amountTrans) -> merchnumFF

###calculate the average 
# train_nofraud%>%
#   group_by(merchnum)%>%
#   summarize(avgamountTrans_m = sum(amount)/n()) ->avg_merch
# 
# train_nofraud%>%
#   group_by(cardnum)%>%
#   summarize(avgamountTrans_c = sum(amount)/n()) ->avg_card

# days_data=read.csv("days_data.csv")[,-1]
# ##subtract 1 from the variables
# cardVars_30_dt=cardVars_30_dt-1
# cardVars_30_dt$record=cardVars_30_dt$record+1
# merchnumVars_30_dt=merchnumVars_30_dt-1
# merchnumVars_30_dt$record=merchnumVars_30_dt$record+1
# merchdescVars_30_dt=merchdescVars_30_dt-1
# merchdescVars_30_dt$record = merchdescVars_30_dt$record+1
# merchzipVars_30_dt=merchzipVars_30_dt-1
# merchzipVars_30_dt$record=merchzipVars_30_dt$record+1

###now combine all the variables together and remove the older variables which will not be required anymore
# card_copy%>%
#   left_join(cardVars_30_dt)%>%
#   left_join(merchnumVars_30_dt)%>%
#   left_join(merchdescVars_30_dt)%>%
#   #left_join(merchstateVars_30_dt)%>%
#   left_join(merchzipVars_30_dt)%>%
#   left_join(days_data) -> feature_data
# 
# 
# 
# write.csv(feature_data,"to_check.csv")

####there are some card numbers and merch number which are there only for fraud transactions and not for non fraud
############just checking
# check = card_copy[,c("cardnum","merchnum","dayofw")]

# sum(is.na(selected_features$normamountTrans_m))
# train%>%
#   filter(record==80682)
# check=selected_features[is.na(selected_features$numTransFF_c),]
# train%>%
#   group_by(cardnum)%>%
#   summarize(n())%>%
#   nrow()
# train_nofraud%>%
#   group_by(cardnum)%>%
#   summarize(n())%>%
#   nrow()

##done checking##replace the NA values with high numbers to indicate fraud

feature_data%>%
  left_join(dayofmFF)%>%
  left_join(dayofwFF) ->feature_data



sum(is.na(feature_data$avg_amount_w))
feature_data%>%
  mutate(normamountTrans_dow = amount/avg_amount_w)%>%
  mutate(normamountTrans_dom = amount/avg_amount_m) -> feature_data

feature_data%>%
  filter(is.na(normamountTrans_dow))%>%
  filter(fraud=="0")
names(feature_data)
##remove the unwanted variables
##add 2 more sets of variables...risk tables (not using OOT)


# risk_table_data=card_copy%>%
#   filter(date<=ymd("2010-10-31"))
# 
# risk_table_data$fraud=as.numeric(risk_table_data$fraud)
# 
# risk_table_data_c=risk_table_data%>%
#   group_by(cardnum)%>%
#   summarize(cardnum_attrib = sum(fraud)/n())
# 
# risk_table_data_m=risk_table_data%>%
#   group_by(merchnum)%>%
#   summarize(merchnum_attrib = sum(fraud)/n())
# 
# 
# feature_data=left_join(feature_data,risk_table_data_c)
# feature_data=left_join(feature_data,risk_table_data_m)


###remove the unneeded columns

sum(is.na(feature_data$merchnum_attrib))
colnames(feature_data)
temp=feature_data
feature_data=temp
feature_data=feature_data[,-c(2,4:6,9:11,72,73)]

##rearrange the variables
feature_data=feature_data[,c(1,2,4,3,5:66)]

###write out this dataset
write.csv(feature_data,"all variables_3-5-18.csv")
###write out the training and oot sample
oot = feature_data%>%
  filter(date>=ymd("2010-11-01"))

train = feature_data%>%
  filter(date<=ymd("2010-10-31"))
##write these out to a csv
write.csv(oot,"oot_sample_3-5-18.csv")
write.csv(train,"train_sample_3-5-18.csv")


####chaneg teh NA values to 1
#test=feature_data[,100:110]

sum(is.na(feature_data$numTransFF_c))
feature_data%>%
  filter(date>=ymd("2010-11-01"))%>%
  filter(is.na(numTransFF_c))%>%
  filter(fraud=="0")%>%
  nrow()



# temp=feature_data
# 
# max(feature_data$numTransFF_c,na.rm=T)
# feature_data%>%
#   mutate(numTransFF_c = if_else(is.na(numTransFF_c),0,numTransFF_c))%>%
#   mutate(numTransFF_m = if_else(is.na(numTransFF_m),0,numTransFF_m))%>%
#   mutate(amountTransFF_c = if_else(is.na(amountTransFF_c),0,amountTransFF_c))%>%
#   mutate(amountTransFF_m = if_else(is.na(amountTransFF_m),0,amountTransFF_m))%>%
#   mutate(normamountTrans_c = if_else(is.na(normamountTrans_c),0,normamountTrans_c))%>%
#   mutate(normamountTrans_m = if_else(is.na(normamountTrans_m),0,normamountTrans_m)) -> feature_data
  
#sum(is.na(feature_data$numTransFF_c))         
         # mutate(cardnum_attrib = if_else(is.na(cardnum_attrib),0,cardnum_attrib))%>%
  # mutate(merchnum_attrib = if_else(is.na(merchnum_attrib),0,merchnum_attrib)) -> feature_data

names(feature_data)
###write this out into the csv file
write.csv(feature_data,"all_features_2-5-2018.csv")
#features = read.csv("all_features.csv")
#feature_data=features[,-1]
#feature_data$date=as.character(feature_data$date)
#feature_data$date=ymd(feature_data$date)
##remove the card attrib and merch attrib variables
#feature_data=feature_data[,-c(110,111)]

##separate out into train and oot
names(feature_data)
#feature_data=feature_data[,-c(2,4:7,9:10)]
oot = feature_data%>%
  filter(date>=ymd("2010-11-01"))

train = feature_data%>%
  filter(date<=ymd("2010-10-31"))
##write these out to a csv
write.csv(oot,"oot_sample_2-5-18.csv")
write.csv(train,"train_sample_2-5-18.csv")

sum(is.na(oot$card_fraud_r))
##write out the dataset for only time woindow and distinct window counts
# write.csv(oot,"oot.lowvariables.csv")
# write.csv(train,"train.lowvariables.csv")
####check

###days varibales
days_cardnum=vector()
days_merchnum=vector()
#norm_amount_card=vector()
#norm_amount_merch = vector()
card_isfraud=vector()
merch_isfraud=vector()
date_begin=ymd("2010-01-01")
##change the type of fraud
card_copy$fraud=as.numeric(card_copy$fraud)
for (i in 1:nrow(card_copy)){
  print(i)
  temp=card_copy[i,]
  samplecardnum=card_copy[1:i-1,]%>%
    filter(cardnum==temp$cardnum)
  # if(nrow(samplecardnum)>=1){
  #   days_cardnum[i] = temp$date - samplecardnum[samplecardnum$record==max(samplecardnum$record),]$date
  # }
  # else {
  #   days_cardnum[i]=temp$date-date_begin
  # }
  
   
   samplemerch=card_copy[1:i-1,]%>%
     filter(merchnum==temp$merchnum)
  # if(nrow(samplemerch)>=1){
  #   days_merchnum[i] = temp$date - samplemerch[samplemerch$record==max(samplemerch$record),]$date
  # }
  # else {
  #   days_merchnum[i]=temp$date-date_begin
  # }
if(is.na(sum(samplecardnum$fraud)/nrow(samplecardnum))){
  card_isfraud[i]=0
}
   
   else {
     card_isfraud[i]= sum(samplecardnum$fraud)/nrow(samplecardnum)
   }
   
   if(is.na(sum(samplemerch$fraud)/nrow(samplemerch))){
     merch_isfraud[i]=0
   }
   else{
     merch_isfraud[i]=sum(samplemerch$fraud)/nrow(samplemerch)
   }
  
  # card_copy[1:i-1,]%>%
  #   group_by(cardnum)%>%
  #   summarize(avg_amount = sum(amount)/n()) -> check1
  # if(length(temp$amount/check1[check1$cardnum==temp$cardnum,]$avg_amount)==0) {
  #   norm_amount_card[i] = 1
  # } else {
  #   norm_amount_card[i] = temp$amount/check1[check1$cardnum==temp$cardnum,]$avg_amount
  # }
  # card_copy[1:i-1,]%>%
  #   group_by(merchnum)%>%
  #   summarize(avg_amount = sum(amount)/n()) -> check1
  # if(length(temp$amount/check1[check1$merchnum==temp$merchnum,]$avg_amount)==0) {
  #   norm_amount_merch[i] = 1
  # } else {
  #   norm_amount_merch[i] = temp$amount/check1[check1$merchnum==temp$merchnum,]$avg_amount
  # } 
  
    
}



card_isfraud=as.data.frame(card_isfraud)
card_copy[1:5000,]%>%
  filter(fraud==1)%>%
  nrow()

norm_amount_merch
temp=card_copy[17,]
card_copy[1:16,]%>%
  group_by(cardnum)%>%
  summarize(avg_amount = sum(amount)/n()) -> check1
  if(length(temp$amount/check1[check1$cardnum==temp$cardnum,]$avg_amount)==0) {
  norm_amount_card = 1
  } else {
  norm_amount_card = temp$amount/check1[check1$cardnum==temp$cardnum,]$avg_amount
  }

tail(days_cardnum)
card_copy%>%
  filter(record==7381)

card_copy%>%
  filter(date<=ymd("2010-01-31"))%>%
  nrow()

