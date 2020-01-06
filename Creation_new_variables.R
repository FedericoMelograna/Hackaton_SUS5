train <- read.csv("C:/Users/federico/Desktop/Competizione/train.csv",stringsAsFactors = F)
test <- read.csv("C:/Users/federico/Desktop/Competizione/test.csv",stringsAsFactors = F)


# Libreries ----------------------------------------------------------------


library(fasttime)
library(lubridate)
library(rpart)
library(plyr)
library(dplyr)

library(dplyr)


# SEED --------------------------------------------------------------------


set.seed(123)
n<-nrow(train)
indici=sample(1:n,0.1*n,replace=F)



# Merging Data ----------------------------------------------------------


train=train %>% select(-"weight")
test$Target_cost_euro<-NA
totale=rbind(train,test)

str(train)



# Temporal Variables  ------------------------------------------------------------------


#(totale$hour=hour(fastPOSIXct(totale$Date_of_accident,"GMT")) )
(totale$monthaccident=month(fastPOSIXct(totale$Date_of_accident,"GMT")) )[1:5]
totale$Date_of_accident[1:5]
(totale$yearaccident=year(fastPOSIXct(totale$Date_of_accident,"GMT")) )[1:5]
totale$Date_of_accident[1:5]
(totale$wdayaccident=wday(fastPOSIXct(totale$Date_of_accident,"GMT")) )[1:5]
totale$Date_of_accident[1:5]
(totale$dayaccident=day(fastPOSIXct(totale$Date_of_accident,"GMT")) )[1:5]




(totale$monthclaim=month(fastPOSIXct(totale$Date_claim_opening,"GMT")) )[1:5]
totale$Date_claim_opening[1:5]
(totale$yearclaim=year(fastPOSIXct(totale$Date_claim_opening,"GMT")) )[1:5]
totale$Date_claim_opening[1:5]
(totale$wdayclaim=wday(fastPOSIXct(totale$Date_claim_opening,"GMT")) )[1:5]
totale$Date_claim_opening[1:5]
(totale$dayclaim=day(fastPOSIXct(totale$Date_claim_opening,"GMT")) )[1:5]
#totale<-totale%>%select(-c("dayclaim","wdayclaim", "monthclaim","yearclaim"))


(totale$annimmatricola=year(fastPOSIXct(totale$Date_vehicle_immatriculation,"GMT")) )[1:5]


date_acc = totale$Date_of_accident
date_cla = totale$Date_claim_opening
dateacc2 = strptime(date_acc, format = "%Y-%m-%d") # convert to datetime objects
datecla2 = strptime(date_cla, format = "%Y-%m-%d") # convert to datetime objects
# dateacc2[160]
# datecla2[160]
# Difference in Days
# datetimes[1]

# You can use the diff in days to get some of our later answers
# str(datetimes)
# diff_in_days = difftime(datetimes[2], datetimes[1], units = "days") # days
diffday=vector()
for (i in 1:nrow(totale)){
  diffday[i]=difftime(datecla2[i], dateacc2[i], units = "days")
  
}
# diffday[1:30]
# round(diffday)[1:30]
diffday=round(diffday)

totale$diffday=diffday

# Bareme's Table ---------------------------------------------------------


library(readxl)
Bareme <- read_excel("C:/Users/federico/Desktop/Competizione/Baréme.xlsx",col_names = FALSE)
Bareme<-as.data.frame(Bareme)
# View(Bareme)
ss=seq(17,0,by=-1)
ss2=seq(0,17,by=1)
rownames(Bareme)<-c(ss,"AA")
colnames(Bareme)<-c("BB",ss2)
# Bareme
Baremefin<-Bareme[c(1:18),c(2:19)]
# Baremefin


summary(totale$Bareme_table_code_customer)

str(totale$Bareme_table_code_customer)
comodo=vector()

for (i in 1:nrow(totale)){
  comodo[i]<-Baremefin[paste0(totale$Bareme_table_code_customer[i]),paste0(totale$Bareme_table_code_other_driver[i])]
  
  
}

# comodo<-Baremefin[paste0(totale$Bareme_table_code_customer[2]),paste0(totale$Bareme_table_code_other_driver[2])]
comodo
table(comodo)
# totale$Bareme_table_code_customer[2]
# totale$Bareme_table_code_other_driver[2]
table(totale$Bareme_table_code_customer)
table(totale$Bareme_table_code_other_driver)

table(totale$Bareme_table_code_customer,totale$Bareme_table_code_other_driver)

totale$TableBarcompl<-as.factor(comodo)

aa<-totale[totale$TableBarcompl=="R","Target_cost_euro"]
summary(aa)
bb<-totale[totale$TableBarcompl=="C","Target_cost_euro"]
summary(bb)
cc<-totale[totale$TableBarcompl=="T","Target_cost_euro"]
summary(cc)

## We obtain that just table T is significant


# GRAPHS --------------------------------------------------------------


library(ggplot2)
ggplot(data=totale[!is.na(totale$Target_cost_euro),], aes(x=factor(Bareme_table_code_customer), y=Target_cost_euro))+
  geom_boxplot(col='blue') + labs(x='Overall.Qual') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000))


ggplot(data=totale[!is.na(totale$Target_cost_euro),], aes(x=factor(Bareme_table_code_customer), y=log(Target_cost_euro)))+
  geom_boxplot(col='blue') + labs(x='Overall.Qual') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000))


ggplot(data=totale[!is.na(totale$Target_cost_euro),], aes(x=factor(Bareme_table_code_other_driver), y=Target_cost_euro))+
  geom_boxplot(col='blue') + labs(x='Overall.Qual') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000))


ggplot(data=totale[!is.na(totale$Target_cost_euro),], aes(x=factor(Bareme_table_code_other_driver), y=log(Target_cost_euro)))+
  geom_boxplot(col='blue') + labs(x='Overall.Qual')



ggplot(data=totale[!is.na(totale$Target_cost_euro),], aes(x=factor(Customer_merit_class), y=log(Target_cost_euro)))+
  geom_boxplot(col='blue') + labs(x='Overall.Qual')


ggplot(data=totale[!is.na(totale$Target_cost_euro),], aes(x=factor(Customer_merit_class), y=Target_cost_euro))+
  geom_boxplot(col='blue') + labs(x='Overall.Qual')




# Region pericolosity --------------------------------------------------

table(totale$Region)
regioni$Regione[2]<-"VALLE D'AOSTA"
regioni$Regione[5]<-"TRENTINO-ALTO ADIGE"
regioni$Regione<-toupper(regioni$Regione)
regioni$Regione
x<-vector()
for (el in regioni$Regione){
  if (el %in% totale$Region){
    
    
  }
}
table(unique(regioni$Regione),unique(totale$Region))
a<-unique(regioni$Regione)
b<-unique(totale$Region)
a %in% b
b %in% a 
regioni[totale$Region[1]==regioni$Regione,2]

incidentirate=vector()
for (i in 1:nrow(totale)){
  incidentirate[i]=regioni[totale$Region[i] ==regioni$Regione,2][[1]]
  
}




totale$Regioneindiceincidenti<-incidentirate


# PROVINCIA ---------------------------------------------------------------

province
ind<-which(is.na(totale$Province_code)==TRUE)
totale[ind,"Province_code"]<-"NA"
temp=unique(province$Province)
tempaa=unique(totale$Province_code)
sum(is.na(totale$Province_code))
"NA" %in% tempaa
temp %in% tempaa
temp[34]
(tempaa %in% temp)

incprov=data.frame()
for (i in 1:nrow(totale)){
  incprov[i,1]<-province[totale$Province_code[i] ==province$Province,2][[1]]
  incprov[i,2]<-province[totale$Province_code[i] ==province$Province,3][[1]]
  
}
head(incprov)

(totale$IndiceIndProv<-incprov[,1])
totale$RankrischioProv<-incprov[,2]


###############