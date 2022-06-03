#loading the file in R enviroment
tele<-read.csv('Comcast Telecom Complaints data.csv')
View(tele)
str(tele)
names(tele)
sum(is.na(tele))

#converting the required variables as factor
tele$Received.Via=as.factor(tele$Received.Via)
tele$City=as.factor(tele$City)
tele$State=as.factor(tele$State)
tele$Status=as.factor(tele$Status)
tele$Filing.on.Behalf.of.Someone=as.factor(tele$Filing.on.Behalf.of.Someone)

#converting the date column to in date format
library(lubridate)
tele$Date<-dmy(tele$Date)

#Creating the chart for the number of complaints at monthly and daily granularity levels.
library(dplyr)
monthly_data<-summarise(group_by(tele,month=as.integer(month(Date))),Count=n())
monthly_data

daily_data<-summarise(group_by(tele,Date),Count=n())
daily_data

library(ggplot2)

ggplot(monthly_data,aes(x=month,y=Count,label=Count)) + geom_line(colour='blue',linetype=1)+
  geom_text()+
  scale_x_continuous(breaks = monthly_data$month)+
  labs(title = "No. of Complains received on Monthly Basis",x='Months',y="No of Tickets raised")

 ggplot(daily_data,aes(x=as.POSIXct(Date),y=Count))+geom_line()+
   theme(axis.text.x  =element_text(angle=90) )+
   scale_x_datetime(breaks = "1 weeks",date_labels='%d/%m')+
   labs(title ='No. of Complains received on Daily Basis',x='Days',y="No of Tickets raised" )

  #Provide a table with the frequency of complaint types.
 #Which complaint types are maximum i.e., around internet, network issues, or across any other domains
 library(tidyverse)
 network_tickets<-contains(tele$Customer.Complaint,match='network',ignore.case = T)
 internet_tickets<-contains(tele$Customer.Complaint,match ='internet',ignore.case = T)
 bill_tickets<-contains(tele$Customer.Complaint,match='bill',ignore.case = T)
 email_tickets<-contains(tele$Customer.Complaint,match="email",ignore.case = T)
 charge_tickets<-contains(tele$Customer.Complaint,match='charge',ignore.case = T)

 tele$ComplaintType[network_tickets]<-'Network'
 tele$ComplaintType[internet_tickets]<-'Internet'
 tele$ComplaintType[bill_tickets]<-'Billing'
 tele$ComplaintType[email_tickets]<-'Email'
 tele$ComplaintType[charge_tickets]<-'Charges'
 tele$ComplaintType[-c(network_tickets,internet_tickets,bill_tickets,email_tickets,charge_tickets)]<-"Others"

                               
 View(tele)
 table(tele$ComplaintType)
 
 
 #creating a new column for  Open & Pending as Open and Closed & Solved as Closed.
 
tele$Complain_Status<-ifelse(tele$Status == "Open"| tele$Status =="Pending",'Open','Closed')

#creating the stacked bar for the no of complains received on the basis of cities

Complain<-summarise(group_by(tele,State,Complain_Status),Count=n())
ggplot(Complain,aes(x=State,y=Count,fill=Complain_Status))+geom_col()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title ='No. of Complains received on State Basis',x='States',y="No of Tickets raised" )

#Calculating the State with highest unclosed_data cases

Complain %>% filter(Complain_Status=='Open')%>%arrange(desc(Count)) %>% head(1)

#Calculating the percentage  of complaints closed_data till date, 
#which were received through the Internet and customer care calls.
resolved_data <- group_by(tele,Complain_Status)
total_resloved<- summarise(resolved_data ,percentage =(n()/nrow(resolved_data))*100) 
total_resloved
resolved_data1 <- group_by(tele,Received.Via,Complain_Status)
Category_resloved<- summarise(resolved_data1 ,percentage =(n()/nrow(resolved_data1))*100) 
Category_resloved
# Ploting Pie Chart for Total Resolved Vs Category Resolved

total<-ggplot(total_resloved,
              aes(x= "",y =percentage,fill = Complain_Status))+
  geom_bar(stat = "identity",width = 1)+
  coord_polar("y",start = 0)+
  geom_text(aes(label = paste0(round(percentage),"%")),
            position = position_stack(vjust = 0.5))
  
total
# Pie Chart for Category wise Ticket Status
category<-ggplot(Category_resloved,
                 aes(x= "",y =percentage,fill = Received.Via))+
  geom_bar(stat = "identity",width = 1)+
  coord_polar("y")+
  geom_text(aes(label = paste0(Complain_Status,"-",round(percentage),"%")),
            position = position_stack(vjust = 0.5))
category
