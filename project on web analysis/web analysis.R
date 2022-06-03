library(readxl)
web<-read_xlsx('1555058318_internet_dataset.xlsx')
str(web)

#converting the char value to categorical value
web$Continent<-as.factor(web$Continent)
web$Sourcegroup<-as.factor(web$Sourcegroup)

#summary of the data to get a basic understanding of the dataset and to prepare for further analysis.
summary(web)

#checking whether there is a relation between uniquepageviews and Visits
cor(web$Uniquepageviews,web$Visits)
ano<-aov(Uniquepageviews~Visits,data=web)
summary(ano)

#checking the factors thats affect the Exits
anoe<-aov(Exits~.,data=web)
summary(anoe)


#checking the factors that affects the timeinpage on the website
anot<-aov(Timeinpage~.,data=web)
summary(anot)

#checking the factors thats affect the Bounce
#data value should be between 0 to 1 so using BounsNew variable
logb<-glm(BouncesNew~Timeinpage+Continent+Sourcegroup+Uniquepageviews+Visits,data = web,family = "binomial")
summary(logb)
