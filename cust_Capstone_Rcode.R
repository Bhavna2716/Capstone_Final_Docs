#load packages 
library(tidyr)
library(dplyr)
library(ggplot2)
#DATA WRANGLING
#Load and examin the dataset 
cust.df <- read.csv("http://goo.gl/PmPkaG")
str(cust.df) 
glimpse(cust.df)

#find columns with missing values and zeros 
colSums(is.na(cust.df))
any.zeros <- lapply(cust.df, function(x){length(which(x==0))}) 
any.zeros  
#replace missing satisfaction service/selection values with mean
cust.df$sat.service[is.na(cust.df$sat.service)] <- round(mean(cust.df$sat.selection,na.rm = TRUE))
count(cust.df,sat.service)
cust.df$sat.selection[is.na(cust.df$sat.selection)] <- round(mean(cust.df$sat.selection,na.rm = TRUE))
count(cust.df,sat.selection)

#change email into a binary variable 
count(cust.df,email)
cust.df<- cust.df %>% mutate(email = ifelse(email == "yes",1,0)) 
str(cust.df) 
#Make two new columns; 'total.spend' that contains sum of online and store spending,
#total.trans that is the sum of online.trans and store.trans
cust.df<- cust.df %>% mutate(total.spend = (online.spend + store.spend)) %>% 
                      mutate(total.trans = (online.trans+store.trans)) %>% 
                      mutate(total.spend = round(total.spend,1))
cust.df <- round(cust.df,0)
str(cust.df)
levels<- c(-Inf,0,50,250,500,4000) 
labels <-c("Prospects","Regular","Premium","VIP","VVIP")
cust.df <- cust.df %>% mutate(cust.category = cut(total.spend,levels,labels = labels))
count(cust.df,cust.category)
#Identify the customers as per their categories by selecting cust.id for each group 
Target_Prospects <- cust.df[cust.df$cust.category == "Prospects","cust.id"] 
Target_Regular <- cust.df[cust.df$cust.category == "Regular", "cust.id"] 
Target_Premium <- cust.df[cust.df$cust.category == "Premium", "cust.id"]
Target_VIP <- cust.df[cust.df$cust.category == "VIP", "cust.id"]
Target_VVIP <- cust.df[cust.df$cust.category == "VVIP", "cust.id"]

#Map the percentage of every customer category for each satisfaction ratings. 
service_dist_Prospects <- cust.df[cust.df$cust.category == "Prospects", "sat.service"]
service_dist_Prospects
prop.table(table(service_dist_Prospects))
service_dist_Regular <- cust.df[cust.df$cust.category == "Regular","sat.service"]
service_dist_Regular
prop.table(table(service_dist_2))
service_dist_Premium <- cust.df[cust.df$cust.category == "Premium","sat.service"]
service_dist_Premium
prop.table(table(service_dist_3))
service_dist_VIP <- cust.df[cust.df$cust.category == "VIP","sat.service"]
service_dist_VIP
prop.table(table(service_dist_4))
service_dist_VVIP <- cust.df[cust.df$cust.category == "VVIP","sat.service"]
service_dist_VVIP
prop.table(table(service_dist_Prospects))
prop.table(table(service_dist_Regular))
prop.table(table(service_dist_Premium))
prop.table(table(service_dist_VIP))
prop.table(table(service_dist_VVIP)) 

#VISUALIZATION AND STATISTICAL ANALYSIS  

#Find mean value of columns and compare dependent variables.  
colMeans(cust.df[sapply(cust.df,is.numeric)], na.rm = TRUE)
#load packages 
library(psych)
library(ggplot2)
library(scales)
library(corrplot)    
library(gplots)
library(ggmap)
# overview of co-relation between variables 
pairs(cust.df[ , c(2:10)])
cor.test(cust.df$age, cust.df$credit.score)
describe(cust.df)

#Plot the distribution of customers as per categories 
count(cust.df$cust.category) #see the pattern 
cust.catgr <- ggplot(cust.df, aes(x= cust.category, fill = cust.category)) + geom_bar()
cust.catgr

#Plot a points of total.spend on total.trans and color on cust.category 
count(cust.df$online.trans) #see the distribution pattern 
cust.df.trans <- ggplot(cust.df, aes(x = total.trans,y = total.spend, color = cust.category))+
  geom_point(size = 2)  
cust.df.trans
#Online trasection and spend distribution 
cust.trans.onln <- ggplot(cust.df, aes(x = online.trans,y = online.spend)) +
  geom_point(size = 2, color = "blue")  
cust.trans.onln
#In-store trasection and spend distribution 
cust.trans.str <- ggplot(cust.df, aes(x = store.trans,y = store.spend))+ geom_point(size = 2, color = "red")  
cust.trans.str

# Plot the association between online.spend and store.spend
cust.spend.plot <- ggplot(cust.df, aes(x = online.spend, y = store.spend)) + 
                  geom_point(size = 2)
cust.spend.plot

# Association between online and store trans. 
cust.ttrans <- ggplot(cust.df, aes(x = online.trans, y = store.trans)) +
                         geom_point(size = 2)  
cust.ttrans
count(cust.df,online.trans)#see the pattern 
count(cust.df,store.trans)#see the pattern 

# How do age affects the purchase? 
#plot age on various transections 
cust.age.otrans <- ggplot(cust.df, aes(x = age,y = online.trans))+ geom_point() 
cust.age.otrans
cust.age.strans <- ggplot(cust.df, aes(x = age,y = store.trans))+ geom_point() 
cust.age.strans
cust.age.ttrans <- ggplot(cust.df, aes(x = age, y = total.trans)) + geom_point()
cust.age.ttrans
#Draw plots representing association between online.spend/store.spend with total.spend
count(cust.df,sat.selection)# see the distribution 
cust.df.prchs.onln <- ggplot(cust.df, aes(x = online.spend, y = total.spend,color = cust.category))+ 
  geom_point()+geom_smooth(method = "lm")
cust.df.prchs.onln
cust.df.prchs.str <- ggplot(cust.df, aes(x = store.spend, y = total.spend,color = cust.category))+ 
  geom_point()
cust.df.prchs.str 

#Draw a bar graph that shows distribution of satifaction service and selction ratings. 
cust.sat.service <- ggplot(cust.df, aes(x = sat.service, fill = cust.category)) + 
                      geom_bar()
cust.sat.service
count(cust.df$sat.service)
cust.sat.selection <- ggplot(cust.df, aes(x = sat.selection, fill = cust.category)) + 
                        geom_bar()
cust.sat.selection
count(cust.df,sat.selection)
#Draw a plot that shows how satisfaction ratings affect total.spend 
cust.sat.tspend<- ggplot(cust.df,aes(x = sat.service, y = total.spend, color = cust.category)) + 
  geom_jitter() 
cust.sat.tspend
#=====
#Draw a plot to see if number of online visits directly translate into online transections 
cust.onvst.trans<- ggplot(cust.df,aes(x =online.visits, y = online.trans, col = online.spend)) +
                      geom_point()
cust.onvst.trans
-----
  
#How satisfaction ratings affect online and store purchases 
#Online Purchases 
cust.sat.onln <- ggplot(cust.df, aes(x = sat.service, y = sat.selection, color = online.spend)) + 
                    geom_jitter()
cust.sat.onln
#In-store Purchases  
cust.sat.str <- ggplot(cust.df, aes(x = sat.service, y = sat.selection, color = store.spend)) + 
                      geom_jitter()
cust.sat.str

cust.age.cc<- ggplot(cust.df,aes(x = age ,y = credit.score))+ 
                       geom_point(size = 3, alpha = 0.5,position_dodge(width = 0.2))
cust.age.cc
# are distance to store and store.trans related ? 
cust.dstnc.str<- ggplot(cust.df, aes(x= distance.to.store, y = store.trans, color = store.spend))+
                   geom_point(size = 2, position = "dodge")
cust.dstnc.str

#Online trasection
cust.onln.trans <- ggplot(cust.df, aes(x= distance.to.store, y = online.trans, color = online.spend))+
                    geom_point(size = 2, position = "dodge")
cust.onln.trans

# Online spend mapped on satisfection ratings
cust.sat.ospend <- ggplot(cust.df , aes(x = sat.service, y = online.spend)) + 
                    geom_point(shape = 21, size = 4, alpha = 0.6)
cust.sat.ospend

#Find association between online visits and the money customers spend online? 

cust.df.sat <- ggplot(cust.df,aes(x = online.visits, y = online.spend)) + geom_point()
                   
#Plot to find corelation between credit.score and total.spend  

cust.crdt.spend <- ggplot(cust.df, aes(x = credit.score, y = total.spend)) + geom_point()
cust.crdt.spend
#Do credit score get better with age  
cust.crdt.age <- ggplot(cust.df, aes( x = age, y = credit.score)) + geom_point(aes(color = online.spend)) + 
                   lims(color = c(0,1000))# limits set to get a closure look at the graph 
cust.crdt.age
# How distance to store affect the number of purchases buy a customer 
cust.dis.store <- ggplot(cust.df, aes(x = distance.to.store, y = store.spend)) + 
                    geom_point(size = 2,alpha = 0.7)
cust.dis.store
#Distance to store and online sales 
cust.dis.onln <- ggplot(cust.df, aes(x = distance.to.store, y = online.trans)) + 
                          geom_point(size = 2,alpha = 0.7) 
cust.dis.onln 

cust.tsale.ctgr <- ggplot(cust.df,aes(x = total.trans, y = total.spend, color = cust.category)) + 
                     geom_point()
cust.tsale.ctgr

#visualize corelation between sat.service and total transection/total spend  
cust.sat.srvc <- ggplot(cust.df, aes(x = sat.service, y = total.spend)) + ylim(0,1000) +
                    geom_point(alpha = 0.2) + geom_smooth(aes(col = credit.score), se = FALSE)  
cust.sat.srvc
cust.srvc.trns<- ggplot(cust.df, aes(x = sat.service, y = total.trans)) + 
                    geom_point(alpha = 0.2)
cust.srvc.trns

cust.onlnspend.sel <- ggplot(cust.df, aes(x = sat.selection, y = online.spend)) + 
                            ylim(0,1000) + geom_point(alpha = 0.2) 
                     
cust.onlnspend.sel

cust.df.sat1 <- ggplot (cust.df, aes(x = sat.selection)) + geom_histogram(binwidth = 1)
cust.df.tsat2 <- ggplot (cust.df, aes(x = sat.service)) + geom_histogram(binwidth = 1)
#what percentage of people who have given sat rating have email? 
cust.df.tsat <- ggplot(cust.df, aes(x = sat.service, col = email)) +
                     geom_freqpoly(binwidth = 1,position = "identity") 
cust.df.tsat
#========
#Prediction Model 
#===========
#Linear regression
#Build linear regression models to find the most suitable prediction model for maximum sale i.e, total.spend
cust.tsale.1 <- lm(total.spend ~ online.visits, data = cust.df)
summary(cust.mod.1)
cust.tsale.2 <- lm(total.spend ~ online.visits + email + age, data = cust.df)
summary(cust.mod.2)
coef(cust.sale.2)
cust.df$credit.score <- as.factor(cust.df$credit.score)
cust.tsale.3 <- lm(total.spend ~ online.visits + online.trans + store.spend, data = cust.df)
summary(cust.sale.3)
coef(cust.sale.3)
#try and find co-relation between key variables
#transections and total.spend 
cust.ttrans.visits <- lm(total.spend ~ online.trans + store.trans, data = cust.df)
summary(cust.ttrans.visits) 
coef(cust.ttrans.visits)
#online.transection 
cust.onln.trans <- lm(online.trans ~ online.visits, data = cust.df)
summary(cust.onln.trans)
coef(cust.onln.trans)

