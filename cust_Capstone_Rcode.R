#load packages 
library(tidyr)
library(dplyr)
library(ggplot2)
inst
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
count(cust.df,sat.service) #check for the change 
cust.df$sat.selection[is.na(cust.df$sat.selection)] <- round(mean(cust.df$sat.selection,na.rm = TRUE))
count(cust.df,sat.selection) #check for the change 

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
str(cust.df) #examine the change 
#returning customer 
cust.df <- cust.df %>% mutate(Returning_cust = total.trans>1)
count(cust.df,Returning_cust)
online_return <- cust.df[cust.df$online.trans >1, "cust.id"]
online_return
store_return <- cust.df[cust.df$store.trans >1, "cust.id"]
store_return
online_trans1 <- cust.df[cust.df$online.trans>0, "cust.id"]
online_trans1 #(total = 633, return = 515) 
store_return1<- cust.df[cust.df$store.trans>0, "cust.id"]
store_return #(total = 620, return = 341) 
#online_return% = 85% 
#store_return% = 55%  
#Divide customers into category based on total.spend
levels<- c(-Inf,0,50,250,500,4000) 
labels <-c("Prospects","Regular","Premium","VIP","VVIP")
cust.df <- cust.df %>% mutate(cust.category = cut(total.spend,levels,labels = labels))
count(cust.df,cust.category)#examine the change

cust.df <- cust.df %>% mutate(service_distr = )
#Identify the customers as per their categories by selecting cust.id for each group 
Target_Prospects <- cust.df[cust.df$cust.category == "Prospects","cust.id"] 
Target_Regular <- cust.df[cust.df$cust.category == "Regular", "cust.id"] 
Target_Premium <- cust.df[cust.df$cust.category == "Premium", "cust.id"]
Target_VIP <- cust.df[cust.df$cust.category == "VIP", "cust.id"]
Target_VVIP <- cust.df[cust.df$cust.category == "VVIP", "cust.id"]

#Statistical Analysis 

#SERVICE SATISFACTION
cust.df <- cust.df %>% mutate(service_dist_category =  )
  
#Map the percentage of every customer category for distinct satisfaction service ratings. 
service_dist_Prospects <- cust.df[cust.df$cust.category == "Prospects", "sat.service"]
service_dist_Prospects
service_dist_Regular <- cust.df[cust.df$cust.category == "Regular","sat.service"]
service_dist_Regular
service_dist_Premium <- cust.df[cust.df$cust.category == "Premium","sat.service"]
service_dist_Premium
service_dist_VIP <- cust.df[cust.df$cust.category == "VIP","sat.service"]
service_dist_VIP
service_dist_VVIP <- cust.df[cust.df$cust.category == "VVIP","sat.service"]
service_dist_VVIP

#find percetage wise distribution of sat.service for each category 
prop.table(table(service_dist_Prospects))
prop.table(table(service_dist_Regular))
prop.table(table(service_dist_Premium))
prop.table(table(service_dist_VIP))
prop.table(table(service_dist_VVIP)) 
#SELECTION SATISFACTION

#Find the customer category distribution for satisfaction selection ratings.  
selection_dist_Prospects <- cust.df[cust.df$cust.category == "Prospects","sat.selection"]
selection_dist_Prospects
selection_dist_Regular <- cust.df[cust.df$cust.category == "Regular","sat.selection"]
selection_dist_Regular
selection_dist_Premium <- cust.df[cust.df$cust.category == "Premium","sat.selection"]
selection_dist_Premium
selection_dist_VIP <- cust.df[cust.df$cust.category == "VIP","sat.selection"]
selection_dist_VIP
selection_dist_VVIP <- cust.df[cust.df$cust.category == "VVIP","sat.selection"]
selection_dist_VVIP

#find percetage wise distribution of sat.seLECTION for each category 
prop.table(table(selection_dist_Prospects))
prop.table(table(selection_dist_Regular))
prop.table(table(selection_dist_Premium))
prop.table(table(selection_dist_VIP))
prop.table(table(selection_dist_VVIP)) 

#VISUALIZATION AND DETAILED STATISTICAL ANALYSIS  

#Find mean value of columns and compare dependent variables.  
colMeans(cust.df[sapply(cust.df,is.numeric)], na.rm = TRUE)
#load packages 
library(psych) 
library(ggplot2)
library(scales)
library(corrplot)    
library(ggmap)
# overview of co-relation between variables 
pairs(cust.df[ , c(2:12)])
describe(cust.df)

### Returning customer 
cust.returning <- ggplot(cust.df, aes (x = cust.id, fill = Returning_cust)) + geom_histogram(binwidth = 20) +
                coord_polar(theta = fill)
cust.returning 
### CUSTOMER CATEGORIES 
#*Plot the distribution of customers as per categories 
count(cust.df,cust.category) #see the number pattern 
cust.catgr <- ggplot(cust.df, aes(x= cust.category, fill = cust.category)) + geom_bar()
cust.catgr

#TRANSACTION AND SPENDING

#Plot a graph of total.spend on total.trans and color on cust.category 
count(cust.df,total.trans) #see the distribution pattern 
cust.total.trans <- ggplot(cust.df, aes(x = total.trans,y = total.spend, color = cust.category))+
                   geom_point(size = 2)  
cust.total.trans
#*Online trasection and spend distribution 
cust.trans.onln <- ggplot(cust.df, aes(x = online.trans,y = online.spend)) +
                   geom_point(size = 2, color = "blue")  
cust.trans.onln 
#*In-store trasection and spend distribution 
cust.trans.str <- ggplot(cust.df, aes(x = store.trans,y = store.spend))+ geom_point(size = 2, color = "red")  
cust.trans.str
# Online spend mapped on satisfection ratings

#Draw a plot to see if number of online visits directly translate into online transections 
cust.onvst.trans<- ggplot(cust.df,aes(x =online.visits, y = online.trans, col = online.spend)) +
                   geom_point()
cust.onvst.trans
#Draw plots representing association between online.spend/store.spend with total.spend

cust.prchs.onln <- ggplot(cust.df, aes(x = online.spend, y = total.spend,color = cust.category))+ 
                   geom_point()+geom_smooth(method = "lm")
cust.prchs.onln
cust.prchs.str <- ggplot(cust.df, aes(x = store.spend, y = total.spend,color = cust.category))+ 
                  geom_point()
cust.prchs.str 
#total.trans and spend according to cust.category 
cust.tsale.ctgr <- ggplot(cust.df,aes(x = total.trans, y = total.spend, color = cust.category)) + 
                   geom_point()
cust.tsale.ctgr
# Plot the association between online.spend and store.spend
cust.spend.plot <- ggplot(cust.df, aes(x = online.spend, y = store.spend, color = cust.category)) + 
                  geom_point(size = 2)
cust.spend.plot 
# Association between online and store transections  
cust.ttrans <- ggplot(cust.df, aes(x = online.trans, y = store.trans, color = cust.category)) +
               geom_point(size = 2)  
cust.ttrans
count(cust.df,online.trans)#see the pattern 
count(cust.df,store.trans)#see the pattern  
#======
###AGE
#======
#Does age affect number of transections? 
cor.test(cust.df$age, cust.df$credit.score)
cust.age.bar <- ggplot(cust.df, aes(x = age, fill = cust.category)) + geom_bar()
cust.age.bar
#*plot age on various transections 
cust.age.otrans <- ggplot(cust.df, aes(x = age,y = online.trans, color = cust.category))+ geom_point() 
cust.age.otrans
cust.age.strans <- ggplot(cust.df, aes(x = age,y = store.trans, color = cust.category))+ geom_point() 
cust.age.strans
cust.age.ttrans <- ggplot(cust.df, aes(x = age, y = total.trans, color = cust.category)) + geom_point()
cust.age.ttrans
# plot to see a co-relation between age and credit 
cust.age.cc<- ggplot(cust.df,aes(x =age, y = credit.score)) +
              geom_point(color = "blue")
cust.age.cc

### DISTANCE TO STORE AND TRANSACTIONS 
cust.dstnc.total <- ggplot(cust.df, aes(x= distance.to.store, y = total.trans))+
                     geom_point(size = 2, position = "dodge")
cust.dstnc.total

# are distance to store and store.trans related ? 
cust.dstnc.str<- ggplot(cust.df, aes(x= distance.to.store, y = store.trans))+
                     geom_point(size = 2, position = "dodge", color = "red")
cust.dstnc.str

#Online trasaction mapped to online variables 
cust.dstnc.onln <- ggplot(cust.df, aes(x= distance.to.store, y = online.trans))+
                   geom_point(size = 2, position = "dodge",color = "blue")
cust.dstnc.onln
 


# How distance to store affect the number of purchases buy a customer 
cust.dist.store <- ggplot(cust.df, aes(x = distance.to.store, y = store.spend)) + 
                   geom_point(size = 2,alpha = 0.7)
cust.dist.store

#Distance to store and online sales 
cust.dist.onln <- ggplot(cust.df, aes(x = distance.to.store, y = online.trans)) + 
                  geom_point(size = 2,alpha = 0.7) 
cust.dist.onln 


### SATISFACTION SERVICE AND SELECTION 

#**Draw a bar graph that shows distribution of satifaction service and selection ratings. 
#Service
cust.sat.service <- ggplot(cust.df, aes(x = sat.service, fill = cust.category)) + 
                      geom_bar()
#Selection
count(cust.df,sat.selection)# see the distribution 
cust.sat.service <- ggplot(cust.df, aes(x = sat.service, fill = cust.category)) + geom_bar()
cust.sat.service
count(cust.df$sat.selection)
cust.sat.selection <- ggplot(cust.df, aes(x = sat.selection, fill = cust.category)) + 
                        geom_bar()
cust.sat.selection 
count(cust.df,sat.selection)
#**Draw a plot that show how both satisfaction ratings affect total.spend 
cust.service.tspend<- ggplot(cust.df,aes(x = sat.service, y = total.spend, color = cust.category)) + 
                    geom_jitter() 
cust.service.tspend
cust.selection.tspend<- ggplot(cust.df,aes(x = sat.selection, y = total.spend, color = cust.category)) + 
                      geom_jitter() 
cust.selection.tspend
#visualize correlation between sat.service and total transaction/total spend  
cust.sat.srvc <- ggplot(cust.df, aes(x = sat.service, y = total.spend)) + ylim(0,1000) +
                 geom_point(alpha = 0.2) + geom_smooth(aes(col = credit.score), se = FALSE)  
cust.sat.srvc
cust.srvc.trans<- ggplot(cust.df, aes(x = sat.service, y = total.trans)) + 
                  geom_point(alpha = 0.2)
cust.srvc.trans
cust.sat.ospend <- ggplot(cust.df , aes(x = sat.service, y = online.spend)) + 
  geom_point(size = 4, alpha = 0.6)
cust.sat.ospend
cust.onlnspend.sel <- ggplot(cust.df, aes(x = sat.selection, y = online.spend)) + 
                     ylim(0,1000) + geom_point(alpha = 0.2) 
cust.onlnspend.sel

#Draw bar graph for sat.slection and sat.service 
cust.df.sat1 <- ggplot (cust.df, aes(x = sat.selection)) + geom_histogram(binwidth = 1)
cust.sat.selection
cust.df.tsat2 <- ggplot (cust.df, aes(x = sat.service)) + geom_histogram(binwidth = 1)
cust.df.tsat2
#Distribution of service and selection rating categorized by customer type 
catgory.service <- ggplot(cust.df, aes(x = cust.category, y = sat.service, color = cust.category)) +
                  geom_jitter()
catgory.service             
catgory.selection<- ggplot(cust.df, aes(x = cust.category, y = sat.selection, color = cust.category)) +
                 geom_jitter()
catgory.selection 

#How satisfaction ratings affect online and store purchases 
#Online Purchases 
cust.sat.onln <- ggplot(cust.df, aes(x = sat.service, y = sat.selection, color = online.spend)) + 
                    geom_jitter() 
cust.sat.onln

#In-store Purchases  
cust.sat.str <- ggplot(cust.df, aes(x = sat.service, color = store.spend)) + 
                geom_jitter()
cust.sat.str

#Find association between online visits and the money customers spend online? 
cust.ovisit.ospend <- ggplot(cust.df,aes(x = online.visits, y = online.spend)) + geom_point()
cust.ovisit.ospend                  
#Plot to find corelation between credit.score and total.spend  
cust.crdt.tspend <- ggplot(cust.df, aes(x = credit.score, y = total.spend)) + geom_point()
cust.crdt.tspend

#what percentage of people who have given sat rating have email? 
cust.df.tsat <- ggplot(cust.df, aes(x = sat.service, fill = email)) + geom_bar()
cust.df.tsat
#========
#Prediction Model 
#===========
#Linear regression
#Build linear regression models to find the most suitable prediction model for maximum sale i.e, total.spend
Max.tsell <- subset(cust.df, select = c("total.spend", "online.visits"))
summary(MAx.sell)
cor(Max.tsell)
plot(Max.tsell)
#model 2 
max.sell <- lm(total.spend ~ online.visits + store.trans, data = cust.df) 
summary(max.sell)
coef(max.sell) 
#model 3 
max.sell2 <- lm(total.spend ~ online.visits + email + age, data = cust.df)
summary(max.sell2)
coef(max.sell2)
#model 4 
cust.df$credit.score <- as.factor(cust.df$credit.score)
cust.max.sell3 <- lm(total.spend ~ online.visits + online.trans + store.spend, data = cust.df)
summary(cust.max.sell3)
coef(cust.max.sell3)
# Examine the model objects
class(cust.max.sell3)
names(cust.max.sell3)
methods(class = class(cust.max.sell3))[1:12]

