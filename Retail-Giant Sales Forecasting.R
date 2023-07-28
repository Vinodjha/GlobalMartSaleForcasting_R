#########################Time Series Case Study- Retail-Giant Sales Forecasting######
#     Project: Time Series Case Study                                               #  
#     FileNAme: Retail-Giant Sales Forecasting.R                                    #
#     Description: File consist the code in R language for the analysis required    #
#     to model time serie analysis on data and model to predict the sales forcast   # 
#     Project Team: Amitabha Banerjee                                               #
#                   Rishabh Shrivastava                                             #
#                   Saurav Kumar                                                    #
#                   Vinod Jha                                                       #
#####################################################################################


# if required install the below packages 
#install.packages("graphics")
#install.packages("tseries")
#install.packages("forecast")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("stringr")
#install.packages("ggpubr")

#required libraries
require(graphics)
library(tseries)
library(forecast)
library(dplyr,quietly = TRUE)
library(ggplot2,quietly = TRUE)
library(stringr,quietly = TRUE)
library(ggpubr,quietly = TRUE)

#if required set  working directory
#setwd("G:/Upgrad/Time series Case study")

#Loading data of global superstore into data frame "gs" 
gs<-read.csv("Global Superstore.csv",stringsAsFactors = TRUE)
summary(gs)
colnames(gs)
sum(duplicated(gs))  #no duplicate rows in the dataframe
sum(duplicated(gs$Row.ID))  #no duplicate rows ID  in the dataframe, can act as primary id for the data frame 

#checking for missing values 
colSums(is.na(gs),dims = 1)
#as per above command postal code has 41296 rows as NA 
#we can populate this data with the help of city & state information 
#But we are not using postal code as analysis parmeter hence not populting the values 

#Standarizing & Data Prepartion 

#In our anaylyis the columns Row.ID, Order.ID, Ship.Mode, Ship.Date, Customer.ID, Customer.Name, 
#Region, City, State, Country, Postal.code, Product.ID,Category,sub.category, Product.Name, Discount,
#Shipping.Cost and  Order.Priority will not be required.
# Because we want to forcast the aggregated forcast, quantity and profit of the 3 segments over 7 markets for every order month.
#So it will be a good idea to remove them before preparing our data for modeling

red_col<-c("Row.ID","Order.ID","Ship.Mode","Ship.Date","Customer.ID","Customer.Name", "City","State",
           "Country", "Postal.Code","Region","Product.ID","Category","Sub.Category","Product.Name",
           "Shipping.Cost","Order.Priority","Discount")

gs1<-gs[,-which(names(gs)%in%red_col)]

#Check point backup 
gs1_bckup1<-gs1
#gs1<-gs1_bckup1

summary(gs1)
str(gs1)

#checking for NA values, if any in other columns 
sum(is.na(gs1)) #0, no  NA values

summary(gs1$Segment)#3 segments: consumer, coprporate and Home office, which is consistent with the problem statement, no cleaning required
summary(gs1$Market)# 7 markets: no data cleaning required for this variable

# Now we are left with 6 variables such as Segemnts which have 3 factors, Market having 7 factors, forcast, qunatity and profit
# so the variables of factor type are clean. We need to check the cleanliness of Date, forcast, Quantity

gs1$Order.Date<-as.Date(gs1$Order.Date,format="%d-%m-%Y")
sum(is.na(gs1$Order.Date)) # 0, which shows that order.Date column was also in single format, else 
#we would have got some NA vlaues.

#Since Time Series Analysis need ordered data, hence sorting the data based on Order.Date using arrange function

gs1<-arrange(gs1,Order.Date)
head(gs1) # verifying the intial few rows 

salests<-ts(gs1$Sales)

#plotting hte sales data to have overview of the distribution 
plot(salests)
lines(rep(5000,nrow(gs1)),col="red")

#It can be observed that there are certain spikes in the sales data which need to be removed as outliers
sum(salests>5000) #44 orders are worth more than 5000
#checking whether they are wrong entries or the right ones
gs2<-gs[which(gs$Sales>5000),]
View(gs2)

#we can see that these 44 entries are not outliers. Reasons for there high values are their segment and 
#the quantity of the product. Most of these high cost orders are for mobile, copier etc. Some of them are 
#high because of high quantity of things ordered
#No need to remove them(will remove them if model does not fit properly beacuse of these points)

#similary checking other numeric variables
#Quantity
#hist(gs1$Quantity) 
ggplot(gs1,aes(Quantity))+geom_bar() 
# so maximum order quantity=14 which is reasonable
quantity<-as.factor(gs1$Quantity)
summary(quantity)#another check

#Profit

plot(ts(gs1$Profit)) #seeing the sales range, profit is not out of bound. So entries seems alright
gs2<-gs[abs(gs$Profit)>2000,] #there are 38 such entries which may be outliers, but if we see the 
#product name, qunatity, Sales etc, we can conclude that the entries are not wrong.
View(gs2)


#Check point backup 
gs1_bckup2<-gs1

########data is clean to be USed
# later on we need to aggregate the monthly profit etc., So its easier if we only keep month
# and year of order date

gs1$Order.Date<-format(gs1$Order.Date,"%Y-%m")


#Finding out the 21 buckets

#for consumer and the 7 markets 
cons_africa<-filter(gs1, Segment=="Consumer" & Market=="Africa")
cons_apac<-filter(gs1, Segment=="Consumer" & Market=="APAC")
cons_canada<-filter(gs1, Segment=="Consumer" & Market=="Canada")
cons_emea<-filter(gs1, Segment=="Consumer" & Market=="EMEA")
cons_eu<-filter(gs1, Segment=="Consumer" & Market=="EU")
cons_latam<-filter(gs1, Segment=="Consumer" & Market=="LATAM")
cons_us<-filter(gs1, Segment=="Consumer" & Market=="US")

#for Corporate and the 7 markets 
corp_africa<-filter(gs1, Segment=="Corporate" & Market=="Africa")
corp_apac<-filter(gs1, Segment=="Corporate" & Market=="APAC")
corp_canada<-filter(gs1, Segment=="Corporate" & Market=="Canada")
corp_emea<-filter(gs1, Segment=="Corporate" & Market=="EMEA")
corp_eu<-filter(gs1, Segment=="Corporate" & Market=="EU")
corp_latam<-filter(gs1, Segment=="Corporate" & Market=="LATAM")
corp_us<-filter(gs1, Segment=="Corporate" & Market=="US")

#for Home Office and the 7 markets 
home_africa<-filter(gs1, Segment=="Home Office" & Market=="Africa")
home_apac<-filter(gs1, Segment=="Home Office" & Market=="APAC")
home_canada<-filter(gs1, Segment=="Home Office" & Market=="Canada")
home_emea<-filter(gs1, Segment=="Home Office" & Market=="EMEA")
home_eu<-filter(gs1, Segment=="Home Office" & Market=="EU")
home_latam<-filter(gs1, Segment=="Home Office" & Market=="LATAM")
home_us<-filter(gs1, Segment=="Home Office" & Market=="US")


#Aggregating sales, quantities and profits over months
# also we will add the data to a single data frame to identify the most consistant profitable segment and market 

#for consumer and the 7 markets 

cons_africa<-as.data.frame(cons_africa %>% group_by(Order.Date)%>%summarise(monthly_Sales=sum(Sales),
                    monthly_Quantity=sum(Quantity),monthly_Profit=sum(Profit)))

#arranging the data based on groupped month since the order is important for time series
cons_africa <- arrange(cons_africa,Order.Date)


cons_apac<-as.data.frame(cons_apac %>% group_by(Order.Date)%>%summarise(monthly_Sales=sum(Sales),
                           monthly_Quantity=sum(Quantity),monthly_Profit=sum(Profit)))

#arranging the data based on groupped month since the order is important for time series
cons_apac <- arrange(cons_apac,Order.Date)


cons_canada<-as.data.frame(cons_canada %>% group_by(Order.Date)%>%summarise(monthly_Sales=sum(Sales),
                          monthly_Quantity=sum(Quantity),monthly_Profit=sum(Profit)))
cons_canada <- arrange(cons_canada,Order.Date)


cons_emea<-as.data.frame(cons_emea %>% group_by(Order.Date)%>%summarise(monthly_Sales=sum(Sales),
                           monthly_Quantity=sum(Quantity),monthly_Profit=sum(Profit)))
cons_emea <- arrange(cons_emea,Order.Date)


cons_eu<-as.data.frame(cons_eu %>% group_by(Order.Date)%>%summarise(monthly_Sales=sum(Sales),
                           monthly_Quantity=sum(Quantity),monthly_Profit=sum(Profit)))
cons_eu <- arrange(cons_eu,Order.Date)


cons_latam<-as.data.frame(cons_latam %>% group_by(Order.Date)%>%summarise(monthly_Sales=sum(Sales),
                           monthly_Quantity=sum(Quantity),monthly_Profit=sum(Profit)))

cons_latam <- arrange(cons_latam,Order.Date)

cons_us<-as.data.frame(cons_us %>% group_by(Order.Date)%>%summarise(monthly_Sales=sum(Sales),
                           monthly_Quantity=sum(Quantity),monthly_Profit=sum(Profit)))

cons_us <- arrange(cons_us,Order.Date)

#for corporate and the 7 markets 

corp_africa<-as.data.frame(corp_africa %>% group_by(Order.Date)%>%summarise(monthly_Sales=sum(Sales),
                           monthly_Quantity=sum(Quantity),monthly_Profit=sum(Profit)))
corp_africa <- arrange(corp_africa,Order.Date)

corp_apac<-as.data.frame(corp_apac %>% group_by(Order.Date)%>%summarise(monthly_Sales=sum(Sales),
                           monthly_Quantity=sum(Quantity),monthly_Profit=sum(Profit)))
corp_apac <- arrange(corp_apac,Order.Date)


corp_canada<-as.data.frame(corp_canada %>% group_by(Order.Date)%>%summarise(monthly_Sales=sum(Sales),
                           monthly_Quantity=sum(Quantity),monthly_Profit=sum(Profit)))
corp_canada <- arrange(corp_canada,Order.Date)


corp_emea<-as.data.frame(corp_emea %>% group_by(Order.Date)%>%summarise(monthly_Sales=sum(Sales),
                           monthly_Quantity=sum(Quantity),monthly_Profit=sum(Profit)))
corp_emea <- arrange(corp_emea,Order.Date)

corp_eu<-as.data.frame(corp_eu %>% group_by(Order.Date)%>%summarise(monthly_Sales=sum(Sales),
                           monthly_Quantity=sum(Quantity),monthly_Profit=sum(Profit)))
corp_eu <- arrange(corp_eu,Order.Date)

corp_latam<-as.data.frame(corp_latam %>% group_by(Order.Date)%>%summarise(monthly_Sales=sum(Sales),
                           monthly_Quantity=sum(Quantity),monthly_Profit=sum(Profit)))
corp_latam <- arrange(corp_latam,Order.Date)

corp_us<-as.data.frame(corp_us %>% group_by(Order.Date)%>%summarise(monthly_Sales=sum(Sales),
                           monthly_Quantity=sum(Quantity),monthly_Profit=sum(Profit)))
corp_us <- arrange(corp_us,Order.Date)

#for Home Office and the 7 markets 

home_africa<-as.data.frame(home_africa %>% group_by(Order.Date)%>%summarise(monthly_Sales=sum(Sales),
                           monthly_Quantity=sum(Quantity),monthly_Profit=sum(Profit)))
home_africa <- arrange(home_africa,Order.Date)

home_apac<-as.data.frame(home_apac %>% group_by(Order.Date)%>%summarise(monthly_Sales=sum(Sales),
                           monthly_Quantity=sum(Quantity),monthly_Profit=sum(Profit)))
home_apac <- arrange(home_apac,Order.Date)


home_canada<-as.data.frame(home_canada %>% group_by(Order.Date)%>%summarise(monthly_Sales=sum(Sales),
                           monthly_Quantity=sum(Quantity),monthly_Profit=sum(Profit)))
home_canada <- arrange(home_canada,Order.Date)


home_emea<-as.data.frame(home_emea %>% group_by(Order.Date)%>%summarise(monthly_Sales=sum(Sales),
                           monthly_Quantity=sum(Quantity),monthly_Profit=sum(Profit)))
home_emea <- arrange(home_emea,Order.Date)

home_eu<-as.data.frame(home_eu %>% group_by(Order.Date)%>%summarise(monthly_Sales=sum(Sales),
                           monthly_Quantity=sum(Quantity),monthly_Profit=sum(Profit)))
home_eu <- arrange(home_eu,Order.Date)


home_latam<-as.data.frame(home_latam %>% group_by(Order.Date)%>%summarise(monthly_Sales=sum(Sales),
                           monthly_Quantity=sum(Quantity),monthly_Profit=sum(Profit)))
home_latam <- arrange(home_latam,Order.Date)


home_us<-as.data.frame(home_us %>% group_by(Order.Date)%>%summarise(monthly_Sales=sum(Sales),
                           monthly_Quantity=sum(Quantity),monthly_Profit=sum(Profit)))
home_us <- arrange(home_us,Order.Date)


#determining net profit(np) and coefficient of variation(CV) for the buckets made from the 
# using function sd for standard deviation and mean for avg values 
#segment --------- and market --------:
segment_market <- c()
profit_vector<-c()
coef_var_vector<-c()
#Consumer, Africa
segment_market[1]<- "Consumer, Africa"
profit_vector[1]<-sum(cons_africa$monthly_Profit)#np=47772.1
coef_var_vector[1]<-sd(cons_africa$monthly_Profit)/mean(cons_africa$monthly_Profit)# cv=1.32

#Consumer, APAC
segment_market[2]<-"Consumer, APAC"
profit_vector[2]<-sum(cons_apac$monthly_Profit)# np=222817.6
coef_var_vector[2]<-sd(cons_apac$monthly_Profit)/mean(cons_apac$monthly_Profit)# cv=0.63

#Consumer, Canada
segment_market[3]<-"Consumer, Canada"
profit_vector[3]<-sum(cons_canada$monthly_Profit) #np=9677.7
coef_var_vector[3]<-sd(cons_canada$monthly_Profit)/mean(cons_canada$monthly_Profit) #cv=1.39

#Consumer, EMEA
segment_market[4]<-"Consumer, EMEA"
profit_vector[4]<-sum(cons_emea$monthly_Profit) #np=25532.57
coef_var_vector[4]<-sd(cons_emea$monthly_Profit)/mean(cons_emea$monthly_Profit) #cv=2.19

#Consumer, EU
segment_market[5]<-"Consumer, EU"
profit_vector[5]<-sum(cons_eu$monthly_Profit) #np=188687.7
coef_var_vector[5]<-sd(cons_eu$monthly_Profit)/mean(cons_eu$monthly_Profit)#cv=0.62

#Consumer, LATAM
segment_market[6]<-"Consumer, LATAM"
profit_vector[6]<-sum(cons_latam$monthly_Profit) # np=120632.9
coef_var_vector[6]<-sd(cons_latam$monthly_Profit)/mean(cons_latam$monthly_Profit) #cv=0.66

#Consumer, US
segment_market[7]<-"Consumer, US"
profit_vector[7]<-sum(cons_us$monthly_Profit) # np=120632.9
coef_var_vector[7]<-sd(cons_us$monthly_Profit)/mean(cons_us$monthly_Profit) #cv=1.012

#Corporate, Africa
segment_market[8]<-"Corporate, Africa"
profit_vector[8]<-sum(corp_africa$monthly_Profit)#np=20686.97
coef_var_vector[8]<-sd(corp_africa$monthly_Profit)/mean(corp_africa$monthly_Profit)# cv=1.78

#Corporate, APAC
segment_market[9]<-"Corporate, APAC"
profit_vector[9]<-sum(corp_apac$monthly_Profit)# np=129737.2
coef_var_vector[9]<-sd(corp_apac$monthly_Profit)/mean(corp_apac$monthly_Profit)# cv=0.70

#Corporate, Canada
segment_market[10]<-"Corporate, Canada"
profit_vector[10]<-sum(corp_canada$monthly_Profit) #np=5036.46
coef_var_vector[10]<-sd(corp_canada$monthly_Profit)/mean(corp_canada$monthly_Profit) #cv=1.55

#Corporate, EMEA
segment_market[11]<-"Corporate, EMEA"
profit_vector[11]<-sum(corp_emea$monthly_Profit) #np=12499.13
coef_var_vector[11]<-sd(corp_emea$monthly_Profit)/mean(corp_emea$monthly_Profit) #cv=4.47

#Corporate, EU
segment_market[12]<-"Corporate, EU"
profit_vector[12]<-sum(corp_eu$monthly_Profit) #np=123394
coef_var_vector[12]<-sd(corp_eu$monthly_Profit)/mean(corp_eu$monthly_Profit)#cv=0.76

#Corporate, LATAM
segment_market[13]<-"Corporate, LATAM"
profit_vector[13]<-sum(corp_latam$monthly_Profit) # np=57875.42
coef_var_vector[13]<-sd(corp_latam$monthly_Profit)/mean(corp_latam$monthly_Profit) #cv=0.811

#Corporate, US
segment_market[14]<-"Corporate, US"
profit_vector[14]<-sum(corp_us$monthly_Profit) # np=91979.13
coef_var_vector[14]<-sd(corp_us$monthly_Profit)/mean(corp_us$monthly_Profit) #cv=1.002


#Home, Africa
segment_market[15]<-"Home, Africa"
profit_vector[15]<-sum(home_africa$monthly_Profit)#np=20412.57
coef_var_vector[15]<-sd(home_africa$monthly_Profit)/mean(home_africa$monthly_Profit)# cv=1.79

#Home , APAC
segment_market[16]<-"Home , APAC"
profit_vector[16]<-sum(home_apac$monthly_Profit)# np=83445.25
coef_var_vector[16]<-sd(home_apac$monthly_Profit)/mean(home_apac$monthly_Profit)# cv=0.63

#Home , Canada
segment_market[17]<-"Home , Canada"
profit_vector[17]<-sum(home_canada$monthly_Profit) #np=9677.7
coef_var_vector[17]<-sd(home_canada$monthly_Profit)/mean(home_canada$monthly_Profit) #cv=1.05

#Home , EMEA
segment_market[18]<-"Home , EMEA"
profit_vector[18]<-sum(home_emea$monthly_Profit) #np=5866.263
coef_var_vector[18]<-sd(home_emea$monthly_Profit)/mean(home_emea$monthly_Profit) #cv=5.88

#Home , EU
segment_market[19]<-"Home , EU"
profit_vector[19]<-sum(home_eu$monthly_Profit) #np=60748.05
coef_var_vector[19]<-sd(home_eu$monthly_Profit)/mean(home_eu$monthly_Profit)#cv=1.12

#Home , LATAM
segment_market[20]<-"Home , LATAM"
profit_vector[20]<-sum(home_latam$monthly_Profit) # np=43135.13
coef_var_vector[20]<-sd(home_latam$monthly_Profit)/mean(home_latam$monthly_Profit) #cv=1.18

#Home , US
segment_market[21]<-"Home , US"
profit_vector[21]<-sum(home_us$monthly_Profit) # np=60298.68
coef_var_vector[21]<-sd(home_us$monthly_Profit)/mean(home_us$monthly_Profit) #cv=1.096

df<-data.frame(segment_market,profit_vector,coef_var_vector)

#df<-data.frame(c(1:21),profit_vector,coef_var_vector)
#names(df)[1]<-"serial"
plot1<-ggplot(data=df,aes(x=segment_market,y=profit_vector))+geom_col()+theme(axis.text.x = element_text(angle = 90,hjust = 0.5,vjust=0.5))
#geom_text(aes(label=profit_vector,angle=90,colour="Orange"))
plot2<-ggplot(data=df,aes(x=segment_market,y=coef_var_vector))+geom_col()+theme(axis.text.x = element_text(angle = 90,hjust = 0.5,vjust=0.5))
#geom_text(aes(label=coef_var_vector))
ggarrange(plot1,plot2,nrow=2,ncol=1) #2nd and 4th serial are giving the two most profitable and 
#consistently profitable basket
### It is obersved that the two most profittable and consistently profitable 
#baskets are cons_apac(consumer segment of APAC) and cons_eu(consumer segment of EU)

# Cofficient of variance is lowest for Consumer - APAC and Consumer-EU , which concludes that the two segments are most profitable and consistant 



#########Time series MOdelling of sales and quantity data for the two baskets


indata1<-ts(cons_apac$monthly_Sales[1:42])# leaving out last 6 month data for forcasting
indata2<-ts(cons_apac$monthly_Quantity[1:42])
indata3<-ts(cons_eu$monthly_Sales[1:42])
indata4<-ts(cons_eu$monthly_Quantity[1:42])


#time series analysis for monthly sales # Consumer APAC

plot(ts(cons_apac$monthly_Sales)) #an upward trend  is visible, seasonality is difficult to predict

#smoothing the monthly_sales time series( taking window width=3, #taking longer window size will smoothen the graph more)
indata1_smooth<-stats::filter(indata1, filter=rep(1/3,3),method="convolution",sides=2)

diff <- indata1_smooth[3] - indata1_smooth[2]
indata1_smooth[1] <- indata1_smooth[2] - diff #smoothing the starting points

diff<-indata1_smooth[41]-indata1_smooth[40] #smoothing the end points as well
indata1_smooth[42]<-indata1_smooth[41]+diff

plot(indata1_smooth)
month<-seq(1,42)
indt1_sm_df<-as.data.frame(cbind(month,indata1_smooth))
colnames(indt1_sm_df) <- c('Month', 'Sales')

#lmfit1<-lm(Sales~(sin(0.025*Month/pi)*poly(Month,3)+cos(0.025*Month/pi)*poly(Month,3))+Month,data=indt1_sm_df)
lmfit1<-lm(Sales~poly(Month,1),data=indt1_sm_df)
summary(lmfit1)
global_fit1<-predict(lmfit1,indt1_sm_df)

lines(month,global_fit1,col="red",lwd=2)
local_fit<-indt1_sm_df$Sales-global_fit1
plot(ts(local_fit))
#We cannot predict any kind of periodicity in it
#it looks like a weak or a strong stationary time series, lets test
kpss.test(local_fit) # p-value=0.1 which shows that local_fit is strongly stationary
adf.test(local_fit)#p-value=0.33, which contradicts KPSS test.
 acf(local_fit) 
 pacf(local_fit)#it does not seems to be a white noise, some autoregressive part seems to be present
 #lets model the autoregressive part using ARMA process
 local_fit_arma<-auto.arima(local_fit)
 tsdiag(local_fit_arma)
 local_fit_arma
 residual1<-local_fit-fitted(local_fit_arma)
 pacf(residual1)
 acf(residual1)#which justifies that it is a white noise
 plot(indata1_smooth)
 total_fit1<-global_fit1+fitted(local_fit_arma)
 lines(total_fit1,col="red") # which shows a failry decent fit
 
 #forcasting the next 6 months sales
 outdata1<-as.data.frame(c(43:48));
 
 
 colnames(outdata1) <- c('Month')
 global_forcast1<-predict(lmfit1,outdata1)
 local_forcast1<-predict(local_fit_arma,n.ahead = 6)
 cons_apac_net_sales_foracst<-global_forcast1+local_forcast1$pred
 net_sale1<-c(total_fit1,cons_apac_net_sales_foracst)
 plot(ts(cons_apac$monthly_Sales)) # this is not the smoothened data on which model is being made
 lines(ts(net_sale1),col="red")
 
 Mape_val_arma1<- accuracy(cons_apac_net_sales_foracst,cons_apac$monthly_Sales[43:48])[5]
 Mape_val_arma1 #22.76
 
 
 #modeling the ARIMA model for the sales of consumer segment of APAC
 
cons_apac_sales_arima<-auto.arima(indata1)
cons_apac_sales_arima
# ARIMA(0,1,1) 
# 
# Coefficients:
#   ma1
# -0.7559
# s.e.   0.1381
# 
# sigma^2 estimated as 174361555:  log likelihood=-447.11
# AIC=898.23   AICc=898.55   BIC=901.66

# looking at AIC, AICc and BIC values, the ARMA fit seemes more accurate
cons_apac_sales_arima_pred<-fitted(cons_apac_sales_arima)
resi_arima1<-indata1-cons_apac_sales_arima_pred
kpss.test(resi_arima1)
adf.test(resi_arima1) # alternative hythesis is true, so residual is stationary
pacf(resi_arima1)
acf(resi_arima1)#acf and pacf also justifies the stationarity, 
cons_apac_net_sales_foracst_arima<-predict(cons_apac_sales_arima,n.ahead = 6)
plot(ts(cons_apac$monthly_Sales))
lines(c(cons_apac_sales_arima_pred,cons_apac_net_sales_foracst_arima$pred),col="red")
#The fit does not look as comprehensing as ARMA fit

Mape_val_arima1<-accuracy(cons_apac_net_sales_foracst_arima$pred,cons_apac$monthly_Sales[43:48])[5]
Mape_val_arima1 #27.69

#The ARMA Model suits better here as the mape value is also less for ARMA

#############################################################################



#time series analysis for monthly quantity sold in APAC

plot(ts(cons_apac$monthly_Quantity)) #an upward trend  is visible, seasonality is difficult to predict

#smoothing the monthly_sales time series( taking window width=3, #taking longer window size will smoothen the graph more)
indata2_smooth<-stats::filter(indata2, filter=rep(1/3,3),method="convolution",sides=2)

diff <- indata2_smooth[3] - indata2_smooth[2]
indata2_smooth[1] <- indata2_smooth[2] - diff #smoothing the starting points

diff<-indata2_smooth[41]-indata2_smooth[40] #smoothing the end points as well
indata2_smooth[42]<-indata2_smooth[41]+diff
#plot(indata2_smooth)

month<-seq(1,42)
indt2_sm_df<-as.data.frame(cbind(month,indata2_smooth))
colnames(indt2_sm_df) <- c('Month', 'Sales')

#lmfit1<-lm(Sales~(sin(0.025*Month/pi)*poly(Month,3)+cos(0.025*Month/pi)*poly(Month,3))+Month,data=indt1_sm_df)
lmfit2<-lm(Sales~poly(Month,1),data=indt2_sm_df)
summary(lmfit2) #Residual standard error: 89.76 on 40 degrees of freedom
#Multiple R-squared:  0.5393
global_fit2<-predict(lmfit2,indt2_sm_df)
plot(indata2_smooth)
lines(month,global_fit2,col="red",lwd=2)
local_fit<-indt2_sm_df$Sales-global_fit2
plot(ts(local_fit)) #after removing the trend some seasonality can be seen. 

#we tried many global models(with trend and seasonality both) and we also reached to a better fit than this which was:
#lmfit232<-lm(Sales~poly(Month,1)+sin(2*pi*Month/13)+
#              Month*sin(2*pi*Month/13)+Month*cos(2*pi*Month/13)+
 #             Month*cos(2*pi*Month/6.5)+Month*sin(2*pi*Month/6.5),data=indt2_sm_df)
#summary(lmfit232)#Residual standard error: 38.83 on 32 degrees of freedom
#Multiple R-squared:  0.931,	Adjusted R-squared:  0.9116 

#however when we took the autoregressive part into picture, as it will be shown next,
#the model with only trend and autoregressive part(no seasonality) is giving almost perfect fit.
#so we persisted with simpler global model

#it looks like a weak or a strong stationary time series, lets test

acf(local_fit) 
pacf(local_fit)#it does not seems to be a white noise, some autoregressive part seems to be present
#lets model the autoregressive part using ARMA process
local_fit_arma<-auto.arima(local_fit)
tsdiag(local_fit_arma)
local_fit_arma
residual2<-local_fit-fitted(local_fit_arma)
pacf(residual2)
acf(residual2)#which justifies that it is a white noise
kpss.test(residual2) 
adf.test(residual2)
plot(indata2_smooth)
total_fit2<-global_fit2+fitted(local_fit_arma)
lines(total_fit2,col="red") # which shows a failry decent fit, as stated earlier

#forcasting the next 6 months quantity

global_forcast2<-predict(lmfit2,outdata1)
local_forcast2<-predict(local_fit_arma,n.ahead = 6)
cons_apac_net_quantity_foracst<-global_forcast2+local_forcast2$pred
net_quantity1<-c(total_fit2,cons_apac_net_quantity_foracst)

Mape_val_arma<- accuracy(cons_apac_net_quantity_foracst,cons_apac$monthly_Quantity[43:48])[5]
Mape_val_arma #36.12

#modeling the ARIMA model for the product quantity of consumer segment of APAC

cons_apac_quantity_arima<-auto.arima(indata2)
cons_apac_quantity_arima 
#ARIMA(0,1,0) 

#sigma^2 estimated as 25366:  log likelihood=-266.07
#AIC=534.14   AICc=534.24   BIC=535.85

# looking at AIC, AICc and BIC values, the first fit seemes more accurate
cons_apac_forcast_arima_pred<-fitted(cons_apac_quantity_arima)
resi_arima2<-indata2-cons_apac_forcast_arima_pred
kpss.test(resi_arima2)#KPSS value is very small
adf.test(resi_arima2) # alternative hythesis is true, so residual is stationary
pacf(resi_arima2)
acf(resi_arima2)#acf and pacf also justifies the stationarity, 
cons_apac_net_quantity_foracst_arima<-predict(cons_apac_quantity_arima,n.ahead = 6)
plot(ts(cons_apac$monthly_Quantity))
lines(c(cons_apac_forcast_arima_pred,cons_apac_net_quantity_foracst_arima$pred),col="red")
# the forcasted values are becoming constant and repeating the last oberved value. This is because
#there is no autoregressivness in the model. 
Mape_val_arima<-accuracy(cons_apac_net_quantity_foracst_arima$pred,cons_apac$monthly_Quantity[43:48])[5]
Mape_val_arima #26.24

#in this case the Auto.Arima model is giving a better accuracy over ARMA model. However since the 
#forcast is becoming constant so MAPE will certainly increase if we forcast for more months.
#Infact the trend is also lost. 
#So We will use ARMA model again for forcasting the quantity of sales in APAC

######################################################################################

#Modelling the time series for the sales of EU

plot(ts(cons_eu$monthly_Sales)) #an upward trend  is visible, seasonality is difficult to predict

#smoothing the monthly_sales time series( taking window width=3, #taking longer window size will smoothen the graph more)
indata3_smooth<-stats::filter(indata3, filter=rep(1/3,3),method="convolution",sides=2)

diff <- indata3_smooth[3] - indata3_smooth[2]
indata3_smooth[1] <- indata3_smooth[2] - diff #smoothing the starting points

diff<-indata3_smooth[41]-indata3_smooth[40] #smoothing the end points as well
indata3_smooth[42]<-indata3_smooth[41]+diff
#plot(indata3_smooth)

month<-seq(1,42)
indt3_sm_df<-as.data.frame(cbind(month,indata3_smooth))
colnames(indt3_sm_df) <- c('Month', 'Sales')

#lmfit1<-lm(Sales~(sin(0.025*Month/pi)*poly(Month,3)+cos(0.025*Month/pi)*poly(Month,3))+Month,data=indt1_sm_df)
lmfit3<-lm(Sales~poly(Month,1),data=indt3_sm_df)
summary(lmfit3) 

global_fit3<-predict(lmfit3,indt3_sm_df)
plot(indata3_smooth)
lines(month,global_fit3,col="red",lwd=2)
local_fit<-indt3_sm_df$Sales-global_fit3
plot(ts(local_fit)) #after removing the trend some seasonality can be seen. 


#it looks like a weak or a strong stationary time series, lets test

acf(local_fit) 
pacf(local_fit)#it does not seems to be a white noise, some autoregressive part seems to be present
#lets model the autoregressive part using ARMA process
local_fit_arma<-auto.arima(local_fit)
tsdiag(local_fit_arma)
local_fit_arma
residual3<-local_fit-fitted(local_fit_arma)
pacf(residual3)
acf(residual3)#which justifies that it is a white noise
kpss.test(residual3) #KPSS valuesa are very small
adf.test(residual3) #residual is stationary
plot(indata3_smooth)
total_fit3<-global_fit3+fitted(local_fit_arma)
lines(total_fit3,col="red") # which shows a failry decent fit, as stated earlier

#forcasting the next 6 months sales

global_forcast3<-predict(lmfit3,outdata1)
local_forcast3<-predict(local_fit_arma,n.ahead = 6)
cons_eu_net_sale_foracst<-global_forcast3+local_forcast3$pred
net_sale2<-c(total_fit3,cons_eu_net_sale_foracst)

Mape_val_arma<- accuracy(cons_eu_net_sale_foracst,cons_eu$monthly_Sales[43:48])[5]
Mape_val_arma #30.9

#modeling the ARIMA model for the product quantity of consumer segment of eu

cons_eu_sale_arima<-auto.arima(indata3)
cons_eu_sale_arima 
# ARIMA(2,1,0) 
# 
# Coefficients:
#   ar1      ar2
# -0.5796  -0.4906
# s.e.   0.1346   0.1310
# 
# sigma^2 estimated as 168564623:  log likelihood=-445.84
# AIC=897.67   AICc=898.32   BIC=902.81


cons_eu_forcast_arima_pred<-fitted(cons_eu_sale_arima)
resi_arima3<-indata3-cons_eu_forcast_arima_pred
kpss.test(resi_arima3)#KPSS value is very small
adf.test(resi_arima3) # alternative hythesis is true, so residual is stationary
pacf(resi_arima3)
acf(resi_arima3)#acf and pacf also justifies the stationarity, 

#forcasting for next 6 months using ARIMA
cons_eu_net_sale_foracst_arima<-predict(cons_eu_sale_arima,n.ahead = 6)
plot(ts(cons_eu$monthly_Sales))
lines(c(cons_eu_forcast_arima_pred,cons_eu_net_sale_foracst_arima$pred),col="red")
# the forcasted values are becoming constant and repeating the last oberved value. This is because
#there is no autoregressivness in the model. 
Mape_val_arima<-accuracy(cons_eu_net_sale_foracst_arima$pred,cons_eu$monthly_Sales[43:48])[5]
Mape_val_arima #28.92

#The second model is giving a better prediction according to the MAPE value

###########################################################################################

#Time series modeling of quantity of Quantity of consumer products in EU

#Modelling the time series for the Quantity of EU

plot(ts(cons_eu$monthly_Quantity)) #an upward trend  is visible, seasonality is difficult to predict

#smoothing the monthly_sales time series( taking window width=3, #taking longer window size will smoothen the graph more)
indata4_smooth<-stats::filter(indata4, filter=rep(1/3,3),method="convolution",sides=2)

diff <- indata4_smooth[3] - indata4_smooth[2]
indata4_smooth[1] <- indata4_smooth[2] - diff #smoothing the starting points

diff<-indata4_smooth[41]-indata4_smooth[40] #smoothing the end points as well
indata4_smooth[42]<-indata4_smooth[41]+diff
plot(indata4_smooth)

month<-seq(1,42)
indt4_sm_df<-as.data.frame(cbind(month,indata4_smooth))
colnames(indt4_sm_df) <- c('Month', 'Sales')

#lmfit1<-lm(Sales~(sin(0.025*Month/pi)*poly(Month,3)+cos(0.025*Month/pi)*poly(Month,3))+Month,data=indt1_sm_df)
lmfit4<-lm(Sales~poly(Month,1),data=indt4_sm_df)
summary(lmfit4) 

global_fit4<-predict(lmfit4,indt4_sm_df)
plot(indata4_smooth)
lines(month,global_fit4,col="red",lwd=2)
local_fit<-indt4_sm_df$Sales-global_fit4
plot(ts(local_fit)) # 


#it looks like a weak or a strong stationary time series, lets test

acf(local_fit) 
pacf(local_fit)#it does not seems to be a white noise, some autoregressive part seems to be present
#lets model the autoregressive part using ARMA process
local_fit_arma<-auto.arima(local_fit)
tsdiag(local_fit_arma)
local_fit_arma
# ARIMA(2,0,1) with zero mean 
# 
# Coefficients:
#   ar1      ar2      ma1
# 1.5434  -0.7719  -0.7242
# s.e.  0.1209   0.1010   0.1451
# 
# sigma^2 estimated as 2024:  log likelihood=-218.65
# AIC=445.3   AICc=446.39   BIC=452.26

residual4<-local_fit-fitted(local_fit_arma)
pacf(residual4)
acf(residual4)#which justifies that it is a white noise
kpss.test(residual4) #KPSS valuesa are very small
adf.test(residual3) #residual is stationary
plot(indata4_smooth)
total_fit4<-global_fit4+fitted(local_fit_arma)
lines(total_fit4,col="red") # which shows a failry decent fit, as stated earlier

#forcasting the next 6 months quantity of sales

global_forcast4<-predict(lmfit4,outdata1)
local_forcast4<-predict(local_fit_arma,n.ahead = 6)
cons_eu_net_quantity_forcast<-global_forcast4+local_forcast4$pred
net_quantity2<-c(total_fit4,cons_eu_net_quantity_forcast)

Mape_val_arma<- accuracy(cons_eu_net_quantity_forcast,cons_eu$monthly_Quantity[43:48])[5]
Mape_val_arma #31.12

#modeling the ARIMA model for the product quantity of consumer segment of eu

cons_eu_quantity_arima<-auto.arima(indata4)
cons_eu_quantity_arima 
# ARIMA(2,1,0) 
# 
# Coefficients:
#   ar1      ar2
# -0.7359  -0.5879
# s.e.   0.1224   0.1185
# 
# sigma^2 estimated as 21185:  log likelihood=-261.9
# AIC=529.8   AICc=530.44   BIC=534.94

cons_eu_forcast_arima_pred<-fitted(cons_eu_quantity_arima)
resi_arima4<-indata4-cons_eu_forcast_arima_pred
kpss.test(resi_arima4)#KPSS value is very small
adf.test(resi_arima4) # alternative hythesis is true, so residual is stationary
pacf(resi_arima4)
acf(resi_arima4)#acf and pacf also justifies the stationarity, 

#forcasting for next 6 months using ARIMA
cons_eu_net_quantity_foracst_arima<-predict(cons_eu_quantity_arima,n.ahead = 6)
plot(ts(cons_eu$monthly_Quantity))
lines(c(cons_eu_forcast_arima_pred,cons_eu_net_quantity_foracst_arima$pred),col="red")
# the forcasted values are becoming constant and repeating the last oberved value. This is because
#there is no autoregressivness in the model. 
Mape_val_arima<-accuracy(cons_eu_net_quantity_foracst_arima$pred,cons_eu$monthly_Quantity[43:48])[5]
Mape_val_arima #30.13

#MApe values are nearly equal for AIC and AICc values for ARMA model is much better than ARIMA
#model. So ARMA model is a better model for forcasting further