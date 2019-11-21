# Load libraries----
library(dplyr)
library(ggplot2)
library(magrittr)
library(readxl)
library(tibble)
library(readxl)
library(depmixS4)
library(quantmod)
library(MASS)
library(reshape2)
library(gridExtra)

## 1.Data cleaning process ----

# find the file names of consumption, spot prices and production for each year
consum_names <- list.files(path = "/Users/Yako/Desktop/BAN421/Data", 
                           pattern = "consumption-per.*.xlsx", full.names = T)
elspot_names<-list.files(path = "/Users/Yako/Desktop/BAN421/Data",
                         pattern="elspot.*.xlsx",full.names = T)
production_names<-list.files(path = "/Users/Yako/Desktop/BAN421/Data",
                             pattern="production-per.*.xlsx",full.names = T)

# try to load all 5 years consumption data into one dataframe
consumption<-data.frame() # create one empty dataframe first
system.time(for(i in consum_names){
  #since our data is  pure xlsx format, read_xlsx fits the most
 consumption <- read_xlsx(i)%>% 
       bind_rows(consumption)%>% #combine each year's data together by rows
   na.omit()
})

# we can also try to use tibble and compare
consumption_tib<-tibble()
system.time(for(i in 1:length(consum_names)){
  consumption_tib <- read_xlsx(consum_names[i])%>% 
    bind_rows(consumption_tib)  
})
# only takes the columns that we need
# the processing time is almost same 
# check the data structure in each, and found out they have same data structure
str(consumption) 
str(consumption_tib)

# so we continue to use dataframe to load production and price data
production <-data.frame()
for(i in 1:length(production_names)){
  production <- read_xlsx(production_names[i])%>% 
    bind_rows( production)%>%
    na.omit()
}

price <-data.frame()
for(i in 1:length(elspot_names)){
  price<- read_xlsx(elspot_names[i])%>% 
    bind_rows( price)
}
price<-price[,-5]
price<-na.omit(price)

# continue to load the prognosis data for consumption and production
# so that we can use that to predict based on our model
consum_pre<-read_xlsx("consumption-prognosis_2019_hourly.xlsx")
produc_pre<-read_xlsx("production-prognosis_2019_hourly.xlsx")
# sum up all productions from small areas in each country and take the total only
produc_pre<- produc_pre%>%
  mutate(NO=NO1+NO2+NO3+NO4+NO5)%>%
  mutate(SE=SE1+SE2+SE3+SE4)%>%
  mutate(DK=DK1+DK2) %>%
  select("Date","Hours","FI","DK","EE",
         "LV","LT","NO","SE")

# save all these data as R.data

save(consum_pre,consumption,price,produc_pre,production,file="BAN421.Rdata")

## 2.Plotting ----
# before we explore the relationship, we can plot the trend in these 5 years first

# create another "year" variable 
consumption<-consumption%>%
  mutate(year = format(Date, "%Y"))
production<-production%>%
  mutate(year = format(Date, "%Y"))
price<-price%>%
  mutate(year = format(Date, "%Y"))

# 1. consumption
# create the yearly plotting data
plot<-consumption %>%
  group_by(year) %>%
  select("year","NO","SE","DK","FI","LV","LT")%>%
  summarise_each(funs(sum))

#use ggplot to plot consumtion trend in Norway 
ggplot(data=consumption,mapping=aes(x=consumption$Date))+
  geom_line(aes(y=consumption$NO, color="NO"))
#and all countires
ggplot(data=plot,mapping=aes(x=year))+
  geom_point(aes(y=plot$NO, color="NO"))+
  geom_point(aes(y=plot$SE,color="SE"))+
  geom_point(aes(y=plot$DK,color="DK"))+
  geom_point(aes(y=plot$FI,color="FI"))+
  geom_point(aes(y=plot$LV,color="LV"))+
  geom_point(aes(y=plot$LT,color="LT"))+
  ggtitle("consumption trend")+
  ylab("consumtion")

#2.production
# create the yearly plotting data
plot_p<-production %>%
  group_by(year) %>%
  select("year","NO","SE","DK","FI","LV","LT")%>%
  summarise_each(funs(sum)) #sum the toal production each country each year
#use ggplot to plot production trend in Norway 
ggplot(data=production,mapping=aes(x=production$Date))+
  geom_line(aes(y=production$NO, color="NO"))+
  xlab("Date")+
  ylab("Norway")
#and all  countries
ggplot(data=plot_p,mapping=aes(x=year))+
  geom_point(aes(y=plot_p$NO, color="NO"))+
  geom_point(aes(y=plot_p$SE,color="SE"))+
  geom_point(aes(y=plot_p$DK,color="DK"))+
  geom_point(aes(y=plot_p$FI,color="FI"))+
  geom_point(aes(y=plot_p$LV,color="LV"))+
  geom_point(aes(y=plot_p$LT,color="LT"))+
  ggtitle("production trend")+
  ylab("production")

#3.price
# create the yearly plotting data
plot_price<-price %>%
  group_by(year) %>%
  select("year","Bergen","Tromsø")%>%
  summarise_each(funs(mean))#take the mean price each year
# #use ggplot to plot production trend in Norway 
ggplot(data=price,mapping=aes(x=price$Date))+
  geom_line(aes(y=price$Bergen, color="Bergen"))+
  geom_line(aes(y=price$Tromsø, color="Tromsø"))+
  xlab("date")
#a general trend in each year in all  countries
ggplot(data=plot_price,mapping=aes(x=year))+
  geom_point(aes(y=plot_price$Bergen, color="Bergen"))+
  geom_point(aes(y=plot_price$Tromsø,color="Tromsø"))+
  ggtitle("price trend")+
  ylab("mean_price")

## 3.Statistical methods ----
# Combine tables with all the data and preproceess the data
comb <-  inner_join(consumption, production,
                   by = c("Date", "Hours"))

comb <- comb %>% 
  mutate(NO = NO.y - NO.x,
         DK = DK.y - DK.x,
         SE = SE.y - SE.x,
         NORD = Nordic.y - Nordic.x)

comb <- comb[c("Date", "Hours", "NO", "DK", "SE", "NORD")]

comb <- comb %>% 
  group_by(Date) %>% 
  summarise(NO = sum(NO),
            DK = sum(DK),
            SE = sum(SE),
            NORD = sum(NORD))

comb <- comb %>% 
    mutate(NO_state = as.logical(NO > 0),
           DK_state = as.logical(DK >0),
           SE_state = as.logical(SE > 0),
           NORD_state = as.logical(NORD > 0))

comb <- as.data.frame(lapply(comb, function(y) gsub("TRUE", "Surplus", y)))
comb <- as.data.frame(lapply(comb, function(y) gsub("FALSE", "Deficit", y)))

comb[6:9] <- lapply(comb[6:9], as.character)
comb$Date <- as.Date(comb$Date)

# Define the price fluctuations
fluc <- diff(log(as.numeric(comb$NORD)))
fluc <- c(0, fluc)

# Run and fit HMM
hmm <- depmix(fluc ~ 1, 
              family = gaussian(), 
              nstates = 2, 
              data = comb)

fit.hmm <- fit(hmm)
est.states <- posterior(fit.hmm)

# Analyse the results
tbl <- table(est.states$state, comb$NO_state)
colnames(est.states)[2:3] <- c("Deficit", "Surplus")
est.states$Date <- comb$Date

grep("Deficit", comb$NO_state)
grep("1", est.states$state)

est.states$state <- gsub("1", "Deficit", est.states$state)
est.states$state <- gsub("2", "Surplus", est.states$state)
tbl

# Plot the results
mycols <- c("darkmagenta", "turquoise")

g1 <- ggplot(comb, aes(x = Date, y = NO_state, fill = NO_state, col = NO_state)) + 
         geom_bar(stat = "identity", alpha = I(0.75)) + 
         scale_fill_manual(values = mycols, labels = c("Deficit", "Surplus")) +
         scale_color_manual(values = mycols, labels = c("Deficit", "Surplus")) +
         theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
         labs(y = "Actual State")

g2 <- ggplot(est.states, aes(x = Date, y = state, fill = state, col = state)) + 
  geom_bar(stat = "identity", alpha = I(0.75)) +
  scale_fill_manual(values = mycols, name = "State:\nProduction of\nNorway", labels = c("Deficit", "Surplus")) +
  scale_color_manual(values = mycols, name = "State:\nProduction of\nNorway", labels = c("Deficit", "Surplus")) +
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) + 
  labs(y = "Estimated State")

grid.arrange(g1, g2, 
             widths = 1, 
             nrow = 2)
