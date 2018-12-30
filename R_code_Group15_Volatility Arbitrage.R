#Read dataframe
df = read.csv("~/Desktop/output.csv") # Our option price dataset
price = read.csv("~/Desktop/price_.csv") # Price data of SPY

price[,1]=as.Date(as.character(price[,1]),format="%m/%d/%Y")
df[,1]=as.Date(as.character(df[,1]),format="%Y/%m/%d")
df[,2]=as.Date(as.character(df[,2]),format="%Y/%m/%d")

#Function calculate delta
delta = function(S,K,vol,t,Rf){
  d1 = (log(S/K) + (Rf+vol^2/2)*t)/(vol*sqrt(t))
  return(pnorm(d1))
}


#Calculating delta, daily stock positions, cash flows and PnL
P_L=c()
contain = list()
for(z in 1:nrow(df)){
  start = which(df[z,1]==price[,1])  #Start date of price series
  end = which(df[z,2]==price[,1]) #End date of price series
  DF = price[start:end,]
  DF$K = df[z,"K"]
  time=c()
  for(i in 1:nrow(DF)){
    time[i] = as.numeric(DF[nrow(DF),1]-DF[i,1])/365
  }
  DF$t=time
  DF$Rf = 0.025
  
  #Calculating daily delta
  del=c()
  del[1]=df[z,"Delta.of.the.Option"]
  for(j in 2:nrow(DF)-1){
    S = DF[j,"Price"]
    K = DF[j,"K"]
    vol = DF[j,"implied_vol"]
    t = DF[j,"t"]
    Rf = DF[j,"Rf"]
    del[j]= delta(S,K,vol,t,Rf)
  }
  del[nrow(DF)]=0
  DF$delta = del
  
  #Calculating SPY shares inflow / outflow
  share_flow=c()
  if(df[z,"Signal"]=="Short"){
    share_flow[1]=DF$delta[1]*100
    share_flow=c(share_flow,100*diff(DF$delta))
  }
  else{
    share_flow[1]=-DF$delta[1]*100
    share_flow=c(share_flow,-100*diff(DF$delta))
  }
  
  share_flow[nrow(DF)]=0
  
  
  DF$share_flow=share_flow
  DF$pos = cumsum(DF$share_flow)
  
  #Calculating dynamic hedging cost
  Dynamic_cost=c()
  for(k in 1:nrow(DF)-1){
    Dynamic_cost[k]=DF$share_flow[k]*DF$Price[k]*(-1)
  }
  Dynamic_cost[nrow(DF)]=0
  DF$Dynamic_cost = Dynamic_cost
  
  #Calculating profit and losses
  P_L[z] = sum(Dynamic_cost)+DF$Price[nrow(DF)]*(DF$pos[nrow(DF)-1])+100*(max(DF$Price[nrow(DF)]-DF$K[nrow(DF)],0)-df[z,"Call"])
  contain[[z]]=DF
}

#Calculating the total cost of trading
Cost=c()
long=c()
short=c()
for(i in 1:nrow(df)){
  dyna = -sum(contain[[i]]$Dynamic_cost[which(contain[[i]]$Dynamic_cost<0)])
  short = contain[[i]]$Price[1]*contain[[i]]$delta[1]*100
  long = -contain[[i]]$Price[length(contain[[i]]$Price)]*contain[[i]]$pos[length(contain[[i]]$pos)]
  if(df[i,"Signal"]=="Short"){
    Cost[i] = dyna+short
  }
  else{
    Cost[i] = dyna+long
  }
}

df$P_L=P_L
df$Cost = Cost
#Output dataframe
write.csv(df,"Rehedged.csv")

##Read return and cumulative return of strategy and plot PnL
df = read.csv("~/Desktop/RTN.csv")
library(xts)
library(PerformanceAnalytics)
library(dplyr)
library(dygraphs)
RTN = xts(df$RTN,as.Date(df$Date))
colnames(RTN) = "return"
CumRTN = xts(df$CumRTN,as.Date(df$Date))
colnames(CumRTN) = "cumulative return"
dygraph(merge(RTN,CumRTN))
