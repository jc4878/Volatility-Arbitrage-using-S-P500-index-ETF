VVVV <- read.csv("~/Desktop/VVV.csv")
DATA = read.csv("~/Desktop/DATA.csv")
price = read.csv("~/Desktop/price.csv")

Start = as.Date(as.character(DATA[,2]),format="%m/%d/%Y")
End = as.Date(as.character(DATA[,3]),format="%m/%d/%Y")
diff = as.numeric(End-Start)

DATA = DATA[,c(2,3,4,6,9,10)]
DATA$diff = diff

price[,1]=as.Date(as.character(price[,1]),format="%m/%d/%Y")
VVVV[,1]=as.Date(as.character(VVVV[,1]),format="%m/%d/%Y")
DATA[,1]=as.Date(as.character(DATA[,1]),format="%m/%d/%Y")
DATA[,2]=as.Date(as.character(DATA[,2]),format="%m/%d/%Y")




output = data.frame()
for(i in 1:length(VVVV[,1])){
  dat = DATA[which(DATA[,1]==VVVV[i,1]),]
  dat = dat[which(is.na(dat[,"S"])==FALSE),]
  dat = dat[which(is.na(dat[,"Delta.of.the.Option"])==FALSE),]
  dat = dat[which(dat[,"diff"]==max(dat[,"diff"])),]
  dat = dat[which(abs(dat[,"S"]-VVVV[i,2])==min(abs(dat[,"S"]-VVVV[i,2]))),]
  dat = dat[1,]
  
  output = rbind(output,dat)
  
}

#final = cbind(output,VVVV)


bgn_S = c()
end_S = c()
for(i in 1:nrow(output)){
  bgn_S[i] = price[which(as.character(price[,1])==as.character(output[i,1])),2]
  end_S[i] = price[which(as.character(price[,1])==as.character(output[i,2])),2]
}
output$bgn_S = bgn_S
output$end_S = end_S

P_L = c()
for(i in 1:nrow(output)){
  delta = output$Delta.of.the.Option[i]
  St     = output$end_S[i]
  S0     = output$bgn_S[i]
  K      = output$S[i]
  Call   = output$Price[i]
  
  if(VVVV$Signal[i]=="Long"){
      P_L[i] = (-Call+max(St-K,0))*100 + delta*100*(S0-St)
    }
  else{
      P_L[i] = -((-Call+max(St-K,0))*100 + delta*100*(S0-St))
  }
}
output$P_L=P_L

write.csv(output,"result.csv")





