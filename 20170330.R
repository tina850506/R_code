
#install.packages("xlsx")
#library(xlsx)

RevenueCal = function(demand,cap,pri,cancel,noShow,refund,penalty,overbook)
{
  revenue <- c()
  Revenuelist <- list()
  onBoard <- c()
  accept <- c()
  penalty = c() 
  
  for(i in 1:length(demand)){
    accept[i] = (cap/(1-cancel))/(1-noShow)+overbook*cap
    onBoard[i] <- min(demand[i],accept[i])*(1-cancel)*(1-noShow)
    penalty[i] = (max(onBoard[i]-cap,0))^2*p
    revenue[i] <- pri*min(demand[i],accept[i])-pri*min(demand[i],accept[i])*cancel*refund- penalty[i]
  }
  Revenuelist1 <- list(Demand=demand,Onboard=onBoard,Revenue=revenue,Accept=accept,Penalty = penalty, Overbook= overbook)
  return(Revenuelist1)

}

  

demand1 <- read.csv(file = "fare1.csv", header = T)$x
#Fare(1200)
demand2 <- read.csv(file = "fare2.csv", header = T)$x
#Fare(1000)
demand3 <- read.csv(file = "fare3.csv", header = T)$x
#Fare(900)
demand4 <- read.csv(file = "fare4.csv", header = T)$x
#Fare(800)
demand5 <- read.csv(file = "fare5.csv", header = T)$x
#Fare(600)

#C3C4 Capacity Change#
p <- 48  #penalty 係數
Total_R <- c()
PenaltyMean <- c()
C_number <- c()
C2_number <- c()

FareNumberCal = function(C2Cap,Faredemand_1,FarePrice_1,FareCancel_1,FareNoShow_1,FareRefund_1,FareOverB_1,Faredemand_2,FarePrice_2,FareCancel_2,FareNoShow_2,FareRefund_2,FareOverB_2){
  C1 <- 0
  C2 <- C2Cap
  while( C2 > 0){ 
    Revenue1 = RevenueCal(Faredemand_1,C1,FarePrice_1,FareCancel_1,FareNoShow_1,FareRefund_1,penalty,FareOverB_1)
    #plot(Revenue1$Demand,Revenue1$Revenue)
    z1 <- cbind(Revenue1$Demand,Revenue1$Onboard,Revenue1$Revenue,Revenue1$Accept,Revenue1$Penalty)
    C1 <- C1+1
    Revenue2 = RevenueCal(Faredemand_2,C2,FarePrice_2,FareCancel_2,FareNoShow_2,FareRefund_2,penalty, FareOverB_2)
    #plot(Revenue2$Demand,Revenue2$Revenue)
    z2 <- cbind(Revenue2$Demand,Revenue2$Onboard,Revenue2$Revenue,Revenue2$Accept,Revenue2$Penalty)
    C2 <- C2-1
    Total <- z1[,3] + z2[,3] 
    Total_R <- c(Total_R,mean(Total))
    Penalty <- z1[,5] + z2[,5]
    PenaltyMean <- c(PenaltyMean,mean(Penalty))
    C1_number <- c(C1_number , C1)
    C2_number <-c(C2_number, C2)

    
  }

  TwoFare <- cbind(C1_number,C2_number,Total_R, PenaltyMean,FareOverB_1,FareOverB_2)
  TwoFDataFrame <- data.frame(TwoFare)
  return(TwoFDataFrame)
}


#C1 Capacity Change (Keep the Max)#
ChangeFareCap = function(RunTimes,C2Cap,Faredemand_1,FarePrice_1,FareCancel_1,FareNoShow_1,FareRefund_1,FareOverB_1,Faredemand_2,FarePrice_2,FareCancel_2,FareNoShow_2,FareRefund_2,FareOverB_2){
  DataChange <-c()  
  for (k in 1:RunTimes){
    Comparison <- FareNumberCal(k,Faredemand_1 ,FarePrice_1, FareCancel_1, FareNoShow_1, FareRefund_1, FareOverB_1, Faredemand_2, FarePrice_2, FareCancel_2, FareNoShow_2, FareRefund_2, FareOverB_2)
    MaxCom <- Comparison[ Comparison[,3] ==max(Comparison[,3]),]
    #m[m[, "three"] == 11,]
    DataChange <- rbind.data.frame(DataChange,MaxCom) 
  }
  
  return(DataChange)
}
C34<- ChangeFareCap(RunTimes=100,C2Cap=100,Faredemand_1 = demand3,FarePrice_1 = 900,FareCancel_1 = 0.05,FareNoShow_1 = 0.15,FareRefund_1 = 0.8,FareOverB_1 = 0.2 ,Faredemand_2 = demand4,FarePrice_2 = 800,FareCancel_2 = 0.03,FareNoShow_2 = 0.15,FareRefund_2 = 0.6, FareOverB_2 = 0.2)
C35<- ChangeFareCap(RunTimes=100,C2Cap=100,Faredemand_1 = demand3,FarePrice_1 = 900,FareCancel_1 = 0.05,FareNoShow_1 = 0.15,FareRefund_1 = 0.8,FareOverB_1 = 0.2 ,Faredemand_2 = demand4,FarePrice_2 = 600,FareCancel_2 = 0.01,FareNoShow_2 = 0.15,FareRefund_2 = 0.3, FareOverB_2 = 0.2)
C45<- ChangeFareCap(RunTimes=100,C2Cap=100,Faredemand_1 = demand3,FarePrice_1 = 800,FareCancel_1 = 0.03,FareNoShow_1 = 0.15,FareRefund_1 = 0.6,FareOverB_1 = 0.2 ,Faredemand_2 = demand4,FarePrice_2 = 600,FareCancel_2 = 0.01,FareNoShow_2 = 0.15,FareRefund_2 = 0.3, FareOverB_2 = 0.2)

#Calculate C1 Revenue#
C3 <- 100
C3_number <- c()
Rev3 <- c()
R_Three <- c()

Three_Fare = function(Faredemand_3,FarePrice_3,FareCancel_3,FareNoShow_3,FareRefund_3,FareOverB_3){
  while (C3 >= 1){
    C3 <- C3-1
    Revenue3 = RevenueCal(Faredemand_3,C3,FarePrice_3,FareCancel_3,FareNoShow_3,FareRefund_3,penalty,FareOverB_3)
    #plot(Revenue1$Demand,Revenue1$Revenue)
    z3 <- cbind(Revenue3$Demand,Revenue3$Onboard,Revenue3$Revenue,Revenue3$Accept,Revenue3$Penalty)
    Rev3 <- c(Rev3,mean(z3[,3]))
    C3_number <- c(C3_number,C3)
  }
  ThreeFare <- cbind(C3_number,Rev3)
  ThreeFDataFrame <- data.frame(ThreeFare)
  return(ThreeFDataFrame)
}
C1_1200 <- Three_Fare(Faredemand_3=demand1,FarePrice_3 = 1200,FareCancel_3 = 0.1,FareNoShow_3 = 0.15,FareRefund_3 = 1,FareOverB_3 = 0)
C2_1000 <- Three_Fare(Faredemand_3=demand2,FarePrice_3 = 1000,FareCancel_3 = 0.08,FareNoShow_3 = 0.15,FareRefund_3 = 0.9,FareOverB_3 = 0)

#Three Fare Comparison to xlsx#
C134 <- cbind.data.frame(C1_1200,C34)
C134 <- cbind(C134 , C134$Rev3+C134$Total_R)
write.xlsx(C134,"C:\\Users\\user\\Desktop\\R code\\C1comparision.xlsx","C134" , row.names = F)

C135 <- cbind.data.frame(C1_1200,C35)
C135 <- cbind(C135 , C135$Rev3+C135$Total_R)
write.xlsx(C135,"C:\\Users\\user\\Desktop\\R code\\C1comparision.xlsx","C135" ,row.names = F,append=TRUE)

C145 <- cbind.data.frame(C1_1200,C45)
C145 <- cbind(C145 , C145$Rev3+C145$Total_R)
write.xlsx(C145,"C:\\Users\\user\\Desktop\\R code\\C1comparision.xlsx","C145",row.names = F,append=TRUE)


C234 <- cbind.data.frame(C2_1000,C34)
C234 <- cbind(C234, C234$Rev3+C234$Total_R)
write.xlsx(C234,"C:\\Users\\user\\Desktop\\R code\\C2comparision.xlsx","C234",row.names = F)

C235 <- cbind.data.frame(C2_1000,C35)
C235 <- cbind(C235 , C235$Rev3+C235$Total_R)
write.xlsx(C235,"C:\\Users\\user\\Desktop\\R code\\C2comparision.xlsx","C235" ,row.names = F,append=TRUE)

C245 <- cbind.data.frame(C2_1000,C45)
C245 <- cbind(C245 , C245$Rev3+C245$Total_R)
write.xlsx(C245,"C:\\Users\\user\\Desktop\\R code\\C2comparision.xlsx","C245" ,row.names = F,append=TRUE)

