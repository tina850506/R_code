setwd("C:\\Users\\user\\Desktop\\R code")

RevenueCal = function(demand,cap,pri,cancel,noShow,refund,penalty)
{
  revenue <- c()
  Revenuelist <- list()
  onBoard <- c()
  accept <- c()
  
  for(i in 1:length(demand)){
    accept[i] = (cap/(1-cancel))/(1-noShow)
    onBoard[i] <- min(demand[i],accept[i])*(1-cancel)*(1-noShow)
    revenue[i] <- pri*min(demand[i],accept[i])-pri*min(demand[i],accept[i])*cancel*refund- (max(onBoard[i]-cap,0))^2*penalty
  }
  Revenuelist1 <- list(Demand=demand,Onboard=onBoard,Revenue=revenue)
  return(Revenuelist1)

}

  
penalty <- 48
Total_R <- c()
C1_number <- c()
C2_number <- c()

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

C1 <- 0
C2 <- 105
FareNumberCal = function(Faredemand_1,FarePrice_1,FareCancel_1,FareNoShow_1,FareRefund_1,Faredemand_2,FarePrice_2,FareCancel_2,FareNoShow_2,FareRefund_2){
  while(C1 <= 103 && C2 >= 0){ 
    Revenue1 = RevenueCal(Faredemand_1,C1,FarePrice_1,FareCancel_1,FareNoShow_1,FareRefund_1,penalty)
    #plot(Revenue1$Demand,Revenue1$Revenue)
    z1 <- cbind(Revenue1$Demand,Revenue1$Onboard,Revenue1$Revenue)
    C1 <- C1+1
    Revenue2 = RevenueCal(Faredemand_2,C2,FarePrice_2,FareCancel_2,FareNoShow_2,FareRefund_2,penalty)
    #plot(Revenue2$Demand,Revenue2$Revenue)
    z2 <- cbind(Revenue2$Demand,Revenue2$Onboard,Revenue2$Revenue)
    C2 <- C2-1
    Total <- z1[,3] + z2[,3] 
    
    Total_R <- c(Total_R,mean(Total))
    C1_number <- c(C1_number , C1)
    C2_number <-c(C2_number, C2)
    
  }
  TwoFare <- cbind(C1_number,C2_number,Total_R)
  return(TwoFare)
}


ComparisonC1C2 <- FareNumberCal(Faredemand_1 = demand1,FarePrice_1 = 1200,FareCancel_1 = 0.1,FareNoShow_1 = 0.1,FareRefund_1 = 1,Faredemand_2 = demand2,FarePrice_2 = 1000,FareCancel_2 = 0.08,FareNoShow_2 = 0.08,FareRefund_2 = 0.9)
ComparisonC1C3 <- FareNumberCal(Faredemand_1 = demand1,FarePrice_1 = 1200,FareCancel_1 = 0.1,FareNoShow_1 = 0.1,FareRefund_1 = 1,Faredemand_2 = demand3,FarePrice_2 = 900,FareCancel_2 = 0.05,FareNoShow_2 = 0.05,FareRefund_2 = 0.8)
ComparisonC1C4 <- FareNumberCal(Faredemand_1 = demand1,FarePrice_1 = 1200,FareCancel_1 = 0.1,FareNoShow_1 = 0.1,FareRefund_1 = 1,Faredemand_2 = demand4,FarePrice_2 = 800,FareCancel_2 = 0.03,FareNoShow_2 = 0.03,FareRefund_2 = 0.6)
ComparisonC1C5 <- FareNumberCal(Faredemand_1 = demand1,FarePrice_1 = 1200,FareCancel_1 = 0.1,FareNoShow_1 = 0.1,FareRefund_1 = 1,Faredemand_2 = demand5,FarePrice_2 = 600,FareCancel_2 = 0.01,FareNoShow_2 = 0.01,FareRefund_2 = 0.3)
ComparisonC2C3 <- FareNumberCal(Faredemand_1 = demand2,FarePrice_1 = 1000,FareCancel_1 = 0.08,FareNoShow_1 = 0.08,FareRefund_1 = 0.9,Faredemand_2 = demand3,FarePrice_2 = 900,FareCancel_2 = 0.05,FareNoShow_2 = 0.05,FareRefund_2 = 0.8)
ComparisonC2C4 <- FareNumberCal(Faredemand_1 = demand2,FarePrice_1 = 1000,FareCancel_1 = 0.08,FareNoShow_1 = 0.08,FareRefund_1 = 0.9,Faredemand_2 = demand4,FarePrice_2 = 800,FareCancel_2 = 0.03,FareNoShow_2 = 0.03,FareRefund_2 = 0.6)
ComparisonC2C5 <- FareNumberCal(Faredemand_1 = demand2,FarePrice_1 = 1000,FareCancel_1 = 0.08,FareNoShow_1 = 0.08,FareRefund_1 = 0.9,Faredemand_2 = demand5,FarePrice_2 = 600,FareCancel_2 = 0.01,FareNoShow_2 = 0.01,FareRefund_2 = 0.3)
ComparisonC3C4 <- FareNumberCal(Faredemand_1 = demand3,FarePrice_1 = 900,FareCancel_1 = 0.05,FareNoShow_1 = 0.05,FareRefund_1 = 0.8,Faredemand_2 = demand4,FarePrice_2 = 800,FareCancel_2 = 0.03,FareNoShow_2 = 0.03,FareRefund_2 = 0.6)
ComparisonC3C5 <- FareNumberCal(Faredemand_1 = demand3,FarePrice_1 = 900,FareCancel_1 = 0.05,FareNoShow_1 = 0.05,FareRefund_1 = 0.8,Faredemand_2 = demand5,FarePrice_2 = 600,FareCancel_2 = 0.01,FareNoShow_2 = 0.01,FareRefund_2 = 0.3)
ComparisonC4C5 <- FareNumberCal(Faredemand_1 = demand4,FarePrice_1 = 800,FareCancel_1 = 0.03,FareNoShow_1 = 0.03,FareRefund_1 = 0.6,Faredemand_2 = demand5,FarePrice_2 = 600,FareCancel_2 = 0.01,FareNoShow_2 = 0.01,FareRefund_2 = 0.3)

max(ComparisonC3C5[,3])
max(ComparisonC3C4[,3])
max(ComparisonC4C5[,3])
max(ComparisonC2C5[,3])
max(ComparisonC2C4[,3])
max(ComparisonC2C3[,3])

plot(x = ComparisonC1C2[,1],y = ComparisonC1C2[,3],xlab = "C1_num",ylab = "Revenue",type = "l",main = "C1 and C2",ylim = range(c(30000,80000)))
plot(x = ComparisonC1C3[,1],y = ComparisonC1C3[,3],xlab = "C1_num",ylab = "Revenue",type = "l" ,main = "C1 and C3",ylim = range(c(30000,80000)))
plot(x = ComparisonC1C4[,1],y = ComparisonC1C4[,3],xlab = "C1_num",ylab = "Revenue",type = "l" ,main = "C1 and C4",ylim = range(c(30000,80000)))
plot(x = ComparisonC1C5[,1],y = ComparisonC1C5[,3],xlab = "C1_num",ylab = "Revenue",type = "l" ,main = "C1 and C5",ylim = range(c(30000,80000)))
plot(x = ComparisonC2C3[,1],y = ComparisonC2C3[,3],xlab = "C2_num",ylab = "Revenue",type = "l" ,main = "C2 and C3",ylim = range(c(30000,80000)))
plot(x = ComparisonC2C4[,1],y = ComparisonC2C4[,3],xlab = "C2_num",ylab = "Revenue",type = "l" ,main = "C2 and C4",ylim = range(c(30000,80000)))
plot(x = ComparisonC2C5[,1],y = ComparisonC2C5[,3],xlab = "C2_num",ylab = "Revenue",type = "l" ,main = "C2 and C5",ylim = range(c(30000,80000)))
plot(x = ComparisonC3C4[,1],y = ComparisonC3C4[,3],xlab = "C3_num",ylab = "Revenue",type = "l" ,main = "C3 and C4",ylim = range(c(30000,80000)))
plot(x = ComparisonC3C5[,1],y = ComparisonC3C5[,3],xlab = "C3_num",ylab = "Revenue",type = "l" ,main = "C3 and C5",ylim = range(c(30000,80000)))
plot(x = ComparisonC4C5[,1],y = ComparisonC4C5[,3],xlab = "C4_num",ylab = "Revenue",type = "l" ,main = "C4 and C5",ylim = range(c(30000,80000)))