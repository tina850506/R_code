
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
demand1 <- c(17,29,17,17,19)
demand2 <- c(36,44,29,38,39)

C1 <- 15
C2 <- 45

while(C1 <= 30 && C2 >= 40){ 
  Revenue1 = RevenueCal(demand1,C1,1200,0.1,0.1,1,penalty)
  #plot(Revenue1$Demand,Revenue1$Revenue)
  z1 <- cbind(Revenue1$Demand,Revenue1$Onboard,Revenue1$Revenue)
  C1 <- C1+1
  Revenue2 = RevenueCal(demand2,C2,900,0.05,0.1,0.8,penalty)
  #plot(Revenue2$Demand,Revenue2$Revenue)
  z2 <- cbind(Revenue2$Demand,Revenue2$Onboard,Revenue2$Revenue)
  C2 <- C2-1
  Total <- z1[,3] + z2[,3] 
  
  Total_R <- c(Total_R,mean(Total))
  C1_number <- c(C1_number , C1)
  C2_number <-c(C2_number, C2)
  
}


TwoFare <- cbind(C1_number,C2_number,Total_R)
TwoFare