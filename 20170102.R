RevenueCal = function(up,lo,cap,pri,cancel,noShow,refund,times,demand_mean,demand_sd,penalty)
{
  revenue <- c()
  Revenuelist <- list()
  onBoard <- c()
  accept <- c()
  
  for(i in 1:times){
    repeat{
      demand[i] <- rnorm(1,mean = demand_mean,sd = demand_sd)
      if(demand[i] < up && demand[i] > lo ) break 
    }
    accept[i] = (cap/(1-cancel))/(1-noShow)
    onBoard[i] <- min(demand[i],accept[i])*(1-cancel)*(1-noShow)
    revenue[i] <- pri*min(demand[i],accept[i])-pri*min(demand[i],accept[i])*cancel*refund- (max(onBoard[i]-cap,0))^2*penalty
  }
  Revenuelist1 <- list(Demand=demand,Onboard=onBoard,Revenue=revenue)
  return(Revenuelist1)
}

#par( mfrow = c(3,4))
#values for variables

demand <- 0  
penalty <- 48
Total_R <- c()
C1_number <- c()
C2_number <- c()
C3_number <- c()
Diff <- c()

C1 <- 15
C2 <- 45
C3 <- 50
while(C1 <= 30 && C2 >= 30){ 
  Revenue1 = RevenueCal(35,15,C1,1200,0.1,0.1,1,100,22,8.4,penalty)
  #plot(Revenue1$Demand,Revenue1$Revenue)
  z1 <- cbind(Revenue1$Demand,Revenue1$Onboard,Revenue1$Revenue)
  C1 <- C1+1
  Revenue2 = RevenueCal(68,30,C2,900,0.05,0.1,0.8,100,39.8,28.1,penalty)
  #plot(Revenue2$Demand,Revenue2$Revenue)
  z2 <- cbind(Revenue2$Demand,Revenue2$Onboard,Revenue2$Revenue)
  C2 <- C2-1
  Revenue3 = RevenueCal(100,45,C3,600,0.01,0.1,0.3,100,55.5,13.6,penalty)
  z3 <- cbind(Revenue3$Demand,Revenue3$Onboard,Revenue3$Revenue)

  Total <- z1[,3] + z2[,3] + z3[,3]
  Total_R <- c(Total_R,mean(Total))
  C1_number <- c(C1_number , C1)
  C2_number <-c(C2_number, C2)
  C3_number <-c(C3_number , C3)
  d1 = z1[,1]
  d2 = z2[,1]
}

Diff <- C2_number - C1_number
TwoFare <- cbind(C1_number,C2_number,C3_number,Total_R)
d1
d2