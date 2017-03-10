setwd("C:\\Users\\user\\Desktop\\R code")

demandCal = function(up,lo,times,demand_mean,demand_sd)
{
  demandlist <- list()
  demand <- c()
  for(i in 1:times){
    repeat{
      demand[i] <- rnorm(1,mean = demand_mean,sd = demand_sd)
      if(demand[i] < up && demand[i] > lo ) break 
    }
}
  demandlist <- demand
  return(demandlist)
}
fare1 <- demandCal(up= 35,lo= 15,times= 1000,demand_mean = 22,demand_sd =8.4)
write.csv(fare1, "C:\\Users\\user\\Desktop\\R code\\fare1.csv",row.names = F, col.names = F)
fare2 <- demandCal(up = 50,lo = 28,times = 1000,demand_mean = 35.6,demand_sd = 16.3)
write.csv(fare2, "C:\\Users\\user\\Desktop\\R code\\fare2.csv",row.names = F, col.names = F)
fare3 <- demandCal(up = 68,lo = 30,times = 1000,demand_mean = 39.8,demand_sd = 28.1)
write.csv(fare3, "C:\\Users\\user\\Desktop\\R code\\fare3.csv",row.names = F, col.names = F)
fare4 <- demandCal(up = 80,lo = 35,times = 1000,demand_mean = 50.4,demand_sd = 10.4)
write.csv(fare4, "C:\\Users\\user\\Desktop\\R code\\fare4.csv",row.names = F, col.names = F)
fare5 <- demandCal(up = 100,lo = 45,times = 1000,demand_mean = 55.5,demand_sd = 13.6)
write.csv(fare5, "C:\\Users\\user\\Desktop\\R code\\fare5.csv",row.names = F, col.names = F)

