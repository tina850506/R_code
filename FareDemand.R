FareDemand = function(lo,up,times,demand_mean,demand_sd)
{
  
  for(i in 1:times){
    repeat{
      demand[i] <- rnorm(1,mean = demand_mean,sd = demand_sd)
      if(demand[i] < up && demand[i] > lo ) break 
    }

  }
  Demand <- c(demand)
  return(Demand)
}
demand <- 0
Demand1 = FareDemand(15,35,10,22,8.4)
Demand1
Demand2 = FareDemand(28,50,10,35.6,16.3)
Demand2
Demand3 = FareDemand(30,68,10,39.8,28.1)
Demand3
Demand4 = FareDemand(35,80,10,50.4,10.4)
Demand4
Demand5 = FareDemand(45,100,10,55.5,13.6)
Demand5