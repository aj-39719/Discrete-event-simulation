# Clear your environment of variables
rm(list = ls())

# Set seed
set.seed(1)

T = 12

M = 300 # monthly charge per customer

lambda = 3

mu = 1/12

alpha = (3.5/12) # since rate of accident was given per year, we divide by 12

C = 30000 # Capital limit

c_0 = 50000 # Initial capital

mu_f = function(t){
  
  return(1/(12+t))
  
}

time_next_departure = function(s, mu){
  t = s
  flag = 1
  
  while(flag){
    U1 = runif(1)
    t = t - (1/mu) * log(U1)
    
    U2 = runif(1)
    
    if (U2 <= mu_f(t)/mu){
      T_s = t
      flag = 0
    }
  }
  
  return(T_s)
}

time_next_arrival = function(s,lambda){
  t = s
  U = runif(1)
  t =  t - (1/lambda) * log(U) 
  return(t)
  
}

time_next_accident = function(s,alpha){
  t_accident = s
  U = runif(1)
  t_accident =  t_accident - (1/alpha) * log(U) 
  return(t_accident)
  
}

generate_claim = function(){
  # The chance of claim is 60%, so we generate a uniform random number and see if it has value less than 0.6.
  # If it is, then we generate discrete uniform random number between 1 and 10 to calculate claim amount
  # else we return 0, since no amount is claimed.
  
  P = runif(1)
  
  if(P<0.6){
    # Code for payout in case of accident claim
    # Payout amount is 300X + 500
    X = floor(runif(1)*11)
    claim_amt = 300*X + 500
  }
  else{
    claim_amt = 0
  }
  return(claim_amt)
}

count = 0

amount_tuple = c()

for (j in 1:1000){
  # Initialize variables
  
  t = 0 # time
  n = 0 # number of customers in Carsafe
  N_A = 0 # number of arrivals
  N_D = 0 # number of departures
  N_X = 0 # number of accidents encountered by Carsafe customers during the year
  
  T_A = c() # A vector for storing arrival times
  
  T_D = c() # A vector for storing departure times
  
  T_X = c() # A vector for storing accident times
  
  
  
  # ******* Case 1 - Generating Arrival Times *********
  
  t_A = time_next_arrival(t,lambda)
  
  while(t < T){
    
    t_A = time_next_arrival(t,lambda)
    
    if(t_A >12){
      #do nothing as we dont need arrivals for next year
      }
    
    
    else{
      
      n = n + 1 # increment number of customers in the system
      
      N_A = N_A + 1 # increment the number of arrivals in the system
      
      T_A = c(T_A,t_A) # Store the time of arrivals
    }
    # set t = new arrival time for generation of next one in the loop
    t=t_A
  }
  
  # ******* Case 2 - Generating Departures Times *********
  
  for(i in 1:length(T_A)){
    
    # We generate a departure time for each arrival of customer.
    # The departure times are non-homogeneous poisson process with rate mu(t) = 1/(12+t)
    
    t_D = time_next_departure(T_A[i],mu)
    
    if (t_D < 12){ #if the generated time is less than 12 we keep it. 
      
      n = n - 1 # reduce number of customers in the system
      
      N_D = N_D + 1 # increment the number of customers departed
      
      T_D = c(T_D,t_D) # Store the time of departures
      
    }
    else{ # we set the departure time to 12, assuming the customer doesn't leave in that year
      
      T_D = c(T_D,12)
    }
    
  }

  # ******* Case 3 - Generating Accident Times *********
  
  for(i in 1:length(T_A)){
    
    # We use arrival times as a starting point for generating accidents for each customer
    
    t_X = time_next_accident(T_A[i],alpha)

    if(t_X<T_D[i]){
      N_X = N_X + 1
      T_X = c(T_X,t_X)
      
      while (t_X<T_D[i])
        { 
          t_X = time_next_accident(t_X,alpha)
          if(t_X < T_D[i]){
            T_X = c(T_X,t_X)
            N_X = N_X + 1
            
          }
      
        }
    }else{
       
    }
  }
  
  
  # Calculate time each customer spends in Carsafe
  time_in_system = T_D - T_A
  
  # Calculating revenue from each customer, based on time spent
  revenue = sum(time_in_system*M)
  
  claim = 0
  
  for(i in 1:length(T_X)){
    
    claim = claim + generate_claim()
    
  }

  amount = c_0 + revenue - claim # here, c_0 =50000
  
  amount_tuple = c(amount_tuple,amount) # update the list containing amount at the end of year
  
  if (amount < C){
    count = count +1 # count occurences where amount goes below 30000
  }
  
} #end of for loop



# Generating our event list 

event_list = matrix(c(T_A, T_D), nrow=length(T_A), ncol=2)

# making a copy of our output variables

TA_copy = T_A 
TD_copy = T_D

# starting from t = 0
t = 0
n = 0

# We record the statte time pairs as follows - 
stpair = matrix(c(n,t),nrow = 1, ncol = 2)

# We append minimum arrival/departure times and store our event list and st-pairs as follows-

min_arrival = min(TA_copy)
min_depart = min(TD_copy)

flag = 1

while(flag){

if (min_arrival<min_depart){
  n = n+1 # increment state
  stpair = rbind(stpair,c(n,min_arrival)) # append st pair list
  TA_copy = TA_copy[!(TA_copy == min_arrival)] # remove time of arriaval from list
  min_arrival = min(TA_copy) # update value of time of arrival
}

  else if(min_depart<min_arrival){
    n = n-1 # decrement state
    stpair = rbind(stpair,c(n,min_depart)) # append st pair list
    TD_copy = TD_copy[!(TD_copy == min_depart)] # remove time of departure of list
    min_depart = min(TD_copy) #update value of time of departure
  }
  else{
    flag = 0
  }
}


print(paste("The probability of capital going below 30000 at the end of year is", count/1000))

