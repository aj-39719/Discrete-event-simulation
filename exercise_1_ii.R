# Clear your environment of variables
rm(list = ls())
set.seed(1)

T = 12

M = 300 # monthly charge per customer

lambda = 3

mu = 1/12

alpha = (3.5/12)

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
    amt = 300*X + 500
  }
  else{
    amt = 0
  }
  return(amt)
}

# Exercise 1(ii): we want 1.65d = 500 => d = 500/1.65 (for Exercise 1(iii) see the end)
d = 500/1.65

# We will run this for at least 100 and until S_k/sqrt(k) < d,
# where S_k is the sample standard deviation

# I will use the function gen_sample_mean_var to iteratively calculate
# the sample mean and sample variance,
# This next piece of code was taken from Dr. Papadaki's solution to mock project

gen_sample_mean_var = function(n, Z, Z_bar, S_sq){
  
  # This function updates sample and mean and sample variance when the new
  # simulation value is available.
  
  # It takes as input Z_bar,S_sq,n the sample mean, sample variance and the number
  # of simulation observations they are based on (n). It also takes as input the 
  # new observation Z.
  
  Z_bar_new = Z_bar + (Z-Z_bar)/(n+1)
  
  S_sq_new = (1-(1/n))*S_sq + (n+1)*(Z_bar_new - Z_bar)^2
  
  return(c(Z_bar_new,S_sq_new))
}


# Initialize output variables
amount_tuple = c() 
count = 0
# we store these for each iteration.

flag = 1
k = 1 # counting iterations

while(flag){
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
  
  amount = c_0 + revenue - claim
  amount_tuple = c(amount_tuple,amount)

  
  ### Here we update the sample mean and sample variance,
  ### Code referred from Dr. Papadaki's solution for mock project
  if (k == 1){
    amount_bar = mean(amount_tuple)
    Var_k = 0
    
  } else{
    out = gen_sample_mean_var(n = k-1,Z = amount,Z_bar = amount_bar,S_sq = Var_k)
    amount_bar = out[1]
    Var_k = out[2]
    
  }
  
  ### We check the stopping conditions
  if ((k >= 100) & (sqrt(Var_k/k) < d)){
    flag = 0
  }
  k = k + 1
  
} # end of while(flag)

### for exercise 1 (ii)

print(paste("The estimator for capital at the end of 12 months is ", round(amount_bar, digits =4)))
print(paste("The sample standard deviation of the capital estimate is ", round(sqrt(Var_k/k),digits=4)))
print(paste("The number of iterations it ran was ", k))
