# Clear your environment of variables
rm(list = ls())

# Set the data
set.seed(1)

# Scroll to bottom for Exercise 2(iv)

# Exercise 2b(iii): 

N = 10000 # number of steps I run the Markov Chain

L = floor(N/2) # burn-out steps

n = 0 # the stage of the Markov Chain

# We generate uniform for the initial state

X_current = runif(1) # current state

X = c(X_current) # we store it

n = n + 1

while (n <= N){
  # We generate a state from the proposal chain
  # The proposal chain here is iid uniforms
  
  X_prop = runif(1) #proposed state
  
  # We accept or reject this proposal 
  # We calculate the acceptance probability
  
  ratio = (X_prop * exp(-(X_prop^2)))/(X_current * exp(-(X_current^2)))
  
  prob_accept = min(ratio,1) #probability of acceptance
  
  U = runif(1) # uniform random variable for comparison 
  
  if (U < prob_accept){ # we accept
    
    X_current = X_prop
    
  }
  
  # if we reject the X_current stays the same
  
  n = n + 1 # move to next markov stage
  
  X = c(X,X_current) #store the current value
  
}# end of steps

print(X_current)
