# Clear your environment of variables
rm(list = ls())
set.seed(1)
# We will use the rejection method to generate a discreet random variable
# X that takes values in 0,1,2,..., 10 with probabilities:

s = 0.6321311
p = c(s,s*exp(-1),s*exp(-2), s*exp(-3),s*exp(-4),s*exp(-5),s*exp(-6),s*exp(-7),s*exp(-8),s*exp(-9),s*exp(-10))

# We will do it from a Uniform discrete in {0,..,10} with mass q:

q =  replicate(11, 1/11)

# Find value of c such that c <= p(j)/q(j) for all j
# Setting it to the maximum of this ratio would work

c = max(p/q) # creates a vector p/q and takes its maximum


n = 11
x = c(0,1,2,3,4,5,6,7,8,9,10)

acceptance_ratio = exp(-x)


x_tuple = c()
counter_tuple = c()

for(i in 1:10000){
flag = 1
counter = 0

while(flag){
  counter = counter + 1
  # First generate a Uniform discrete random variable in {0,1,2,...,10}
  # as we have done before:
  
  U1 = runif(1)
  Y = floor(n*U1) 
  
  # Generate another Uniform(0,1)
  U2 = runif(1)
  
  # Check if the condition is met
  
  K = p[Y+1]/(c*q[Y+1])
  # We can also use K = acceptance_ratio[Y+1], instead of above
  
  if (U2 <= K) {
    X = Y
    flag = 0
  }
}

x_tuple = c(x_tuple,X)
counter_tuple = c(counter_tuple,counter)

}

hist(x_tuple,c(0,1,2,3,4,5,6,7,8,9,10))

claim_amt = (300*x_tuple + 500)

hist(claim_amt,
     col = "darkblue",
     main = "Histogram for Team 1 Claim Amount",
     xlab = "Claim Amount",
     ylim = c(0,7000),
     border = "white",
     labels = TRUE)

