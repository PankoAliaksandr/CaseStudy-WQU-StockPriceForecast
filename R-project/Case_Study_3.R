# Case Study 3 "Normal Distributions and Stock Price"

#Step 1. Implement function to calculate stock prices

# FUNCTION: Calculate Stock Price
# ----------------------------------------------------------------------------------------------------------
Calculate_Stock_Price = function( stock_price_initial,return_annual, volatility_annual, time_total,
                                  number_of_steps, epsilon){
  
  stock_price_previous = stock_price_initial
  time_interval        = time_total / number_of_steps
  
  expected_stock_value <- c(stock_price_initial) 
  
  # epsilon single value VS epsilon vector
  epsilon_lenght = length(epsilon)
  
  if( epsilon_lenght == 1 ){
    #single case
    
    for(i in 1:number_of_steps){
      # calculate new stock price
      stock_price_new = stock_price_previous * exp( (return_annual - (1/2 * volatility_annual^2)) * time_interval
                                                    + volatility_annual * epsilon * sqrt(time_interval) )  
      # save the value in vector
      expected_stock_value[i+1] =  stock_price_new
      
      # make calculated stock price as previous 
      stock_price_previous = stock_price_new
    }
    
  }
  else{
    #epsilon is a vector
    
    for(i in 1:number_of_steps){
      # calculate new stock price
      stock_price_new = stock_price_previous * exp( (return_annual - (1/2 * volatility_annual^2)) * time_interval
                                                    + volatility_annual * epsilon[i] * sqrt(time_interval) )  
      # save the value in vector
      expected_stock_value[i+1] =  stock_price_new
      
      # make calculated stock price as previous 
      stock_price_previous = stock_price_new
    }
  }
  
  return(expected_stock_value)
}

# --------------------------------End of the function Calculate_Stock_Price--------------------------------------

# Step 2. Use function with "epsilon" = single constant value

#Initial Data:

stock_price_initial = 10  
return_annual       = 0.15      
volatility_annual   = 0.20
time_total          = 1
number_of_steps     = 100
epsilon             = 0.15

expected_stock_price = Calculate_Stock_Price(stock_price_initial = stock_price_initial, return_annual = return_annual,
                                             volatility_annual = volatility_annual, time_total = time_total,
                                             number_of_steps = number_of_steps, epsilon = epsilon)

# Step 3. Use function with "epsilon" = random value from a standard normal distribution.

# Generate random epsilon from standard normal distribution
ratio_v <- c()

for(k in 1:5000){

# Create empty matrix for 5 trials  
epsilon_st_norm_matrix = matrix(nrow = 100, ncol = 5)

# Generate 5 vectors for epsilon
epsilon_st_norm_matrix[,1] = rnorm(100, mean = 0, sd = 1)   
epsilon_st_norm_matrix[,2] = rnorm(100, mean = 0, sd = 1)
epsilon_st_norm_matrix[,3] = rnorm(100, mean = 0, sd = 1)   
epsilon_st_norm_matrix[,4] = rnorm(100, mean = 0, sd = 1)   
epsilon_st_norm_matrix[,5] = rnorm(100, mean = 0, sd = 1)

# Create empty matrix for prices calculations
expected_stock_price_matrix_n = matrix(nrow = 101, ncol = 5)

# Trial 1
expected_stock_price_matrix_n[,1] = Calculate_Stock_Price(stock_price_initial = stock_price_initial, return_annual = return_annual,
                                                        volatility_annual = volatility_annual, time_total = time_total,
                                                        number_of_steps = number_of_steps, epsilon = epsilon_st_norm_matrix[,1])

# Trial 2
expected_stock_price_matrix_n[,2] = Calculate_Stock_Price(stock_price_initial = stock_price_initial, return_annual = return_annual,
                                                        volatility_annual = volatility_annual, time_total = time_total,
                                                        number_of_steps = number_of_steps, epsilon = epsilon_st_norm_matrix[,2])

# Trial 3
expected_stock_price_matrix_n[,3] = Calculate_Stock_Price(stock_price_initial = stock_price_initial, return_annual = return_annual,
                                                        volatility_annual = volatility_annual, time_total = time_total,
                                                        number_of_steps = number_of_steps, epsilon = epsilon_st_norm_matrix[,3])

# Trial 4
expected_stock_price_matrix_n[,4] = Calculate_Stock_Price(stock_price_initial = stock_price_initial, return_annual = return_annual,
                                                        volatility_annual = volatility_annual, time_total = time_total,
                                                        number_of_steps = number_of_steps, epsilon = epsilon_st_norm_matrix[,4])

# Trial 5
expected_stock_price_matrix_n[,5] = Calculate_Stock_Price(stock_price_initial = stock_price_initial, return_annual = return_annual,
                                                        volatility_annual = volatility_annual, time_total = time_total,
                                                        number_of_steps = number_of_steps, epsilon = epsilon_st_norm_matrix[,5])


# Step 4. Graphical representation

# Case 1. Single epsilon = 0.15

plot( expected_stock_price, main = 'Stock price over 1 year', xlab = 'Steps', ylab = 'Expected stock price',
      col = 'red', type = 'l' )

# Case 2. epsilon is normally distriduted random value (mean = 0, sigma = 1)
min_price = min(c(expected_stock_price_matrix_n[,1],expected_stock_price_matrix_n[,2], expected_stock_price_matrix_n[,3],
                expected_stock_price_matrix_n[,4], expected_stock_price_matrix_n[,5]))

max_price = max(c(expected_stock_price_matrix_n[,1],expected_stock_price_matrix_n[,2], expected_stock_price_matrix_n[,3],
                expected_stock_price_matrix_n[,4], expected_stock_price_matrix_n[,5]))

plot( expected_stock_price_matrix_n[,1], main = 'Stock prices ', xlab = 'Steps', ylab = 'Expected stock price',
      col = 'red', type = 'l', ylim = c(min_price,max_price) )

lines(expected_stock_price_matrix_n[,2], col = 'blue')
lines(expected_stock_price_matrix_n[,3], col = 'green')
lines(expected_stock_price_matrix_n[,4], col = 'black')
lines(expected_stock_price_matrix_n[,5], col = 'magenta')

# Step 5. Compare the result with the one based on epsilon from uniform distribution [-2,2]

# Create empty matrix for 5 trials  
epsilon_unif_matrix = matrix(nrow = 100, ncol = 5)

# Generate 5 vectors for epsilon
epsilon_unif_matrix[,1] = runif(100, min = -2, max = 2)   
epsilon_unif_matrix[,2] = runif(100, min = -2, max = 2)
epsilon_unif_matrix[,3] = runif(100, min = -2, max = 2)   
epsilon_unif_matrix[,4] = runif(100, min = -2, max = 2)   
epsilon_unif_matrix[,5] = runif(100, min = -2, max = 2)


# Create empty matrix for prices calculations
expected_stock_price_matrix_u = matrix(nrow = 101, ncol = 5)

# Trial 1
expected_stock_price_matrix_u[,1] = Calculate_Stock_Price(stock_price_initial = stock_price_initial, return_annual = return_annual,
                                                        volatility_annual = volatility_annual, time_total = time_total,
                                                        number_of_steps = number_of_steps, epsilon = epsilon_unif_matrix[,1])

# Trial 2
expected_stock_price_matrix_u[,2] = Calculate_Stock_Price(stock_price_initial = stock_price_initial, return_annual = return_annual,
                                                        volatility_annual = volatility_annual, time_total = time_total,
                                                        number_of_steps = number_of_steps, epsilon = epsilon_unif_matrix[,2])

# Trial 3
expected_stock_price_matrix_u[,3] = Calculate_Stock_Price(stock_price_initial = stock_price_initial, return_annual = return_annual,
                                                        volatility_annual = volatility_annual, time_total = time_total,
                                                        number_of_steps = number_of_steps, epsilon = epsilon_unif_matrix[,3])

# Trial 4
expected_stock_price_matrix_u[,4] = Calculate_Stock_Price(stock_price_initial = stock_price_initial, return_annual = return_annual,
                                                        volatility_annual = volatility_annual, time_total = time_total,
                                                        number_of_steps = number_of_steps, epsilon = epsilon_unif_matrix[,4])

# Trial 5
expected_stock_price_matrix_u[,5] = Calculate_Stock_Price(stock_price_initial = stock_price_initial, return_annual = return_annual,
                                                        volatility_annual = volatility_annual, time_total = time_total,
                                                        number_of_steps = number_of_steps, epsilon = epsilon_unif_matrix[,5])

min_price = min(c(expected_stock_price_matrix_u[,1],expected_stock_price_matrix_u[,2], expected_stock_price_matrix_u[,3],
                  expected_stock_price_matrix_u[,4], expected_stock_price_matrix_u[,5]))

max_price = max(c(expected_stock_price_matrix_u[,1],expected_stock_price_matrix_u[,2], expected_stock_price_matrix_u[,3],
                  expected_stock_price_matrix_u[,4], expected_stock_price_matrix_u[,5]))

# Graphical representation
plot( expected_stock_price_matrix_u[,1], main = 'Stock prices ', xlab = 'Steps', ylab = 'Expected stock price',
      col = 'red', type = 'l', ylim = c(min_price,max_price) )

lines(expected_stock_price_matrix_u[,2], col = 'blue')
lines(expected_stock_price_matrix_u[,3], col = 'green')
lines(expected_stock_price_matrix_u[,4], col = 'black')
lines(expected_stock_price_matrix_u[,5], col = 'magenta')

# Find variance for normal and uniform trials

# For normal distribution

  

variance_vector_n <- c()

for(i in 1: ncol(expected_stock_price_matrix_n)){
  variance = var(expected_stock_price_matrix_n[,i])
  variance_vector_n[i] = variance
}

average_variance_n = mean(variance_vector_n)

# For uniform distribution
variance_vector_u <- c()

for(i in 1: ncol(expected_stock_price_matrix_u)){
  variance = var(expected_stock_price_matrix_u[,i])
  variance_vector_u[i] = variance
}

average_variance_u = mean(variance_vector_u)
  
ratio = average_variance_u / average_variance_n
ratio_v[k] = ratio
print(k)
}
length(ratio_v)
mean(ratio_v)


