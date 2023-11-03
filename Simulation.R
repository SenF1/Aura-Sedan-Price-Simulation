# Load necessary packages.
rm(list=ls())
library(ggplot2)

# Set seed for reproducibility
set.seed(1234)

# Number of Simulation
total_simulations <- 1000

# Boundaries of price
min_price_aura <- 21000
max_price_aura <- 40000
price_aura_step <- 100

# Environment Setup
cost_of_delay <- 5e6   # Estimate the cost of delay on other project is $5 million
fixed_cost <- 5e6      # Fixed cost of retooling the manufacturing plant is approximately $5 million
cost_price <- runif(total_simulations, 15000, 20000)     # Cost price of producing one car lie between $15,000 to $20,000
market_price_asta <- sample(c(22000, 27000), total_simulations, replace = TRUE, prob = c(0.3, 0.7)) # Simulate market price of Asta
discount_rate <- 0.1

# A function for the annual volume
annual_volume <- function(price) {
  epsilon <- runif(1, min = -1, max = 1) # the random error
  demand <- 10 - 3 * price / 10000 + epsilon
  return(max(0, demand))
}

# Function to calculate discounted cash flow
calculate_dcf <- function(revenue, discount_rate, year) {
  dcf <- revenue / (1 + discount_rate)^year
  return(dcf)
}

# Initialize a list to store the results
results <- list()

# Run a separate simulation for each possible price of Aura
for (price_aura in seq(min_price_aura, max_price_aura, by = price_aura_step)) {
  # Run the simulation
  for (i in 1:total_simulations) {
    total_volume <- 1000*annual_volume((market_price_asta[i] + price_aura)/2)   # Price is the average market price of Asta and Aura.
    volume_aura <- total_volume * market_price_asta[i] / (market_price_asta[i] + price_aura)
    revenue <- volume_aura * price_aura
    cost <- volume_aura * cost_price[i]
    profit <- revenue - cost
    total_profit = 0 # Total profit in 5 years
    
    # Calculate the net present value for each year
    for (year in 2025:2030) {
      discount_factor <- 1 / ((1 + discount_rate) ^ (year - 2025))
      DCF_aura = profit * discount_factor
      total_profit = total_profit + DCF_aura
    }
    
    total_profit_after_fixed_costs = total_profit - fixed_cost - cost_of_delay
    
    results[[length(results) + 1]] <- data.frame(
      Price_Aura = price_aura,
      Pure_Profit= total_profit_after_fixed_costs,
      # Ignore the following to speed up simulation
      Price_Asta = market_price_asta[i],
      total_volume=total_volume,
      Volume_Aura=volume_aura,
      revenue=revenue,
      cost=cost,
      total_profit=total_profit
    )
    
    # Print progress
    cat("Price Aura: ", price_aura, "/", max_price_aura, " Simulation: ", i, "/", total_simulations, "\r")
    flush.console() 
  }
}

# Combine all the results into a single data frame
results <- do.call(rbind, results)

# Calculate the mean profit for each price
mean_profits <- aggregate(Pure_Profit ~ Price_Aura, data = results, FUN = mean)

# Find the price that gives the maximum mean profit
optimal_price <- mean_profits$Price_Aura[which.max(mean_profits$Pure_Profit)]

# Create the plot
ggplot(mean_profits, aes(x = Price_Aura, y = Pure_Profit)) +
  geom_point() + # Scatter plot
  scale_x_continuous(labels = scales::label_number_si(scale = 0.001, suffix = "k"), name = "Price for Aura (in USD)") + # Format x-axis
  scale_y_continuous(labels = scales::comma_format(scale = 0.000001), name = "Net Present Value for 6 years (in millions USD)") +   
  labs(title = "Average Projected Net Present Value (NPV) over Time (2025-2030)") +
  theme_minimal() + 
  geom_vline(xintercept = optimal_price, linetype = "dashed", color = "green", size = 1) + # Add vertical line
  geom_text(aes(x = optimal_price, y = max(Pure_Profit), label = paste("Optimal Price\n", optimal_price/1000, "k")), 
            vjust = 2.5, color = "red", size = 4) + # Add text annotation 
  theme(plot.title = element_text(hjust = 0.5))  # Center the title

cat("Number of Simulations: ", total_simulations, "\n",
    "       Optimal Price: ", optimal_price, "\n")

