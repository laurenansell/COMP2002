library(dplyr)
library(lubridate)
library(tidyr)
library(nsga2R)
library(plotly)

nyse <- data.table::fread("../prices.csv")

nyse <- nyse %>% 
  mutate(date = ymd(date)) %>% 
  filter(year(date) == 2015,
         month(date) %in% c(1:3))

head(nyse)


securities <- data.table::fread("../securities.csv")

securities <- securities %>% 
  select(`Ticker symbol`, Security) %>% 
  rename(symbol = `Ticker symbol`,
         name = Security)


nyse <- nyse %>% 
  left_join(securities, by = c("symbol")) %>% 
  select(date, symbol, name, everything())

head(nyse)

set.seed(2)
selected_stock <- sample(nyse$symbol, 30)

nyse <- nyse %>% 
  filter(symbol %in% selected_stock)

head(nyse)


nyse <- nyse %>%   
  rename(price = close) %>% 
  select(date, symbol, name, price) %>% 
  group_by(symbol, name) %>%
  mutate(price_prev = lag(price),
         returns = (price - price_prev)/price_prev) %>% 
  slice(-1) %>% 
  ungroup()

head(nyse)


mean_stock <- nyse %>% 
  group_by(symbol) %>% 
  summarise(mean = mean(returns)) %>% 
  arrange(desc(mean))

head(mean_stock)


rf <- 0.04/100

nyse_wide <- nyse %>%
  pivot_wider(id_cols = c(date, symbol), names_from = symbol, values_from = returns) %>% 
  select(-date)

# Create Excess Return
for (i in 1:n_distinct(nyse$symbol)) {
  nyse_wide[,i]<- nyse_wide[,i] - as.numeric(mean_stock[i,2])
}

head(nyse_wide)

nyse_cov <- cov(x = nyse_wide)

fitness <- function(x){
  # Assign weight for each stocks
  weight_stock <- x
  
  # Calculate the total returns
  f1 <- numeric()
  for (i in 1:n_distinct(nyse$symbol)) {
    f1[i] <- weight_stock[i]*mean_stock$mean[i]
  }
  mean_return <- sum(f1) - 1e9 * (round(sum(weight_stock),10)-1)^2 
  
  # Calculate the total risk
  f2 <- numeric()
  for (i in 1:n_distinct(nyse$symbol)) {
    f3 <- numeric()
    
    for (j in 1:n_distinct(nyse$symbol)) {
      f3[j] <- weight_stock[i]*weight_stock[j]*nyse_cov[i,j]
    }
    f2[i] <- sum(f3)
  }
  risk <- sum(f2) + 1e9 * (round(sum(weight_stock),10)-1)^2
  
  # Calculate the number of assets
  card <- length(weight_stock[weight_stock > 0]) 
  
  return(c(-mean_return, risk, card))
}


set.seed(8)
n_asset <- n_distinct(nyse$symbol)
finance_optim <- nsga2R(fn = fitness, varNo = n_asset, objDim = 3, generations = 1000,
                        mprob = 0.2, popSize = 200, cprob = 0.8,
                        lowerBounds = rep(0, n_asset), upperBounds = rep(1, n_asset))


finance_optim$objectives[finance_optim$paretoFrontRank == 1, ] %>% 
  matrix(ncol = 3) %>% 
  as.data.frame() %>% 
  distinct() %>% 
  mutate(V1 = round(-V1, 5),
         V2 = round(V2, 5)) %>% 
  rename(`Total Return` = V1,
         Risk = V2,
         `Number of Assets` = V3)

sum(finance_optim$parameters[1, ])

data.frame(symbol = unique(nyse$symbol),
           name = unique(nyse$name),
           weight = finance_optim$parameters[1, ]) %>% 
  mutate(return = round(weight * mean_stock$mean, 5),
         weight = round(weight,5))





### Car example

fitness <- function(x){
  # Calculate f1(x)
  f1 <- 1640.2823 + 2.3573285*x[1] + 2.3220035*x[2] +4.5688768*x[3] +
    7.7213633*x[4] + 4.4559504*x[5]
  
  # Calculate f2(x)
  f2 <- 6.5856 + 1.15*x[1] - 1.0427*x[2] + 0.9738*x[3] + 0.8364*x[4] - 
    0.3695*x[1]*x[4] + 0.0861*x[1]*x[5] + 0.3628*x[2]*x[4] - 
    0.1106*x[1]^2 - 0.3437*x[3]^2 + 0.1764*x[4]^2
  
  # Calculate f3(x)
  f3 <- -0.0551 + 0.0181*x[1] + 0.1024*x[2] + 0.0421*x[3] - 
    0.0073*x[1]*x[2] + 0.024*x[2]*x[3] - 0.0118*x[2]*x[4] -
    0.0204*x[3]*x[4] - 0.008*x[3]*x[5] - 0.0241*x[2]^2 + 0.0109*x[4]^2
  
  return(c(f1,f2,f3))
}


set.seed(32)
car_optim <- nsga2R(fn = fitness, varNo = 5, objDim = 3, generations = 5000, popSize = 100, 
                    lowerBounds = rep(1, 5), upperBounds = rep(3, 5))

car_result <- car_optim$objectives %>% 
  as.data.frame() %>% 
  bind_cols(as.data.frame(car_optim$parameters)) %>% 
  filter(car_optim$paretoFrontRank == 1) %>% 
  mutate_all(.funs = function(x){round(x,3)}) %>%
  distinct() %>%  
  rename("f1" = V1...1, "f2" = V2...2, "f3" = V3...3,
         "x_1" = V1...4, "x_2" = V2...5, "x_3" = V3...6, "x_4" = V4, "x_5" = V5)

car_result

car_result[1:3, ]

car_result %>% 
  filter(f1 %in% c(min(f1), max(f1)))

plot_ly(car_result, x = ~f1, y = ~f2, z = ~f3) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Mass'),
                      yaxis = list(title = 'Collision Acceleration'),
                      zaxis = list(title = 'Intrusion')))
