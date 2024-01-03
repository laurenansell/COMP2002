# Load the data:
data(trees)
# rename columns
names(trees) <- c("DBH_in","height_ft", "volume_ft3")
# Show the top few entries:
head(trees)


set.seed(2) # to get the same results as me
# 100 random 'a' values based on a uniform distribution from -100 to 100
a_coef <- runif(min=-100, max=100, n=100)
# 100 random 'b' values based on a uniform distribution from -100 to 100
b_coef <- runif(min=-100, max=100, n=100)
# pair these together into a dataframe of two columns and call this a population
# and also add in a column for fitness so that we can keep track of the fitness
# of each organism/model:
population <- data.frame(a_coef, b_coef, fitness=NA)
head(population)

gen_starting_pop <- function(){
  # 100 random 'a' values based on a uniform distribution from -100 to 100
  a_coef <- runif(min=-100, max=100, n=100)
  # 100 random 'b' values based on a uniform distribution from -100 to 100
  b_coef <- runif(min=-100, max=100, n=100)
  # pair these together into a dataframe of two columns and call this a population
  # and also add in a column for fitness so that we can keep track of the fitness
  # of each organism/model:
  population <- data.frame(a_coef, b_coef, fitness=NA)
  return(population)
}

# loop through each organism:
for(i in 1:100){
  # for each organism, calculate predicted DBH values
  DBH_predicted = population[i,"a_coef"] + population[i,"b_coef"]*trees$height_ft
  # Now, subtract the real DBH values from the predicted values to get the difference:
  difference <- DBH_predicted - trees$DBH_in
  # calculate the sum of squared differences:
  sum_sq_diff <- sum(difference^2)
  # make the fitness value range from 0 to 1:
  fitness <- 1/sum_sq_diff
  # finally, save the fitness values to the population dataframe:
  population$fitness[i] <- fitness
}

# find the index of the top 10 (highest) fitness values:
top_10_fit <- order(population$fitness, decreasing = T)[1:10]
# then use those to index the population:
survivors <- population[top_10_fit,]
survivors

evaluate_fitness <- function(population){
  # loop through each organism:
  for(i in 1:100){
    # for each organism, calculate predicted DBH values
    DBH_predicted = population[i,"a_coef"] + population[i,"b_coef"]*trees$height_ft
    # Now, subtract the real DBH values from the predicted values to get the difference:
    difference <- DBH_predicted - trees$DBH_in
    # calculate the sum of squared differences:
    sum_sq_diff <- sum(difference^2)
    # make the fitness value range from 0 to 1:
    fitness <- 1/sum_sq_diff
    # finally, save the fitness values to the population dataframe:
    population$fitness[i] <- fitness
  }
  # find the index value the top 10 (highest) fitness values:
  top_10_fit <- order(population$fitness, decreasing = T)[1:10]
  # then use those to index the population:
  survivors <- population[top_10_fit,]
  # return survivors
  return(survivors)
}
population <- evaluate_fitness(population)

population

# First, choose the parents at randome from the 10 possible survivors:
parents <- sample(1:10, size=100, replace=T)
# and use those parents (index values) to clone offspring:
offspring <- survivors[parents,]
# Then add mutations:
# choose a mutation rate:
mutation_rate <- 0.6
# total number of mutations is our population (100) * the rate, and rounded to
# make sure the result is an integer value:
total_mutations <- round(100*mutation_rate)
# choose which models recieve mutations for a or b coefficients:
a_to_mutate <- sample(x=c(1:100), size=total_mutations)
b_to_mutate <- sample(x=c(1:100), size=total_mutations)
# then generate a set of random mutations for the a and b coefficients:
a_mutations <- rnorm(n = total_mutations, mean=0, sd=3)
b_mutations <- rnorm(n = total_mutations, mean=0, sd=3)
# and apply those mutations:
offspring$a_coef[a_to_mutate] <- offspring$a_coef[a_to_mutate] + a_mutations
offspring$b_coef[b_to_mutate] <- offspring$b_coef[b_to_mutate] + b_mutations
# finally, reset the row names from 1 to 100:
row.names(offspring) <- 1:100

mate_and_mutate <- function(survivors){
  # create a series of 100 random indexes chosen from the survivors:
  # First, choose the parents at randome from the 10 possible survivors:
  parents <- sample(1:10, size=100, replace=T)
  # and use those parents (index values) to clone offspring:
  offspring <- survivors[parents,]
  # Then add mutations:
  # choose a mutation rate:
  mutation_rate <- 0.6
  # total number of mutations is our population (100) * the rate, and rounded to
  # make sure the result is an integer value:
  total_mutations <- round(100*mutation_rate)
  # choose which models recieve mutations for a or b coefficients:
  a_to_mutate <- sample(x=c(1:100), size=total_mutations)
  b_to_mutate <- sample(x=c(1:100), size=total_mutations)
  # then generate a set of random mutations for the a and b coefficients:
  a_mutations <- rnorm(n = total_mutations, mean=0, sd=3)
  b_mutations <- rnorm(n = total_mutations, mean=0, sd=3)
  # and apply those mutations:
  offspring$a_coef[a_to_mutate] <- offspring$a_coef[a_to_mutate] + a_mutations
  offspring$b_coef[b_to_mutate] <- offspring$b_coef[b_to_mutate] + b_mutations
  # finally, reset the row names from 1 to 100:
  row.names(offspring) <- 1:100
  # return the new generation of offspring:
  return(offspring)
}


# First set the starting population:
population <- gen_starting_pop()
# set how many generations you want to run this for.
# we'll start with 5 for now:
generations <- 5
# begin the for loop:
for(i in 1:generations){
  # 1) Evaluate fitness:
  survivors <- evaluate_fitness(population)
  # 2) Mate and mutate survivors to generate next generation:
  next_generation <- mate_and_mutate(survivors)
  # 3) Redefine the population using the new generation:
  population <- next_generation
}
survivors <- evaluate_fitness(population)
head(survivors)


set.seed(8)
# First set the starting population:
population <- gen_starting_pop()
# set how many generations you want to run this for.
# Use 100 now:
generations <- 1000
# define empty variable to collect fitness values:
fitness <- NULL
# begin the for loop:
for(i in 1:generations){
  # 1) Evaluate fitness:
  survivors <- evaluate_fitness(population)
  # 2) Mate and mutate survivors to generate next generation:
  next_generation <- mate_and_mutate(survivors)
  # 3) Redefine the population using the new generation:
  population <- next_generation
  # save fitness value from each generation to plot it over time
  fitness[i] <- max(population$fitness)
}


plot(fitness ~ c(1:generations), type="l", lwd=2, xlab="Generation", ylab="Fitness")

set.seed(32)
# First set the starting population:
population <- gen_starting_pop()
# set how many generations you want to run this for.
generations <- 1000
# define empty variable to collect fitness values:
fitness <- NULL
# begin the for loop:
for(i in 1:generations){
  # 1) Evaluate fitness:
  survivors <- evaluate_fitness(population)
  # 2) Mate and mutate survivors to generate next generation:
  next_generation <- mate_and_mutate(survivors)
  # 3) Redefine the population using the new generation:
  population <- next_generation
  fitness[i] <- max(population$fitness)
  #Every 5 generations, pause the simulation and plot the
  # points with the current best-fit line:
  if(i %% 50 == 0){
    survivors <- evaluate_fitness(population)
    plot(DBH_in ~ height_ft, data = trees, pch=16, cex=1.5)
    title(main=paste0("Generation: ",i), cex.main=2)
    abline(a=survivors$a_coef[1], b=survivors$b_coef[1], lwd=3, col="red")
    # pause for 1 second:
    Sys.sleep(.5)
  }
}

# Extract the best model:
top_models <- evaluate_fitness(population) # Be sure to run this first
best_model <- top_models[which.max(top_models$fitness),]
# compare this to a basic linear model result with 'lm()':
evo_model <- best_model
lm_model <- lm(DBH_in ~ height_ft, data=trees)
# Plot the scatterplot and add the best-fit lines:
plot(DBH_in ~ height_ft, data = trees, pch=16, cex=1.5)
# red line for our model solution
abline(a=evo_model$a_coef, b=evo_model$b_coef, col="red", lwd=3)
# blue line for the lm() model solution:
abline(lm_model, col="blue", lwd=2,lty=3)


evo_model
lm_model
