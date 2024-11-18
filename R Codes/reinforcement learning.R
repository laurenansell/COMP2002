library(ReinforcementLearning)

devtools::install_github("markdumke/reinforcelearn",force = TRUE)
library(reinforcelearn)

## Example 1 - simple gridworld

# Define state and action sets
states <- c("s1", "s2", "s3", "s4")
actions <- c("up", "down", "left", "right")


# Load built-in environment function for 2x2 gridworld 
env <- gridworldEnvironment
print(env)


# Sample N = 1000 random sequences from the environment
data <- sampleExperience(N = 1000, 
                         env = env, 
                         states = states, 
                         actions = actions)
head(data)


# Define reinforcement learning parameters
control <- list(alpha = 0.1, gamma = 0.5, epsilon = 0.1)

# Perform reinforcement learning
model <- ReinforcementLearning(data, 
                               s = "State", 
                               a = "Action", 
                               r = "Reward", 
                               s_new = "NextState", 
                               control = control)

# Print policy
computePolicy(model)

# Print state-action function
print(model)

# Print summary statistics
summary(model)

## Applying a policy to unseen data

# Example data
data_unseen <- data.frame(State = c("s1", "s2", "s1"), 
                          stringsAsFactors = FALSE)

# Pick optimal action
data_unseen$OptimalAction <- predict(model, data_unseen$State)

data_unseen

## Updating an existing policy

# Sample N = 1000 sequences from the environment
# using epsilon-greedy action selection
data_new <- sampleExperience(N = 1000, 
                             env = env, 
                             states = states, 
                             actions = actions, 
                             actionSelection = "epsilon-greedy",
                             model = model, 
                             control = control)

# Update the existing policy using new training data
model_new <- ReinforcementLearning(data_new, 
                                   s = "State", 
                                   a = "Action", 
                                   r = "Reward", 
                                   s_new = "NextState", 
                                   control = control,
                                   model = model)

# Print result
print(model_new)


# Plot reinforcement learning curve
plot(model_new)


## Example 2 - More complex gridworld

env=makeGridworld(shape=c(4,4),goal.states=c(0,15))


