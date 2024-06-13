library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(neuralnet) 
library(MASS)

#### Regression example

housing_data<-read.csv("housing.csv")

ggplot(housing_data,aes(x=median_income,y=households,col=median_house_value))+
  geom_point()+scale_color_gradient(low="blue", high="red")+
  scale_y_continuous(labels = label_comma())+
  theme(legend.position = "none",
        axis.title = element_text(size=16),
        axis.text = element_text(size=14))+
  labs(x="Median income",y="Number of households")

housing_data_scaled<-as.data.frame(scale(housing_data[,-10]))
  

ggplot(housing_data_scaled,aes(x=median_income,y=households,col=median_house_value))+
  geom_point()+scale_color_gradient(low="blue", high="red")+
  scale_y_continuous(labels = label_comma())+
  theme(legend.position = "none",
        axis.title = element_text(size=16),
        axis.text = element_text(size=14))+
  labs(x="Median income",y="Number of households")


set.seed(2) 

housing_data_scaled<-housing_data_scaled[complete.cases(housing_data_scaled),]

# Split the data into training and testing set 
index <- sample(1:nrow(housing_data_scaled), round(0.75 * nrow(housing_data_scaled))) 
training_data <- housing_data_scaled[index,] 
test_data <- housing_data_scaled[-index,]


nn <- neuralnet(median_house_value ~.,  
                data = training_data, hidden = c(5, 3),  
                linear.output = TRUE) 

# Predict on test data 
pr.nn <- compute(nn, test_data) 

# Compute mean squared error 
pr.nn_ <- pr.nn$net.result * (max(housing_data$median_house_value) - 
                                min(housing_data$median_house_value))  + 
  min(housing_data$median_house_value) 
test.r <- (test_data$median_house_value) * (max(housing_data$median_house_value) - 
                                              min(housing_data$median_house_value)) +  
  min(housing_data$median_house_value) 
MSE.nn <- sum((test.r - pr.nn_)^2) / nrow(test_data) 

# Plot the neural network 
plot(nn)

plot(test_data$median_house_value, pr.nn_, col = "red",  
     main = 'Real vs Predicted') 

summary(nn)


results<-as.data.frame(nn$result.matrix)


#### Classification example

penguins <- read.csv("./R Codes/penguins.csv") %>% mutate_if(is.character, as.factor)

penguins<-dplyr::select(penguins,species,bill_length_mm,bill_depth_mm,flipper_length_mm,
                              body_mass_g) %>% drop_na()
set.seed(2)
data_rows <- floor(0.80 * nrow(penguins))
train_indices <- sample(c(1:nrow(penguins)), data_rows)
train_data <- penguins[train_indices,]
test_data <- penguins[-train_indices,]

model = neuralnet(
  species~bill_length_mm+bill_depth_mm+flipper_length_mm+body_mass_g,
  data=train_data,
  hidden=c(4,2),
  linear.output = FALSE
)

plot(model,rep = "best")

pred <- predict(model, test_data)
labels <- c("Chinstrap", "Gentoo", "Adelie")
prediction_label <- data.frame(max.col(pred)) %>%     
  mutate(pred=labels[max.col.pred.]) %>%
  dplyr:: select(2) %>%
  unlist()

table(test_data$species, prediction_label)





