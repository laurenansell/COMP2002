library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(neuralnet) 
library(MASS)

#### Regression example

advertising_data<-read.csv("./R Codes/Advertising.csv")

ggplot(advertising_data,aes(x=TV,y=sales,col=sales))+
  geom_point()+scale_color_gradient(low="blue", high="red")+
  scale_y_continuous(labels = label_comma())+
  theme(legend.position = "none",
        axis.title = element_text(size=16),
        axis.text = element_text(size=14))+
  labs(x="TV",y="Sales")

advertising_data_scaled<-as.data.frame(scale(advertising_data))
  

ggplot(advertising_data_scaled,aes(x=TV,y=sales,col=sales))+
  geom_point()+scale_color_gradient(low="blue", high="red")+
  scale_y_continuous(labels = label_comma())+
  theme(legend.position = "none",
        axis.title = element_text(size=16),
        axis.text = element_text(size=14))+
  labs(x="TV",y="Sales")


set.seed(2) 

advertising_data_scaled<-advertising_data_scaled[complete.cases(advertising_data_scaled),]

# Split the data into training and testing set 
index <- sample(1:nrow(advertising_data_scaled), round(0.8 * nrow(advertising_data_scaled))) 
training_data <- advertising_data_scaled[index,] 
test_data <- advertising_data_scaled[-index,]


nn <- neuralnet(sales ~.,  
                data = training_data, hidden = c(5, 3),  
                linear.output = TRUE) 

# Predict on test data 
pr.nn <- compute(nn, test_data) 

# Compute mean squared error 
pr.nn_ <- pr.nn$net.result * (max(advertising_data$sales) - 
                                min(advertising_data$sales))  + 
  min(advertising_data$sales) 
test.r <- (test_data$sales) * (max(advertising_data$sales) - 
                                              min(advertising_data$sales)) +  
  min(advertising_data$sales) 
MSE.nn <- sum((test.r - pr.nn_)^2) / nrow(test_data) 

# Plot the neural network 
plot(nn)

plot(test_data$sales, pr.nn_, col = "red",  
     main = 'Real vs Predicted') 

summary(nn)


results<-as.data.frame(nn$result.matrix)


#### Classification example

penguins <- read.csv("./R Codes/penguins.csv") %>% mutate_if(is.character, as.factor)

penguins<- penguins %>% drop_na()

str(penguins)

penguins$island <- as.numeric(penguins$island)

penguins$bill_length_mm<-scale(penguins$bill_length_mm)
penguins$bill_depth_mm<-scale(penguins$bill_depth_mm)
penguins$flipper_length_mm<-scale(penguins$flipper_length_mm)
penguins$body_mass_g<-scale(penguins$body_mass_g)



set.seed(2)
data_rows <- floor(0.90 * nrow(penguins))
train_indices <- sample(c(1:nrow(penguins)), data_rows)
train_data <- penguins[train_indices,]
test_data <- penguins[-train_indices,]

model = neuralnet(
  species~island+bill_length_mm+bill_depth_mm+flipper_length_mm+body_mass_g,
  data=train_data,
  hidden=c(8,4),
  linear.output = FALSE
)

plot(model,rep = "best")

pred <- predict(model, test_data)
labels <- c("Adelie","Chinstrap" ,"Gentoo")
prediction_label <- data.frame(max.col(pred)) %>%     
  mutate(pred=labels[max.col.pred.]) %>%
  dplyr:: select(2) %>%
  unlist()

table(test_data$species, prediction_label)





