library(tidyverse)

penguins<-read.csv("./R Codes/penguins.csv")

## There are missing values in this dataset that need to be removed

penguins<-na.omit(penguins)

## Check the structure of the data

str(penguins)

## There are three variables which are of the type character so for simplicity we will 
## remove these

penguins<-penguins %>% select(species,bill_length_mm,bill_depth_mm,flipper_length_mm,
                              body_mass_g)


penguins_pca<-princomp(penguins[,-1],cor=TRUE)

plot(penguins_pca)

new_variables<-as.data.frame(penguins_pca$scores)

penguins_data<-separate(penguins,"species",
                        c("species","Other_Info"), ## the name and other information
                        extra = "merge") ## Only split into at most two items, merging as necessary 


new_variables$species <- penguins_data$species

ggplot(new_variables, aes(x = Comp.1, y = Comp.2,col=species)) + geom_point(size=3)+
  labs(x = "First Principal Component", 
       y = "Second Principal Component") +
  coord_fixed(ratio = 1)+
  theme(axis.title = element_text(size=16),
        legend.position = "none")

