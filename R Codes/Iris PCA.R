library(tidyverse)


attach(iris)

iris_pca <- princomp(iris[, -5], cor = TRUE) 

plot(iris_pca)

new_variables <- as.data.frame(iris_pca$scores)
  
iris_data <-  separate(iris, "Species",
                           c("Species", "Other_Info"),  # the name and other information
                           extra = "merge" # Only split into at most two items, 
                           # merging as necessary  
)

new_variables$species <- iris_data$Species

ggplot(new_variables, aes(x = Comp.1, y = Comp.2,col=species)) + geom_point(size=3)+
  labs(x = "First Principal Component", 
       y = "Second Principal Component") +
  coord_fixed(ratio = 1)+
  theme(axis.title = element_text(size=16),
        legend.position = "none")

