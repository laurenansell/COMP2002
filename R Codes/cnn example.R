library(keras)
library(EBImage) ## If this pack does not install, please use the code below (commented out)

## if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
## BiocManager::install("EBImage")

library(stringr)
library(pbapply)

## This example is using the cats vs dogs dataset available from Kaggle here: 
## https://www.kaggle.com/datasets/shaunthesheep/microsoft-catsvsdogs-dataset

secondCat <- readImage("./R Codes/dogcat/train/cat.1.jpg")
display(secondCat)

# Set image size
width <- 50
height <- 50


extract_feature <- function(dir_path, width, height, labelsExist = T) {
  img_size <- width * height
  
  ## List images in path
  images_names <- list.files(dir_path)
  
  if(labelsExist){
    ## Select only cats or dogs images
    catdog <- str_extract(images_names, "^(cat|dog)")
    # Set cat == 0 and dog == 1
    key <- c("cat" = 0, "dog" = 1)
    y <- key[catdog]
  }
  
  print(paste("Start processing", length(images_names), "images"))
  ## This function will resize an image, turn it into greyscale
  feature_list <- pblapply(images_names, function(imgname) {
    ## Read image
    img <- readImage(file.path(dir_path, imgname))
    ## Resize image
    img_resized <- resize(img, w = width, h = height)
    ## Set to grayscale (normalized to max)
    grayimg <- channel(img_resized, "gray")
    ## Get the image as a matrix
    img_matrix <- grayimg@.Data
    ## Coerce to a vector (row-wise)
    img_vector <- as.vector(t(img_matrix))
    return(img_vector)
  })
  ## bind the list of vector into matrix
  feature_matrix <- do.call(rbind, feature_list)
  feature_matrix <- as.data.frame(feature_matrix)
  ## Set names
  names(feature_matrix) <- paste0("pixel", c(1:img_size))
  
  if(labelsExist){
    return(list(X = feature_matrix, y = y))
  }else{
    return(feature_matrix)
  }
}


# Takes approx. 15min
trainData <- extract_feature("./R Codes/dogcat/train/", width, height)
# Takes slightly less
testData <- extract_feature("./R Codes/dogcat/test1", width, height, labelsExist = F)

# Check processing on second cat
par(mar = rep(0, 4))
testCat <- t(matrix(as.numeric(trainData$X[2,]),
                    nrow = width, ncol = height, T))
image(t(apply(testCat, 2, rev)), col = gray.colors(12),
      axes = F)

# Save
save(trainData, testData, file = "catdogData.RData")

# Fix structure for 2d CNN
train_array %>% 
  layer_dropout(rate = 0.25) %>%
  layer_flatten() %>%
  layer_dense(units = 50, activation = "relu") %>%
  layer_dropout(rate = 0.25) %>%
  layer_dense(units = 1, activation = "sigmoid")

summary(model)

model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = "adam",
  metrics = c('accuracy')
)

history <- model %>%  fit(
  x = train_array, y = as.numeric(trainData$y),
  epochs = 30, batch_size = 100,
  validation_split = 0.2
)

plot(history)

# Compute probabilities and predictions on test set
predictions <-  predict_classes(model, test_array)
probabilities <- predict_proba(model, test_array)

# Visual inspection of 32 cases
set.seed(100)
random <- sample(1:nrow(testData), 32)
preds <- predictions[random,]
probs <- as.vector(round(probabilities[random,], 2))

par(mfrow = c(4, 8), mar = rep(0, 4))
for(i in 1:length(random)){
  image(t(apply(test_array[random[i],,,], 2, rev)),
        col = gray.colors(12), axes = F)
  legend("topright", legend = ifelse(preds[i] == 0, "Cat", "Dog"),
         text.col = ifelse(preds[i] == 0, 2, 4), bty = "n", text.font = 2)
  legend("topleft", legend = probs[i], bty = "n", col = "white")
}

# Save model
save(model, file = "CNNmodel.RData")

