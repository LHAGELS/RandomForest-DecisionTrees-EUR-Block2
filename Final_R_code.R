
#### Final Assignment
setwd("...")

#-----------------------------------------------------------------------------------------------
#### Packages
# Cleaning
library(readr)
library(stringr)
# Analysis 1
library(factoextra)
library(clustrd)
library(RColorBrewer)
library(colorspace)
library(ggplot2)
# Summary Tables
library(tidyverse)
library(table1)
# Analysis 2
library(caret)
library(rpart)
library(randomForest)
library(e1071)
library(broom)
library(ipred)
library(tuneRanger)
library(mlr)
library(ranger)
# Model-Agnostic
library(gridExtra)
library(pdp)
library(DALEX)
library(DALEXtra)
library(iml)
library(ALEPlot)
library(ggpubr)

#-----------------------------------------------------------------------------------------------
#### Data Preparation
## read in data
products <- read_csv("summer-products-with-rating-and-performance_2020-08.csv")

## drop unnecessary columns
products <- products[, -c(1,22:23,36,38:43)]
products

## Clean colors
# transform all to lowercase
products$product_color <- tolower(products$product_color)
unique(products$product_color)

## replace values that contain a color with this certain color
for (i in 1:nrow(products[,"product_color"])){
  products[i,"product_color"] <- ifelse(grepl("blue",products[i,"product_color"], fixed = TRUE) == TRUE, "blue",products[i,"product_color"])
  products[i,"product_color"] <- ifelse(grepl("navy",products[i,"product_color"], fixed = TRUE) == TRUE, "blue",products[i,"product_color"])
  products[i,"product_color"] <- ifelse(grepl("green",products[i,"product_color"], fixed = TRUE) == TRUE, "green",products[i,"product_color"])
  products[i,"product_color"] <- ifelse(grepl("jasper",products[i,"product_color"], fixed = TRUE) == TRUE, "green",products[i,"product_color"])
  products[i,"product_color"] <- ifelse(grepl("grey",products[i,"product_color"], fixed = TRUE) == TRUE, "grey",products[i,"product_color"])
  products[i,"product_color"] <- ifelse(grepl("gray",products[i,"product_color"], fixed = TRUE) == TRUE, "grey",products[i,"product_color"])
  products[i,"product_color"] <- ifelse(grepl("white",products[i,"product_color"], fixed = TRUE) == TRUE, "white",products[i,"product_color"])
  products[i,"product_color"] <- ifelse(grepl("yellow",products[i,"product_color"], fixed = TRUE) == TRUE, "yellow",products[i,"product_color"])
  products[i,"product_color"] <- ifelse(grepl("red",products[i,"product_color"], fixed = TRUE) == TRUE, "red",products[i,"product_color"])
  products[i,"product_color"] <- ifelse(grepl("wine",products[i,"product_color"], fixed = TRUE) == TRUE, "red",products[i,"product_color"])
  products[i,"product_color"] <- ifelse(grepl("claret",products[i,"product_color"], fixed = TRUE) == TRUE, "red",products[i,"product_color"])
  products[i,"product_color"] <- ifelse(grepl("burgundy",products[i,"product_color"], fixed = TRUE) == TRUE, "red",products[i,"product_color"])
  products[i,"product_color"] <- ifelse(grepl("beige",products[i,"product_color"], fixed = TRUE) == TRUE, "brown",products[i,"product_color"])
  products[i,"product_color"] <- ifelse(grepl("orange",products[i,"product_color"], fixed = TRUE) == TRUE, "orange",products[i,"product_color"])
  products[i,"product_color"] <- ifelse(grepl("black",products[i,"product_color"], fixed = TRUE) == TRUE, "black",products[i,"product_color"])
  products[i,"product_color"] <- ifelse(grepl("purple",products[i,"product_color"], fixed = TRUE) == TRUE, "purple",products[i,"product_color"])
  products[i,"product_color"] <- ifelse(grepl("violet",products[i,"product_color"], fixed = TRUE) == TRUE, "purple",products[i,"product_color"])
  products[i,"product_color"] <- ifelse(grepl("pink",products[i,"product_color"], fixed = TRUE) == TRUE, "pink",products[i,"product_color"])
  products[i,"product_color"] <- ifelse(grepl("rose",products[i,"product_color"], fixed = TRUE) == TRUE, "pink",products[i,"product_color"])
  products[i,"product_color"] <- ifelse(grepl("gold",products[i,"product_color"], fixed = TRUE) == TRUE, "gold",products[i,"product_color"])
  products[i,"product_color"] <- ifelse(grepl("khaki",products[i,"product_color"], fixed = TRUE) == TRUE, "green",products[i,"product_color"])
  products[i,"product_color"] <- ifelse(grepl("ivory",products[i,"product_color"], fixed = TRUE) == TRUE, "white",products[i,"product_color"])
  products[i,"product_color"] <- ifelse(grepl("army",products[i,"product_color"], fixed = TRUE) == TRUE, "pattern",products[i,"product_color"])
  products[i,"product_color"] <- ifelse(grepl("rainbow",products[i,"product_color"], fixed = TRUE) == TRUE, "pattern",products[i,"product_color"])
  products[i,"product_color"] <- ifelse(grepl("star",products[i,"product_color"], fixed = TRUE) == TRUE, "pattern",products[i,"product_color"])
  products[i,"product_color"] <- ifelse(grepl("leopard",products[i,"product_color"], fixed = TRUE) == TRUE, "pattern",products[i,"product_color"])
  products[i,"product_color"] <- ifelse(grepl("multicolor",products[i,"product_color"], fixed = TRUE) == TRUE, "pattern",products[i,"product_color"])
  products[i,"product_color"] <- ifelse(grepl("camouflage",products[i,"product_color"], fixed = TRUE) == TRUE, "pattern",products[i,"product_color"])
  products[i,"product_color"] <- ifelse(grepl("floral",products[i,"product_color"], fixed = TRUE) == TRUE, "pattern",products[i,"product_color"])
  products[i,"product_color"] <- ifelse(grepl("coffee",products[i,"product_color"], fixed = TRUE) == TRUE, "brown",products[i,"product_color"])
  products[i,"product_color"] <- ifelse(grepl("nude",products[i,"product_color"], fixed = TRUE) == TRUE, "brown",products[i,"product_color"])
  products[i,"product_color"] <- ifelse(grepl("camel",products[i,"product_color"], fixed = TRUE) == TRUE, "brown",products[i,"product_color"])
  products[i,"product_color"] <- ifelse(grepl("tan",products[i,"product_color"], fixed = TRUE) == TRUE, "brown",products[i,"product_color"])
  products[i,"product_color"] <- ifelse(grepl("apricot",products[i,"product_color"], fixed = TRUE) == TRUE, "brown",products[i,"product_color"])
  products[i,"product_color"] <- ifelse(grepl("silver",products[i,"product_color"], fixed = TRUE) == TRUE, "pattern",products[i,"product_color"])
}
unique <- unique(products[,"product_color"])
table(products$product_color)

## Clean Sizes
# transform all to lowercase
products$product_variation_size_id <- tolower(products$product_variation_size_id)
x <- table(products$product_variation_size_id)
unique(products$product_variation_size_id)
sort(x)
# clothing
products$product_variation_size_id[products$product_variation_size_id == "choose a size"] <- NA
products$product_variation_size_id[products$product_variation_size_id == "size -xxs"] <- "xxs"
products$product_variation_size_id[products$product_variation_size_id == "size-xxs"] <- "xxs"
products$product_variation_size_id[products$product_variation_size_id == "size xxs"] <- "xxs"
products$product_variation_size_id[products$product_variation_size_id == "xs."] <- "xs"
products$product_variation_size_id[products$product_variation_size_id == "size-xs"] <- "xs"
products$product_variation_size_id[products$product_variation_size_id == "size xs"] <- "xs"
products$product_variation_size_id[products$product_variation_size_id == "s."] <- "s"
products$product_variation_size_id[products$product_variation_size_id == "s"] <- "s"
products$product_variation_size_id[products$product_variation_size_id == "us-s"] <- "s"
products$product_variation_size_id[products$product_variation_size_id == "size--s"] <- "s"
products$product_variation_size_id[products$product_variation_size_id == "size-s"] <- "s"
products$product_variation_size_id[products$product_variation_size_id == "size s."] <- "s"
products$product_variation_size_id[products$product_variation_size_id == "s pink"] <- "s"
products$product_variation_size_id[products$product_variation_size_id == "size s"] <- "s"
products$product_variation_size_id[products$product_variation_size_id == "s.."] <- "s"
products$product_variation_size_id[products$product_variation_size_id == "size/s"] <- "s"
products$product_variation_size_id[products$product_variation_size_id == "s(pink & black)"] <- "s"
products$product_variation_size_id[products$product_variation_size_id == "s(bust 88cm)"] <- "s"
products$product_variation_size_id[products$product_variation_size_id == "s (waist58-62cm)"] <- "s"
products$product_variation_size_id[products$product_variation_size_id == "s diameter 30cm"] <- "s"
products$product_variation_size_id[products$product_variation_size_id == "pants-s"] <- "s"
products$product_variation_size_id[products$product_variation_size_id == "suit-s"] <- "s"
products$product_variation_size_id[products$product_variation_size_id == "m."] <- "m"
products$product_variation_size_id[products$product_variation_size_id == "Size m"] <- "m"
products$product_variation_size_id[products$product_variation_size_id == "sizel"] <- "l"
products$product_variation_size_id[products$product_variation_size_id == "l."] <- "l"
products$product_variation_size_id[products$product_variation_size_id == "size-l"] <- "l"
products$product_variation_size_id[products$product_variation_size_id == "x   l"] <- "xl"
products$product_variation_size_id[products$product_variation_size_id == "1 pc - xl"] <- "xl"
products$product_variation_size_id[products$product_variation_size_id == "2xl"] <- "xxl"
products$product_variation_size_id[products$product_variation_size_id == "3xl"] <- "xxxl"
products$product_variation_size_id[products$product_variation_size_id == "4xl"] <- "xxxxl"
products$product_variation_size_id[products$product_variation_size_id == "size4xl"] <- "xxxxl"
products$product_variation_size_id[products$product_variation_size_id == "size-4xl"] <- "xxxxl"
products$product_variation_size_id[products$product_variation_size_id == "5xl"] <- "xxxxxl"
products$product_variation_size_id[products$product_variation_size_id == "size-5xl"] <- "xxxxxl"
products$product_variation_size_id[products$product_variation_size_id == "6xl"] <- "xxxxxxl"
products$product_variation_size_id[products$product_variation_size_id == "one size"] <- "one-size"

unique(products$product_variation_size_id)

## Keep only clothing
sizes <- c("xxxs","xxs", "xs", "s", "m", "l", "xl", "xxl", "xxxl", "xxxxl", "xxxxxl", "xxxxxxl", "one-size", NA)
clothing <- products[products$product_variation_size_id %in% sizes,]
unique(clothing$product_variation_size_id)
table(clothing$product_variation_size_id)

## clean Urgency_banner
clothing$has_urgency_banner <- ifelse(is.na(clothing$has_urgency_banner) == TRUE, 0, 1)

## clean rating counts
## Delete products that have less than 15 reviews to have reliable ratings
clothing <- clothing[clothing$rating_count >= 15,]

### Feature Engineering of tags
## Get dummies for each tag
tags.split <- strsplit(tolower(clothing$tags), ",")
lev <- unique(unlist(tags.split))
tags.dummy <- lapply(tags.split, function(x) table(factor(x, levels=lev)))
clothing2 <- with(clothing, data.frame(clothing, do.call(rbind, tags.dummy)))
#> 2047 columns now!
#> Create a table to find important tags

# Create a dataframe
sums <- lapply(clothing2[,34:2080], function(x) sum(x))
sums_df <- t(as.data.frame(sums))
sums_df <- as.data.frame(cbind(sums_df,rownames(sums_df)))
colnames(sums_df) <- c("count", "tag")
sums_df$count <- as.numeric(as.character(sums_df$count))
sums_df$tag <- as.character(sums_df$tag)
sums_df <- sums_df[order(-sums_df$count),]
rownames(sums_df) <- NULL

#> Many tags are only used a couple of times
#> drop columns that discriminate too much
sums_df_larger10 <- sums_df[sums_df$count >= 10,] 
keeplist <- list(sums_df_larger10$tag)[[1]]

# Create a dataframe
clothing_ifcb <- clothing2[,keeplist]

#-----------------------------------------------------------------------------------------------
#### Analysis 1: i-FCB Method
## drop variables that discriminate too less
clothing_ifcb <- clothing_ifcb[,4:247]
clothing_ifcb <- as.data.frame(lapply(clothing_ifcb,factor))

set.seed(1)
fviz_nbclust(clothing_ifcb, kmeans, method = "silhouette", k.max=12)

# Cluster with i-FCB
set.seed(1)
# 10 cluster, 10 dimension with 10 reps
res.ifcb.plot <- clusmca(clothing_ifcb, 10, 10, method = "iFCB", seed = 1, inboot=10)
# plot cluster solution to explore the results 
plot(res.ifcb.plot, dim = c(1,2), what = c(TRUE, FALSE), cludesc = TRUE)

#Category 1: Sleveless Tops
#Category 2: Polka Dresses
#Category 3: T-shirts/Tops/Blouses
#Category 4: Men wear
#Category 5: Pants/Shorts
#Category 6: Long Dresses
#Category 7: Summer Dresses
#Category 8: Jumpsuits/Overalls
#Category 9: Mini Dresses
#Category 10:Swimwear

# Rename Clusters
res.ifcb <- res.ifcb.plot
res.ifcb$cluster <- ifelse(res.ifcb$cluster == 1, "Sleeveless.Tops",
                           ifelse(res.ifcb$cluster == 2, "Polka.Dresses",
                                  ifelse(res.ifcb$cluster == 3, "T-Shirts/Blouses",
                                         ifelse(res.ifcb$cluster == 4,"Men/Sportswear",
                                                ifelse(res.ifcb$cluster == 5,"Pants/Shorts",
                                                       ifelse(res.ifcb$cluster == 6, "Long.Dresses",
                                                              ifelse(res.ifcb$cluster == 7, "Summer.Dresses",
                                                                     ifelse(res.ifcb$cluster == 8, "Jumpsuits/Overalls",
                                                                            ifelse(res.ifcb$cluster == 9, "Mini.Dresses",
                                                                                   ifelse(res.ifcb$cluster == 10, "SwimWear", "NA"))))))))))

# Rename dimensions to use them in the graph
colnames(res.ifcb$obscoord) <- c("dim1","dim2","dim3","dim4","dim5","dim6","dim7","dim8","dim9","dim10")

# create a list with cluster names
category_names <- c("Sleeveless.Tops","Polka.Dresses","T-Shirts/Blouses","Men/Sportswear","Pants/Shorts",
                    "Long.Dresses","Summer.Dresses","Jumpsuits/Overalls","Mini.Dresses","Swim.Wear")

# assign cluster to an object
Categories <- res.ifcb$cluster

# Create a ggplot that plots the observations in a 2D-Space
biplot1_2 <- ggplot(as.data.frame(res.ifcb$obscoord), aes(x=dim1, y=dim2, fill=Categories)) +
  labs(x="Dimension 1", y="Dimension 2") +
  geom_point(alpha=1, size=2.75,pch=21,color="white") +
  scale_fill_brewer(palette="Set3") +
  annotate(geom="text" , label = category_names[1], x=res.ifcb$centroid[1,1], y=res.ifcb$centroid[1,2],size=3)+
  annotate(geom="text" , label = category_names[2], x=res.ifcb$centroid[2,1], y=res.ifcb$centroid[2,2]-0.05,size=3)+
  annotate(geom="text" , label = category_names[3], x=res.ifcb$centroid[3,1], y=res.ifcb$centroid[3,2],size=3)+
  annotate(geom="text" , label = category_names[4], x=res.ifcb$centroid[4,1], y=res.ifcb$centroid[4,2],size=3)+
  annotate(geom="text" , label = category_names[5], x=res.ifcb$centroid[5,1], y=res.ifcb$centroid[5,2],size=3)+
  annotate(geom="text" , label = category_names[6], x=res.ifcb$centroid[6,1], y=res.ifcb$centroid[6,2]-0.1,size=3)+
  annotate(geom="text" , label = category_names[7], x=res.ifcb$centroid[7,1], y=res.ifcb$centroid[7,2],size=3)+
  annotate(geom="text" , label = category_names[8], x=res.ifcb$centroid[8,1], y=res.ifcb$centroid[8,2]-0.05,size=3)+
  annotate(geom="text" , label = category_names[9], x=res.ifcb$centroid[9,1], y=res.ifcb$centroid[9,2],size=3)+
  annotate(geom="text" , label = category_names[10], x=res.ifcb$centroid[10,1], y=res.ifcb$centroid[10,2]+0.1,size=3)+
  theme_minimal() +
  theme(legend.key = element_rect(fill = "lightgrey", colour = "white"),
        legend.position = "bottom")

biplot1_2

clothing$category <- as.factor(res.ifcb$cluster)

# drop columns that are not used
clothing <- clothing[,-c(4,14,18,24,29:30)]

## Create a variable LIKE that is LIKE if rating >4 and dislike if rating <=4
hist(clothing$rating)
summary(clothing$rating)
clothing$LIKE <- factor(ifelse(clothing$rating > 4, 1,0))
table(clothing$LIKE)
#> 70/30 of Dislike/Like!

## Normalize numeric values
discount <- clothing$retail_price - clothing$price
clothing$discount <- factor(ifelse(discount/clothing$retail_price <= 0,"no discount",
                                  ifelse(discount/clothing$retail_price <= 0.1,"up to 10%",
                                         ifelse(discount/clothing$retail_price <= 0.2,"up to 20%",
                                                ifelse(discount/clothing$retail_price <= 0.3,"up to 30%",
                                                       ifelse(discount/clothing$retail_price <= 0.5,"up to 50%",
                                                              ifelse(discount/clothing$retail_price <= 0.7,"up to 70%",
                                                                     ifelse(discount/clothing$retail_price <= 0.9,"up to 90%", "above 90%"))))))),
                            ordered = TRUE, 
                            levels=c("no discount", "up to 10%", "up to 20%", "up to 30%", "up to 50%", "up to 70%", "up to 90%", "above 90%"))
summary(clothing$discount)
colnames(clothing)

## Summary Table s
table1(~ price + shipping_option_price + units_sold + rating_count + merchant_rating_count, data = clothing, 
       caption="Table 1: Descriptive Statistics",digits = 2, rounding_perc = 1, transpose=TRUE)

## Drop columns
clothing <- clothing[,-c(1,3,6,8:12,19,22:24,26)]

clothing$product_color[is.na(clothing$product_color) == TRUE] <- "unknown"
clothing$product_variation_size_id[is.na(clothing$product_variation_size_id) == TRUE] <- "unknown"
colnames(clothing)
clothing[,c(3,5:9,12,14:17)] <- lapply(clothing[,c(3,5:9,12,14:17)], as.factor)

#-----------------------------------------------------------------------------------------------
##### Analysis 2: Predict Product Likeability
## Sort the Dataframe and create formula
clothing_sorted <- data.frame(LIKE=clothing[,16],clothing[,c(1:15,17)])
colnames(clothing_sorted)
formula <-as.formula("LIKE~price + units_sold + uses_ad_boosts + rating_count + badge_local_product + badge_product_quality +
                     badge_fast_shipping + product_color + product_variation_size_id + shipping_option_price + countries_shipped_to +
                     has_urgency_banner + merchant_rating_count + merchant_has_profile_picture + category + discount")

## change type of discount column
clothing_sorted$discount <- factor(as.character(clothing_sorted$discount))
## Stratify Data --> train/test (50/50)
set.seed(1)
train.index <- createDataPartition(clothing_sorted[,1], p = .65, list = FALSE)
train_data <- clothing_sorted[train.index,]
test_data  <- clothing_sorted[-train.index,]
table(train_data$LIKE)

#-----------------------------------------------------------------------------------------------
##### Analysis 2: Predict Product Likeability
# Decision Tree
set.seed(1)
dtree <- rpart(formula, data = train_data, method = 'class')

predicted <- predict(dtree, test_data, type = 'class')
test_data$predicted <- predicted
# Create a confusion matrix with the caret-package
confusionMatrix(data = predicted, reference = test_data$LIKE)

##### -----------------------------------------------------------------------------------------
## Bagging
set.seed(1)
# Test to see if random forest improves over bagging first use bagging with m=p
gbag <- bagging(formula, data = train_data, coob=TRUE)
print(gbag)

# Method 1 - accuracy on test set
predicted <- predict(gbag, test_data, type = 'class')
test_data$predicted <- predicted
# Create a confusion matrix with the caret-package
confusionMatrix(data = predicted, reference = test_data$LIKE)

##### -------------------------------------------------------------------------------------------
## Random Forest

# Tuning
set.seed(1)
# A mlr task has to be created in order to use the package
# (Classification task with makeClassifTask)
LIKE.task = makeClassifTask(data = train_data, target = "LIKE")

# Rough Estimation of the Tuning time
estimateTimeTuneRanger(LIKE.task)

results <- matrix(0,0,8)
# Store tuning results in results to find optimum number of trees
# Tuning measure is the multiclass brier score
for (i in c(100,250,500,750,1000)){
  set.seed(1)
  assign(paste0("rf_model_",i,sep=""), tuneRanger(LIKE.task,
                                                  measure = list(multiclass.brier),
                                                  num.trees = i,
                                                  num.threads = 5,
                                                  iters = 100,
                                                  iters.warmup = 20))
# Model with the new tuned hyperparameters
rf_pred <- predict(get(paste0("rf_model_",i,sep=""))$model, newdata = test_data)
# Create a confusion matrix with the caret-package
conf <- confusionMatrix(data = rf_pred$data[,4], reference = test_data$LIKE)

#Build results df
df <- cbind(Trees = i, get(paste0("rf_model_",i,sep=""))$recommended.pars[1:4], Accuracy = conf$overall[1], Kappa = conf$overall[2], F1_Score = conf$byClass[7])
results <- rbind(results, df)
par(mfrow=c(1,1))
plot(rownames(get(paste0("rf_model_",i,sep=""))$results), get(paste0("rf_model_",i,sep=""))$results[,4], main=paste("Stability using",i,"trees"), ylim=c(0.26,0.35))
lines(smooth.spline(rownames(get(paste0("rf_model_",i,sep=""))$results), get(paste0("rf_model_",i,sep=""))$results[,4], spar=0.4), col='blue', lwd=2)
}

res <- cbind(Trees_100 = rf_model_100$results[31:90,4],
             Trees_250 = rf_model_250$results[31:90,4],
             Trees_500 = rf_model_500$results[31:90,4],
             Trees_750 = rf_model_750$results[31:90,4],
             Trees_1000= rf_model_1000$results[31:90,4])

par(mar=c(5,6,4,1))
boxplot(res, horizontal = TRUE, las=1, 
        ylim =c(0.28,0.36), xlab="Loss-Function [Multiclass.Brier]", 
        main="Distribution of Random Forests", col = "grey")
points(x = results[,5], y = c(1,2,3,4,5), col="red", pch=20, cex = 1.2)
#> Choose 500 Trees
#> 1. Best Evaluation Scores
#     Accuracy: 78%
#     Kappa:    0.43
#     F1Score:  0.85
#> 2. Best Trade-off between Efficiency and Accuracy
rf_model_500$recommended.pars
#> Best Model:
#> Mtry = 7
#> min.node.size = 4
#> sample fraction = 0.708631

# Model with the new tuned hyperparameters
rf_pred <- predict(rf_model_500$model, newdata = test_data, type="prob")
rf_pred[,1]
# Create a confusion matrix with the caret-package
final_conf <- confusionMatrix(data = rf_pred$data[,4], reference = test_data$LIKE)
final_conf
#> Accuracy = 78%
#> Kappa =    0.43
final_conf$byClass
#> F1Score =  0.85

#-----------------------------------------------------------------------------------------------
############
## Determine final Random Forest

# Reload package due to some re-occuring error if the package is not reloaded...
detach("package:caret", unload = TRUE)
library(caret)

# Apply repeated cv on the final model
# Translate LIKE column from binary (0,1) to binary (DISLIKE, LIKE)
levels(train_data$LIKE) <- c("DISLIKE", "LIKE")
set.seed(1)
fitControl <- trainControl(method='repeatedcv',number=5,repeats=10, verboseIter=TRUE)
rf_grid <- expand.grid(mtry = 7,
                       splitrule = "gini",
                       min.node.size = 4)

final_model <- train(formula, data= train_data,
                     method="ranger",
                     trControl = fitControl,
                     tuneGrid = rf_grid,
                     replace = FALSE,
                     sample.fraction = 0.7086310,
                     num.trees = 500,
                     importance="impurity")
# Model with the new tuned hyperparameters
rf_pred <- predict(final_model, newdata = test_data)
# Create a confusion matrix with the caret-package
final_conf <- confusionMatrix(data = rf_pred, reference = factor(test_data$LIKE, labels = c("DISLIKE","LIKE")))
final_conf
#> Accuracy = 0.80
#> Kappa =    0.46
final_conf$byClass
#> F1Score =  0.87

#-----------------------------------------------------------------------------------------------
##### Model Agnostic
## 1. VIP
# create custom predict function
final_model_exp <- DALEX::explain(model = rf_model_500$model$learner.model, 
                                  data = train_data[,-1],
                                  y = train_data$LIKE == "LIKE", 
                                  label = "Random Forest")

set.seed(1)
# Create model_parts object for each model
vip_rf <- model_parts(explainer = final_model_exp, B = 50, type="difference", N=NULL, loss_function = loss_one_minus_auc)

# Plot VIP for each model
plot(vip_rf) +
  ggtitle("Mean variable-importance")

############################################################################
#### ALE PLOTS
set.seed(1)
#Create predictor to inspect feature effects
predictor <- Predictor$new(final_model, data = train_data, type="raw", class=2)

## First-order ALE (Main Effects)
# Price
set.seed(1)
ALE_price <- FeatureEffect$new(predictor,
                               feature=c("price"),
                               method = "ale",
                               center.at = NULL,
                               grid.size = 30)
# create ggplot for price
price <- plot(ALE_price) +
  theme_classic() +
  theme(legend.position = "bottom")

# Quality Badge
set.seed(1)
ALE_badge <- FeatureEffect$new(predictor,
                               feature=c("badge_product_quality"),
                               method = "ale",
                               center.at = NULL,
                               grid.size = 30)

# create ggplot for quality badge
badge <- plot(ALE_badge) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title.y=element_blank())

# Category
set.seed(1)
ALE_category <- FeatureEffect$new(predictor,
                               feature=c("category"),
                               method = "ale",
                               center.at = NULL,
                               grid.size = 30)

# create ggplot for categories
cat <- plot(ALE_category) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 25, vjust = 1, hjust=0.9))

# Rating_count
set.seed(1)
ALE_rating <- FeatureEffect$new(predictor,
                                  feature=c("rating_count"),
                                  method = "ale",
                                  center.at = NULL,
                                  grid.size = 25)

# create ggplot for rating_count
rat <- plot(ALE_rating) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 25, vjust = 1, hjust=0.9),
        axis.title.y=element_blank())

# Arrange ggplots in final object 
ggarrange(ggarrange(price, badge,rat, ncol=3),
          cat, nrow=2, heights=c(1,1.25))


## Second-order ALE (interaction effects)

# Category, Price
ALE_category_price <- FeatureEffect$new(predictor,
                                        feature=c("category","price"),
                                        method = "ale",
                                        center.at = NULL,
                                        grid.size = 30)

# create ggplot for category and price
cat_price <- plot(ALE_category_price) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 25, vjust = 1, hjust=0.9))

# Quality Badge, Price
ALE_badge_price <- FeatureEffect$new(predictor,
                         feature=c("badge_product_quality","price"),
                         method = "ale",
                         center.at = NULL,
                         grid.size = 30)

# create ggplot for badge and price
plot(ALE_badge_price) +
  theme_classic() +
  theme(legend.position = "bottom")

##### END #####