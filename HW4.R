library (freqparcoord)
library(regtools)
data(mlb)

xvalpart <- function(data, p) { 
  n <- nrow(data)
  ntrain <- round(p*n)
  trainidxs <- sample(1:n, ntrain ,replace = FALSE)
  list(train = data[trainidxs,], valid = data[-trainidxs,])
}

xvallm <- function(data, ycol, predvars, p, meanabs=TRUE) { 
  tmp <- xvalpart(data,p)
  train <- tmp$train
  valid <- tmp$valid
  trainy <- train [,ycol]
  trainpreds <- train [,predvars]
  trainpreds <- as.matrix(trainpreds)
  lmout <- lm( trainy ~ trainpreds )
  validpreds <- as.matrix(valid[,predvars])
  predy <- cbind(1,validpreds) %*% coef(lmout)
  realy <- valid [,ycol]
  if (meanabs)
    return(mean(abs(predy - realy )))
  list (predy = predy , realy = realy )
}

xvalknn <-
  function(data, ycol, predvars, k, p, meanabs=TRUE){
    data <- data[, c(predvars, ycol)]
    tmp <- xvalpart(data,p)
    train <- tmp$train
    valid <- tmp$valid
    valid <- as.matrix(valid)
    xd <- preprocessx(train[,-ycol],k)
    kout <- knnest(train[,ycol],xd,k)
    predy <- predict(kout , valid[,-ycol ], TRUE)
    realy <- valid [, col]
    if (meanabs)
      return(mean(abs(predy - realy )))
    list (predy = predy , realy = realy )
  }

index = c(1,2,3,4,5)
lm_matrix <- matrix(NA, nrow= 5, ncol = 1)
knn_matrix <-  matrix(NA, nrow= 5, ncol = 1)

for (i in index) {
  lm_matrix[i] <- xvallm(mlb, 5 , c(4,6), 2/3)
  knn_matrix[i] <- xvallm(mlb, 5 , c(4,6), 2/3)
}

output.df = data.frame(cbind(lm_matrix, knn_matrix))
colnames(output.df) = c("LM Validation", "KNN Validation")
print(output.df)

##Part 2 
data(prgeng)
prgeng$age2 <- prgeng$age^2
edu <- prgeng$educ
prgeng$ms <- as.integer(edu == 14)
prgeng$phd <- as.integer(edu == 16)
prgeng$fem <- prgeng$sex - 1
tmp <- prgeng[edu >= 13,]
pe <- tmp[,c(1, 12, 9 , 13, 14, 15, 8)]
pe <- as.matrix(pe)

org_lm <- lm(wageinc ~ age+age2+wkswrkd+ms+phd+fem, data=prgeng)
mod_lm <- lm(wageinc ~ age+age2+wkswrkd+ms+phd+fem + age:fem + age2:fem, data=prgeng)

pred_org_lm <- predict(org_lm, data.frame(age = 32, age2 = 32^2, 
                                          wkswrkd = mean(prgeng$wkswrkd), ms = 1, phd = 0, fem = 1))
pred_mod_lm <- predict(mod_lm,  data.frame(age = 32, age2 = 32^2, 
                                           wkswrkd = mean(prgeng$wkswrkd), ms = 1, phd = 0, fem = 1))

output2.df = data.frame(cbind(pred_org_lm, pred_mod_lm))
colnames(output2.df) = c("Original Model", "Mod. Model (Interactive Terms)")
print(output2.df)

##Part 3 
library(readr)
bodyfat <- read_csv("bodyfat.csv")
bodyfat2 <- subset(bodyfat, select = siri:wrist)

bf_temp <- xvalpart(bodyfat2, 0.25)

bodyfat_lm <- lm(density ~ siri + age + weight + height + neck + chest + abdomen + hip + thigh + 
                   ankle + forearm + wrist, data = bf_temp$train)

bf_temp$train$density_pred <- predict(bodyfat_lm, data.frame(bf_temp$train))
bf_temp$train$abs_error <- abs(bf_temp$train$density - bf_temp$train$density_pred)
bf_temp$train$sqrt_abs_error <- sqrt((bf_temp$train$density - bf_temp$train$density_pred)^2)

bodyfat_lm2 <- lm(density ~ siri + age + weight + height + neck + chest + abdomen + hip + thigh + 
                    ankle + forearm + wrist, data = bf_temp$valid)

bf_temp$valid$density_pred <- predict(bodyfat_lm2, data.frame(bf_temp$valid))
bf_temp$valid$abs_error <- abs(bf_temp$valid$density - bf_temp$valid$density_pred)
bf_temp$valid$sqrt_abs_error <- sqrt((bf_temp$valid$density - bf_temp$valid$density_pred)^2)

output3.df = data.frame(cbind(mean(bf_temp$train$abs_error), mean(bf_temp$train$sqrt_abs_error), 
                              mean(bf_temp$valid$abs_error), mean(bf_temp$valid$sqrt_abs_error)))
colnames(output3.df) = c("Train MAE", "Train RMSE", "Valid MAE", "Valid - RMSE")
print(output3.df)





##page 120 exericse 2
shar <- read.csv("day.csv")
shar$temp2 <- (shar$temp)^2
shar$clearday <- as.integer(shar$weathersit == 1)
bikeshare_lm <- lm(registered ~ temp + temp2 + workingday + clearday + yr, data = shar)
confint(bikeshare_lm, "yr")

