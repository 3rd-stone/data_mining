setwd("C:/Users/C.Lei/Desktop/8410 assignment2/assignment2")

stock = read.csv("stock_prices_2018.csv",header = T)
attach(stock)

#improved knn

dist_p <- function(p, train_noy, trainline_number, test_noy){
  sum_dist_p <- 0
  for (i in 1:ncol(test_noy)){
    if (is.factor(test_noy[1, i]) == TRUE){
      if (train_noy[trainline_number, i] == test_noy[1, i]){
        sum_dist_p <- sum_dist_p + 0
      }
      else{
        sum_dist_p <- sum_dist_p + 1
      }
    }
    else{
      sum_dist_p <- sum_dist_p + (train_noy[trainline_number, i] - test_noy[1, i])^p
    }
  }
  sum_dist_p^(1/p)
}

find_k_nearest <- function(train, test, k, p){
  k_nearest <- data.frame()
  for (i in 1:nrow(train)){
    if (i <= k){
      k_nearest <- rbind(k_nearest,data.frame(train[i, -ncol(train)], dist_p(p, train[, -ncol(train)], i, test[, -ncol(test)]), train[i, ncol(train)]))
    }else{
      if (dist_p(p, train[, -ncol(train)], i, test[, -ncol(test)]) == max(k_nearest[, ncol(k_nearest)-1])){
        k_nearest <- rbind(k_nearest,data.frame(train[i, -ncol(train)], dist_p(p, train[, -ncol(train)], i, test[, -ncol(test)]), train[i, ncol(train)]))
        }
      if (dist_p(p, train[, -ncol(train)], i, test[, -ncol(test)]) < max(k_nearest[, ncol(k_nearest)-1])){
        k_nearest <- k_nearest[-which.max(k_nearest[, ncol(k_nearest)-1]), ]
        k_nearest <- rbind(k_nearest,data.frame(train[i, -ncol(train)], dist_p(p, train[, -ncol(train)], i, test[, -ncol(test)]), train[i, ncol(train)]))
      }
    }
  }
  colnames(k_nearest)[ncol(k_nearest)-1] <- 'distance'
  colnames(k_nearest)[ncol(k_nearest)] <- 'Change'

  sort_k_nearest <- k_nearest[order(k_nearest$distance),]
  if (k == nrow(sort_k_nearest)){
    return(sort_k_nearest)
  }else{
    for (i in k:nrow(sort_k_nearest)-1){
      if (sort_k_nearest[i, ncol(k_nearest)-1] != sort_k_nearest[i+1, ncol(k_nearest)-1]){
        return(sort_k_nearest[1:i, ])
      }else{
        i <- i + 1
      }
    }
  }
}


knn <- function(train, test, k, p){
  k_nearest <- find_k_nearest(train, test, k, p)
  if (sum(k_nearest[, ncol(k_nearest)] == "up") < sum(k_nearest[, ncol(k_nearest)] == "down")){
    predict_change <- "down"
  }else{
    predict_change <- "up"
  }
  predict_change
}



#trials n times
n = 10
seq_up_down <- c(0,0,0,0)

for (i in 1:n){
  set.seed(123)
  sample_sector <- sample(unique(SubSector), 1, replace = F)
  train <- which(SubSector == sample_sector)
  
  testline <- sample(train, 1, replace = F)
  trainline <- train[-match(testline, train)]
  
  scale_stock_contin <- scale(data.frame(Open, High, Low, Close, Volume, PriorClose, HMLOL))
  knn_train_subsector <- data.frame(Weekday, Month, scale_stock_contin, Change)[trainline, ]
  knn_test_subsector <- data.frame(Weekday, Month, scale_stock_contin, Change)[testline, ]
  
  predict_change <- knn(train = knn_train_subsector, test=knn_test_subsector, k=10, p=2)
  if (predict_change == knn_test_subsector[1, ncol(knn_test_subsector)]){
    if (predict_change == "up"){
      seq_up_down[1] <- seq_up_down[1]+1
    }
    if (predict_change == "down"){
      seq_up_down[4] <- seq_up_down[4]+1
    }
  }else{
    if (predict_change == "up"){
      seq_up_down[3] <- seq_up_down[3]+1
    }
    if (predict_change == "down"){
      seq_up_down[2] <- seq_up_down[2]+1
    }
  }
  print(seq_up_down)
}




set.seed(3)
sample_sector <- sample(unique(SubSector), 1, replace = F)
selected_lines <- which(SubSector == sample_sector)

#5 fold cross validation
for (i in (1:5)){
  scale_stock_contin <- scale(data.frame(Open, High, Low, Close, Volume, PriorClose, HMLOL))
  
  testline <- selected_lines[round((i-1)/5*length(selected_lines)):round((i)/5*length(selected_lines))]
  trainline <- selected_lines[-match(testline, selected_lines)]
  
  knn_train_subsector <- data.frame(Weekday, Month, scale_stock_contin, Change)[trainline, ]
  knn_test_subsector <- data.frame(Weekday, Month, scale_stock_contin, Change)[testline, ]
  
  seq_up_down <- c(0,0,0,0)
  for (j in(1:nrow(knn_test_subsector))){
    predict_change <- knn(train = knn_train_subsector, test=knn_test_subsector[j, ], k=10, p=2)
  
    if (predict_change == knn_test_subsector[j, ncol(knn_test_subsector)]){
      if (predict_change == "up"){
        seq_up_down[1] <- seq_up_down[1]+1
      }
      if (predict_change == "down"){
        seq_up_down[4] <- seq_up_down[4]+1
      }
    }else{
      if (predict_change == "up"){
        seq_up_down[3] <- seq_up_down[3]+1
      }
      if (predict_change == "down"){
        seq_up_down[2] <- seq_up_down[2]+1
      }
    }
    print(seq_up_down)
  }
  #confusion matrix
  result = matrix(seq_up_down,c(2,2))
  colnames(result) <- c("real-up","real-down")
  rownames(result) <- c("pre-up","pre-down")
  pring(result)
}










errorrate = 1- (seq_up_down[1] + seq_up_down[4])/n
errorrate






#svm
