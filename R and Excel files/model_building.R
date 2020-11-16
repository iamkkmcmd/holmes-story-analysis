# Clean up R studio environment
rm(list = ls())
graphics.off()

# set directory
setwd('K:/Data Science/Sherlock Holmes/')
list.files(pattern = '.csv')

# read the data
data <- read.csv('holmesfeatures.csv')

# Checking missing value
length(which(!complete.cases(data)))
str(data)
dim(data)

# Convert rank(1 to 56) into quality (1 to 5) star
data$quality <- as.numeric(cut(-data$rank, breaks = 5, labels = FALSE))

# Model with all variable
str(data)
data1 <- data[,-c(1,2,3)]
fml_full <- formula(quality~nos+now+awps+acpw+em+qm+adj+adv+pron+propn+verb)

# Cross validation folds
cv <- sample(rep(1:10, length_out = nrow(data1)))

# create model
max_vars <- 0:10
mse <- rep(0, length(max_vars))
for(i in 1:length(max_vars)){
  pred <- rep(0, nrow(data1))
  for(j in 1:10){
    train <- cv != j
    test <- !train
    m_null <- lm(quality~1, data1, subset = train)
    m_sel <- step(m_null, fml_full, steps = max_vars[i], k = 0)
    pred[test] <- predict(m_sel, data1)[test]
  }
  mse[i] <- mean((pred-data1$quality)^2)
}

# Eaxamine error vs number of variables in model
plot(max_vars, mse, type = 'b', xlab = 'Number of variable in model',
     ylab = 'Cross-Validation Error', main = 'Plot for choosing number of variables')
points(max_vars[which.min(mse)], min(mse), pch = 16, col = 'red')

m_null <- lm(quality~1, data1)
m_sel <- step(m_null, fml_full, steps = 3, k = 0)
summary(m_sel)


# Random forest model
data2 <- data[,-c(1,2,3,4,5,28)]
str(data2)
table(data2$collection)
set.seed(1234)
ind <- sample(2, nrow(data2), replace = TRUE, prob = c(0.7, 0.3))
train <- data2[ind == 1,]
test <- data2[ind == 2,]

library(randomForest)
set.seed(1234)
rf <- randomForest(collection ~ ., data = train, ntree = 200, mtry = 8, importance = TRUE, proximity = TRUE)
rf
attributes(rf)

# Prediction
library(caret)
p1 <- predict(rf, train)
head(p1)
head(train$collection)
confusionMatrix(p1, train$collection)

p2 <- predict(rf, test)
confusionMatrix(p2, test$collection)

plot(rf)

t <- tuneRF(train[,-1], train[,1],
       stepFactor = 0.5,
       plot = TRUE,
       ntreeTry = 200,
       trace = TRUE,
       improve = 0.05)


ggplot(data = data,
       aes(x = now))+geom_histogram()
