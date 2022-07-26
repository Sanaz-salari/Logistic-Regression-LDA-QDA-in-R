library(ISLR)
?Auto
head(Auto)
str(Auto)
dim(Auto)
length(unique(Auto$mpg))
colnames(Auto)

median(Auto$mpg) #calculate a median of a column in a dataframe
#mpg01=1 when mpg contains a value higher than median
mpg01 <- ifelse(Auto$mpg > median(Auto$mpg), 1, 0) #define a new binary variable
Auto$mpg01 <- mpg01 #add a new column to a dataframe
df <- data.frame(Auto) #create a new dataframe
colnames(df)

cor(df[,-9]) #calculate correlation between all columns except those are non numeric
head(df)
str(df)
pairs(df)
plot(df$mpg01, df$cylinders)
plot(df$mpg01, df$mpg)

#To plot a pairs() plot with two colors:
cols <- character(nrow(df))
cols[] <- "black"
cols[df$mpg01 == 1] <- "orangered"
cols[df$mpg01 == 0] <- "cornflowerblue"
pairs(df, col=cols)

par(mfrow=c(2,3))
boxplot(weight ~ mpg01, data = df, main = "Weight", 
        xlab = "mpg01", ylab = "Weight",
        col = c("cornflowerblue", "orangered"))
boxplot(year ~ mpg01, data = df, main = "Year", 
        xlab = "mpg01", ylab = "Year",
        col = c("cornflowerblue", "orangered"))
boxplot(cylinders ~ mpg01, data = df, main = "Cylinders", 
        xlab = "mpg01", ylab = "Cylinders",
        col = c("cornflowerblue", "orangered"))
boxplot(acceleration ~ mpg01, data = df, main = "Acceleration", 
        xlab = "mpg01", ylab = "Acceleration",
        col = c("cornflowerblue", "orangered"))
boxplot(displacement ~ mpg01, data = df, main = "Displacement", 
        xlab = "mpg01", ylab = "Displacement",
        col = c("cornflowerblue", "orangered"))
boxplot(horsepower ~ mpg01, data = df, main = "Horsepower", 
        xlab = "mpg01", ylab = "Horsepower",
        col = c("cornflowerblue", "orangered"))

library(MASS)
n <- nrow(df)
train_indx <- sample(1:n, 0.8*n)
test_indx <- setdiff(1:n, train_indx)
ml <- lda(mpg01 ~ weight + horsepower + displacement + cylinders, data = df, subset = train_indx)
summary(ml)
pred <- predict(ml, data = df[test_indx,])

#pred <- predict(ml, newdata = data.frame(weight=3500))
mq <- qda(mpg01 ~ weight + horsepower + displacement + cylinders, data = df, subset = train_indx)
summary(mq)
pred <- predict(mq, data = df[test_indx,])


mg <- glm(mpg01 ~ weight + horsepower + displacement + cylinders, data = df, subset = train_indx)
summary(mg)
pred <- predict(mg, data = df[test_indx,])

library(class)
train.X <-  cbind(cylinders, weight, displacement, horsepower)[train,]
test.X <-  cbind(cylinders, weight, displacement, horsepower)[test,]
train.mpg01 <-  mpg01[train]
set.seed(1)

# KNN (k=1)
knn.pred <-  knn(train.X, test.X, train.mpg01, k = 1)
mean(knn.pred != mpg01.test)

# KNN (k=10)
knn.pred <-  knn(train.X, test.X, train.mpg01, k = 10)
mean(knn.pred != mpg01.test)

# KNN (k=100)
knn.pred <-  knn(train.X, test.X, train.mpg01, k = 100)
mean(knn.pred != mpg01.test)
