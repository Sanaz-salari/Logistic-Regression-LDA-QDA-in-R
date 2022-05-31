func1 <- function(newdata){
  newdata <- na.omit(newdata)
  n <- nrow(newdata)
  score <- numeric(n)
  for (i in 1:n){
    score[i] <- exp(-8.259e-02 + -1.897e-03*newdata[i,'Distance'] +1.211e-02*newdata[i,'SchedElapsedTime']+-5.720e-02*newdata[i,'DepDelay']+7.418e-03*newdata[i,'ArrDelay']) /(1+exp(-1.897e-03*newdata[i,'Distance'] +1.211e-02*newdata[i,'SchedElapsedTime']+-5.720e-02*newdata[i,'DepDelay']+7.418e-03*newdata[i,'ArrDelay']))  
    }
return(score)
}

#exp(-1.391e-01 + 2.824e-04*newdata[i,'DepatureTime'] + -8.560e-01*newdata[i,'UniqueCarrierDL'] + -2.239e-01*newdata[i,'UniqueCarrierUA'] + 1.380e-02*newdata[i,'SchedElapsedTime'] + 1.092e-02*newdata[i,'ArrDelay'] + -6.229e-02*newdata[i,'DepDelay'] + -2.112e-03*newdata[i,'Distance'])/(1+exp(-1.391e-01 + 2.824e-04*newdata[i,'DepatureTime'] + -8.560e-01*newdata[i,'UniqueCarrierDL'] + -2.239e-01*newdata[i,'UniqueCarrierUA'] + 1.380e-02*newdata[i,'SchedElapsedTime'] + 1.092e-02*newdata[i,'ArrDelay'] + -6.229e-02*newdata[i,'DepDelay'] + -2.112e-03*newdata[i,'Distance']))


func2 <- function(newdata){
  score <- func1(newdata)
  label <- ifelse(score > 0.7, 1, 0)
  return(label)
  
}
func1(data_set)
func2(data_set)
