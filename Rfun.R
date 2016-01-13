# advanced R function

# 1. return a ordered data.frame of a table(column) with rate
tableX <- function(data,decreasing = T){
  a <- table(factor(data))
  b <- data.frame(item = names(a),
                  count = as.numeric(a))
  b$rate <- b$count/sum(b$count)
  
  if(decreasing == T)
    b <- b[order(b$rate,decreasing = T),]
  else
    b <- b[order(b$rate,decreasing = F),]
  row.names(b) <- NULL
  return(b)
}

# 2. return a data.frame with all column factored
factorX <- function(data){
  a <- sapply(data,class)
  for(i in 1:length(a)){
    if(a[i]=='factor')
      data[,i] <- factor(data[,i])
  }
  return(data)
}

# 3. 计算A列中每种类型中B占比的熵,再以A中各类的占比作为权值计算平均熵,即B关于A的条件熵
entropy <- function(A,B) {
  A <- factor(A)
  B <- factor(B)
  t1 <- table(A)
  t2 <- table(B)
  t1 <- data.frame(name = names(t1),perc = as.numeric(t1)/sum(t1))
  t2 <- data.frame(name = names(t2),perc = as.numeric(t2)/sum(t2))
  t1$entro <- 0
  tmp <- tapply(B,A,function(x){
    s <- length(x)
    per <- as.numeric(table(x))/s
    per <- per[per!=0]
    return(-1*sum(per*log2(per)))         #entropy of each class of A
  })
  entro_A <- sum(t1$perc*as.numeric(tmp))      #conditional entropy
  return(list(entro = entro_A,entro_each = tmp))
}

# 4. factor selected column
factorColX <- function(data,col){
  for (c in col){
    data[[c]] <- factor(data[[c]])
  }
  data
}