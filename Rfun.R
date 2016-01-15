# advanced R function

# F1. return a ordered data.frame of a table(column) with rate
tableX <- function(data,decreasing = T){
  a <- table(factor(data))
  b <- data.frame(item = names(a),
                  count = as.numeric(a))
#   if (is.numeric(as.numeric(levels(names(a))))){
#     b$item <- as.numeric(levels(b$item)[b$item])
#   }
  b$rate <- b$count/sum(b$count)
  
  if(decreasing == T)
    b <- b[order(b$rate,decreasing = T),]
  else
    b <- b[order(b$rate,decreasing = F),]
  row.names(b) <- NULL
  return(b)
}

# F2. return a data.frame with all column factored
factorX <- function(data){
  a <- sapply(data,class)
  for(i in 1:length(a)){
    if(a[i]=='factor')
      data[,i] <- factor(data[,i])
  }
  return(data)
}

# F3. 计算A列中每种类型中B占比的熵,再以A中各类的占比作为权值计算平均熵,即B关于A的条件熵
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

# F4. factor selected column
factorColX <- function(data,col){
  for (c in col){
    data[[c]] <- factor(data[[c]])
  }
  data
}

# F5. 为多行内容进行table
colTableX <- function(data,col,decreasing = T,rm.na = F){
  colMerge <- data[[col[1]]]
  for (i in seq(2,length(col))){
    colMerge <- paste(colMerge,data[[col[i]]],sep='_')
  }
  return(tableX(colMerge,decreasing = decreasing))
}

# F6. 拆分合并之后的col，并输出data.frame
splitToDF <- function(data,split = '_',header = ''){
  r <- data.frame(matrix(unlist(strsplit(as.character(data),split)),byrow = T,nrow = length(data)))
  if (header[1] != ''){
    names(r) <- header
  }
  return(r)
}