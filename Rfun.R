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

# F3. calculate entropy for A column.
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

# F5. table for more columns
colTableX <- function(data,col,decreasing = T,rm.na = F){
  colMerge <- data[[col[1]]]
  for (i in seq(2,length(col))){
    colMerge <- paste(colMerge,data[[col[i]]],sep='_')
  }
  return(tableX(colMerge,decreasing = decreasing))
}

# F6. split col and merger them into a new data.frame
splitToDF <- function(data,split = '_',header = ''){
  r <- data.frame(matrix(unlist(strsplit(as.character(data),split)),byrow = T,nrow = length(data)))
  if (header[1] != ''){
    names(r) <- header
  }
  return(r)
}

# F7. Convert factor to its original data type
fct2ori <- function(f){
  a <- levels(f)[f]
  a
}

# F8. Quantile with seq(0,1,0.01)
quantileX <- function(v){
  quantile(v,seq(0,1,0.01),na.rm = T)
}

# F9. match array of attr from A to B(No Merge)
mchAttr <- function(dfA,dfB,idxA,idxB,attr){
  for(a in attr){
    dfA[[a]] <- dfB[[a]][match(dfA[[idxA]],dfB[[idxB]])]
  }
  dfA
}

# F10.unname all columns in dataframe
unnameX <- function(df){
  for(i in 1:length(df)){
    df[,i] <- unname(df[,i])
  }
  df
}

# F11. Multiplot
multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  require(grid)
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots == 1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# F12.convert list from tapply to data.frame
list2df <- function(list,n = NULL,br = T,rec = F){
  df <- data.frame(matrix(unlist(list,recursive = rec),byrow = br,nrow = length(list)))
  x <- ifelse(is.null(names(list)),1 == 1,df$item <- names(list))
  x <- ifelse(is.null(n),1 == 1,names(df) <- n)
  df
}

# F13.convert factor to origin, then convert to numeric
fct2num <- function(arr){
  arr <- as.numeric(fct2ori(arr))
  arr
}

# F14.return multiple value and recive by list
# from https://raw.githubusercontent.com/ggrothendieck/gsubfn/master/R/list.R
list <- structure(NA,class="result")
"[<-.result" <- function(x,...,value) {
  args <- as.list(match.call())
  args <- args[-c(1:2,length(args))]
  length(value) <- length(args)
  for(i in seq(along=args)) {
    a <- args[[i]]
    if(!missing(a)) eval.parent(substitute(a <- v,list(a=a,v=value[[i]])))
  }
  x
}

# F15.extract some lines from a data.frame randomly
smp_df <- function(df,perc,rep = F){
  len <- nrow(df)
  if(perc <= 1){
    df[sample(seq_len(len),len*perc,rep),]
  }else{
    df[sample(seq_len(len),perc,rep),]
  }
}

# F16. colMax like colSum
colMax <- function(data) sapply(data, max, na.rm = TRUE)

# F17. parallal lapply
lapplyX <- function(paraObj,fun = fun,coreUse = 0.9){
  require(doParallel)
  numCore <- floor(detectCores()*coreUse)
  ck <- makeCluster(min(numCore,length(paraObj)),outfile = '')
  registerDoParallel(ck)
  r <- foreach(i = paraObj,.verbose = T) %dopar% fun(i)
  stopCluster(ck)
  r
}

# F18.order or column change
col_order <- function(df,tar,app){
  n <- names(df)
  idx.tar <- which(n == tar)
  
  n.noapp <- n[n != tar]
  idx.app <- which(n == app)
  
  
  newN <- c(n.noapp[1:idx.app],n[idx.tar],n.noapp[(idx.app+1):length(n.noapp)])
  
  df <- df[,newN]
}

# F19.return a data.frame with all factor column restored
fct2oriX <- function(data){
  a <- sapply(data,class)
  for(i in 1:length(a)){
    if(a[i]=='factor')
      data[,i] <- fct2ori(data[,i])
  }
  return(data)
}

# F20.melt table
meltX <- function(x,y){
  tmp <- melt(table(x,y))
  tmp <- subset(tmp,value != 0)
  tmp
}

# F21. as.POSIXct convert
as.p <- function(ts){
  if(class(ts) == 'factor'){
    r <-   as.POSIXct(fct2ori(ts),tz = 'UTC')
  }else if(class(ts) == 'character'){
    r <-   as.POSIXct(ts,tz = 'UTC')
  }
  r
}

# F22.factorX(subset)
subsetX <- function(...){
  factorX(subsetX(...))
}

# F23.foreachX
foreachX <- function(Object.para,func,...){
  f <- get(func)
  para <- Object.para
  require(doParallel)
  ck <- makeCluster(min(40,length(para)), outfile = '')
  registerDoParallel(ck)
  r <- foreach(i = para,.verbose = T) %dopar% f(i,...)
  stopCluster(ck)
  r
}

# F24. define margin for plot function
def_margin <- function(){
  par(mar = rep(1,4))
}

# F25. evaluate biclassification
base_eval <- function(resp,pred){
  TP <- sum(resp == 1 & pred == 1)
  FN <- sum(resp == 1 & pred == 0)
  TN <- sum(resp == 0 & pred == 0)
  FP <- sum(resp == 0 & pred == 1)
  
  P <- TP + FN
  N <- TN + FP
  
  FDR <- TP/P
  FAR <- FP/N
  
  ACC <- (TP + TN)/length(resp)
  
  F1 <- (2*TP)/(2*TP + FP + FN)
  
  list(tp = TP,fn = FN,tn = TN,fp = FP,
       p = P,n = N,
       fdr = FDR,far = FAR,f1 = F1,acc = ACC)
}

# F26. change names from A to B in a data.frame
change_name <- function(df,n1,n2){
  names(df)[which(names(df) == n1)] <- n2
  df
}

# F27. normalize array/col of matrix in range of [0,1]
normalize <- function(x){
  x <- as.matrix(x)
  minAttr=apply(x, 2, min)
  maxAttr=apply(x, 2, max)
  x <- sweep(x, 2, minAttr, FUN="-") 
  x=sweep(x, 2,  maxAttr-minAttr, "/") 
  attr(x, 'normalized:min') = minAttr
  attr(x, 'normalized:max') = maxAttr
  return (x)
}

# F28.extract string with regexp
extract_reg <- function(pat,str,sameLen = T){
  idx_need <- grep(pat,str)
  idx_start <- regexpr(pat,str)
  r <- rep('',length(idx_start))
  r[idx_need] <- regmatches(str,idx_start)
  if(sameLen)return(r)
  else return(r[idx_need])
}

# F29.sort level of factor by number
sort_level <- function(f){
  f <- factor(f,levels = sort(fct2num(f)))
}

# F30. melt(table(x)) ,remove item of zero and modify the name
melt_table <- function(...){
  r <- melt(table(...))
  r <- subset(r,value != 0)
  r
}

# F31. coefficient variable
coef_var <- function(arr){
  sd(arr,na.rm = T)/mean(arr,na.rm = T)
}

# F32. round with 4 digits
roundX <- function(arr){
  round(arr,digits = 4)
}

# F33. remove items(lines) with any element of it is some value (default:NA)
remove_line_byvalue <- function(df,v = NA){
  idx <- which(rowSums(is.na(df)) != 0)
  factorX(df[-idx,])
}

# F34. create a data.frame which is in the same structure and a default value with a existing one
create_mirror_df <- function(df,default = 0){
  df[seq_len(dim(df)[1]),seq_len(dim(df)[2])] <- default
  df
}

# F35. rbind data.frame from lapply
lapplyX <- function(...){
  do.call(rbind,lapply(...))
}