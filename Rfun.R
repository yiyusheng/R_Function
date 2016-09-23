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
list2df <- function(list,byrow = T,names = NULL){
  df <- data.frame(matrix(unlist(list),byrow = T,nrow = length(list)))
  df$item <- as.numeric(names(list))
  row.names(df) <- NULL
  if(!is.null(names)){
    names(df) <- names
  }
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
  df[sample(seq_len(len),len*perc,rep),]
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