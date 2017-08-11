#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: R_custom_lib.R
#
# Description: this file saves all global custom function I write to improve my efficience. 
#              For each project, they have a base.R to save the custom function for the project 
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-03-20 16:25:46
#
# Last   modified: 2017-04-07 17:13:18
#
#
#

# F1. return a data.frame with all column factored
factorX <- function(data){
  a <- sapply(data,class)
  for(i in 1:length(a)){
    if(a[i]=='factor')
      data[,i] <- factor(data[,i])
  }
  return(data)
}

# F2. Convert factor to its original data type
fct2ori <- function(f){
  a <- levels(f)[f]
  a
}

# F3.convert factor to origin, then convert to numeric
fct2num <- function(arr){
  arr <- as.numeric(fct2ori(arr))
  arr
}

# F4.return a data.frame with all factor column restored
fct2oriX <- function(data){
  a <- sapply(data,class)
  for(i in 1:length(a)){
    if(a[i]=='factor')
      data[,i] <- fct2ori(data[,i])
  }
  return(data)
}

# F5. Quantile with seq(0,1,0.01)
quantileX <- function(v,itv = 0.01,dgt = 4){
  round(quantile(v,seq(0,1,itv),na.rm = T),digits = dgt)
}

# F6. match array of attr from A to B(No Merge)
mchAttr <- function(dfA,dfB,idxA,idxB,attr){
  for(a in attr){
    dfA[[a]] <- dfB[[a]][match(dfA[[idxA]],dfB[[idxB]])]
  }
  dfA
}

# F7.unname all columns in dataframe to remove Attr
unnameX <- function(df){
  for(i in 1:length(df)){
    df[,i] <- unname(df[,i])
  }
  df
}

# F8. Multiplot
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

# F9.convert list from tapply to data.frame
list2df <- function(list,n = NULL,br = T,rec = F){
  df <- data.frame(matrix(unlist(list,recursive = rec),byrow = br,nrow = length(list)))
  x <- ifelse(is.null(names(list)),1 == 1,df$item <- names(list))
  x <- ifelse(is.null(n),1 == 1,names(df) <- n)
  # for(i in seq_len(ncol(df))){
  #   if(is.list(df[,i]))df[,i] = unlist(df[,i])
  # }
  df
}



# F10.return multiple value and recive by list
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

# F11.extract some lines from a data.frame randomly
smp_df <- function(df,perc,rep = F){
  len <- nrow(df)
  if(perc <= 1){
    df[sample(seq_len(len),len*perc,rep),]
  }else{
    df[sample(seq_len(len),perc,rep),]
  }
}

# F12.order or column change
col_order <- function(df,tar,app){
  n <- names(df)
  idx.tar <- which(n == tar)
  
  n.noapp <- n[n != tar]
  idx.app <- which(n == app)
  
  newN <- c(n.noapp[1:idx.app],n[idx.tar],n.noapp[(idx.app+1):length(n.noapp)])
  df <- df[,newN]
}

# F13. as.POSIXct convert
as.p <- function(ts){
  if(class(ts) == 'factor'){
    r <-   as.POSIXct(fct2ori(ts),tz = 'UTC')
  }else if(class(ts) == 'character'){
    r <-   as.POSIXct(ts,tz = 'UTC')
  }
  r
}

as.p1 <- function(ts){
  if(class(ts) == 'factor'){
    r <-   as.POSIXct(fct2ori(ts))
  }else if(class(ts) == 'character'){
    r <-   as.POSIXct(ts)
  }
  r
}

# F14. define margin for plot function
def_margin <- function(){
  par(mar = rep(1,4))
}

# F15. evaluate biclassification
base_classification_eval <- function(resp,pred){
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

# F16. change names from A to B in a data.frame
change_name <- function(df,n1,n2){
  names(df)[which(names(df) == n1)] <- n2
  df
}

# F17.extract string with regexp
extract_reg <- function(pat,str,sameLen = T){
  idx_need <- grep(pat,str)
  idx_start <- regexpr(pat,str)
  r <- rep('',length(idx_start))
  r[idx_need] <- regmatches(str,idx_start)
  if(sameLen)return(r)
  else return(r[idx_need])
}

# F18.sort level of factor by number
sort_level <- function(f){
  f <- factor(f,levels = sort((as.numeric(levels(f)))))
}

# F19. melt(table(x)) ,remove item of zero and modify the name
melt_table <- function(...){
  r <- melt(table(...))
  r <- subset(r,value != 0)
  r
}

# F20. coefficient variable
coef_var <- function(arr){
  sd(arr,na.rm = T)/mean(arr,na.rm = T)
}

# F21. round with 4 digits
roundX <- function(arr){
  round(arr,digits = 4)
}

# F22. remove items(lines) with any element of it is some value (default:NA)
remove_line_byvalue <- function(df,v = NA){
  if(is.na(v)|is.null(v)){
    idx <- which(rowSums(is.na(df)|is.null(v)) != 0)
  }else{
    idx <- which(rowSums(df == v) != 0)
  }
  if(length(idx) == 0){return(df)}
  else{return(factorX(df[-idx,]))}
}

# F23. create a data.frame which is in the same structure and a default value with a existing one
create_mirror_df <- function(df,default = 0){
  df[seq_len(dim(df)[1]),seq_len(dim(df)[2])] <- default
  df
}

# F24A. rbind data.frame from lapply
lapplyX <- function(...){
  do.call(rbind,lapply(...))
}

# F24B. rbind data.frame from tapply
tapplyX <- function(...){
  do.call(rbind,tapply(...))
}

# F25. wrap foreach from doParallel
foreachX <- function(idx,func,outname = NULL,frac_cores = 0.8,...){
  f <- get(func)
  
  if(is.null(outname))outname <- func
  if(outname == -1)outname <- NULL
  outname <- paste(outname,format(Sys.time(), "%Y%m%d-%H%M%S"),sep='@')
  path_outname <- file.path(dir_d,'log',outname)
  if(file.exists(path_outname))x <- file.remove(path_outname)
  
  require(doParallel)
  ck <- makeCluster(min(floor(detectCores()*frac_cores),length(idx)),type = 'FORK',outfile = path_outname)
  registerDoParallel(ck)
  r <- foreach(i = idx,.verbose = F) %dopar% tryCatch(f(i,...),error = function(e){cat(sprintf("ERROR:%s\nIn %s\n",i,e))})
  stopCluster(ck)
  r
}

# F26. parallal lapply
lapply_foreach <- function(lst,func,outname = 'on',...){
  f <- get(func)
  idx <- length(lst)
  r <- foreachX(idx,func,outname,...)
}

# F27. give a random palette
rand_palette <- function(){
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  cbPalette[sample(seq_len(length(cbPalette)),length(cbPalette),F)]
}

# F28. generate rate of a array
# When you use this array with apply,the result will be column based
# transfor the result if needed
array_rate <- function(arr){
  roundX(arr/sum(arr,na.rm = T))
}

# F29. subset wrapped by factorX
subsetX <- function(...){
  factorX(subset(...))
}

# F30. check exist of dir. If false, then create it
check_dir <- function(d){
  if(!dir.exists(d)){
    dir.create(d)
    cat(sprintf('CREATE DIRECTORY %s',d))
  }
}

# F31. replace value1 with value2 in a data.frame
replace_value <- function(df,v1 = NA,v2 = 0){
  if(is.na(v1)){
    df[is.na(df)] <- v2
  }else if(is.null(v1)){
    df[apply(df,2,function(x)is.null(as.numeric(x)))] <- v2
  }else if(is.infinite(v1)){
    df[apply(df,2,function(x)is.infinite(as.numeric(x)))] <- v2
  }else if(is.nan(v1)){
    df[apply(df,2,function(x)is.nan(as.numeric(x)))] <- v2
  }else{
    df[df == v1] <- v2
  }
  return(df)
}

# F32. modify unformat time to 1970-01-01 00:00:00
modify_unformat_time <- function(arr){
  if(is.factor(arr))arr <- fct2ori(arr)
  arr[!grepl('\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}',arr)] <- '1970-01-01 00:00:00'
  as.p(arr)
}

# F33. return the last element
last <- function(x) { return( x[length(x)] ) }

# F34. mode of a array of numerical
mode_num <- function(arr){
  return(as.numeric(names(sort(-table(arr)))[1]))
}

# F35. clear directory
clear_dir <- function(dir){
  fname <- list.files(dir)
  file.remove(file.path(dir,fname))
  return(0)
}

# F36. get index of split
get_split_index <- function(fct){
  split(seq_len(length(fct)),fct)
}