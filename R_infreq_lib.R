#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: R_infreq_lib.R
#
# Description: custom function weed out from R_custom_lib.R
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-03-20 16:25:46
#
# Last   modified: 2017-03-20 16:25:48
#
#
#

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
# F2. calculate entropy for A column.
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

# F3. factor selected column
factorColX <- function(data,col){
  for (c in col){
    data[[c]] <- factor(data[[c]])
  }
  data
}

# F4. normalize array/col of matrix in range of [0,1]
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