#@@@ 计算A列中每种类型中B占比的熵,再以A中各类的占比作为权值计算平均熵,即B关于A的条件熵
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