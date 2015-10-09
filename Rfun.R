#Rfun
tableX <- function(data,decreasing = T){
  a <- table(data)
  b <- data.frame(item = names(a),
                  count = as.numeric(a))
  b$rate <- b$count/sum(b$count)
  if(decreasing == T)
    b <- b[order(b$rate,decreasing = T),]
  else
    b <- b[order(b$rate),]
  return(b)
}