# na.R

# 
# 
# ggplot(subset(df,sn == levels(df$sn)[6]),aes(x = time,y = result)) + 
#   geom_line() + ggtitle(levels(df$sn)[6])
# 
# roc <- roc(df$bilabel,df$result)
# roc.curve <- data.frame(threshold = roc$thresholds,1
#                         specificaties = roc$specificities,
#                         sensitivities = roc$sensitivities,
#                         FAR = 1 - roc$specificities,
#                         FDR = roc$sensitivities)
# ggplot(roc.curve,aes(x = FAR,y = FDR)) + geom_line() + xlim(c(0,0.1))

# dist.lt <- factorX(subset(result.pred,overThre == 1 & bilabel == 1))
# dist.chaos <- subset(result.pred,overThre == 1)
# dist.chaos$thre <- thre
# tmp1 <- subset(dist.chaos,bilabel == 1)
# tmp2 <- subset(dist.chaos,bilabel == 0)
# chaosRateT = sum(tmp1$exist.chaos)/nrow(tmp1), 
# chaosLenT = mean(tmp1$length[tmp1$exist.chaos == 1]),
# chaosRateF = sum(tmp2$exist.chaos)/nrow(tmp2),
# chaosLenF = mean(tmp2$length[tmp2$exist.chaos == 1]),
# leadtime = dist.lt,
# ltchaos = dist.chaos,
# evaluate lead time
# perf.lt <- do.call(rbind,lapply(tmp,'[[','leadtime'))
# p2 <- ggplot(subset(perf.lt,lt != -1),aes(x = lt/1440,group = thre,color = factor(thre))) + stat_ecdf()

# evaluate chaos region
# perf.chaos <- do.call(rbind,lapply(tmp,'[[','ltchaos'))
# p3 <- ggplot(subset(perf.chaos,exist.chaos == 1),aes(x = length/1440,group = thre,color = factor(thre))) + stat_ecdf()
