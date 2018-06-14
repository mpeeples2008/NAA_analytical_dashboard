group.mem.probs <- function(x2.l,attr1,grps) {
  
  probs <- list()
  for (m in 1:length(grps)) {
    x <- x2.l[which(attr1$CORE==grps[m]),]
    probs[[m]] <- matrix(0,nrow(x),length(grps))
    colnames(probs[[m]]) <- grps
    rownames(probs[[m]]) <- rownames(x)
    
    grps2 <- grps[-m]
    
    p.val <- NULL
    for (i in 1:nrow(x)) {p.val[i] <- HotellingsT2(x[i,],x[-i,])$p.value}
    probs[[m]][,m] <- round(p.val,5)*100
    
    for (j in 1:length(grps2)) {
      p.val2 <- NULL
      for (i in 1:nrow(x)) {p.val2[i] <- HotellingsT2(x[i,],x2.l[which(attr1$CORE==grps2[j]),])$p.value}
      probs[[m]][,which(grps==grps2[j])] <- round(p.val2,5)*100}}
  return(probs)}
