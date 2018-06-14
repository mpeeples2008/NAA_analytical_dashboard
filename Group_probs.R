# Initialize libraries
library(ICSNP)
library(kableExtra)
library(DataExplorer)
library(mice)

# Function for calculating group membership probabilities by chemical compositional distance using Mahalanobis distances and Hotellings T^2 statistic
group.mem.probs <- function(x2.l,attr1.grp,grps) {
  
  # x2.l = transformed element data
  # attr1 = group designation by sample
  # grps <- vector of groups to evaluate
  
  probs <- list()
  for (m in 1:length(grps)) {
    x <- x2.l[which(attr1.grp==grps[m]),]
    probs[[m]] <- matrix(0,nrow(x),length(grps))
    colnames(probs[[m]]) <- grps
    rownames(probs[[m]]) <- rownames(x)
    
    grps2 <- grps[-m]
    
    p.val <- NULL
    for (i in 1:nrow(x)) {p.val[i] <- HotellingsT2(x[i,],x[-i,])$p.value}
    probs[[m]][,m] <- round(p.val,5)*100
    
    for (j in 1:length(grps2)) {
      p.val2 <- NULL
      for (i in 1:nrow(x)) {p.val2[i] <- HotellingsT2(x[i,],x2.l[which(attr1.grp==grps2[j]),])$p.value}
      probs[[m]][,which(grps==grps2[j])] <- round(p.val2,5)*100}}
  return(probs)}

# read in sample data INAA_test, create attribute and element data.frames, impute missing data and transform
mydat <- read.csv('INAA_test.csv',header=T,row.names=1)
attr1 <- mydat[,c(1,3,5,7)] # pull out attributes for plotting
chem1 <- mydat[,c(8:21,23:40)] # pull out element data (excluing Ni)
chem1[chem1==0] <- NA # set 0 values to NA
chem.imp <- complete(mice(chem1,method='rf')) # impute missing data using the random forest approach
chem.t <- log10(chem.imp) # log-base-10 transform raw element data

# run script and view output as "kable"
kable(group.mem.probs(chem.t,attr1$CORE,unique(attr1$CORE)))
