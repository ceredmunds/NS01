# Difference-adjusted confidence intervals.
# This code was written by Thom Baguley 2012-03-18
# and is available here:
# https://seriousstats.wordpress.com/2012/03/18/cis-for-anova/
# The above link describes the rationale for this approach.

# Where pooled.error=TRUE, this function should produce the 
# same output as the bs.ci() from the Supplementary Materials of
# the following peer-reviewed article:

# Baguley, T. (2012b). Calculating and graphing within-subject 
# confidence intervals for ANOVA. Behavior Research Methods, 
# 44, 158-175.

# Note: Although the title of the above paper mentions only
# within-subject CI, it also contains routines for between-subject
# CI. 

bsci <- function(data.frame, group.var=1, dv.var=2, difference=FALSE, 
                 pooled.error=FALSE, conf.level=0.95) {
  data <- subset(data.frame, select=c(group.var, dv.var))
  fact <- factor(data[[1]])
  dv <- data[[2]]
  J <- nlevels(fact)
  N <- length(dv)
  ci.mat <- matrix(,J,3, dimnames=list(levels(fact), c('lower', 'mean', 'upper')))
  ci.mat[,2] <- tapply(dv, fact, mean)
  n.per.group <- tapply(dv, fact, length)
  if(difference==TRUE) diff.factor= 2^0.5/2 else diff.factor=1
  if(pooled.error==TRUE) {
    for(i in 1:J) {
      moe <- summary(lm(dv ~ 0 + fact))$sigma/(n.per.group[[i]])^0.5 * qt(1-(1-conf.level)/2,N-J) * diff.factor
      ci.mat[i,1] <- ci.mat[i,2] - moe
      ci.mat[i,3] <- ci.mat[i,2] + moe
    }
  }
  if(pooled.error==FALSE) {
    for(i in 1:J) {
      group.dat <- subset(data, data[1]==levels(fact)[i])[[2]]
      moe <- sd(group.dat)/sqrt(n.per.group[[i]]) * qt(1-(1-conf.level)/2,n.per.group[[i]]-1) * diff.factor
      ci.mat[i,1] <- ci.mat[i,2] - moe
      ci.mat[i,3] <- ci.mat[i,2] + moe
    }
  }
  ci.mat
}
