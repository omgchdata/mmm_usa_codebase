
################################################
# this R function creates scatter plots of 2 variables
# and marginal distribution of the 2. 
# It is very useful to illustrate how hierarical bayes prior
# "shrink" the coefficients of pannel to the prior.
# copied from SAS and R examples: 
# https://www.r-bloggers.com/example-8-41-scatterplot-with-marginal-histograms/
# small modifications by Julia Liu on 2020-09-07:
#   set xlim and ylim, so the range of x and y are the same. 
#   set back to default par(mfrow=c(1,1))
################################################

scatterhist = function(x, y, xlab="", ylab=""){
  zones=matrix(c(2,0,1,3), ncol=2, byrow=TRUE)
  layout(zones, widths=c(4/5,1/5), heights=c(1/5,4/5))
  xhist = hist(x, plot=FALSE)
  yhist = hist(y, plot=FALSE)
  top = max(c(xhist$counts, yhist$counts))
  par(mar=c(3,3,1,1))
  plot(x,y, xlim=c(min(x,y), max(x,y)), ylim=c(min(x,y), max(x,y)), pch=20)
  par(mar=c(0,3,1,1))
  barplot(xhist$counts, axes=FALSE, ylim=c(0, top), space=0, col="lightblue")
  par(mar=c(3,0,1,1))
  barplot(yhist$counts, axes=FALSE, xlim=c(0, top), space=0, horiz=TRUE, col="lightblue")
  par(oma=c(3,3,0,0))
  mtext(xlab, side=1, line=1, outer=TRUE, adj=0, 
        at=.8 * (mean(x) - min(x))/(max(x)-min(x)))
  mtext(ylab, side=2, line=1, outer=TRUE, adj=0, 
        at=(.8 * (mean(y) - min(y))/(max(y) - min(y))))
  par(mfrow=c(1,1))    # set the par back to default
}
