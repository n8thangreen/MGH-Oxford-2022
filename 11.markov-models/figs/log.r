pdf("uniform.pdf")
par(lwd=3, cex.axis=1.5, cex.lab=3, mar=c(5,6,4,2)+0.1)
x <- seq(0,1,length=100)
plot(x, dunif(x), type="l", xlab=expression(pi), ylab="Density", ylim=c(0,1.1))
dev.off()


library(costeff)
#data.path <- "/project/hecon/course/prac/disk/markov-models"
data.path <- "Q:/hecon/course/prac/disk/markov-models"
setwd("Q:/hecon/course/talks/markov-models/plots")
markov <- get.delta.cebugs(file.path(data.path,"markov-5state"))

pdf("den.pdf")
par(cex=1.5, lwd=4,cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
xlim<-c(-0.5,4)
ylim<-c(-200, 250)
ylab<-"Difference in costs"
contour(markov, levels=c(0.05, 0.8), drawlabels=F, xlim=xlim, ylim=ylim, ylab=ylab)
abline(h=0, v=0)
title("")
dev.off()

pdf("ceac.pdf")
par(cex=1.5, lwd=4,cex.main=1.5, cex.lab=1.5, cex.axis=1.5, mar=c(5,4,4,2)+0.1)
xlim<-c(0,200)
ceac(markov, xlim=xlim, xlab="", ylab="")
mtext("Probability (cost-effective)", 2, line=3, cex=2)
mtext("Willingness-to-pay\nfor a STW", 1, line=4, cex=2)
dev.off()
