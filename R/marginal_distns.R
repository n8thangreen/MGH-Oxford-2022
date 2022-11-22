

# save plot
png(filename = "marginal-distns.png", width = 150, height = 480)

par(mfrow = c(4,1),
    mar = c(2.4,0,1.1,1))

plot(seq(0,1,.01),dbeta(seq(0,1,.01),20,80),axes=F,t="l",lwd=2,xlab="",ylab="")
# plot(NULL, axes=F, xlab="", ylab="", xlim = c(0, 1), ylim = c(0,1))
axis(1)
points(20/(20+80),par()$usr[3],pch=19,col="red",cex=1.5,lwd=3,xpd=T)
points(20/(20+80) - 0.1, par()$usr[3],pch=19,col="blue",cex=1.5,lwd=3,xpd=T)
points(20/(20+80) + 0.1, par()$usr[3],pch=19,col="blue",cex=1.5,lwd=3,xpd=T)
# points(rbeta(1,20,80),par()$usr[3],pch=19,col="red",cex=1.5,lwd=3,xpd=T)

plot(seq(0,1,.01),dbeta(seq(0,1,.01),15,10),axes=F,t="l",lwd=2,xlab="",ylab="")
# plot(NULL, axes=F, xlab="", ylab="", xlim = c(0, 1), ylim = c(0,1))
axis(1)
# points(rbeta(1,15,10),par()$usr[3],pch=19,col="red",cex=1.5,lwd=3,xpd=T)
points(15/(15+10),par()$usr[3],pch=19,col="red",cex=1.5,lwd=3,xpd=T)
points(15/(15+10) - 0.2,par()$usr[3],pch=19,col="red",cex=1.5,lwd=3,xpd=T)
points(15/(15+10) + 0.2,par()$usr[3],pch=19,col="red",cex=1.5,lwd=3,xpd=T)

plot(seq(0,1,.01),dbeta(seq(0,1,.01),5,7),axes=F,t="l",lwd=2,xlab="",ylab="")
# plot(NULL, axes=F, xlab="",ylab="", xlim = c(0, 1), ylim = c(0,1))
axis(1)
# points(rbeta(1,5,7),par()$usr[3],pch=19,col="red",cex=1.5,lwd=3,xpd=T)
points(5/(5+7),par()$usr[3],pch=19,col="red",cex=1.5,lwd=3,xpd=T)
points(5/(5+7) - 0.3,par()$usr[3],pch=19,col="red",cex=1.5,lwd=3,xpd=T)
points(5/(5+7) + 0.3,par()$usr[3],pch=19,col="red",cex=1.5,lwd=3,xpd=T)

# cost in hospital
plot(seq(0,10000,100),dnorm(seq(0,10000,100),6000,1000),axes=F,t="l",lwd=2,xlab="",ylab="")
# plot(NULL, axes=F, xlab="",ylab="", xlim = c(0, 1), ylim = c(0,1))
axis(1)
# points(rbeta(1,5,7),par()$usr[3],pch=19,col="red",cex=1.5,lwd=3,xpd=T)
points(6000, par()$usr[3],pch=19,col="red",cex=1.5,lwd=3,xpd=T)
points(6000 - 3000, par()$usr[3],pch=19,col="red",cex=1.5,lwd=3,xpd=T)
points(6000 + 3000, par()$usr[3],pch=19,col="red",cex=1.5,lwd=3,xpd=T)

dev.off()


