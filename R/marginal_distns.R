

# means only

png(filename = "means-only.png", width = 150, height = 480)

par(mfrow = c(4,1),
    mar = c(2.4,0,1.1,1))

plot(seq(0,1,.01),dbeta(seq(0,1,.01),20,80),axes=F,t="l",lwd=2,xlab="",ylab="")
axis(1)
points(20/(20+80),par()$usr[3],pch=19,col="red",cex=1.5,lwd=3,xpd=T)
text(0.5, 7, labels = bquote(pi[0]), cex=2.5)

plot(seq(0,1,.01),dbeta(seq(0,1,.01),15,10),axes=F,t="l",lwd=2,xlab="",ylab="")
axis(1)
points(15/(15+10),par()$usr[3],pch=19,col="red",cex=1.5,lwd=3,xpd=T)
text(0.1, 1, labels = bquote(rho), cex=2.5)

plot(seq(0,1,.01),dbeta(seq(0,1,.01),5,7),axes=F,t="l",lwd=2,xlab="",ylab="")
axis(1)
points(5/(5+7),par()$usr[3],pch=19,col="red",cex=1.5,lwd=3,xpd=T)
text(0.8, 1, labels = bquote(gamma), cex=2.5)

# cost in hospital
plot(seq(0,10000,100),dnorm(seq(0,10000,100),6000,1000),axes=F,t="l",lwd=2,xlab="",ylab="")
axis(1)
points(6000, par()$usr[3],pch=19,col="red",cex=1.5,lwd=3,xpd=T)
text(2000, 0.0001, labels = bquote(c^hospital), cex=2.5)

dev.off()

# one-way max min pi_0

png(filename = "max-min-pi.png", width = 150, height = 480)

par(mfrow = c(4,1),
    mar = c(2.4,0,1.1,1))

plot(seq(0,1,.01),dbeta(seq(0,1,.01),20,80),axes=F,t="l",lwd=2,xlab="",ylab="")
axis(1)
points(20/(20+80) - 0.1, par()$usr[3],pch=19,col="blue",cex=1.5,lwd=3,xpd=T)
points(20/(20+80) + 0.1, par()$usr[3],pch=19,col="blue",cex=1.5,lwd=3,xpd=T)
text(0.5, 7, labels = bquote(pi[0]), cex=2.5)

plot(seq(0,1,.01),dbeta(seq(0,1,.01),15,10),axes=F,t="l",lwd=2,xlab="",ylab="")
axis(1)
points(15/(15+10),par()$usr[3],pch=19,col="red",cex=1.5,lwd=3,xpd=T)
text(0.1, 1, labels = bquote(rho), cex=2.5)

plot(seq(0,1,.01),dbeta(seq(0,1,.01),5,7),axes=F,t="l",lwd=2,xlab="",ylab="")
axis(1)
points(5/(5+7),par()$usr[3],pch=19,col="red",cex=1.5,lwd=3,xpd=T)
text(0.8, 1, labels = bquote(gamma), cex=2.5)

# cost in hospital
plot(seq(0,10000,100),dnorm(seq(0,10000,100),6000,1000),axes=F,t="l",lwd=2,xlab="",ylab="")
axis(1)
points(6000, par()$usr[3],pch=19,col="red",cex=1.5,lwd=3,xpd=T)
text(2000, 0.0001, labels = bquote(c^hospital), cex=2.5)

dev.off()

# one-way max-min rho

png(filename = "max-min-rho.png", width = 150, height = 480)

par(mfrow = c(4,1),
    mar = c(2.4,0,1.1,1))

plot(seq(0,1,.01),dbeta(seq(0,1,.01),20,80),axes=F,t="l",lwd=2,xlab="",ylab="")
axis(1)
points(20/(20+80),par()$usr[3],pch=19,col="red",cex=1.5,lwd=3,xpd=T)
text(0.5, 7, labels = bquote(pi[0]), cex=2.5)

plot(seq(0,1,.01),dbeta(seq(0,1,.01),15,10),axes=F,t="l",lwd=2,xlab="",ylab="")
axis(1)
points(15/(15+10) - 0.2,par()$usr[3],pch=19,col="blue",cex=1.5,lwd=3,xpd=T)
points(15/(15+10) + 0.2,par()$usr[3],pch=19,col="blue",cex=1.5,lwd=3,xpd=T)
text(0.1, 1, labels = bquote(rho), cex=2.5)

plot(seq(0,1,.01),dbeta(seq(0,1,.01),5,7),axes=F,t="l",lwd=2,xlab="",ylab="")
axis(1)
points(5/(5+7),par()$usr[3],pch=19,col="red",cex=1.5,lwd=3,xpd=T)
text(0.8, 1, labels = bquote(gamma), cex=2.5)

# cost in hospital
plot(seq(0,10000,100),dnorm(seq(0,10000,100),6000,1000),axes=F,t="l",lwd=2,xlab="",ylab="")
axis(1)
points(6000, par()$usr[3],pch=19,col="red",cex=1.5,lwd=3,xpd=T)
text(2000, 0.0001, labels = bquote(c^hospital), cex=2.5)

dev.off()

