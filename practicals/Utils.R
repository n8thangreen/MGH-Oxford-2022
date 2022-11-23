betaPar2 <- function(mode,upp,prob){
## Compute the parameters of a Beta distribution, given a prior guess for:
##  mode = the mode of the distribution
##  upp  = an upper bound value for the distribution
##  prob = the estimated probability that (theta <= upp)
## Based on "Bayesian ideas and data analysis", page 100. Optimisation method to identify the values of a,b that give required conditions on the Beta distribution
N <- 10000
b <- 1:N
a <- (1+mode*(b-2))/(1-mode)
sim <- qbeta(prob,a,b)
m <- ifelse(prob>=.5,max(which(sim>=upp)),min(which(sim>=upp)))
M <- ifelse(prob>=.5,min(which(sim<=upp)),max(which(sim<=upp)))

b <- min(m,M)+(b/N)
a <- (1+mode*(b-2))/(1-mode)
sim <- qbeta(prob,a,b)
m <- ifelse(prob>=.5,max(which(sim>=upp)),min(which(sim>=upp)))
M <- ifelse(prob>=.5,min(which(sim<=upp)),max(which(sim<=upp)))
a <- ifelse(m==M,a[m],mean(a[m],a[M]))
b <- ifelse(m==M,b[m],mean(b[m],b[M]))

step <- 0.001
theta <- seq(0,1,step)
density <- dbeta(theta,a,b)
#plot(theta,density,t="l",xlab=expression(theta),
#	ylab=paste("Beta(",format(a,digits=4),",",
#	format(b,digits=4),")",sep=""))

norm.dens <- density/sum(density)
cdf <- cumsum(norm.dens)
M <- min(which(cdf>=.5))
m <- max(which(cdf<=.5))

theta.mode <- theta[which(density==max(density))]
theta.mean <- a/(a+b)
theta.median <- mean(theta[m],theta[M])
theta.sd <- sqrt((a*b)/(((a+b)^2)*(a+b+1)))
beta.params <- c(a,b,theta.mode,theta.mean,theta.median,theta.sd)
res1 <- beta.params[1]
res2 <- beta.params[2]
theta.mode <- beta.params[3]
theta.mean <- beta.params[4]
theta.median <- beta.params[5]
theta.sd <- beta.params[6]
list(
res1=res1,res2=res2,theta.mode=theta.mode,theta.mean=theta.mean,theta.median=theta.median,theta.sd=theta.sd)
}

betaPar <- function(m,s){
	a <- m*( (m*(1-m)/s^2) -1 )
	b <- (1-m)*( (m*(1-m)/s^2) -1 )
	list(a=a,b=b)
}

lognPar <- function(m,s) {
## Computes mean and sd of a log-normal distribution
## so that on the *natural* scale, the parameters are mu and sigma
	s2 <- s^2
	mulog <- log(m) - .5*log(1+s2/m^2)
	s2log <- log(1+(s2/m^2))
	sigmalog <- sqrt(s2log)
	list(mulog=mulog,sigmalog=sigmalog)
}

gammaPar <- function(m,s) {
## Computes the parameters of a Gamma distribution (shape & rate) 
## so that on the *natural* scale, the mean and sd are the inputs m,s
	shape <- (m/s)^2
	rate <- m/(s^2)
	list(shape=shape,rate=rate,scale=1/rate)
}

