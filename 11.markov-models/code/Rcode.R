#
# [DESCRIPTION] 
# Code for making plots for the markov-models talks
#
# [INPUT]
# uk700.rda
# 
# [ASSUMPTIONS] 
# 
# [OUTPUT] 
#
# [AUTHOR]
# Richard Nixon
#
# [LANGUAGE]
# R-2.15.1
#
# [DATE]
# 2013 Dec
#
#####################################################################
# First run _Rsetup.R in LaTeX directory
setwd(file.path(getwd(), "10.markov-models", "code"))

bugs.directory <-  "c:/Program Files/WinBUGS143"
figs <- file.path("..", "figs")
set.seed(1234)

library(R2WinBUGS)
library(BCEA)

library(ggplot2)
#library(reshape)
#library(MASS)
#library(gridExtra)

source("myfuns.R")

# ggplot theme
text.size <- 20

mytheme <- theme(strip.text = element_text(size = text.size)) +
  theme(axis.title = element_text(size = text.size)) +
  theme(axis.text = element_text(size = text.size)) +
  theme(plot.title = element_text(size = text.size)) +
  theme(legend.text = element_text(size = text.size)) +
  theme(legend.title = element_text(size = text.size))

#####################################################################
#
# WinBUGS MODELS
#
#####################################################################

debug <- TRUE
n.chains <- 3
n.burnin <- 1000
n.iter <- ceiling(10000/n.chains) + n.burnin

# Data 

r.sfc <- matrix(c(210,60,0,1,1, 88,641,0,4,13, 0,0,0,0,0, 1,0,0,0,1), nrow = 4, byrow = TRUE)
n.sfc <- c(272,746,0,2)
weekly.cost.sfc <- c(7.96,7.96,1821.17,100.79)
r.fp <- matrix( c(66,32,0,0,2, 42,752,0,5,20, 0,0,0,0,0, 0,4,0,1,0), nrow = 4, byrow = TRUE)
n.fp <- c(100,819,0,5)
weekly.cost.fp <- c(2.38,2.38,1851.58,95.21)

prior.sfc <- matrix( c(1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1), nrow = 4, byrow = TRUE)
prior.fp <- matrix( c(1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1), nrow = 4, byrow = TRUE)
p.fixed <- c(0,0,0,0,1)
s.start <- c(1,0,0,0,0)

data <- c("r.sfc", "n.sfc", "weekly.cost.sfc", "r.fp", "n.fp", "weekly.cost.fp", "prior.sfc", "prior.fp", "p.fixed", "s.start")
bugs.data(data)

# The parameters to monitor
parameters.to.save <- c(
"INB", 
"p.sfc", 
"p.fp", 
"Q",
"mu.e",
"mu.c" 
)


# Perform the MCMC simulation with WinBUGS
markov5 <- bugs(
  data = data, 
  inits = NULL, 
  parameters.to.save = parameters.to.save,
  model.file = "markov-5state-model.txt", 
  n.chains = n.chains, 
  n.iter = n.iter, 
  n.thin = 1, 
  n.burnin = n.burnin,
  bugs.directory = bugs.directory,
  debug = debug)


#####################################################################

# BCEA
# Make objects for beca function 

c <- markov5$sims.list$mu.c
e <- markov5$sims.list$mu.e

# Apply bcea function
bcea.markov5 <- bcea(e, c, ref = 2, interventions = c("Fluticason", "Salmeterol"), Kmax = 150)

# PLOTS
p.contour <- my.contour.bcea(bcea.markov5, nlevels = 4, graph = "ggplot2")  # 0.25, 0.5, 0.75
p.contour <- p.contour + 
  ggtitle("Cost-Effectiveness Plane") +
  coord_cartesian(xlim = c(-0.5, 4), ylim = c(-200, 250)) + 
  mytheme 
#limits.contour1 <- coord_cartesian(xlim = c(-40, 40), ylim = c(-10, 10))
ggsave(file.path(figs, paste0("markov5-contour.pdf")), p.contour, width = 8, height = 6)
  
p.ceac <- ceac.plot(bcea.markov5, graph = "ggplot2") 
p.ceac <- p.ceac + mytheme #+ xlab("Willinfness to pay\nfor a STW")
ggsave(file.path(figs, paste0("markov5-ceac.pdf")), p.ceac, width = 8, height = 6)









attach("~/R/myRfunctions/.RData")

res<-bugs2R("/project/hecon/course/prac/disk/markov-models/markov-res.txt")
attach(res)

p.pf<-matrix(mean[1:20], nrow=4, ncol=5, byrow=T)
p.pf<-rbind(p.pf,c(0,0,0,0,1))

p.<-matrix(mean[1:20], nrow=4, ncol=5, byrow=T)
p.pf<-rbind(p.pf,c(0,0,0,0,1))



## uniform prior 
