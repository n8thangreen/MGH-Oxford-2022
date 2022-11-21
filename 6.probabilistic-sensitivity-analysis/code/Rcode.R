# [NAME]
# Rcode.R
#
# [DESCRIPTION] 
# Code for making plots for the probabilistic sensitivity analysis talk
#
# [INPUT]
# 
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
setwd(file.path(getwd(), "6.probabilistic-sensitivity-analysis", "code"))

bugs.directory <-  "c:/Program Files/WinBUGS143"
figs <- file.path("..", "figs")
set.seed(1234)

source("myfuns.R")

# ggplot theme
text.size <- 20

mytheme <- theme(strip.text = element_text(size = text.size)) +
  theme(axis.title = element_text(size = text.size)) +
  theme(axis.text = element_text(size = text.size)) +
  theme(plot.title = element_text(size = text.size)) +
  theme(legend.text = element_text(size = text.size)) +
  theme(legend.title = element_text(size = text.size))

############################################################
## Functions for finding parameters
## Compute the value of parameters (a,b) for a Beta distribution to have mean and sd (m,s)
## Copyright Gianluca Baio 2012
betaPar <- function(m,s){
  a <- m*( (m*(1-m)/s^2) -1 )
  b <- (1-m)*( (m*(1-m)/s^2) -1 )
  list(a=a,b=b)
}

## Compute the value of parameters (mulog,sigmalog) for a logNormal distribution to have mean and sd (m,s)
## Copyright Gianluca Baio 2012
lognPar <- function(m,s) {
  s2 <- s^2
  mulog <- log(m) - 0.5 * log(1+s2/m^2)
  s2log <- log(1+(s2/m^2))
  sigmalog <- sqrt(s2log)
  list(mulog = mulog, sigmalog = sigmalog)
}

############################################################

debug <- FALSE
n.chains <- 3
n.burnin <- 1000
n.iter <- ceiling(3000/n.chains) + n.burnin

# Safety Data
num.pat <- 111 # Number of patients in observed data
num.se <- 27   # Number of patients with side effects, given standard-of-care
num.amb <- 17  # Number of patient with ambulatory care following side effect, given stardard-or-care

# Probability reduction (=1-rho) of side effect given new treatment 
# pi[2] <- rho * pi[1]
m.rho <- 0.8 # Mean 
s.rho <- 0.1 # SE
tau.rho <- 1/s.rho^2

# Costs
# Cost of ambulatory care 
mu.amb <- 120
sd.amb <- 20
m.amb <- lognPar(mu.amb,sd.amb)$mulog
s.amb <- lognPar(mu.amb,sd.amb)$sigmalog
tau.amb <- 1/s.amb^2

# Cost of hospitalization
mu.hosp <- 5500
sd.hosp <- 980
m.hosp <- lognPar(mu.hosp,sd.hosp)$mulog
s.hosp <- lognPar(mu.hosp,sd.hosp)$sigmalog
tau.hosp <- 1/s.hosp^2

# Drug costs
c.drug <- c(110,520)

# Number of patients in the population to model
N <- 1000

# Data needed for model 
data <- list("num.pat", "num.se", "num.amb", 
             "m.rho", "tau.rho",
             "m.amb", "tau.amb",
             "m.hosp", "tau.hosp",
             "c.drug", "N")

# bugs.data(data)

# The initial values generator function 
inits <- function(){
  list(     pi = c(runif(1),NA),
            gamma = runif(1)
  )
}

# bugs.inits(inits, n.chains = n.chains, digits = 2)

parameters.to.save <- c("pi1", "pi2", "rho", "gamma", "c.amb", "c.hosp", "SE", "A", "H", "mu.c", "mu.e")

# Perform the MCMC simulation with WinBUGS
bugs <- bugs(
  data = data, 
  inits = inits, 
  parameters.to.save = parameters.to.save,
  model.file = "chemotherapy-mod.txt", 
  n.chains = n.chains, 
  n.iter = n.iter, 
  n.thin = 1, 
  n.burnin = n.burnin,
  bugs.directory = bugs.directory,
  debug = debug)

# Process results

mu.e <- bugs$sims.list$mu.e
mu.c <- bugs$sims.list$mu.c
interventions  <- c("Standard-of-care","New Chemotherapy")

m <- bcea(e = mu.e, c = mu.c, ref = 2, interventions = interventions, Kmax = 25000)

# ceplane
p <- ceplane.plot(m, graph = "ggplot2") + mytheme

ggsave(file.path(figs, paste0("chemo_ceplane.pdf")), p)

# INB
INB <- apply(m$U, c(1,2), function(x){x[2]-x[1]})
dimnames(INB)[[2]] <- m$k # Assign the WTP to the column names
INB <- melt(INB, varnames = c("sim", "WTP"))
INB.data <- ddply(INB, .(WTP), function(x){
  quant <- quantile(x$value, prob = c(0.025, 0.975))
  data.frame(mean = mean(x$value),
             low = quant[1],
             upp = quant[2]
  )
})

p <- ggplot(INB.data, aes(x = WTP)) + geom_line(aes(y = mean)) +
  geom_line(aes(y = low), linetype = 2) +
  geom_line(aes(y = upp), linetype = 2) +
  labs(title = "Expected Incremental Net Benefit") +
  xlab("Willingness to pay") +
  ylab("E[INB]") +
  theme_bw() + mytheme
ggsave(file.path(figs, paste0("chemo_INB.pdf")), p)

# CEAC
p <- ceac.plot(m, graph = "ggplot2") + mytheme
ggsave(file.path(figs, paste0("chemo_CEAC.pdf")), p)


#p.eib <- eib.plot(m, graph = "ggplot2")

#contour(m)
#summary(m)

# EVPPI
parameters <- c("pi1", "rho", "gamma", "c.amb", "c.hosp")

voi <- evppi(parameters = parameters, 
             inputs = bugs$sims.matrix, 
             he = m, 
             method = "sal")

# plot(voi)

voi.df <- sapply(voi$evppi, function(x){x})
voi.df <- data.frame(K = voi$k, voi.df)
par.order <- rev(names(sort(voi.df[nrow(voi.df), -1])))
labels <- c(expression(rho),
            expression(paste(pi[1])), 
            expression(paste(c^{hosp})), 
            expression(paste(gamma)), 
            expression(paste(c^{amb})))

voi.long <- melt(voi.df, id.vars = "K")
voi.long$variable <- factor(voi.long$variable, levels = par.order)

p <- ggplot(voi.long, aes(x = K, y = value, group = variable)) +
  geom_line(aes(linetype = variable)) +
  ylim(0, max(voi.long$value)) +
  xlab("Willingness to pay") + ylab("EVPPI") +
  guides(linetype = guide_legend("Parameter", label.hjust = 0)) +
  scale_linetype_discrete(labels = labels) +
  mytheme
  
ggsave(file.path(figs, paste0("chemo_EVPPI.pdf")), p)

# Latex table for VOI
wtp <- 5000
# which k index is this wtp
k.ind <- match(wtp, m$k)
head.n <- 5

data.sims <- head(bugs$sims.matrix[, parameters], n = head.n)
colnames(data.sims) <- c("$\\pi_1$", "$\\rho$", "$\\gamma$", "$c^{\\rm{amb}}$", "$c^{\\rm{hosp}}$")

data.nb <- head(m$U[, k.ind, ], n = head.n)
colnames(data.nb) <- c("$NB_1$", "$NB_2$")

data.table <- cbind(data.sims, data.nb)

table <- xtable(data.table)
digits(table)[5:8] <- 0
print(table, 
      sanitize.colnames.function = function(x){x},
      format.args = list(big.mark = " ", decimal.mark = "."))

