# Depression decision tree model
# Deterministic sensitivity analysis

# for demonstration purposes we will focus on treatment 2


###########
# set up

# Number and names of treatments
t.names <- c("No treatment", "CBT", "Antidepressant")
n.treat <- length(t.names)

# load in the fixed input parameter values from file
inputs_tab <- read.csv(file = "det_sa_inputs.csv")
n_scenarios <- nrow(inputs_tab)
n_par <- ncol(inputs_tab)

# convert the matrix to a list so that it is easier to reference
# each scenario
inputs <- split(inputs_tab, 1:n_scenarios)

# Willingness to pay threshold
lambda.target <- 20000

# Create structures to save the incremental costs, effects, and net benefits,
# as well as # absolute costs, effects, and net benefits

incremental.costs <- incremental.effects <- incremental.nb <-
  costs <- effects <- net.benefit <-
  rep(NA, n.treat)

# Now name these vectors
incremental.costs <- incremental.effects <- incremental.nb <-
  names(c.treat) <- names(costs) <- names(effects) <-
  t.names


###########################################
# Calculate the total costs and effects

# create an empty results object
res <- NULL

# loop over all scenarios
for (i in 1:n_scenarios) {
  
  # pick single set of parameter values
  scenario <- inputs[[i]]
  
  # use with() so we don't have to write scenario every time
  cost <-
    with(scenario,
         c.treat + p.rec * (1 - p.rel) * c.rec + p.rec * p.rel * c.rel + (1 - p.rec) * c.norec)
  
  effect <-
    with(scenario,
         p.rec * (1 - p.rel) * q.rec + p.rec * p.rel * q.rel + (1 - p.rec) * q.norec)
  
  # attach to bottom of res
  res <- rbind(res, c(cost, effect))
}

colnames(res) <- c("cost", "effect")


## net benefit

library(dplyr)

res <- as.data.frame(res)
res <- mutate(res, nb = lambda.target*effect - cost)

res

##########
# plot

library(ggplot2)

# add a column with parameter names
# and if the value is for the top of bottom of the range
plot_dat <-
  data.frame(
    res, par = c("mean", rep(names(inputs_tab), each = 2)),
    lim = c(NA, rep(c("max", "min"), n_par)))

# to use ggplot, we need to rearrange the data
# so that there are three columns for max, min and mean 
dat2 <- reshape2::dcast(plot_dat, par ~ lim, value.var = "nb")
dat2$mean <- plot_dat$nb[1]

ggplot(dat2, aes(par, mean)) + 
  coord_flip() + 
  geom_pointrange(aes(ymin = min, ymax = max))



