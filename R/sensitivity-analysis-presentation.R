
# sensitivity analysis plots
# bivariate normal distribution
# for presentation
# Oxford University 2022


library(ggplot2)
library(ggExtra)


# bivariate data
dat <- MASS::mvrnorm(10000, c(0,0), matrix(c(1,0.5,0.1,1), nrow = 2))

colnames(dat) <- c("x", "y")

dat <- as.data.frame(dat)

dat_long <- 
  dat |> 
  reshape2::melt() |> 
  as.data.frame()

p <- ggplot(dat, aes(x=x, y=y)) +
  geom_point(size = -1) +
  geom_density2d() +
  theme(legend.position="none") +
  theme_bw() +
  coord_cartesian(ylim = c(-3, 3), xlim = c(-4,4), clip="off")


# means

p4 <- p +
  geom_segment(aes(x = 0, y = 3.3, xend = 0, yend = 0), col = "red", linewidth = 1.5) +
  geom_segment(aes(x = 0, y = 0, xend = 4.3, yend = 0), col = "red", linewidth = 1.5) +
  geom_point(aes(x=0, y=0), colour="blue", size = 5)
  
p5 <- ggMarginal(p4, type = "density")

ggsave(plot = p5, filename = "fixed-means.png")

################
# max and min
# e.g. 95% CI

# fixed y

p4 <- p +
  geom_segment(aes(x = -1.5, y = 3.3, xend = -1.5, yend = 0), col = "red", lty = 2) +
  geom_segment(aes(x = 0, y = 3.3, xend = 0, yend = 0), col = "red", linewidth = 1.5) +
  geom_segment(aes(x = 1.5, y = 3.3, xend = 1.5, yend = 0), col = "red", lty = 2) +
  geom_segment(aes(x = -1.5, y = 0, xend = 4.3, yend = 0), col = "red", linewidth = 1.5) +
  geom_point(aes(x=-1.5, y=0), colour="blue", size = 5) +
  geom_point(aes(x=1.5, y=0), colour="red", size = 5)+
  geom_point(aes(x=0, y=0), colour="red", size = 5)

p5 <- ggMarginal(p4, type = "density")

ggsave(plot = p5, filename = "fixed-y-min.png")

p4 <- p +
  geom_segment(aes(x = -1.5, y = 3.3, xend = -1.5, yend = 0), col = "red", lty = 2) +
  geom_segment(aes(x = 0, y = 3.3, xend = 0, yend = 0), col = "red", linewidth = 1.5) +
  geom_segment(aes(x = 1.5, y = 3.3, xend = 1.5, yend = 0), col = "red", lty = 2) +
  geom_segment(aes(x = -1.5, y = 0, xend = 4.3, yend = 0), col = "red", linewidth = 1.5) +
  geom_point(aes(x=-1.5, y=0), colour="red", size = 5) +
  geom_point(aes(x=1.5, y=0), colour="blue", size = 5)+
  geom_point(aes(x=0, y=0), colour="red", size = 5)

p5 <- ggMarginal(p4, type = "density")

ggsave(plot = p5, filename = "fixed-y-mean.png")

p4 <- p +
  geom_segment(aes(x = -1.5, y = 3.3, xend = -1.5, yend = 0), col = "red", lty = 2) +
  geom_segment(aes(x = 0, y = 3.3, xend = 0, yend = 0), col = "red", linewidth = 1.5) +
  geom_segment(aes(x = 1.5, y = 3.3, xend = 1.5, yend = 0), col = "red", lty = 2) +
  geom_segment(aes(x = -1.5, y = 0, xend = 4.3, yend = 0), col = "red", linewidth = 1.5) +
  geom_point(aes(x=-1.5, y=0), colour="red", size = 5) +
  geom_point(aes(x=1.5, y=0), colour="red", size = 5)+
  geom_point(aes(x=0, y=0), colour="blue", size = 5)

p5 <- ggMarginal(p4, type = "density")

ggsave(plot = p5, filename = "fixed_y_max.png")

# fixed x
p4 <- p +
  geom_segment(aes(x = 0, y = 3.3, xend = 0, yend = -1.5), col = "red", linewidth = 1.5) +
  geom_segment(aes(x = 0, y = 0, xend = 4.3, yend = 0), col = "red", linewidth = 1.5) +
  geom_segment(aes(x = 0, y = 1.5, xend = 4.3, yend = 1.5), col = "red", lty = 2) +
  geom_segment(aes(x = 0, y = -1.5, xend = 4.3, yend = -1.5), col = "red", lty = 2) +
  geom_point(aes(x=0, y=-1.5), colour="red", size = 5) +
  geom_point(aes(x=0, y=0), colour="blue", size = 5) +
  geom_point(aes(x=0, y=1.5), colour="red", size = 5)
  
p5 <- ggMarginal(p4, type = "density")

ggsave(plot = p5, filename = "fixed-x-max.png")

p4 <- p +
  geom_segment(aes(x = 0, y = 3.3, xend = 0, yend = -1.5), col = "red", linewidth = 1.5) +
  geom_segment(aes(x = 0, y = 0, xend = 4.3, yend = 0), col = "red", linewidth = 1.5) +
  geom_segment(aes(x = 0, y = 1.5, xend = 4.3, yend = 1.5), col = "red", lty = 2) +
  geom_segment(aes(x = 0, y = -1.5, xend = 4.3, yend = -1.5), col = "red", lty = 2) +
  geom_point(aes(x=0, y=-1.5), colour="blue", size = 5) +
  geom_point(aes(x=0, y=0), colour="red", size = 5) +
  geom_point(aes(x=0, y=1.5), colour="red", size = 5)

p5 <- ggMarginal(p4, type = "density")

ggsave(plot = p5, filename = "fixed-x-min.png")

p4 <- p +
  geom_segment(aes(x = 0, y = 3.3, xend = 0, yend = -1.5), col = "red", linewidth = 1.5) +
  geom_segment(aes(x = 0, y = 0, xend = 4.3, yend = 0), col = "red", linewidth = 1.5) +
  geom_segment(aes(x = 0, y = 1.5, xend = 4.3, yend = 1.5), col = "red", lty = 2) +
  geom_segment(aes(x = 0, y = -1.5, xend = 4.3, yend = -1.5), col = "red", lty = 2) +
  geom_point(aes(x=0, y=-1.5), colour="red", size = 5) +
  geom_point(aes(x=0, y=0), colour="red", size = 5) +
  geom_point(aes(x=0, y=1.5), colour="blue", size = 5)

p5 <- ggMarginal(p4, type = "density")

ggsave(plot = p5, filename = "fixed-x-mean.png")

# all points

p4 <- p +
  geom_point(aes(x=-1.5, y=0), colour="red", size = 5) +
  geom_point(aes(x=1.5, y=0), colour="red", size = 5)+
  geom_point(aes(x=0, y=0), colour="red", size = 5)+
  geom_point(aes(x=0, y=-1.5), colour="red", size = 5) +
  geom_point(aes(x=0, y=1.5), colour="red", size = 5)

p5 <- ggMarginal(p4, type = "density")

ggsave(plot = p5, filename = "fixed-all.png")

# one-way sample points

xnew <- data.frame(s = rnorm(20, 0, 1.2), y = 0)

p4 <- p +
  geom_point(data = xnew, aes(x=s, y=y), colour="red", size = 3)

p5 <- ggMarginal(p4, type = "density")

ggsave(plot = p5, filename = "oneway_y.png")

p4 <- p +
  geom_point(data = xnew, aes(x=y, y=s), colour="red", size = 3)

p5 <- ggMarginal(p4, type = "density")

ggsave(plot = p5, filename = "oneway-x.png")

# full PSA

xnew <- dat[1:50, ]

p4 <- p +
  geom_point(data = xnew, aes(x=x, y=y), colour="red", size = 3)

p5 <- ggMarginal(p4, type = "density")

ggsave(plot = p5, filename = "full-psa.png")
