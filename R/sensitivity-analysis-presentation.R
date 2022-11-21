

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
  ylim(-3,3) +
  xlim(-4,4)

p2 <- ggMarginal(p, type = "density")

p4 <- p +
  geom_segment(aes(x = -1.5, y = 3, xend = -1.5, yend = 0), col = "red") +
  geom_segment(aes(x = 1.5, y = 3, xend = 1.5, yend = 0), col = "red") +
  geom_segment(aes(x = -1.5, y = 0, xend = 4, yend = 0), col = "red") +
  geom_point(aes(x=-1.5, y=0), colour="blue", size = 5) +
  geom_point(aes(x=1.5, y=0), colour="blue", size = 5)

ggMarginal(p4, type = "density")
