# Remove the on plot text
my.contour.bcea <- function (x, comparison = 1, scale = 0.5, nlevels = 4, levels = NULL, 
          pos = c(1, 0), graph = c("base", "ggplot2"), ...) 
{
  require(MASS)
  options(scipen = 10)
  alt.legend <- pos
  base.graphics <- ifelse(isTRUE(pmatch(graph, c("base", "ggplot2")) == 
                                   2), FALSE, TRUE)
  if (base.graphics) {
    if (is.null(comparison)) 
      comparison <- 1
    if (x$n.comparisons == 1) {
      density <- kde2d(x$delta.e, x$delta.c, n = 300, h = c(sd(x$delta.e)/scale, 
                                                            sd(x$delta.c)/scale))
      offset <- 1
      p.ne <- sum(x$delta.e > 0 & x$delta.c > 0)/x$n.sim
      p.nw <- sum(x$delta.e <= 0 & x$delta.c > 0)/x$n.sim
      p.sw <- sum(x$delta.e <= 0 & x$delta.c <= 0)/x$n.sim
      p.se <- sum(x$delta.e > 0 & x$delta.c <= 0)/x$n.sim
      m.c <- range(x$delta.c)[1]
      M.c <- range(x$delta.c)[2]
      m.e <- range(x$delta.e)[1]
      M.e <- range(x$delta.e)[2]
      ch1 <- ifelse(m.e > 0, m.e <- -m.e, m.e <- m.e)
      ch2 <- ifelse(M.e < 0, M.e <- -M.e, M.e <- M.e)
      ch3 <- ifelse(m.c > 0, m.c <- -m.c, m.c <- m.c)
      ch4 <- ifelse(M.c < 0, M.c <- -M.c, M.c <- M.c)
      plot(x$delta.e, x$delta.c, pch = 20, cex = 0.3, col = "dark grey", 
           xlab = "Effectiveness differential", ylab = "Cost differential", 
           main = paste("Cost effectiveness plane contour plot \n", 
                        x$interventions[x$ref], " vs ", x$interventions[x$comp], 
                        sep = ""), xlim = c(m.e, M.e), ylim = c(m.c, 
                                                                M.c))
      abline(h = 0, col = "dark grey")
      abline(v = 0, col = "dark grey")
      if (any(is.na(density$z)) == FALSE) {
        if (is.null(levels) == FALSE) {
          density$z <- (density$z - min(density$z))/(max(density$z) - 
                                                       min(density$z))
          contour(density$x, density$y, density$z, add = TRUE, 
                  levels = levels, drawlabels = TRUE)
        }
        if (is.null(levels) == TRUE) {
          contour(density$x, density$y, density$z, add = TRUE, 
                  nlevels = nlevels, drawlabels = FALSE)
        }
      }
      t1 <- paste("Pr(Delta[e]>0, Delta[c]>0)==", format(p.ne, 
                                                         digits = 4, nsmall = 3), sep = "")
      text(offset * M.e, offset * M.c, parse(text = t1), 
           cex = 0.8, pos = 2)
      t2 <- paste("Pr(Delta[e]<=0, Delta[c]>0)==", format(p.nw, 
                                                          digits = 4, nsmall = 3), sep = "")
      text(offset * m.e, offset * M.c, parse(text = t2), 
           cex = 0.8, pos = 4)
      t3 <- paste("Pr(Delta[e]<=0, Delta[c]<=0)==", format(p.sw, 
                                                           digits = 4, nsmall = 3), sep = "")
      text(offset * m.e, offset * m.c, parse(text = t3), 
           cex = 0.8, pos = 4)
      t4 <- paste("Pr(Delta[e]>0, Delta[c]<=0)==", format(p.se, 
                                                          digits = 4, nsmall = 3), sep = "")
      text(offset * M.e, offset * m.c, parse(text = t4), 
           cex = 0.8, pos = 2)
    }
    if (x$n.comparisons > 1) {
      density <- kde2d(x$delta.e[, comparison], x$delta.c[, 
                                                          comparison], n = 300, h = c(sd(x$delta.e[, comparison])/scale, 
                                                                                      sd(x$delta.c[, comparison])/scale))
      offset <- 1
      p.ne <- sum(x$delta.e[, comparison] > 0 & x$delta.c[, 
                                                          comparison] > 0)/x$n.sim
      p.nw <- sum(x$delta.e[, comparison] <= 0 & x$delta.c[, 
                                                           comparison] > 0)/x$n.sim
      p.sw <- sum(x$delta.e[, comparison] <= 0 & x$delta.c[, 
                                                           comparison] <= 0)/x$n.sim
      p.se <- sum(x$delta.e[, comparison] > 0 & x$delta.c[, 
                                                          comparison] <= 0)/x$n.sim
      m.c <- range(x$delta.c[, comparison])[1]
      M.c <- range(x$delta.c[, comparison])[2]
      m.e <- range(x$delta.e[, comparison])[1]
      M.e <- range(x$delta.e[, comparison])[2]
      ch1 <- ifelse(m.e > 0, m.e <- -m.e, m.e <- m.e)
      ch2 <- ifelse(M.e < 0, M.e <- -M.e, M.e <- M.e)
      ch3 <- ifelse(m.c > 0, m.c <- -m.c, m.c <- m.c)
      ch4 <- ifelse(M.c < 0, M.c <- -M.c, M.c <- M.c)
      plot(x$delta.e[, comparison], x$delta.c[, comparison], 
           pch = 20, cex = 0.3, col = "dark grey", xlab = "Effectiveness differential", 
           ylab = "Cost differential", main = paste("Cost effectiveness plane contour plot \n", 
                                                    x$interventions[x$ref], " vs ", x$interventions[x$comp[comparison]], 
                                                    sep = ""), xlim = c(m.e, M.e), ylim = c(m.c, 
                                                                                            M.c))
      abline(h = 0, col = "dark grey")
      abline(v = 0, col = "dark grey")
      if (any(is.na(density$z)) == FALSE) {
        contour(density$x, density$y, density$z, add = TRUE, 
                drawlabels = TRUE)
        if (is.null(levels) == FALSE) {
          density$z <- (density$z - min(density$z))/(max(density$z) - 
                                                       min(density$z))
          contour(density$x, density$y, density$z, add = TRUE, 
                  levels = levels, drawlabels = TRUE)
        }
        if (is.null(levels) == TRUE) {
          contour(density$x, density$y, density$z, add = TRUE, 
                  nlevels = nlevels, drawlabels = FALSE)
        }
      }
      t1 <- paste("Pr(Delta[e]>0, Delta[c]>0)==", format(p.ne, 
                                                         digits = 4, nsmall = 3), sep = "")
      text(offset * M.e, offset * M.c, parse(text = t1), 
           cex = 0.8, pos = 2)
      t2 <- paste("Pr(Delta[e]<=0, Delta[c]>0)==", format(p.nw, 
                                                          digits = 4, nsmall = 3), sep = "")
      text(offset * m.e, offset * M.c, parse(text = t2), 
           cex = 0.8, pos = 4)
      t3 <- paste("Pr(Delta[e]<=0, Delta[c]<=0)==", format(p.sw, 
                                                           digits = 4, nsmall = 3), sep = "")
      text(offset * m.e, offset * m.c, parse(text = t3), 
           cex = 0.8, pos = 4)
      t4 <- paste("Pr(Delta[e]>0, Delta[c]<=0)==", format(p.se, 
                                                          digits = 4, nsmall = 3), sep = "")
      text(offset * M.e, offset * m.c, parse(text = t4), 
           cex = 0.8, pos = 2)
    }
  }
  else {
    if (!isTRUE(require(ggplot2) & require(grid))) {
      message("falling back to base graphics\n")
      contour.bcea(x, comparison = comparison, scale = scale, 
                   nlevels = nlevels, pos = alt.legend, levels = levels, 
                   graph = "base", ...)
      return(invisible(NULL))
    }
    if (!is.null(levels)) 
      message("option level will be ignored using ggplot2 graphics")
    delta.e <- delta.c <- e <- z <- y <- hjust <- label <- NULL
    k <- x
    rm(x)
    if (!is.null(nlevels)) {
      nlevels <- round(nlevels)
      if (nlevels < 0) 
        nlevels <- 10
      if (nlevels == 0) 
        nlevels <- 1
    }
    if (k$n.comparisons == 1) {
      kd <- data.frame(e = k$delta.e, c = k$delta.c)
      names(kd) <- c("e", "c")
      do.nothing = function(x, limits) return(x)
      range.e <- range(kd$e)
      range.c <- range(kd$c)
      range.e[1] <- ifelse(range.e[1] < 0, range.e[1], 
                           -range.e[1])
      range.c[1] <- ifelse(range.c[1] < 0, range.c[1], 
                           -range.c[1])
      p.ne <- sum(k$delta.e > 0 & k$delta.c > 0)/k$n.sim
      p.ne <- paste0("Pr(Delta[e]>0, Delta[c]>0)==", format(p.ne, 
                                                            digits = 4, nsmall = 3))
      p.nw <- sum(k$delta.e <= 0 & k$delta.c > 0)/k$n.sim
      p.nw <- paste0("Pr(Delta[e]<=0, Delta[c]>0)==", format(p.nw, 
                                                             digits = 4, nsmall = 3))
      p.sw <- sum(k$delta.e <= 0 & k$delta.c <= 0)/k$n.sim
      p.sw <- paste0("Pr(Delta[e]<=0, Delta[c]<=0)==", 
                     format(p.sw, digits = 4, nsmall = 3))
      p.se <- sum(k$delta.e > 0 & k$delta.c <= 0)/k$n.sim
      p.se <- paste0("Pr(Delta[e]>0, Delta[c]<=0)==", format(p.se, 
                                                             digits = 4, nsmall = 3))
      labels.df <- data.frame(x = c(range.e[2], range.e[1], 
                                    range.e[1], range.e[2]), y = c(rep(range.c[2], 
                                                                       2), rep(range.c[1], 2)), label = c(p.ne, p.nw, 
                                                                                                          p.sw, p.se), hjust = as.factor(c(1, 0, 0, 1)))
      points.colour = "grey"
      if (nlevels == 1) 
        points.colour = "black"
      ceplane <- ggplot(kd, aes(e, c)) + geom_hline(aes(yintercept = 0), 
                                                    colour = "grey") + geom_vline(aes(xintercept = 0), 
                                                                                  colour = "grey") + theme_bw() + geom_point(size = 1, 
                                                                                                                             color = points.colour) + scale_x_continuous(limits = range.e, 
                                                                                                                                                                         oob = do.nothing) + scale_y_continuous(limits = range.c, 
                                                                                                                                                                                                                oob = do.nothing)
      if (!is.null(scale) & require(MASS)) {
        density <- kde2d(k$delta.e, k$delta.c, n = 300, 
                         h = c(sd(k$delta.e)/scale, sd(k$delta.c)/scale))
        densitydf <- data.frame(expand.grid(e = density$x, 
                                            c = density$y), z = as.vector(density$z))
        ceplane <- ceplane + geom_contour(aes(z = z), 
                                          data = densitydf, colour = "black", bins = nlevels)
      }
      else {
        ceplane <- ceplane + stat_density2d(color = "black")
      }
#       ceplane <- ceplane + geom_text(data = labels.df, 
#                                      aes(x = x, y = y, hjust = hjust, label = label), 
#                                      parse = TRUE, size = rel(3.5))
     }
    if (k$n.comparisons > 1 & is.null(comparison) == TRUE) {
      kd <- data.frame(c(k$delta.e), c(k$delta.c))
      names(kd) <- c("delta.e", "delta.c")
      kd$comparison <- as.factor(sort(rep(1:k$n.comparisons, 
                                          dim(k$delta.e)[1])))
      colors.label <- paste0("gray", round(seq(0, 100, 
                                               length.out = (k$n.comparisons + 1))[-(k$n.comparisons + 
                                                                                       1)]))
      comparisons.label <- paste0(k$interventions[k$ref], 
                                  " vs ", k$interventions[k$comp])
      do.nothing = function(x, limits) return(x)
      range.e <- range(kd$delta.e)
      range.c <- range(kd$delta.c)
      range.e[1] <- ifelse(range.e[1] < 0, range.e[1], 
                           -range.e[1])
      range.c[1] <- ifelse(range.c[1] < 0, range.c[1], 
                           -range.c[1])
      ceplane <- ggplot(kd, aes(x = delta.e, y = delta.c, 
                                col = comparison)) + geom_hline(yintercept = 0, 
                                                                colour = "grey") + geom_vline(xintercept = 0, 
                                                                                              colour = "grey") + theme_bw() + geom_point(size = 1) + 
        scale_color_manual(label = comparisons.label, 
                           values = colors.label, na.value = "black") + 
        scale_x_continuous(limits = range.e, oob = do.nothing) + 
        scale_y_continuous(limits = range.c, oob = do.nothing)
      if (!is.null(scale) & require(MASS)) {
        require(MASS)
        densitydf <- data.frame()
        for (i in 1:k$n.comparison) {
          temp <- kde2d(k$delta.e[, i], k$delta.c[, i], 
                        n = 300, h = c(sd(k$delta.e[, i])/scale, 
                                       sd(k$delta.c[, i])/scale))
          temp <- data.frame(expand.grid(e = temp$x, 
                                         c = temp$y), z = as.vector(temp$z))
          densitydf <- rbind(densitydf, cbind(temp, rep(i, 
                                                        dim(temp)[[1]])))
        }
        names(densitydf) <- c("delta.e", "delta.c", "z", 
                              "comparison")
        densitydf$comparison <- as.factor(densitydf$comparison)
        ceplane <- ceplane + geom_contour(aes(z = z, 
                                              colour = comparison), data = densitydf, bins = nlevels) + 
          guides(colour = guide_legend(override.aes = list(linetype = 0)))
      }
      else {
        ceplane <- ceplane + stat_density2d() + guides(colour = guide_legend(override.aes = list(linetype = 0)))
      }
    }
    if (k$n.comparisons > 1 & is.null(comparison) == FALSE) {
      k$comp <- k$comp[comparison]
      k$delta.e <- k$delta.e[, comparison]
      k$delta.c <- k$delta.c[, comparison]
      k$n.comparators = length(comparison) + 1
      k$n.comparisons = length(comparison)
      k$interventions = k$interventions[sort(c(k$ref, k$comp))]
      k$ICER = k$ICER[comparison]
      k$ib = k$ib[, , comparison]
      k$eib = k$eib[, comparison]
      k$U = k$U[, , sort(c(k$ref, comparison + 1))]
      k$ceac = k$ceac[, comparison]
      k$ref = rank(c(k$ref, k$comp))[1]
      k$comp = rank(c(k$ref, k$comp))[-1]
      k$mod <- TRUE
      return(contour.bcea(k, scale = scale, pos = alt.legend, 
                          nlevels = nlevels, graph = "ggplot2", comparison = NULL))
    }
    labs.title <- "Cost-Effectiveness Plane"
    labs.title <- paste0(labs.title, ifelse(k$n.comparisons == 
                                              1, paste0("\n", k$interventions[k$ref], " vs ", k$interventions[-k$ref]), 
                                            paste0(ifelse(isTRUE(k$mod), paste0("\n", k$interventions[k$ref], 
                                                                                " vs ", paste0(k$interventions[k$comp], collapse = ", ")), 
                                                          ""))))
    ceplane <- ceplane + labs(title = labs.title, x = "Effectiveness differential", 
                              y = "Cost differential")
    jus <- NULL
    if (isTRUE(alt.legend)) {
      alt.legend = "bottom"
      ceplane <- ceplane + theme(legend.direction = "vertical")
    }
    else {
      if (is.character(alt.legend)) {
        choices <- c("left", "right", "bottom", "top")
        alt.legend <- choices[pmatch(alt.legend, choices)]
        jus = "center"
        if (is.na(alt.legend)) 
          alt.legend = FALSE
      }
      if (length(alt.legend) > 1) 
        jus <- alt.legend
      if (length(alt.legend) == 1 & !is.character(alt.legend)) {
        alt.legend <- c(1, 0)
        jus <- alt.legend
      }
    }
    ceplane <- ceplane + theme(legend.position = alt.legend, 
                               legend.justification = jus, legend.title = element_blank(), 
                               legend.background = element_blank()) + theme(text = element_text(size = 11), 
                                                                            legend.key.size = unit(0.66, "lines"), legend.margin = unit(-1.25, 
                                                                                                                                        "line"), panel.grid = element_blank(), legend.key = element_blank(), 
                                                                            legend.text.align = 0) + theme(plot.title = element_text(lineheight = 1.05, 
                                                                                                                                     face = "bold", size = 14.3))
    return(ceplane)
  }
}