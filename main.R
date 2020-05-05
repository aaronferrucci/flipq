
library(ggplot2)
win <- function(p, N) {
  p1 <- sapply(1:N, function(i) dbinom(i, size=N, prob=p))
  p2 <- sapply(1:N, function(i) (0.5)**i)
  pwin <- sum(p1*p2)
  return(pwin)
}

resolution <- 0.00001
odds <- data.frame(p = seq(resolution, 1, resolution))
odds$win <- sapply(odds$p, function(p) win(p, 4))

ggplot(odds, aes(x=p, y=win)) +
  geom_line()