
library(ggplot2)
win <- function(p, N) {
  p1 <- sapply(1:N, function(i) dbinom(i, size=N, prob=p))
  p2 <- sapply(1:N, function(i) (0.5)**i)
  pwin <- sum(p1*p2)
  return(pwin)
}

resolution <- 0.001
ps <- seq(resolution, 1, resolution)
odds <- data.frame(
  p = numeric(0),
  win = numeric(0),
  N = numeric(0)
)

for (n in 2:10) {
  d1 <- data.frame(p = ps)
  
  d1$win <- sapply(d1$p, function(p) win(p, n))
  d1$N <- n
  odds <- rbind(odds, d1)
}

odds$N <- factor(odds$N)
odds <- odds[order(odds$p),]

p <- ggplot(odds, aes(x=p, y=win, col=N)) +
  geom_point()

print(p)
