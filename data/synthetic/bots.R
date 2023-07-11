set.seed(5)
bots <- rbeta(100000, 2, 3)
hist(bots)
abline(v=0.8, col="red", lty=2, lwd=2)

?abline

# 2692 < 100.000
length(bots[which(bots>0.8)])
