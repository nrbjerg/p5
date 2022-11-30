X <- seq(-5, 5, 0.05)

N <- dnorm(X)
CHI <- dchisq(X, df = 2)
FD <- df(X, df1 = 2, df2 = 2)
TD <- dt(X, df = 1)


plot(X, TD, type = "l", col = "Purple",
     ylim = c(0, 1), xlab = "",
     ylab = "")
lines(X, FD, col = "Green")
lines(X, CHI, col = "Blue")
lines(X, N, col = "Red")

legend(x = "topleft", legend = c(expression(N(0,1)), expression({chi^2}(2)), expression(F(2,2)), expression(t(1))), col = c("Red", "Blue", "Green", "Purple"), 
       lty = rep(1, 4), cex = 1, box.lty = 1)

