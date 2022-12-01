qqnorm(cars$speed)

Speed <- sort(cars$speed)
index <- 1:length(Speed)

m <- median(Speed)

quantile <- (index - 0.5)/length(Speed)

plot(qnorm(quantile),Speed)
