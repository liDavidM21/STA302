beta_0 <- c()
beta_1 <- c()
estimate <- c()
in_CI <- 0
in_CI_POP <- 0
for (i in 1:500){
  x_val <- 0.5
  e <- rnorm(20, 0, 4)
  x <- c()
  for (j in 1:20){
    x <- c(x, x_val)
    x_val = x_val + 0.5
  }
  y <- c()
  for (j in 1:20){
    y <- c(y, 50+10*x[j] + e[j])
  }
  sum_xiyi <- 0
  sum_xi_square <- 0
  for (j in 1:20){
    sum_xiyi = sum_xiyi + x[j]*y[j]
    sum_xi_square = sum_xi_square + x[j]*x[j]
  }
  b1 <- (sum_xiyi - 20*mean(x)*mean(y))/(sum_xi_square - 20*mean(x)*mean(x))
  beta_1 <- c(beta_1, b1)
  b0 <- mean(y) - b1*mean(x)
  beta_0 <- c(beta_0, b0)
  estimate <- c(estimate, b0 + b1*5)
  square_e <- 0
  for(j in 1:20){
    square_e = square_e + e[j]*e[j]
  }
  S <- square_e/(18)
  SXX <- sum_xi_square - 20*mean(x)*mean(x)
  CI_lower = b1-qt(.975,18)*sqrt(S)/sqrt(SXX)
  CI_upper = b1+qt(.975,18)*sqrt(S)/sqrt(SXX)
  if(CI_lower<= 10 && CI_upper >= 10){
    in_CI = in_CI + 1
  }
  CI_POP_lower = b0 + b1*5 - qt(.975,18)*sqrt(S)*sqrt((1/20) + ((5 - mean(x))/SXX))
  CI_POP_upper = b0 + b1*5 + qt(.975,18)*sqrt(S)*sqrt((1/20) + ((5 - mean(x))/SXX))
  if(CI_POP_lower <= 100 && CI_POP_upper >= 100){
    in_CI_POP = in_CI_POP + 1
  }
}
