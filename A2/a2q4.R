prod <- read.csv(file.choose(), header=T)
model <- lm(Midterm.2 ~ Midterm.1, data = prod)
anova(model)
plot(model$residuals ~ model$fitted.values)
hii <- hatvalues(model)
summary(hii)
which(hii > 4/nrow(prod))
grade <- c()
New <- c()

for(i in 1:nrow(prod)){
  grade <- c(grade, prod$Midterm.1[i])
  New <- c(New, 0)
}
for(i in 1:nrow(prod)){
  grade <- c(grade, prod$Midterm.2[i])
  New <- c(New, 1)
}
df <- data.frame(grade, New)
combined_model <- lm(grade ~ New, data = df)
boxplot(df$grade ~ df$New)