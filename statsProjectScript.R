n = names(Mens)
metrics <- n[4:10]
x_0 = Mens[[n[2]]] # this is the x axis, it will always be weight
x_1 = Womens[[n[2]]]
for (i in 4:10) {
  y_0 = Mens[[n[i]]] # this is the y axis, will go from total to GL
  y_1 = Womens[[n[i]]]
  label = n[i]
  if (label == 'Total'){
    label = paste(label, '(Kg)', sep = ' ')
  }
  plot(x_0, y_0, xlab = 'Weight (Kg)', ylab = label, main = paste(n[2], label, sep=" (Kg) vs. ") , xlim = c(min(Womens$Weight) - 5, max(Mens$Weight) + 5), ylim = c(min(y_1) - min(y_1)*0.1, max(y_0) + max(y_0)*0.1), col = 'blue')
  points(x_1, y_1, col = 'red')
  legend(x = "bottomright", legend=c("Men", "Women"), 
         fill = c("blue","red"))
  if (label == 'Total (Kg)'){ # adding trend lines
    abline(lm(formula = Total ~ Weight, data = Mens), col = 'blue')
    abline(lm(formula = Total ~ Weight, data = Womens), col = 'red')
  }
  else if (label == 'xBW') {
    abline(lm(formula = xBW ~ Weight, data = Mens), col = 'blue')
    abline(lm(formula = xBW ~ Weight, data = Womens), col = 'red')
  }

}

corScoresMen = c()

corScoresWomen = c()

for (i in 4:10) {
  test <- cor.test(formula = ~ Mens[[n[i]]] + Mens[[n[2]]])
  corScoresMen <- append(corScoresMen, test$estimate)
}


for (i in 4:10) {
  test <- cor.test(formula = ~ Womens[[n[i]]] + Womens[[n[2]]])
  corScoresWomen <- append(corScoresWomen, test$estimate)
}

mens.lm <- lm(formula = Total ~ Weight,
              data = Mens)
womens.lm <- lm(formula = Total ~ Weight,
              data = Womens)

mensBW.lm <- lm(formula = xBW ~ Weight,
                data = Mens)
womensBW.lm <- lm(formula = xBW ~ Weight,
                data = Womens)
metrics
corScoresMen
corScoresWomen
summary(mens.lm)$coefficients
summary(womens.lm)$coefficients
summary(mensBW.lm)$coefficients
summary(womensBW.lm)$coefficients

meansMen <- c()
meansWomen <- c()

for (i in 4:10) {
  mean <- mean(Mens[[n[i]]])
  meansMen <- append(meansMen, mean)
  mean <- mean(Womens[[n[i]]])
  meansWomen <- append(meansWomen, mean)
}

meansMen
meansWomen
