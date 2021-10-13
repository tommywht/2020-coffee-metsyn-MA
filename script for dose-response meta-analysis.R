library(dosresmeta)
library(tidyverse)
library(rms)
library(readxl)

# import dataset, make sure there are data for 0 consumption as referent group
test <- read_excel("C:/Users/a1032/Desktop/ongoing projects/coffee systematic review and meta-analysis/dose response analysis/sexadj.xlsx")

# calculate log OR and SE
test <- test %>% mutate(logor = log(or),
                        se = (log(ul)-log(ll))/(2*1.96))

## Spline model (REML)
knots <- round(with(test, quantile(dose, probs = c(.25, .5, .75) )), 2)
spl <- dosresmeta(formula = logor ~ rcs(dose, knots), id = id, type = "cc",
                  cases = cases, n = n, se = se, proc = "1stage", data = test, method = "reml")
summary(spl)

## Graphical results
newdata <- data.frame(dose = seq(0, 7, .01))
spl.pred <- predict(spl,newdata,expo=TRUE)
with(predict(spl, newdata, expo = T), {
  plot(get("rcs(dose, knots)dose"), pred, log = "y", type = "l", bty = "n",
       xlab = "Coffee Consumption, cups/day", ylab = "odds ratio",
       ylim = c(.5, 1.5), las = 1)
  matlines(get("rcs(dose, knots)dose"), cbind(ci.lb, ci.ub), 
           lty = 2, col = "black")
})

# linear (not used)
## not run
lin <- dosresmeta(formula = logor ~ dose, id = id, type = "cc",
                  cases = cases, n = n, se = se, proc = "1stage", data = test, method = "reml")

with(predict(lin, order = TRUE, expo = TRUE), {
  plot(dose, pred, log = "y", type = "l",
       xlim = c(0, 7), ylim = c(.4, 2))
  lines(dose, ci.lb, lty = 2)
  lines(dose, ci.ub, lty = 2)
})
## not run

# dose-response for males
male <- read_excel("C:/Users/a1032/Desktop/ongoing projects/coffee systematic review and meta-analysis/dose response analysis/male.xlsx")

# calculate log OR and SE
male <- male %>% mutate(logor = log(or),
                        se = (log(ul)-log(ll))/(2*1.96))

## Spline model (REML)
knots <- round(with(male, quantile(dose, probs = c(.25, .5, .75) )), 2)
spl.male <- dosresmeta(formula = logor ~ rcs(dose, knots), id = id, type = "cc",
                       cases = cases, n = n, se = se, proc = "1stage", data = male, method = "reml")
summary(spl.male)

## Graphical results
newdata <- data.frame(dose = seq(0, 7, .01))
spl.male.pred <- predict(spl.male,newdata,expo=TRUE)
with(predict(spl.male, newdata, expo = T), {
  plot(get("rcs(dose, knots)dose"), pred, log = "y", type = "l", bty = "n",
       xlab = "Coffee Consumption, cups/day", ylab = "odds ratio",
       ylim = c(.5, 1.5), las = 1)
  matlines(get("rcs(dose, knots)dose"), cbind(ci.lb, ci.ub), 
           lty = 2, col = "black")
})

write_excel_csv(spl.male.pred, path = "C:/Users/a1032/Desktop/ongoing projects/coffee systematic review and meta-analysis/dose response analysis/male pred.csv")

# dose-response for females
female <- read_excel("C:/Users/a1032/Desktop/ongoing projects/coffee systematic review and meta-analysis/dose response analysis/female.xlsx")

# calculate log OR and SE
female <- female %>% mutate(logor = log(or),
                            se = (log(ul)-log(ll))/(2*1.96))

## Spline model (REML)
knots <- round(with(female, quantile(dose, probs = c(.25, .5, .75) )), 2)
spl.female <- dosresmeta(formula = logor ~ rcs(dose, knots), id = id, type = "cc",
                         cases = cases, n = n, se = se, proc = "1stage", data = female, method = "reml")
summary(spl.female)

## Graphical results
newdata <- data.frame(dose = seq(0, 7, .01))
spl.female.pred <- predict(spl.female,newdata,expo=TRUE)
with(predict(spl.female, newdata, expo = T), {
  plot(get("rcs(dose, knots)dose"), pred, log = "y", type = "l", bty = "n",
       xlab = "Coffee Consumption, cups/day", ylab = "odds ratio",
       ylim = c(.5, 1.5), las = 1)
  matlines(get("rcs(dose, knots)dose"), cbind(ci.lb, ci.ub), 
           lty = 2, col = "black")
})

write_excel_csv(spl.female.pred, path = "C:/Users/a1032/Desktop/ongoing projects/coffee systematic review and meta-analysis/dose response analysis/female pred.csv")