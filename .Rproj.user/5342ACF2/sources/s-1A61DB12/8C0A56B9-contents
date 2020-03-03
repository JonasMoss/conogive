agreeableness = psychTools::bfi[c("A1", "A2", "A3", "A4", "A5")]
agreeableness[, "A1"] = 7 - agreeableness[, "A1"] # Reverse-coded item.

conscientiousness = psychTools::bfi[c("C1", "C2", "C3", "C4", "C5")]
conscientiousness[, "C4"] = 7 - conscientiousness[, "C4"] # Reverse-coded item.
conscientiousness[, "C5"] = 7 - conscientiousness[, "C5"] # Reverse-coded item.

extraversion = psychTools::bfi[c("E1", "E2", "E3", "E4", "E5")]
extraversion[, "E1"] = 7 - extraversion[, "E1"] # Reverse-coded item.

newdata = extraversion
object = latcon(newdata)

plot(rowSums(newdata, na.rm = TRUE), predict(object, newdata, "optimal"))
cor(rowSums(newdata, na.rm = TRUE), predict(object, newdata, "optimal"), use = "complete.obs")
hist(predict(object, newdata, "optimal"))
hist(predict(object, newdata, "equal"))

extraversion = psychTools::bfi[c("E1", "E2", "E3", "E4", "E5")]
extraversion[, "E1"] = 7 - extraversion[, "E1"] # Reverse-coded item.
fit = latcon(extraversion)

newdata = newdata[1, ]
predict(object, newdata, "equal")

ordinal_omega(object, weights = "optimal", xi = "theoretical")
ordinal_omega(object, weights = "equal", xi = "theoretical")
ordinal_omega(object, weights = "optimal", xi = "sample")
ordinal_omega(object, weights = "equal", xi = "sample")

ordinal_omega(object, weights = "equal", xi = "sample", limit = TRUE)
ordinal_omega(object, weights = "optimal", xi = "sample", limit = TRUE)


plot(rowSums(newdata, na.rm = TRUE), predict(object, newdata, "optimal"))


newdata = ltm::abortion
object = latcon(newdata, fm = "ml")
y = predict(object, newdata)
ordinal_omega(object, weights = "optimal")
ordinal_omega(object, weights = "optimal", limit = TRUE)

z = psych::scoreIrt(psych::irt.fa(newdata), newdata)[, 1]
#library("ltm")
#z = factor.scores(ltm::ltm(ltm::WIRS ~ z1), ltm::WIRS)$score.dat$z1

mod = lm(z ~ y)
plot(y, z)
lines(sort(y), coef(mod)[1] + coef(mod)[2]*sort(y))


agreeableness = psychTools::bfi[c("A1", "A2", "A3", "A4", "A5")]
agreeableness[, "A1"] = 7 - agreeableness[, "A1"] # Reverse-coded item.
object = latcon(agreeableness)
ordinal_alpha(object) # 0.6267724
ordinal_omega(object, weights = "equal") # 0.6394087



f = Vectorize(function(k) {
  fit = latcon(list(lambda = rep(1, 4),
                    sigma = rep(1, 4),
                    cuts = qnorm(seq(0, 1, length.out = k + 1))))
  ordinal_omega(fit)
})

k = 2:30
plot(k, f(k))



agreeableness = psychTools::bfi[c("A1", "A2", "A3", "A4", "A5")]
agreeableness[, "A1"] = 7 - agreeableness[, "A1"] # Reverse-coded item.
object = latcon(agreeableness)
ordinal_alpha(object) # 0.6267724
ordinal_omega(object, weights = "equal") # 0.6394087


agreeableness = psychTools::bfi[c("A1", "A2", "A3", "A4", "A5")]
agreeableness[, "A1"] = 7 - agreeableness[, "A1"] # Reverse-coded item.
object = con
