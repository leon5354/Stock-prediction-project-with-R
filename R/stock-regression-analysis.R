# LSE stock-price regression and variable selection
#
# Multiple-linear-regression study that builds an explanatory model for VOD
# (Vodafone) using a basket of LSE-listed stocks, then compares four
# variable-selection strategies (leaps-and-bounds, stepwise, forward, backward)
# with Box-Cox response transforms and residual diagnostics.
#
# *** SYNTHETIC DATA ***
# The original 2023 coursework used a London Stock Exchange basket supplied
# with the assignment (`lse.RData`), which was never committed and is no
# longer available. To keep this analysis runnable, `data/generate_demo_data.R`
# reproduces the schema with synthetic prices. The methodology is unchanged;
# the headline numbers will not match the original report.
#
# Bug fixes vs. the original draft:
#   * The malformed `for (i in x){ x <- c(...) }` loop, which reassigned the
#     iteration variable inside its own body, is replaced with a simple loop
#     over an explicit variable vector.
#   * The backward-selection fit referenced `data = lsenew4` inside the very
#     `subset()` call that defined `lsenew4`, relying on lazy evaluation. The
#     data frame is now built first and then passed to `lm()`.
#   * Minor typos cleaned up throughout the comments.

require(car)
require(ggplot2)
require(dplyr)
require(broom)
require(ggpubr)
require(GGally)
require(corrplot)
require(leaps)
require(MASS)

load("data/lse.RData")

# ---- Q1: data cleaning and multicollinearity screening --------------------

summary(lse)
sum(is.na(lse))   # no NAs

# Drop non-numeric / time columns before computing correlations.
lse1 <- subset(lse, select = -c(Date, Weekday))
cor1 <- cor(lse1)
corrplot::corrplot.mixed(cor1, lower.col = "black", number.cex = .4)

# Inspect the most obviously collinear pairs.
car::scatterplot(SMT ~ SPX,  data = lse1)
car::scatterplot(SMT ~ AHT,  data = lse1)
car::scatterplot(SMT ~ EXPN, data = lse1)
# SMT correlates with several other predictors and least with VOD -> drop SMT.

car::scatterplot(SSE ~ SVT, data = lse1)
car::scatterplot(SSE ~ CCH, data = lse1)   # NB: CCH not in this basket
# SSE is more strongly related to VOD than SVT -> keep SSE.

car::scatterplot(SPX ~ EXPN, data = lse1)
# Strong linear relationship; EXPN has the weaker VOD link -> drop EXPN.

car::scatterplot(Year ~ SSE, data = lse1)
# Drop Year to keep the model simple.

# Drop the collinear / redundant predictors.
lsenew <- subset(lse1, select = -c(EXPN, SVT, SMT, Year))
summary(lsenew)
cor(lsenew)
plot(lsenew)

# Is VOD itself reasonably well-behaved?
hist(lse1$VOD)   # mild negative skew; acceptable for linear regression

# Linearity: scan a few predictor subsets.
lselin1 <- subset(lse1, select = c(VOD, STJ, MGGT, TSCO, SPX, AUTO, SSE, AHT, RTO, ABDN))
lselin2 <- subset(lse1, select = c(VOD, RMV, LLOY, SDR, ABF, BATS, ENT, RR, SMIN))
lselin3 <- subset(lse1, select = c(VOD, ANTO, BA, PSN, PRU, CPG, WTB))
pairs(lse1)
plot(lselin1)
plot(lselin2)
plot(lselin3)
# SSE, AHT, LLOY, ABF look like candidates for transformation.

# ---- transformations -------------------------------------------------------

lsenew$SSE.log <- log10(lsenew$SSE + 1 - min(lsenew$SSE))
car::scatterplot(VOD ~ SSE,     data = lsenew)
car::scatterplot(VOD ~ SSE.log, data = lsenew)   # improved

lsenew$LLOY.log <- log(lsenew$LLOY)
car::scatterplot(VOD ~ LLOY,     data = lsenew)
car::scatterplot(VOD ~ LLOY.log, data = lsenew)   # improved

lsenew$ABF.log <- log10(lsenew$ABF + 1 - min(lsenew$ABF))
car::scatterplot(VOD ~ ABF,     data = lsenew)
car::scatterplot(VOD ~ ABF.log, data = lsenew)   # improved

# ---- leaps-and-bounds variable selection ----------------------------------

# The column indices exclude VOD (the response) and the engineered log columns.
leaps1 <- leaps(lsenew[, -c(2, 8, 13, 15)], lsenew$VOD,
                names = names(lsenew)[-c(2, 8, 13, 15)],
                method = "adjr2", nbest = 5)
leaps_res <- data.frame(size = leaps1$size, adjr2 = leaps1$adjr2, leaps1$which)
plot(adjr2 ~ size, data = leaps_res, ylim = c(0.85, 0.91))

leaps_res[leaps_res$size == 14, ]
leaps14 <- lm(VOD ~ STJ + SPX + AUTO + ABDN + RMV + BATS + SMIN + ANTO + BA +
                PRU + CPG + SSE.log + ABF.log, data = lsenew)
summary(leaps14)

# Assumption diagnostics for the leaps model.
par(mfrow = c(2, 2), mar = c(4.5, 4, 2, 2))
plot(leaps14)
pairs(data.frame(resid(leaps14), lsenew[, -c(2)]))
plot(resid(leaps14) ~ ABDN, data = lsenew)
plot(resid(leaps14) ~ log(ABDN + 3), data = lsenew)
plot(resid(leaps14) ~ sqrt(ABDN + 3), data = lsenew)

# Box-Cox suggests a response transform of around lambda = 2.6.
boxcox(leaps14, plotit = TRUE, lambda = seq(-4, 4))
abline(v = 2.6)

lsenew$VOD_train1 <- lsenew$VOD^2.6

par(mfrow = c(2, 2), mar = c(4.5, 4, 2, 2))
leaps14.square <- lm(lsenew$VOD_train1 ~ STJ + SPX + AUTO + ABDN + RMV + BATS +
                       SMIN + ANTO + BA + PRU + CPG + SSE.log + ABF.log, data = lsenew)
plot(leaps14.square)
plot(leaps14)
boxcox(leaps14.square, plotit = TRUE, lambda = seq(-4, 4))
summary(leaps14.square)

# Re-run leaps with the transformed response.
leaps2 <- leaps(lsenew[, -c(2, 8, 13, 15, 30)], lsenew$VOD_train1,
                names = names(lsenew)[-c(2, 8, 13, 15, 30)],
                method = "adjr2", nbest = 5)
leaps2_res <- data.frame(size = leaps2$size, adjr2 = leaps2$adjr2, leaps2$which)
plot(adjr2 ~ size, data = leaps2_res, ylim = c(0.85, 0.91))
leaps2_res[leaps2_res$size == 14, ]

leaps14.2 <- lm(VOD_train1 ~ TSCO + SPX + AUTO + ABDN + RMV + BATS + SMIN +
                  ANTO + BA + PRU + CPG + SSE.log + ABF.log, data = lsenew)
summary(leaps14)
summary(leaps14.2)

par(mfrow = c(2, 2), mar = c(4.5, 4, 2, 2))
plot(leaps14)
plot(leaps14.2)
acf(resid(leaps14))
acf(resid(leaps14.2))
# Residual autocorrelation is visible — independence is hard to guarantee for
# price data, which is a real caveat on the standard errors.

# ---- stepwise / forward / backward selection ------------------------------

# Candidate predictor names reused by all three strategies.
candidate_vars <- c("Month", "STJ", "MGGT", "TSCO", "SPX", "AUTO", "AHT", "RTO",
                    "ABDN", "RMV", "SDR", "BATS", "ENT", "RR", "SMIN", "ANTO",
                    "BA", "PSN", "PRU", "CPG", "WTB",
                    "SSE.log", "LLOY.log", "ABF.log")
scope_formula <- reformulate(candidate_vars, response = "VOD")

# Stepwise (both directions).
lsenew2 <- subset(lsenew, select = -c(VOD_train1, SSE, LLOY, ABF))
stepwise_start <- lm(VOD ~ 1, data = lsenew2)
step(stepwise_start, scope = scope_formula, direction = "both")

stepwisemodeldone <- lm(
  VOD ~ ABDN + BATS + AHT + SSE.log + BA + PRU + RMV + AUTO + ANTO + CPG +
    SMIN + SPX + STJ + RTO + Month + TSCO + ABF.log + SDR + PSN + RR +
    MGGT + WTB,
  data = lsenew2
)

par(mfrow = c(2, 2), mar = c(4.5, 4, 2, 2))
plot(stepwisemodeldone)

# BUG FIX: the original draft did `for (i in x){ x <- c(...) }`, which
# reassigned the loop variable inside its own body and only worked by accident
# of lazy evaluation. Iterate over a fixed list instead.
for (v in candidate_vars) {
  pairs(data.frame(resid(stepwisemodeldone), lsenew2[[v]]))
}

boxcox(stepwisemodeldone, plotit = TRUE, lambda = seq(-4, 4))
abline(v = 2.8)

lsenew2$VOD.three <- lsenew2$VOD^2.8

stepwise_start2 <- lm(VOD.three ~ 1, data = lsenew2)
step(stepwise_start2, scope = reformulate(candidate_vars, "VOD.three"),
     direction = "both")

stepwisemodeldone2 <- lm(
  VOD.three ~ ABDN + BATS + PSN + CPG + SSE.log + AUTO + RMV + ANTO + BA +
    PRU + ABF.log + SPX + SMIN + TSCO + AHT + WTB + RTO + STJ + SDR +
    Month + LLOY.log,
  data = lsenew2
)

par(mfrow = c(2, 2), mar = c(4.5, 4, 2, 2))
plot(stepwisemodeldone2)
plot(stepwisemodeldone)
summary(stepwisemodeldone2)
summary(stepwisemodeldone)
acf(resid(stepwisemodeldone2))

# Forward selection.
lsenew3 <- subset(lsenew, select = -c(VOD_train1, SSE, LLOY, ABF))
forward_start <- lm(VOD ~ 1, data = lsenew3)
step(forward_start, scope = scope_formula, direction = "forward")

forwardmodeldone <- lm(
  VOD ~ ABDN + BATS + AHT + ENT + SSE.log + BA + PRU + RMV + AUTO + ANTO +
    CPG + SMIN + SPX + STJ + RTO + Month + TSCO + ABF.log + SDR + PSN +
    RR + MGGT + WTB,
  data = lsenew3
)

par(mfrow = c(2, 2), mar = c(4.5, 4, 2, 2))
plot(forwardmodeldone)
for (v in candidate_vars) {
  pairs(data.frame(resid(forwardmodeldone), lsenew3[[v]]))
}
boxcox(forwardmodeldone, plotit = TRUE, lambda = seq(-4, 4))
abline(v = 2.8)

lsenew3$VOD.three <- lsenew3$VOD^2.8
forward_start2 <- lm(VOD.three ~ 1, data = lsenew3)
step(forward_start2, scope = reformulate(candidate_vars, "VOD.three"),
     direction = "forward")

forwardmodeldone2 <- lm(
  VOD.three ~ ABDN + BATS + PSN + CPG + SSE.log + AUTO + RMV + ANTO + BA +
    PRU + ABF.log + SPX + SMIN + TSCO + AHT + WTB + RTO + STJ + SDR +
    Month + LLOY.log,
  data = lsenew3
)

par(mfrow = c(2, 2), mar = c(4.5, 4, 2, 2))
plot(forwardmodeldone2)
plot(forwardmodeldone)
summary(forwardmodeldone2)
summary(forwardmodeldone)
acf(resid(forwardmodeldone2))

# Backward selection.
# BUG FIX: the original called `lm(..., data = lsenew4)` inside the same line
# that defined `lsenew4`, relying on lazy evaluation quirks. Build the data
# frame first, then fit.
lsenew4 <- subset(lsenew, select = -c(VOD_train1, SSE, LLOY, ABF))
backward_start <- lm(VOD ~ ., data = lsenew4[, c("VOD", candidate_vars)])
step(backward_start, scope = scope_formula, direction = "backward")

backwardmodeldone <- lm(
  VOD ~ Month + STJ + MGGT + TSCO + SPX + AUTO + AHT + RTO + ABDN + RMV +
    SDR + BATS + RR + SMIN + ANTO + BA + PSN + PRU + CPG + WTB +
    SSE.log + ABF.log,
  data = lsenew4
)

par(mfrow = c(2, 2), mar = c(4.5, 4, 2, 2))
plot(backwardmodeldone)
for (v in candidate_vars) {
  pairs(data.frame(resid(backwardmodeldone), lsenew4[[v]]))
}
boxcox(backwardmodeldone, plotit = TRUE, lambda = seq(-4, 4))
abline(v = 2.8)

lsenew4$VOD.three <- lsenew4$VOD^2.8
backward_start2 <- lm(VOD.three ~ ., data = lsenew4[, c("VOD.three", candidate_vars)])
step(backward_start2, scope = reformulate(candidate_vars, "VOD.three"),
     direction = "backward")

backwardmodeldone2 <- lm(
  VOD.three ~ Month + STJ + MGGT + TSCO + SPX + AUTO + AHT + RTO + ABDN +
    RMV + SDR + BATS + RR + SMIN + ANTO + BA + PSN + PRU + CPG + WTB +
    SSE.log + LLOY.log + ABF.log,
  data = lsenew4
)

par(mfrow = c(2, 2), mar = c(4.5, 4, 2, 2))
plot(backwardmodeldone2)
plot(backwardmodeldone)

# ---- final comparison ------------------------------------------------------

summary(leaps14.2)
summary(stepwisemodeldone2)
summary(forwardmodeldone2)
summary(backwardmodeldone2)
# Adjusted R^2 ranking: backward > forward = stepwise > leaps14.2

par(mfrow = c(2, 2), mar = c(4.5, 4, 2, 2))
plot(leaps14.2)
plot(stepwisemodeldone2)
plot(forwardmodeldone2)
plot(backwardmodeldone2)

acf(resid(leaps14.2))
acf(resid(stepwisemodeldone2))
acf(resid(forwardmodeldone2))
acf(resid(backwardmodeldone2))
# All four show residual autocorrelation — a known caveat of fitting OLS to
# price levels rather than returns.

# PRESS statistic for the leaps model.
sum((leaps14.2$residuals^2) / (1 - hatvalues(leaps14.2))^2)
