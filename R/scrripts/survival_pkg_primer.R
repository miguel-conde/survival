# https://cran.r-project.org/web/packages/survival/vignettes/survival.pdf

library(tidyverse)
library(survival)

# SURVIVAL CURVES ---------------------------------------------------------


# One event type, one event per subject -----------------------------------

head(ovarian)

# futime:	survival or censoring time
# fustat:	censoring status - 0 = subject was censored at t and 1 = subject had
#                                an event at t
# age:	in years
# resid.ds:	residual disease present (1=no,2=yes)
# rx:	treatment group
# ecog.ps:	ECOG performance status (1 is better, see reference)

fit1 <- survfit(Surv(futime, fustat) ~ resid.ds, data=ovarian)
print(fit1, rmean= 730)

summary(fit1, times= (0:4)*182.5, scale=365)

plot(fit1, col=1:2, xscale=365.25, lwd=2, mark.time=TRUE,
     xlab="Years since study entry", ylab="Survival")
legend(750, .9, c("No residual disease", "Residual disease"),
         col=1:2, lwd=2, bty='n')

# 1 = alive, 2 = death
head(lung)

fit2 <- survfit(Surv(time, status) ~ sex + ph.ecog, data=lung)
fit2

# The argument fun='event' has caused the death rate D = 1 − S to be plotted
plot(fit2[1:3], lty=1:3, lwd=2, xscale=365.25, fun='event',
     xlab="Years after enrollment", ylab="Survival")
legend(550, .6, paste("Performance Score", 0:2, sep=' ='),
         lty=1:3, lwd=2, bty='n')
text(400, .95, "Males", cex=2)


# COX MODEL ---------------------------------------------------------------

head(lung)

#  One event type, one event per subject ----------------------------------

cfit1 <- coxph(Surv(time, status) ~ age + sex + wt.loss, data=lung)
print(cfit1, digits=3)

summary(cfit1, digits=3)

coef(cfit1)
concordance(cfit1)
fitted(cfit1)
predict(cfit1)
residuals(cfit1)

cfit2 <- coxph(Surv(time, status) ~ age + sex + wt.loss + strata(inst),
               data=lung)
round(cbind(simple= coef(cfit1), stratified=coef(cfit2)), 4)


dummy <- expand.grid(age=c(50, 60), sex=1, wt.loss=5)
dummy

csurv1 <- survfit(cfit1, newdata=dummy)
csurv2 <- survfit(cfit2, newdata=dummy)

dim(csurv1)
dim(csurv2)

plot(csurv1, col=1:2, xscale=365.25, xlab="Years", ylab="Survival")


dummy2 <- data.frame(age=c(50, 60), sex=1:2, wt.loss=5, inst=c(6,11))
csurv3 <- survfit(cfit2, newdata=dummy2)
dim(csurv3)

# Check  proportional hazards
zp1 <- cox.zph(cfit1)
zp1

plot(zp1[2], resid=FALSE)
abline(coef(fit1)[2] ,0, lty=3)

# check for linearity of age
cfit3 <- coxph(Surv(time, status) ~ pspline(age) + sex + wt.loss, lung)
print(cfit3, digits=2)

termplot(cfit3, term=1, se=TRUE)

# check additivity
cfit4 <- update(cfit1, . ~ . + age*sex)

anova(cfit1, cfit4)

