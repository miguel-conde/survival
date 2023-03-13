# https://cran.r-project.org/web/packages/survival/vignettes/survival.pdf

library(tidyverse)
library(survival)

# SURVIVAL CURVES ---------------------------------------------------------


# One event type, one event per subject -----------------------------------

# Kaplan-Meier curve

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

# 1 = alive (censored), 2 = death
head(lung)

fit2 <- survfit(Surv(time, status) ~ sex + ph.ecog, data=lung)
fit2

# The argument fun='event' has caused the death rate D = 1 − S to be plotted
plot(fit2[1:3], lty=1:3, lwd=2, xscale=365.25, fun='event',
     xlab="Years after enrollment", ylab="Survival")
legend(550, .6, paste("Performance Score", 0:2, sep=' ='),
         lty=1:3, lwd=2, bty='n')
text(400, .95, "Males", cex=2)

tibble(time     = summary(fit2)$time, 
       n.risk   = summary(fit2)$n.risk, 
       n.event  = summary(fit2)$n.event, 
       n.censor = summary(fit2)$n.censor, 
       surv     = summary(fit2)$surv, 
       strata   = summary(fit2)$strata, 
       lower    = summary(fit2)$lower, 
       upper    = summary(fit2)$upper) %>% 
  filter(strata == "sex=1, ph.ecog=0") %>% 
  select(time, surv) %>% 
  plot(type = "l")


# Repeated events ---------------------------------------------------------

# In multi-event data, the cumulative hazard is an estimate of the expected 
# number of events for a unit that has been observed for the given amount of 
# time, whereas the survival S estimates the probability that a unit has had 0 
# repairs.
#
# The cumulative hazard is the more natural quantity to plot in such studies; 
# in reliability analysis it is also known as the mean cumulative function.
#
# By default, the survfit routine computes both the survival and the Nelson cumulative
# hazard estimate

data(valveSeats, package = "reda")
head(valveSeats)

valveSeats %>% 
  group_by(ID) %>% 
  arrange(Days) %>% 
  mutate(time1 = lag(Days), .before = Days) %>% 
  rename(time2 = Days) %>% 
  ungroup() %>% 
  mutate(time1 = ifelse(is.na(time1), 0, time1))

vdata <- with(valveSeat, data.frame(id=id, time2=time, status=status))
first <- !duplicated(vdata$id)
vdata$time1 <- ifelse(first, 0, c(0, vdata$time[-nrow(vdata)]))
double <- which(vdata$time1 == vdata$time2)
vdata$time1[double] <- vdata$time1[double] -.01
vdata$time2[double-1] <- vdata$time1[double]
vdata[1:7, c("id", "time1", "time2", "status")]

vdata2 <- valveSeats %>% 
  rename(id = ID, time2 = Days, status = `No.`) %>% 
  group_by(id) %>% 
  arrange(time2) %>% 
  mutate(time1 = lag(time2), .before = time2) %>% 
  mutate(time1 = ifelse(is.na(time1), 0, time1))

double <- which(vdata2$time1 == vdata2$time2)
vdata2$time1[double] <- vdata2$time1[double] -.01
vdata2$time2[double-1] <- vdata2$time1[double]

vdata <- vdata2

survcheck(Surv(time1, time2, status) ~ 1, id=id, data=vdata)

vfit <- survfit(Surv(time1, time2, status) ~1, data=vdata, id=id)
summary(vfit)
plot(vfit, cumhaz=TRUE, xlab="Days", ylab="Cumulative hazard")

plot(vfit, cumhaz=TRUE, xlab="Days", ylab="Cumulative hazard",
     conf.times = 100*c(1:7))


# Competing risks ---------------------------------------------------------

# The curves are computed using the Aalen-Johansen estimator.

crdata <- data.frame(time= c(1:8, 6:8),
                     endpoint=factor(c(1,1,2,0,1,1,3,0,2,3,0),
                                     labels=c("censor", "a", "b", "c")),
                     istate=rep("entry", 11), # Initial state
                     id= LETTERS[1:11])
crdata

# The first level of the factor is used to code censoring while the remaining 
# ones are possible outcomes.
levels(crdata$endpoint)

tfit <- survfit(Surv(time, endpoint) ~ 1, data=crdata, id=id, istate=istate)
dim(tfit)

tfit

plot(tfit, col=1:4, lty=1:4, lwd=2, ylab="Probability in state")

## Ejemplo
mgus

event <- with(mgus2, ifelse(pstat==1, 1, 2*death))
event <- factor(event, 0:2, c("censored", "progression", "death"))
etime <- with(mgus2, ifelse(pstat==1, ptime, futime))
crfit <- survfit(Surv(etime, event) ~ sex, mgus2)
crfit

mgus2_2 <- mgus2 %>% 
  mutate(event = ifelse(pstat==1, 1, 2*death) %>% 
           factor(labels = c("censored", "progression", "death")),
         etime = ifelse(pstat==1, ptime, futime))

mgus2_2

crfit_2 <- survfit(Surv(etime, event) ~ sex, mgus2_2)
crfit_2

summary(crfit_2)

plot(crfit, col=1:2, noplot="",
     lty=c(3,3,2,2,1,1), lwd=2, xscale=12,
     xlab="Years post diagnosis", ylab="P(state)")
legend(240, .65, c("Female, death", "Male, death", "malignancy", "(s0)"),
       lty=c(1,1,2,3), col=c(1,2,1,1), bty='n', lwd=2)



# Multi-state data --------------------------------------------------------

myeloid[1:5,]

# Overall survival curves
sfit0 <- survfit(Surv(futime, death) ~ trt, myeloid)
plot(sfit0, xscale=365.25, xaxs='r', col=1:2, lwd=2,
       xlab="Years post enrollment", ylab="Survival")
legend(20, .4, c("Arm A", "Arm B"),
         col=1:2, lwd=2, bty='n')

# The full multi-state data set can be created with the tmerge routine.
mdata <- tmerge(myeloid[,1:2], myeloid, id=id, death= event(futime, death),
                  sct = event(txtime), cr = event(crtime),
                  relapse = event(rltime))
temp <- with(mdata, cr + 2*sct + 4*relapse + 8*death)
table(temp)

# Our check shows that there is one subject who had CR and stem cell transplant on the same
# day (temp=3). To avoid length 0 intervals, we break the tie so that complete response (CR)
# happens rst.
tdata <- myeloid # temporary working copy
tied <- with(tdata, (!is.na(crtime) & !is.na(txtime) & crtime==txtime))
tdata$crtime[tied] <- tdata$crtime[tied] -1
mdata <- tmerge(tdata[,1:2], tdata, id=id, death= event(futime, death),
                  sct = event(txtime), cr = event(crtime),
                  relapse = event(rltime),
                  priorcr = tdc(crtime), priortx = tdc(txtime))
temp <- with(mdata, cr + 2*sct + 4*relapse + 8*death)
table(temp)

mdata$event <- factor(temp, c(0,1,2,4,8),
                      c("none", "CR", "SCT", "relapse", "death"))
mdata[1:7, c("id", "trt", "tstart", "tstop", "event", "priorcr", "priortx")]

# Subject 1 has a CR on day 44, relapse on day 113, death on day 235 and did not receive a
# stem cell transplant. The data for the rst three subjects looks good. Check it out a little more
# thoroughly using survcheck.
survcheck(Surv(tstart, tstop, event) ~1, mdata, id=id)



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

