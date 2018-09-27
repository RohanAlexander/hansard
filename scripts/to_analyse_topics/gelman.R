library("rstan")
library("rstanarm")
library("arm")
# options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

## 2.  Simulate a data structure with N_per_person measurements on each of J people

J <- 50  # number of people in the experiment
N_per_person <- 10 # number of measurements per person
person_id <- rep(1:J, rep(N_per_person, J))
index <- rep(1:N_per_person, J) 
time <- index - 1  # time of measurements, from 0 to 9
N <- length(person_id)
a <- rnorm(J, 0, 1)
b <- rnorm(J, 1, 1)
theta <- 1
sigma_y <- 1

## 3.  Simulate data from a between-person experiment

z <- sample(rep(c(0,1), J/2), J)
y_pred <- a[person_id] + b[person_id]*time + theta*z[person_id]*time
y <- rnorm(N, y_pred, sigma_y)
z_full <- z[person_id]
exposure <- z_full*time
data_1 <- data.frame(time, person_id, exposure, y)

## 4.  Simulate data from a within-person experiment:  for each person, do one treatment for the first half of the experiment and the other treatment for the second half.

z_first_half <- z
T_switch <- floor(0.5*max(time))
z_full <- ifelse(time <= T_switch, z_first_half[person_id], 1 - z_first_half[person_id])
for (j in 1:J){
  exposure[person_id==j] <- cumsum(z_full[person_id==j])
}
y_pred <- a[person_id] + b[person_id]*time + theta*exposure
y <- rnorm(N, y_pred, sigma_y)
data_2 <- data.frame(time, person_id, exposure, y)

## 5.  Graph the simulated data

pdf("within_design.pdf", height=7, width=10)
par(mfrow=c(2, 2))
par(mar=c(3,3,3,1), mgp=c(1.5, .5, 0), tck=-.01)

plot(range(time), range(data_1$y, data_2$y), xlab="time", ylab="y", type="n", bty="l", main="Between-person design:\nControl group")
for (j in 1:J){
  ok <- data_1$person_id==j
  if (z[j] == 0){
    points(time[ok], data_1$y[ok], pch=20, cex=.5)
    lines(time[ok], data_1$y[ok], lwd=.5, col="blue")
  }
}
plot(range(time), range(data_1$y, data_2$y), xlab="time", ylab="y", type="n", bty="l", main="Between-person design:\nTreatment group")
for (j in 1:J){
  ok <- data_1$person_id==j
  if (z[j] == 1){
    points(time[ok], data_1$y[ok], pch=20, cex=.5)
    lines(time[ok], data_1$y[ok], lwd=.5, col="red")
  }
}
plot(range(time), range(data_1$y, data_2$y), xlab="time", ylab="y", type="n", bty="l", main="Within-person design:\nControl, then treatment")
for (j in 1:J){
  ok <- person_id==j
  if (z[j] == 0) {
    points(time[ok], data_2$y[ok], pch=20, cex=.5)
    lines(time[ok&time<=T_switch], data_2$y[ok&time<=T_switch], lwd=.5, col="blue")
    lines(time[ok&time>=T_switch], data_2$y[ok&time>=T_switch], lwd=.5, col="red")
  }
}
plot(range(time), range(data_1$y, data_2$y), xlab="time", ylab="y", type="n", bty="l", main="Within-person design:\nTreatment, then control")
for (j in 1:J){
  ok <- person_id==j
  if (z[j] == 1) {
    points(time[ok], data_2$y[ok], pch=20, cex=.5)
    lines(time[ok&time<=T_switch], data_2$y[ok&time<=T_switch], lwd=.5, col="red")
    lines(time[ok&time>=T_switch], data_2$y[ok&time>=T_switch], lwd=.5, col="blue")
    for (i in 1:N_per_person) {
      ok2 <- ok & index==i
    }
  }
}
dev.off()

## 6. Fit models using rstanarm

fit_1 <- stan_glmer(y ~ (1 + time | person_id) + time + exposure, data=data_1)
library(lme4)
fit_1 <- glmer(y ~ (1 + time | person_id) + time + exposure, data=data_1)
print(fit_1)
fit_2 <- stan_glmer(y ~ (1 + time | person_id) + time + exposure, data=data_2)
