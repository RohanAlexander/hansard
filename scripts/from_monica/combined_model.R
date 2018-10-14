library(tidyverse)
library(lubridate)
library(rjags)
library(R2jags)
library(splines)
source("plottrace.R")
source("getsplines.R")

# read in and join data
d1 <- read_csv("dates_of_gov_and_election.csv")
d2 <- read_csv("gammas_model_40.csv")

#d_old <- read_csv("gammas_model_prevalence_years_k_20.csv")
#dates_index <- tibble(document = 1:length(unique(d_old$document)), date = unique(d_old$document))

d <- d2 %>% #left_join(dates_index) %>% 
  left_join(d1 %>% rename(document = allDates))


# reduced dataset
dr <- d #%>% filter(electionCounter>41)
one_gov <- dr %>% group_by(governmentChangeDate) %>% 
  summarise(one_obs = n()==20) %>% 
  filter(one_obs==TRUE) %>% 
  select(governmentChangeDate) %>% pull()

one_elec <- dr %>% group_by(electionCounter) %>% 
  summarise(one_obs = n()==20) %>% 
  filter(one_obs==TRUE) %>% 
  select(electionCounter) %>% pull()

dr <- dr %>% filter(!(governmentChangeDate %in% one_gov), !(governmentChangeDate %in% one_elec))

# create sitting period variable 

dr_wide <- dr %>% 
  spread(topic, gamma)

dr_wide <- dr_wide %>% mutate(day_diff = document - lag(document))

k <- 1
dr_wide$group <- NA
for(i in 1:nrow(dr_wide)){
  if(is.na(dr_wide$day_diff[i])|dr_wide$day_diff[i]<7){
    k <- k
  }
  else{
    k <- k+1
  }
  dr_wide$group[i] <- k
}

dr_wide <- dr_wide %>% group_by(group) %>% 
  mutate(day_number = row_number())

dr_long <- dr_wide %>% 
  gather(topic, gamma, -document, -group, -day_number, -day_diff, -electionCounter, -governmentChangeDate) %>% 
  arrange(document)

dr_train <- dr_long

# want something like days until election
#election_start_date <- dr_train %>%  group_by(electionCounter) %>% summarise(start = min(date)) %>% select(start) %>% pull()

dr_train <- dr_train %>% group_by(electionCounter) %>%  mutate(days_since_start = group - min(group))


ntopics <- 40

y.stp <- array(NA, c(max(dr_train$group), max(dr_train$day_number), ntopics))

for (i in 1:ntopics){
  y.stp[,,i] <- dr_train %>% 
    ungroup() %>% 
    filter(topic==i) %>% 
    dplyr::select(-topic, -day_diff, -document, -electionCounter, -governmentChangeDate, -days_since_start) %>% 
    spread(day_number, gamma) %>% 
    dplyr::select(-group) %>% 
    as.matrix()
}

nsittings <- max(dr_train$group)
ndays.s <- dr_train %>% group_by(group) %>% summarise(days = max(day_number)) %>% dplyr::select(days) %>% pull()

election.s <- (dr_train %>% group_by(group) %>% 
                 summarise(election = electionCounter[row_number()==1]) %>% 
                 dplyr::select(election) %>% pull()) 

nelections <- length(unique(dr_train$electionCounter))
election.s <- sapply(1:length(election.s), function(i) which(unique(dr_train$electionCounter) == election.s[i]))

gov.s <- (dr_train %>% group_by(group) %>% 
                 summarise(election = governmentChangeDate[row_number()==1]) %>% 
                 dplyr::select(election) %>% pull()) 

ngovernments <- length(unique(dr_train$governmentChangeDate))
gov.s <- sapply(1:length(gov.s), function(i) which(unique(dr_train$governmentChangeDate) == gov.s[i]))

days_since_start <- (dr_train %>% group_by(group) %>% 
            summarise(day = days_since_start[row_number()==1]) %>% 
            dplyr::select(day) %>% pull()) 

year.s <-  (dr_train %>% group_by(group) %>% 
              summarise(year = year(document[row_number()==1])) %>% 
              dplyr::select(year) %>% pull()) - min(year(dr_train$document)) + 1

#splines

sittings.g <- table(gov.s)

spls <- GetSplines(1:nsittings, I = 5)
B.sk <- spls$B.ik
K= length(spls$knots.k)

# run the model

jags.data <- list(nsittings = nsittings,
                  ngovernments = ngovernments,
                  nelections = nelections,
                  ndays.s = ndays.s, 
                  gov.s = gov.s,
                  election.s = election.s,
                  ntopics = ntopics, 
                  y.stp = y.stp,
                  B.sk = B.sk, K = K, 
                  days_since_start = days_since_start)

parnames <- c("alpha", "mu.sp", "c0", "rho","mu.gov", "mu.election", "tau")

mod <- jags.parallel(data = jags.data, 
            n.iter = 7500,
            parameters.to.save=parnames,
            model.file = "model_combined_means_splines.txt")


max(mod$BUGSoutput$summary[,"Rhat"])
mcmc.array <- mod$BUGSoutput$sims.array

mod$BUGSoutput$summary[which(mod$BUGSoutput$summary[,"Rhat"]>1.5),]

PlotTrace("tau[31,35]", mcmc.array)


mu_res <- c()
for(i in 1:nelections){
  for(j in 1:ntopics)
    mu_res <- rbind(mu_res, tibble(election = i, topic = j, 
                                   median = (median(mcmc.array[,,paste0("rho[",i,",",j,"]")])),
                                   upper = (quantile(mcmc.array[,,paste0("rho[",i,",",j,"]")], 0.95)),
                                   lower = (quantile(mcmc.array[,,paste0("rho[",i,",",j,"]")], 0.05))
    ))
}


mu_res_2 <- c()
for(i in 1:ngovernments){
  for(j in 1:ntopics)
    mu_res_2 <- rbind(mu_res_2, tibble(government = i, topic = j, 
                                   median = (median(mcmc.array[,,paste0("mu.gov[",i,",",j,"]")])),
                                   upper = (quantile(mcmc.array[,,paste0("mu.gov[",i,",",j,"]")], 0.95)),
                                   lower = (quantile(mcmc.array[,,paste0("mu.gov[",i,",",j,"]")], 0.05))
    ))
}

write_csv(mu_res_2, path = "results/mu_gov.csv")

mu_res <- c()
for(i in 1:nelections){
  for(j in 1:ntopics)
    mu_res <- rbind(mu_res, tibble(election = i, topic = j, 
                                   median = (median(mcmc.array[,,paste0("mu.election[",i,",",j,"]")])),
                                   upper = (quantile(mcmc.array[,,paste0("mu.election[",i,",",j,"]")], 0.95)),
                                   lower = (quantile(mcmc.array[,,paste0("mu.election[",i,",",j,"]")], 0.05))
    ))
}

write_csv(mu_res, path = "results/mu_election.csv")

mu_res_2 %>% 
  filter(topic%in%1:20, government!=19, government!=32) %>% 
  ggplot(aes(government, median, color = factor(government))) + 
  geom_point() + geom_errorbar(aes(ymin = lower, ymax = upper)) + 
  facet_wrap(~topic)

mu_res %>% 
  filter(topic%in%1:20) %>% 
  ggplot(aes(election, median, color = factor(election))) + 
  geom_point() + geom_errorbar(aes(ymin = lower, ymax = upper)) + 
  facet_wrap(~topic)


alpha_res <- c()
for(i in 1:nsittings){
  for(j in 1:ntopics)
    alpha_res <- rbind(alpha_res, tibble(sitting = i, topic = j, gov = gov.s[i],
                                         median = (median(mcmc.array[,,paste0("alpha[",i,",",j,"]")])),
                                         upper = (quantile(mcmc.array[,,paste0("alpha[",i,",",j,"]")], 0.95)),
                                         lower = (quantile(mcmc.array[,,paste0("alpha[",i,",",j,"]")], 0.05))
    ))
}

write_csv(alpha_res, path = "results/sitting_proportions.csv")

alpha_res %>% 
  ggplot(aes(sitting, median, fill = factor(gov))) + 
  geom_line(lwd = 0.2, aes(color = factor(gov))) + geom_point(size = 0.1, aes(color = factor(gov))) + facet_wrap(~topic) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2)

alpha_res_2 <- c()
for(i in 1:nsittings){
  for(j in 1:ntopics)
    alpha_res_2 <- rbind(alpha_res_2, tibble(sitting = i, topic = j, gov = gov.s[i],
                                         median = (median(mcmc.array[,,paste0("c0[",i,",",j,"]")])),
                                         upper = (quantile(mcmc.array[,,paste0("c0[",i,",",j,"]")], 0.95)),
                                         lower = (quantile(mcmc.array[,,paste0("c0[",i,",",j,"]")], 0.05))
    ))
}


write_csv(alpha_res_2, path = "results/sitting_proportions_no_delta.csv")

alpha_res_2 %>% 
  ggplot(aes(sitting, median, fill = factor(gov))) + 
  geom_line(lwd = 0.2, aes(color = factor(gov))) + geom_point(size = 0.1, aes(color = factor(gov))) + facet_wrap(~topic) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2)


sigma_res <- c()
for(i in 1:nelections){
  for(j in 1:ntopics)
    sigma_res <- rbind(sigma_res, tibble(election = i, topic = j, 
                                   median = (median(mcmc.array[,,paste0("tau[",i,",",j,"]")])),
                                   upper = (quantile(mcmc.array[,,paste0("tau[",i,",",j,"]")], 0.95)),
                                   lower = (quantile(mcmc.array[,,paste0("tau[",i,",",j,"]")], 0.05))
    ))
}


sigma_res <- sigma_res %>% mutate(sigma_median = sqrt(1/median), two_sd = sigma_median*2)

write_csv(sigma_res, path = "results/sigma.csv")
