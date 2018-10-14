library(tidyverse)
library(lubridate)
library(rjags)
library(R2jags)
setwd("~/from_monica/")
source("plottrace.R")

# read in and join data
d1 <- read_csv("dates_of_gov_and_election.csv")
d2 <- read_csv("gammas_model_60.csv")

d_old <- read_csv("gammas_model_prevalence_years_k_20.csv")
dates_index <- tibble(document = 1:length(unique(d_old$document)), date = unique(d_old$document))

d <- d2 %>% left_join(dates_index) %>% 
  left_join(d1 %>% rename(date = allDates))

# reduced dataset
dr <- d

# create sitting period variable 

dr_wide <- dr %>% 
  spread(topic, gamma)

dr_wide <- dr_wide %>% mutate(day_diff = date - lag(date))

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
  gather(topic, gamma, -document, -date, -group, -day_number, -day_diff, -electionCounter, -governmentChangeDate) %>% 
  arrange(document)


## try and remove some of the data
## length of sitting dats

dr_long %>% 
  filter(topic==1) %>% 
  group_by(group) %>% summarise( n = n()) %>% 
  summarise(max(n), min(n), mean(n))

# want to sample 50% of days within each sitting period
# avoid sitting periods that have less than say 3 days

ngroups <- length(unique(dr_long$group))
ntopics <- 60

dr_train <- c()
set.seed(123)
for(i in 1:ngroups){
  ndays <- dr_long %>% ungroup() %>% filter(group==i) %>% summarise(max(day_number)) %>% pull()
  if(ndays>3){
    no_days_to_sample <- floor(ndays/2)
    sampled_days <- sample(1:ndays, no_days_to_sample)
    dr_group_train <- dr_long %>% ungroup() %>% filter(group==i, day_number %in% sampled_days)
    dr_train <- bind_rows(dr_train, dr_group_train)
  }
  else{
    dr_group_train <- dr_long %>% ungroup() %>% filter(group==i)
    dr_train <- bind_rows(dr_train, dr_group_train)
  }
}

# need to redo day_number

dr_train <- dr_train  %>%  
  arrange(topic, group) %>% 
  group_by(group, topic) %>% 
  mutate(day_number_train = row_number())

# now get into jags format 

y.stp <- array(NA, c(max(dr_train$group), max(dr_train$day_number_train), ntopics))

for (i in 1:ntopics){
  y.stp[,,i] <- dr_train %>% 
    ungroup() %>% 
    filter(topic==i) %>% 
    dplyr::select(-topic, -day_diff, -document, -electionCounter, -governmentChangeDate, -day_number, -date) %>% 
    spread(day_number_train, gamma) %>% 
    dplyr::select(-group) %>% 
    as.matrix()
}

nsittings <- max(dr_train$group)
ndays.s <- dr_train %>% group_by(group) %>% summarise(days = max(day_number_train)) %>% dplyr::select(days) %>% pull()

election.s <- (dr_train %>% group_by(group) %>% 
                 summarise(election = governmentChangeDate[row_number()==1]) %>% 
                 dplyr::select(election) %>% pull()) 

nelections <- length(unique(dr_train$governmentChangeDate))
election.s <- sapply(1:length(election.s), function(i) which(unique(dr_train$governmentChangeDate) == election.s[i]))

# run the model

jags.data <- list(nsittings = nsittings,
                  nelections = nelections,
                  ndays.s = ndays.s, 
                  election.s = election.s,
                  ntopics = ntopics, 
                  y.stp = y.stp)
parnames <- c("alpha", "mu.sp", "mu.alpha", "sigma.s")

mod <- jags(data = jags.data, 
            #n.iter = 5000,
            parameters.to.save=parnames,
            model.file = "model_dirichlet_means.txt")

max(mod$BUGSoutput$summary[,"Rhat"])
mcmc.array <- mod$BUGSoutput$sims.array
save(mcmc.array, file = "mcmc.array.Rda")
PlotTrace("mu.alpha[23,27]", mcmc.array)

mu_res <- c()
for(i in 1:nelections){
  for(j in 1:ntopics)
    mu_res <- rbind(mu_res, tibble(election = i, topic = j, 
                                   median = (median(mcmc.array[,,paste0("mu.alpha[",i,",",j,"]")])),
                                   upper = (quantile(mcmc.array[,,paste0("mu.alpha[",i,",",j,"]")], 0.975)),
                                   lower = (quantile(mcmc.array[,,paste0("mu.alpha[",i,",",j,"]")], 0.025))
    ))
}
write_csv(mu_res, "./governments/mu_res.csv")

mu_res %>% 
  filter(topic %in% 21:40) %>% 
  ggplot(aes(election, median, color = factor(election))) + 
  geom_point() + geom_errorbar(aes(ymin = lower, ymax = upper)) + 
  facet_wrap(~topic)

alpha_res <- c()
for(i in 1:nsittings){
  for(j in 1:ntopics)
    alpha_res <- rbind(alpha_res, tibble(sitting = i, topic = j, election = election.s[i],
                                         median = (median(mcmc.array[,,paste0("alpha[",i,",",j,"]")])),
                                         upper = (quantile(mcmc.array[,,paste0("alpha[",i,",",j,"]")], 0.975)),
                                         lower = (quantile(mcmc.array[,,paste0("alpha[",i,",",j,"]")], 0.025))
    ))
}
write_csv(alpha_res, "./governments/alpha_res.csv")

alpha_res %>% 
  filter(topic %in% 41:60) %>% 
  ggplot(aes(sitting, median, color = factor(election))) + geom_line(lwd = 0.2) + geom_point(size = 0.1) + facet_wrap(~topic)



mod$BUGSoutput$summary[which(mod$BUGSoutput$summary[,"Rhat"]>5),]
