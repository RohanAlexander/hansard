# trying to get results in useable form

library(tidyverse)
load("mcmc.array.Rda")


View(dimnames(mcmc.array))
#
ngovernments <- 32
ntopics <- 60

# pull out musi


mu_res <- c()
for(i in 1:ngovernments){
  for(j in 1:ntopics)
    mu_res <- rbind(mu_res, tibble(government = i, topic = j, 
                                   median = (median(mcmc.array[,,paste0("mu.alpha[",i,",",j,"]")])),
                                   upper = (quantile(mcmc.array[,,paste0("mu.alpha[",i,",",j,"]")], 0.95)),
                                   lower = (quantile(mcmc.array[,,paste0("mu.alpha[",i,",",j,"]")], 0.05))
    ))
}

mu_res <- mu_res %>% mutate(government = ifelse(government>18, government+1, government))
write_csv(mu_res, "./governments/mu_res.csv")


mu_res %>% 
  filter(topic %in% 1:20) %>% 
  ggplot(aes(government, median, color = factor(government))) + 
  geom_point() + geom_errorbar(aes(ymin = lower, ymax = upper)) + 
  facet_wrap(~topic)


alpha_res <- c()
for(i in 1:nsittings){
  for(j in 1:ntopics)
    alpha_res <- rbind(alpha_res, tibble(sitting = i, topic = j, government = election.s[i],
                                         median = (median(mcmc.array[,,paste0("alpha[",i,",",j,"]")])),
                                         upper = (quantile(mcmc.array[,,paste0("alpha[",i,",",j,"]")], 0.95)),
                                         lower = (quantile(mcmc.array[,,paste0("alpha[",i,",",j,"]")], 0.05))
    ))
}

alpha_res <- alpha_res %>% rename(government = election)
write_csv(alpha_res, "./governments/alpha_res.csv")

sigma_res <- c()
for(i in 1:ngovernments){
  sigma_res <- rbind(sigma_res, tibble(government = i,
                                         median = (median(mcmc.array[,,paste0("sigma.s[",i,"]")])),
                                         upper = (quantile(mcmc.array[,,paste0("sigma.s[",i,"]")], 0.95)),
                                         lower = (quantile(mcmc.array[,,paste0("sigma.s[",i,"]")], 0.05))
    ))
}

sigma_res <- sigma_res %>% mutate(government = ifelse(government>18, government+1, government))
write_csv(sigma_res, "./governments/sigma_res.csv")

sigma_res %>% 
  filter(median<0.3) %>% 
  ggplot(aes(government, median)) + geom_point() + geom_errorbar(aes(ymin = lower, ymax = upper))
