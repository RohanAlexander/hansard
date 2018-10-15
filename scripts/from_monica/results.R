### goals

# 1. see which mu.govs are significantly different from the last
# 2. see which mu.elections are dignificantly different
# 3. find outliers within each period
# 4. look at varinace over time

#load("mcmc.array_40.Rda")

# 1. different governments ------------------------------------------------


mu_res_2 <- read_csv("results/mu_gov.csv")
mu_res_2 <- mu_res_2 %>% arrange(topic, government)

#install.packages("DescTools")
library(DescTools)

mu_gov_sig <- c()

for(i in 2:nrow(mu_res_2)){
  sig <- !(c(mu_res_2$lower[i-1], mu_res_2$upper[i-1]) %overlaps% c(mu_res_2$lower[i], mu_res_2$upper[i]))
  mu_gov_sig <- rbind(mu_gov_sig, tibble(government = mu_res_2$government[i], topic = mu_res_2$topic[i], sig))
}


num_sig <- mu_gov_sig %>% spread(topic, sig) %>% 
  select(-government) %>% rowSums()

mu_gov_sig_all <- tibble(government = 1:ngovernments, sig = num_sig>0)

# get original numbering 



mu_gov_sig_all <- mu_gov_sig_all %>% cbind(original_number =
  dr %>% group_by(governmentChangeDate) %>%
    summarise(one_obs = n()==20) %>%
    filter(one_obs ==FALSE) %>%
    select(governmentChangeDate) %>% pull()
)

# let's see who was different

mu_gov_sig_all %>% filter(sig==TRUE)

# fisher, bruce, page, menzies1, menzies2,  holt, whitlam, fraser, hawke, keating, howard, rudd1, gillard

# 1. different elections ------------------------------------------------


mu_res <- read_csv("results/mu_election.csv")
mu_res <- mu_res %>% arrange(topic, election)

mu_gov_sig <- c()

for(i in 2:nrow(mu_res)){
  sig <- !(c(mu_res$lower[i-1], mu_res$upper[i-1]) %overlaps% c(mu_res$lower[i], mu_res$upper[i]))
  mu_gov_sig <- rbind(mu_gov_sig, tibble(election = mu_res$election[i], topic = mu_res$topic[i], sig))
}


num_sig <- mu_gov_sig %>% spread(topic, sig) %>% 
  select(-election) %>% rowSums()

mu_election_sig_all <- tibble(election = 1:nelections, sig = num_sig>0)


# let's see who was different

mu_election_sig_all %>% filter(sig==TRUE)

# 1949 (menzies 2); 1972 (whitlam); 1983 (hawke); 1996 (howard); 2001 (howard); 2004; 2007 (rudd); 2010 (gillard); 2013 (abbot)


# 3. outliers --------------------------------------------------------------

alpha_res <- read_csv("results/sitting_proportions.csv")
alpha_res_2 <- read_csv("results/sitting_proportions_no_delta.csv") 
sigma_res <- read_csv("results/sigma.csv") 

sig_sittings <- alpha_res %>% 
  arrange(topic, gov, sitting) %>% 
  group_by(topic, gov) %>% 
  mutate(min = min(median), max = max(median)) %>% 
  filter(median==min|median==max) %>% 
  arrange(topic, gov) %>% 
  mutate(sig = lower[median==max]>upper[median==min]) %>% 
  filter(sig==TRUE) %>% 
  ungroup() %>% 
  select(sitting) %>% 
  unique()

sig_sittings <- sig_sittings %>% mutate(sig = TRUE)

dr_long %>% filter(topic ==1) %>% 
  select(document, group) %>% 
  rename(sitting = group) %>% 
  left_join(sig_sittings) %>% 
  filter(sig==TRUE) %>% 
  filter(sitting!=1, dplyr::row_number()==1) %>% 
  ungroup() %>% 
  select(document) %>% 
  filter(year(document)>1948) %>% 
  pull()


# the above process gives too many outliers. try: over 3 sd from mean

alpha_res_2 <- alpha_res_2 %>% left_join(dr_long %>% select(group, electionCounter) %>% rename(sitting = group))
  
alpha_res_2 <- alpha_res_2 %>% 
  rename(election = electionCounter, mean_median = median, mean_lower = lower, mean_upper = upper) %>% 
  left_join(sigma_res %>% select(election, topic, sigma_median, two_sd)) %>% 
  left_join(alpha_res %>% select(-gov)) %>% 
  mutate(bound_upper = mean_median + two_sd, bound_lower = mean_median - two_sd) %>% 
  mutate(outlier = median>bound_upper|median<bound_lower)

dr_long %>% filter(topic ==1) %>% 
  select(document, group) %>% 
  rename(sitting = group) %>% 
  right_join(alpha_res_2 %>% 
              filter(outlier==TRUE) %>%  select(sitting) %>% unique()) %>% 
  group_by(sitting) %>% 
  filter(dplyr::row_number()==1|dplyr::row_number()==n(), sitting !=1) %>% 
  mutate(day = ifelse(row_number()==1, "first", "last")) %>% 
  spread(day, document) %>% View()

# need to work out how to deal with the fact that it's sittings, not days. 

musp_res %>% 
  left_join(dr_long %>% rename(sitting=group) %>% mutate(topic = as.numeric(topic))) %>% 
  mutate(sig = gamma>2*upper) %>% 
  #filter(year(document)==2001, month(document)==9, topic == 17) 
  filter(sig==TRUE, upper>0.235, lower>0.000001) %>%
  select(document) %>%
  pull()
