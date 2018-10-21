### goals

# 1. see which mu.govs are significantly different from the last
# 2. see which mu.elections are dignificantly different
# 3. find outliers within each period
# 4. look at varinace over time

#load("mcmc.array_40_2.Rda")

# 1. different governments ------------------------------------------------


mu_res_2 <- read_csv("results/mu_gov.csv")
mu_res_2 <- mu_res_2 %>% arrange(topic, government)

#install.packages("DescTools")
library(DescTools)
library(MCMCpack)

mu_gov_sig <- c()

for(i in 2:nrow(mu_res_2)){
  sig <- !(c(mu_res_2$lower[i-1], mu_res_2$upper[i-1]) %overlaps% c(mu_res_2$lower[i], mu_res_2$upper[i]))
  mu_gov_sig <- rbind(mu_gov_sig, tibble(government = mu_res_2$government[i], topic = mu_res_2$topic[i], sig))
}


num_sig <- mu_gov_sig %>% spread(topic, sig) %>% 
  dplyr::select(-government) %>% rowSums()

mu_gov_sig_all <- tibble(government = 1:ngovernments, sig = num_sig>0)

# get original numbering 



mu_gov_sig_all <- mu_gov_sig_all %>% cbind(original_number =
  dr %>% group_by(governmentChangeDate) %>%
    summarise(one_obs = n()==20) %>%
    filter(one_obs ==FALSE) %>%
    dplyr::select(governmentChangeDate) %>% pull()
)

# let's see who was different

mu_gov_sig_all %>% filter(sig==TRUE) %>% dplyr::select(original_number) %>% pull()

# fisher, bruce, page, menzies1, menzies2,  holt, whitlam, fraser, hawke, keating, howard, rudd1, gillard

prop_gov <- c()

set.seed(1)
for(i in 1:ngovernments){
  test_alpha <- mu_res_2 %>% filter(government==i) %>% dplyr::select(median) %>% pull()
  props <- rdirichlet(1000, exp(test_alpha))
  mean_props <- apply(props, 2, mean)
  prop_gov <- rbind(prop_gov, tibble(government = i, topic = 1:40, prop = mean_props))
}


prop_gov %>% 
  left_join(mu_gov_sig_all) %>% 
  mutate(sig_alpha = ifelse(is.na(sig), 0, ifelse(sig==TRUE, 1, 0.6))) %>% 
#  filter(!(government %in% c(15, 19, 32))) %>% 
  ggplot(aes(original_number, prop, fill = topic, alpha = sig_alpha)) + 
  geom_bar(stat = "identity") + 
  scale_fill_viridis_c(name = "Topic") + 
  scale_alpha(guide = FALSE) + 
  theme_classic() + 
  ylab("Proportion") + xlab("Government") 
ggsave("../../outputs/figures/gov_stack.pdf", width = 10, height = 8, units = "in")

# 1. different elections ------------------------------------------------


mu_res <- read_csv("results/mu_election.csv")
mu_res <- mu_res %>% arrange(topic, election)

mu_gov_sig <- c()

for(i in 2:nrow(mu_res)){
  sig <- !(c(mu_res$lower[i-1], mu_res$upper[i-1]) %overlaps% c(mu_res$lower[i], mu_res$upper[i]))
  mu_gov_sig <- rbind(mu_gov_sig, tibble(election = mu_res$election[i], topic = mu_res$topic[i], sig))
}


num_sig <- mu_gov_sig %>% spread(topic, sig) %>% 
  dplyr::select(-election) %>% rowSums()

mu_election_sig_all <- tibble(election = 1:nelections, sig = num_sig>0)


# let's see who was different

mu_election_sig_all %>% filter(sig==TRUE) %>% dplyr::select(election) %>% pull()

# 1949 (menzies 2); 1972 (whitlam); 1983 (hawke); 1996 (howard); 2001 (howard); 2004; 2007 (rudd); 2010 (gillard); 2013 (abbot)


prop_election <- c()

set.seed(1)
for(i in 1:nelections){
  test_alpha <- mu_res %>% filter(election==i) %>% dplyr::select(median) %>% pull()
  props <- rdirichlet(1000, exp(test_alpha))
  mean_props <- apply(props, 2, mean)
  prop_election <- rbind(prop_election, tibble(election = i, topic = 1:40, prop = mean_props))
}


prop_election %>% 
  #  filter(!(government %in% c(15, 19, 32))) %>% 
  ggplot(aes(election, prop, fill = topic)) + 
  geom_bar(stat = "identity") + 
  scale_fill_viridis_c(name = "Topic") + 
  theme_classic() + ylab("Proportion") + xlab("Election") 
#ggsave("../../outputs/figures/gov_stack.pdf", width = 10, height = 8, units = "in")

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
  dplyr::select(sitting) %>% 
  unique()

sig_sittings <- sig_sittings %>% mutate(sig = TRUE)

dr_long %>% filter(topic ==1) %>% 
  dplyr::select(document, group) %>% 
  rename(sitting = group) %>% 
  left_join(sig_sittings) %>% 
  filter(sig==TRUE) %>% 
  filter(sitting!=1, dplyr::row_number()==1) %>% 
  ungroup() %>% 
  dplyr::select(document) %>% 
  filter(year(document)>1948) %>% 
  pull()


# the above process gives too many outliers. try: over 3 sd from mean

alpha_res_2 <- alpha_res_2 %>% left_join(dr_long %>% dplyr::select(group, electionCounter) %>% rename(sitting = group))
  
alpha_res_2 <- alpha_res_2 %>% 
  rename(election = electionCounter, mean_median = median, mean_lower = lower, mean_upper = upper) %>% 
  left_join(sigma_res %>% dplyr::select(election, topic, sigma_median, two_sd)) %>% 
  left_join(alpha_res %>% dplyr::select(-gov)) %>% 
  mutate(bound_upper = mean_median + two_sd, bound_lower = mean_median - two_sd) %>% 
  mutate(outlier = median>bound_upper|median<bound_lower)

dr_long %>% filter(topic ==1) %>% 
  dplyr::select(document, group) %>% 
  rename(sitting = group) %>% 
  right_join(alpha_res_2 %>% 
              filter(outlier==TRUE) %>%  dplyr::select(sitting) %>% unique()) %>% 
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
  dplyr::select(document) %>% 
  write_csv(path = "results/outliers.csv")

## WAR PLOT

musp_res %>% 
  left_join(dr_long %>% 
              filter(topic==1) %>% 
              group_by(group) %>% 
              filter(row_number()==1) %>% 
              rename(sitting = group) %>% 
              dplyr::select(sitting, document)) %>% 
  group_by(sitting) %>% 
  mutate(med_norm = median/sum(median)) %>% 
  filter(topic %in% c(12, 17, 22, 23)) %>% 
  ggplot(aes(document, med_norm, fill = factor(topic))) + 
  geom_bar(stat = "identity", width = 100) + 
  scale_fill_viridis_d(name = "Topic") + 
  theme_classic() + ylab("Proportion") + xlab("Date") 
ggsave("../../outputs/figures/war_stack.pdf", width = 10, height = 8, units = "in")


musp_res %>% 
  left_join(dr_long %>% 
              filter(topic==1) %>% 
              group_by(group) %>% 
              filter(row_number()==1) %>% 
              rename(sitting = group) %>% 
              dplyr::select(sitting, document)) %>% 
  group_by(sitting) %>% 
  mutate(med_norm = median/sum(median)) %>% 
  filter(topic==17) %>% 
  arrange(-median)


dr_long %>%  filter(topic==17, year(document)==1991) 


