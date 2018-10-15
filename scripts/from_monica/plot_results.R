# want to do some interesting graphs of both elections and governments

# maybe 23 and 39?

mu_res_2 <- mu_res_2 %>% 
  left_join(mu_gov_sig_all) %>% 
  rename(gov_int = government, governmentChangeDate = original_number) %>% 
  left_join(dr_long %>% mutate(topic = as.numeric(topic))) %>% 
  group_by(governmentChangeDate, topic) %>% 
  filter(row_number()==1) 

mu_res <- mu_res %>% 
  left_join(mu_election_sig_all) %>% 
  rename(electionCounter = election) %>% 
  left_join(dr_long %>% mutate(topic = as.numeric(topic))) %>% 
  group_by(electionCounter, topic) %>% 
  filter(row_number()==1) 


# why are there such big gaps?

mu_res_2 %>% 
  filter(topic%in%c(23,39), governmentChangeDate!=19, governmentChangeDate!=33) %>% 
  ggplot(aes(document, median)) + 
  geom_point(aes(color = factor(governmentChangeDate))) + geom_errorbar(aes(ymin = lower, ymax = upper, color = factor(governmentChangeDate))) + 
  facet_wrap(~topic) +
  geom_point(data= mu_res %>% filter(topic %in% c(23, 39)), aes(document, median)) #+ 
  #geom_errorbar(data= mu_res %>% filter(topic %in% c(23, 39)), aes(ymin = lower, ymax = upper))
  



# sigmas

sigma_res <- read_csv("results/sigma.csv")

sigma_res %>% 
  group_by(election) %>% 
  summarise(mean_sigma = median(sigma_median)) %>% 
  ggplot(aes(election, mean_sigma, color = factor(election))) + 
  #geom_errorbar(aes(ymin = lower, ymax = upper)) 
  ylim(c(0, 0.3))+
  geom_point() 
