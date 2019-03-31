library(tidyverse)

# read in results

mu_gov <- read_csv("outputs/results-analysis_model/two_house_results/mu_gov.csv")
mu_election <- read_csv("outputs/results-analysis_model/two_house_results/mu_election.csv")
alpha_hor <- read_csv("outputs/results-analysis_model/two_house_results/alpha_hor.csv")
alpha_senate <- read_csv("outputs/results-analysis_model/two_house_results/alpha_senate.csv")
delta_hor <- read_csv("outputs/results-analysis_model/two_house_results/delta_hor.csv")
delta_senate <- read_csv("outputs/results-analysis_model/two_house_results/delta_senate.csv")
y_hor <- read_csv("outputs/results-analysis_model/two_house_results/pred_hor.csv")
y_senate <- read_csv("outputs/results-analysis_model/two_house_results/pred_senate.csv")

topics <- c(1:10, 12:19)
# we can use the predicted y results to get the first day of each sitting period by chamber

sitting_dates <- y_hor %>% filter(day==1, topic==1) %>% 
  select(sitting, dt) %>% 
  mutate(chamber = "hor") %>% 
  bind_rows(y_senate %>% filter(day==1, topic==1) %>% 
              select(sitting, dt) %>% 
              mutate(chamber = "senate"))


alpha_hor %>% 
  mutate(chamber = "hor") %>% 
  bind_rows(alpha_senate %>% mutate(chamber = "senate")) %>% 
  left_join(sitting_dates) %>% 
  ggplot(aes(dt, median)) + 
  geom_line(aes(color = chamber)) + 
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = chamber), alpha = 0.2) + 
  facet_wrap(~topic)


delta_hor %>% 
  mutate(chamber = "hor") %>% 
  bind_rows(delta_senate %>% mutate(chamber = "senate")) %>% 
  left_join(sitting_dates) %>% 
  mutate(sig = upper>0&lower>0|upper<0&lower<0) %>% 
  mutate(topic = topics[topic]) %>% 
  select(-upper, -lower) %>% 
  group_by(sitting, topic) %>% 
  mutate(diff = median[chamber=="hor"] - median[chamber=="senate"], 
         both_sig = sig[chamber=="hor"]|sig[chamber=="senate"]) %>% 
  filter(chamber == "hor") %>% 
  ggplot(aes(dt, diff)) + 
  geom_line(alpha = 0.5, color = "red") + 
  #geom_ribbon(aes(ymin = lower, ymax = upper, fill = chamber), alpha = 0.2) + 
  facet_wrap(~topic) +
  theme_minimal() + 
  geom_smooth(span = 0.3, se = TRUE) + 
  ylab("difference between HoR and Senate") + xlab("date")
ggsave("outputs/results-analysis_model/two_house_results/diff_hor_senate.pdf", width = 9, height = 6)
