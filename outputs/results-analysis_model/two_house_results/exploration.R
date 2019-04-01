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


y_hor %>%
  mutate(chamber="hor") %>% 
  #bind_rows(y_senate %>% mutate(chamber = "senate")) %>% 
  mutate(topic = topics[topic]) %>% 
  mutate(gamma_out = gamma>upper) %>% 
  filter(gamma_out==TRUE) %>% 
  filter(topic %in% c(1,2,7,13,15,16,18,19)) %>% 
  mutate(mult = gamma/median) %>% 
  filter(mult>15) %>% 
  filter(gamma>0.5, median>0.03) %>% 
  arrange(dt, topic, mult) %>% 
  select(sitting, dt, topic, mult) %>% 
  write_csv("outputs/results-analysis_model/two_house_results/outliers.csv")




alpha_hor %>% 
  mutate(chamber = "hor") %>% 
  bind_rows(alpha_senate %>% mutate(chamber = "senate")) %>% 
  left_join(sitting_dates) %>% 
  filter(topic == 15, chamber=="hor") %>% 
  ggplot(aes(dt, median)) + 
  geom_line(aes(color = chamber)) + 
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = chamber), alpha = 0.2) + 
  facet_wrap(~topic)

y_hor %>% 
  filter(topic==15) %>% 
  ggplot(aes(dt, gamma)) + 
  geom_point(alpha = 0.7, aes(color = "data")) + 
  geom_line(aes(dt, median, color = "fitted")) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  # scale_color_manual(name = "", values = c("data" = "black", "fitted" = "red")) +
  scale_color_viridis_d() +
  theme_minimal() + 
  labs(x = "Date",
       y = "Gamma",
       color = "Type")
  # xlab("date") + ylab("proportion") 
ggsave("outputs/results-analysis_model/two_house_results/defense_topic_fitted_data.pdf", width = 8, height = 5)
  