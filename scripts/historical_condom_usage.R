# condom usage between 1998 and 2003
df %>% filter(
  scenario == "baseline",
  indicator == "FSWCondomUsage",
  year >= 1990, 
  year <= 2010) %>% 
  group_by(year, intervention_year, test_reduction) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  mutate(intervention_year = as.factor(intervention_year)) %>% 
  ggplot(aes(year, mean, group = intervention_year)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha = 0.10, show.legend = F) +
  geom_line(aes()) +
  xlab("Years") +
  ylab("FSW Condom Usage (%)") +
  theme_bw() + theme(text = element_text(size = 12))
  
condom_use_94_to_2004 <- df %>% filter(
  scenario == "baseline",
  indicator == "CondomUsage",
  year >= 1992, 
  year <= 2010) %>% 
  group_by(year, scenario) %>% 
  summarise(mean = mean(value)) 


x <- condom_use_94_to_2004$year
y <- condom_use_94_to_2004$mean
lm(y~x)
