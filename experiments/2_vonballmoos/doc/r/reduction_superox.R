library(ggplot2)
library(ggrepel)
library(dplyr)
library(scales)
library(tidyr)
library(viridis)

df_wide = read.csv("../data/raw/reduction_superox.csv")
df_full = gather(df_wide, series, absorption, wt_sol:neg_lipo)
df_full$series = factor(df_full$series)

df_full$protein[grepl('wt_', df_full$series, fixed=TRUE)] = 'wildtype'
df_full$protein[grepl('mut_', df_full$series, fixed=TRUE)] = 'mutant'
df_full$protein[grepl('neg_', df_full$series, fixed=TRUE)] = 'negative control'

df_full$state[grepl('_sol', df_full$series, fixed=TRUE)] = 'solubilized'
df_full$state[grepl('_lipo', df_full$series, fixed=TRUE)] = 'liposomes'

df_full$protein = factor(df_full$protein)
df_full$state = factor(df_full$state)

absorption_base = df_full %>% 
  filter(protein %in% c('mutant', 'wildtype') & between(time, 0.8, 0.9)) %>%
  group_by(protein, state) %>%
  summarize(n_base = n(), max_absorption_base = max(absorption), min_absorption_base = min(absorption), avg_absorption_base = mean(absorption))
absorption_base

absorption_quinone = df_full %>% 
  filter(protein %in% c('mutant', 'wildtype') & between(time, 2.5, 2.6)) %>%
  group_by(protein, state) %>%
  summarize(n_quinone = n(), max_absorption_quinone = max(absorption), min_absorption_quinone = min(absorption), avg_absorption_quinone = mean(absorption))
absorption_quinone

absorption_reduced = df_full %>% 
  filter(protein %in% c('mutant', 'wildtype') & between(time, 3.9, 4)) %>%
  group_by(protein, state) %>%
  summarize(n_quinone = n(), max_absorption_reduced = max(absorption), min_absorption_reduced = min(absorption), avg_absorption_reduced = mean(absorption))
absorption_reduced

absorption = absorption_base %>%
  left_join(absorption_quinone, by = c('protein', 'state')) %>%
  left_join(absorption_reduced, by = c('protein', 'state')) %>%
  select(protein, state, avg_absorption_base, avg_absorption_quinone, avg_absorption_reduced) %>%
  mutate(relative_reduction = (avg_absorption_quinone - avg_absorption_base) / (avg_absorption_reduced - avg_absorption_base))
absorption

ggplot(df_full, aes(x = time, y = absorption, color = series)) +
  geom_point(size = 0.1) +
  geom_line() +
  facet_wrap(vars(protein, state), ncol = 2) +
  ylim(NA, 0.4) +
  labs(title = "", x = "", y = "") +
  theme_minimal() +
  ggsave('../img/reduction_superox.png', width = 20, units = 'cm', dpi = 'print')
