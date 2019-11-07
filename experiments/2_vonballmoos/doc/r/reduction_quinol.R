library(ggplot2)
library(ggrepel)
library(dplyr)
library(scales)
library(tidyr)
library(viridis)

df_wide = read.csv("../data/raw/reduction_quinol.csv")
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
write.csv(absorption, '../data/reduction_quinol.csv', row.names = FALSE)

ggplot(df_full, aes(x = time, y = absorption, color = series)) +
  geom_point(size = 0.1) +
  geom_line() +
  
  geom_vline(xintercept = 1, linetype = 'dotted', color = 'red') +
  annotate("text", x = 0.9, y = 0.3, label = "Addition of quinol", size = 3, color = 'red', hjust = 1) +
  geom_vline(xintercept = 3, linetype = 'dotted', color = 'black') +
  annotate("text", x = 2.9, y = 0.3, label = "Reduction with DTT", size = 3, color = 'black', hjust = 1) +
  
  facet_wrap(vars(protein), ncol = 1) +
  coord_cartesian(ylim=c(0, 0.4)) +
  labs(title = "Reduction of HS under quinol addition", x = "Time [min]", y = "Absorption at 428nm") +
  theme_minimal() +
  theme(
    legend.text = element_text(size = rel(1.2)),
    legend.title = element_text(size = rel(1.2))
  ) +
  ggsave('../img/reduction_quinol.png', width = 20, units = 'cm', dpi = 'print')

df_reduction = read.csv('../data/reduction_quinol.csv')
ggplot(df_reduction, aes(x = protein, y = relative_reduction, fill = state)) +
  geom_col(position = position_dodge()) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Relative reduction of HS under quinol addition", x = "Sample", y = "Reduction [%]") +
  theme_minimal() +
  theme(
    legend.text = element_text(size = rel(1.2)),
    legend.title = element_text(size = rel(1.2)),
    axis.text.x = element_text(size = rel(1.3))
  ) +
  ggsave('../img/reduction_quinol_relative.png', width = 20, units = 'cm', dpi = 'print')

