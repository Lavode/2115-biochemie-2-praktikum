library(ggplot2)
library(ggrepel)
library(dplyr)
library(scales)
library(tidyr)
library(viridis)

extinction_factor = 46.36
dilution_factor = 750 / 2

df_wide = read.csv("../data/raw/hs_concentration.csv")
df_full = gather(df_wide, series, absorption, hs_wt_ox:hs_dsred_red)

df_full$protein[grepl('_wt_', df_full$series, fixed=TRUE)] = 'wildtype'
df_full$protein[grepl('_mut_', df_full$series, fixed=TRUE)] = 'mutant'
df_full$protein[grepl('_dsred_', df_full$series, fixed=TRUE)] = 'dsred'

df_full$state[grepl('_ox', df_full$series, fixed=TRUE)] = 'oxidated'
# Careful not to match '_dsred', hence the EOL matcher
df_full$state[grepl('_red$', df_full$series)] = 'reduced'

df_full$state = factor(df_full$state)
df_full$protein = factor(df_full$protein)

baseline_correction = df_full %>% 
  filter(between(wavelength, 580, 600)) %>% 
  group_by(protein, state) %>% 
  summarize(n(), mean_absorption = mean(absorption)) %>%
  group_by(protein) %>% 
  summarize(baseline_correction = round(max(mean_absorption) - min(mean_absorption), 3))
baseline_correction

concentrations = df_full %>%
  filter(between(wavelength, 561, 561.1)) %>%
  group_by(protein) %>%
  summarize(absorption_delta = round(max(absorption) - min(absorption), 3)) %>%
  left_join(baseline_correction, by="protein") %>%
  mutate(absorption_delta_corrected = round(absorption_delta + baseline_correction, 3)) %>%
  mutate(concentration = round(absorption_delta_corrected / extinction_factor * dilution_factor, 3))
concentrations
write.csv(concentrations, '../data/hs_concentration.csv', row.names = FALSE, quote=FALSE)

ggplot(df_full, aes(x = wavelength, y = absorption, color = state)) +
  geom_point(size = 0.1) +
  geom_line() +
  facet_wrap(vars(protein), ncol = 1) +
  labs(title = "HS concentration determination", x = "Wavelength [nm]", y = "Absorption") +
  theme_minimal() +
  ggsave('../img/hs_concentration.png', width = 20, units = 'cm', dpi = 'print')

