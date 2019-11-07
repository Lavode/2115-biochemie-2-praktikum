library(ggplot2)
library(ggrepel)
library(ggpubr)
library(dplyr)
library(scales)
library(tidyr)
library(viridis)

mut.df_slopes_long = read.csv('../data/raw/activity_mut_quinone_slope.csv')
mut.df_slopes_long$type = ifelse(mut.df_slopes_long$start == 0.7, 'XO', 'HS')
mut.df_slopes_wide = spread(mut.df_slopes_long %>% select(sample, slope, type, concentration), type, slope)
mut.sod_slope = (mut.df_slopes_wide %>% filter(sample == 'SOD'))$HS
mut.avg_xo_slope = (mut.df_slopes_wide %>% filter(sample != 'SOD') %>% summarize(avg=mean(XO)))$avg
# We want to know how much the reduction of the dye was quenched, as this corresponds with the enzymatic activity.
mut.df_slopes_wide$relative_reduction = 1 - mut.df_slopes_wide$HS / (mut.avg_xo_slope - mut.sod_slope)

mut.df_slopes = mut.df_slopes_wide %>% filter(sample != 'SOD')
mut.df_slopes$concentration_inverse = 1 / mut.df_slopes$concentration
mut.df_slopes$relative_reduction_inverse = 1 / mut.df_slopes$relative_reduction
mut.df_slopes$relative_reduction_vs_conc = mut.df_slopes$relative_reduction / mut.df_slopes$concentration


wt.df_slopes_long = read.csv('../data/raw/activity_wt_quinone_slope.csv')
wt.df_slopes_long$type = ifelse(wt.df_slopes_long$start == 0.65, 'XO', 'HS')
wt.df_slopes_wide = spread(wt.df_slopes_long %>% select(sample, slope, type, concentration), type, slope)
wt.sod_slope = (wt.df_slopes_wide %>% filter(sample == 'SOD'))$HS
wt.avg_xo_slope = (wt.df_slopes_wide %>% filter(sample != 'SOD') %>% summarize(avg=mean(XO)))$avg
# We want to know how much the reduction of the dye was quenched, as this corresponds with the enzymatic activity.
wt.df_slopes_wide$relative_reduction = 1 - wt.df_slopes_wide$HS / (wt.avg_xo_slope - wt.sod_slope)
wt.df_slopes$relative_reduction_vs_conc = wt.df_slopes$relative_reduction / wt.df_slopes$concentration

wt.df_slopes = wt.df_slopes_wide %>% filter(sample != 'SOD')
wt.df_slopes$concentration_inverse = 1 / wt.df_slopes$concentration
wt.df_slopes$relative_reduction_inverse = 1 / wt.df_slopes$relative_reduction


wt.df_slopes$protein = 'Wildtype'
mut.df_slopes$protein = 'Mutant'

df_slopes = rbind(wt.df_slopes, mut.df_slopes)
df_slopes$protein = factor(df_slopes$protein)

ggplot(df_slopes, aes(x = concentration, y = relative_reduction, color = protein)) +
  geom_point() +
  geom_line() +
  # scale_x_log10() +
  #coord_cartesian(ylim=c(0, 0.4)) +
  labs(title = "Determination of HS KM", x = "Quinone concentration [μM]", y = "Relative superoxide oxidation [%]") +
  theme_minimal() +
  theme(
    legend.text = element_text(size = rel(1.2)),
    legend.title = element_text(size = rel(1.2))
  ) +
  ggsave('../img/activity_km.png', width = 20, units = 'cm', dpi = 'print')

ggplot(df_slopes, aes(x = concentration_inverse, y = relative_reduction_inverse, color = protein)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~"))
  ) +
  facet_wrap(vars(protein), scales = 'free', ncol = 1) +
  labs(title = "Determination of HS KM", x = "1 / Quinone concentration [1 / μM]", y = "1 / Relative superoxide oxidation [1 / %]") +
  theme_minimal() +
  theme(
    legend.text = element_text(size = rel(1.2)),
    legend.title = element_text(size = rel(1.2))
  ) +
  ggsave('../img/activity_km_lb.png', width = 20, units = 'cm', dpi = 'print')

ggplot(df_slopes, aes(x = relative_reduction, y = relative_reduction_vs_conc, color = protein)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~"))
  ) +
  facet_wrap(vars(protein), scales = 'free', ncol = 1) +
  labs(title = "Determination of HS KM", x = "", y = "") +
  theme_minimal() +
  theme(
    legend.text = element_text(size = rel(1.2)),
    legend.title = element_text(size = rel(1.2))
  ) +
  ggsave('../img/activity_km_lb_alternative.png', width = 20, units = 'cm', dpi = 'print')

