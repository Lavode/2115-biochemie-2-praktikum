library(ggplot2)
library(ggrepel)
library(dplyr)
library(scales)
library(tidyr)
library(viridis)

df_wide_hs = read.csv("../data/raw/activity_wt_hs_conc.csv")
df_full_hs = gather(df_wide_hs, series, absorption, ref_xo:hs_0.25ul)
df_full_hs$series = factor(df_full_hs$series)

ggplot(df_full_hs, aes(x = time, y = absorption, color = series)) +
  geom_point(size = 0.1) +
  geom_line() +
  #coord_cartesian(ylim=c(0, 0.4)) +
  labs(title = "Determination of ideal HS concentration", subtitle = "Wildtype", x = "Time [min]", y = "Absorption at 455nm") +
  theme_minimal() +
  ggsave('../img/activity_wt_hs.png', width = 20, units = 'cm', dpi = 'print')




df_wide_qn = read.csv("../data/raw/activity_wt_quinone.csv")
df_full_qn = gather(df_wide_qn, series, absorption, quinone_50uM:quinone_0.01uM)
df_full_qn$series = factor(df_full_qn$series)

ggplot(df_full_qn, aes(x = time, y = absorption, color = series)) +
  geom_point(size = 0.1) +
  geom_line() +
  #coord_cartesian(ylim=c(0, 0.4)) +
  labs(title = "Determination of Michaelis constant of HS", subtitle = "Wildtype", x = "Time [min]", y = "Absorption at 455nm") +
  theme_minimal() +
  ggsave('../img/activity_wt_quinone.png', width = 20, units = 'cm', dpi = 'print')

df_slopes_long = read.csv('../data/raw/activity_wt_quinone_slope.csv')
df_slopes_long$type = ifelse(df_slopes_long$start == 0.65, 'XO', 'HS')
df_slopes_wide = spread(df_slopes_long %>% select(sample, slope, type, concentration), type, slope)
sod_slope = (df_slopes_wide %>% filter(sample == 'SOD'))$HS
avg_xo_slope = (df_slopes_wide %>% filter(sample != 'SOD') %>% summarize(avg=mean(XO)))$avg
# We want to know how much the reduction of the dye was quenched, as this corresponds with the enzymatic activity.
df_slopes_wide$relative_reduction = 1 - df_slopes_wide$HS / (avg_xo_slope - sod_slope)

df_slopes = df_slopes_wide %>% filter(sample != 'SOD')
df_slopes$concentration_inverse = 1 / df_slopes$concentration
df_slopes$relative_reduction_inverse = 1 / df_slopes$relative_reduction

ggplot(df_slopes, aes(x = concentration, y = relative_reduction)) +
  geom_point() +
  geom_line() +
  #coord_cartesian(ylim=c(0, 0.4)) +
  labs(title = "Determination of HS KM", subtitle = 'Wildtype', x = "Concentration [uM]", y = "Relative superoxide reduction [%]") +
  theme_minimal() +
  ggsave('../img/activity_wt_km.png', width = 20, units = 'cm', dpi = 'print')

ggplot(df_slopes, aes(x = concentration_inverse, y = relative_reduction_inverse)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  stat_regline_equation(label.x = 75, label.y = 1.72) +
  labs(title = "Determination of HS KM", subtitle = 'Wildtype', x = "1 / concentration [1 / uM]", y = "1 / Relative superoxide reduction [1 / %]") +
  theme_minimal() +
  ggsave('../img/activity_wt_km_lb.png', width = 20, units = 'cm', dpi = 'print')


