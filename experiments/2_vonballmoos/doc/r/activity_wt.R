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

