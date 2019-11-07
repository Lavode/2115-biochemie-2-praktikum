library(ggplot2)
library(ggrepel)
library(dplyr)
library(scales)
library(tidyr)
library(viridis)

df_wide = read.csv("../data/expression_od600.csv")
df = gather(df_wide, time, od, X0:X290)
df$time = as.numeric(substring(df$time, 2, 10))
# Remove incorrect measurements
df = df %>% filter(time != 90)

# OD600 measurements
ggplot(df, aes(x = time, y = od, color = rha:iptg)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = 90, linetype = 'dotted') +
  annotate("text", x = 90, y = 1.5, label = "Addition of IPTG") +
  ylim(0, 1.6) +
  labs(title = "OD600 measurement of E. coli cells during expression with various conditions", x = "Time since inoculation [min]", y = "OD600") +
  facet_wrap(vars(medium), nrow = 3) +
  theme_minimal() +
  theme(
    legend.text = element_text(size = rel(1.3)),
    legend.title = element_text(size = rel(1.3))
  ) +
  ggsave('../img/absorption_expression.png')

df = read.csv("../data/expression_fluorescence.csv")

# Fluorescence measurements
ggplot(df, aes(x = Medium, y = Fluorescence.Normalized, fill = Rhamnose:IPTG)) +
  geom_col(position = position_dodge()) +
  labs(title = "Fluorescence of E. coli cells after expression with various conditions", x = "", y = "Normalized fluorescence") + 
  theme_minimal() +
  theme(
    legend.text = element_text(size = rel(1.3)),
    legend.title = element_text(size = rel(1.3))
  ) +
  ggsave('../img/expression_fluorescence.png')

