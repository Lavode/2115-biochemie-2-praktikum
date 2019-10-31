library(ggplot2)
library(ggrepel)
library(dplyr)
library(scales)
library(tidyr)
library(viridis)

df = read.csv("../data/raw/purification_fluorescence.csv")
names(df$stage) = df$stage_order

# 15 ul, sample volumes in file in ml
measurement_volume = 15 / 1000
df$fluorescence_sample = df$fluorescence / measurement_volume * df$sample_volume

ggplot(df, aes(x = reorder(sample, measurement), y = fluorescence_sample, fill = stage)) +
  geom_col() +
  labs(title = "Fluorescence measurements during purification", x = "Sample", y = "Flourescence (586nm emission, 555nm extinction") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90)) +
  ggsave('../img/purification_fluorescence.png', width = 20, units = 'cm', dpi = 'print')
