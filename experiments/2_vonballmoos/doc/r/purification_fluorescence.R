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
  labs(title = "Fluorescence measurements during purification", subtitle = '586nm emission, 555nm extinction', x = "Sample", y = "Flourescence") +
  theme_minimal() + 
  theme(
    axis.text.y = element_text(size = rel(1.2)),
    axis.text.x = element_text(angle = 90, size = rel(1.2))
  ) +
  ggsave('../img/purification_fluorescence.png', width = 20, units = 'cm', dpi = 'print')
