library(ggplot2)
library(ggrepel)
library(dplyr)
library(scales)
library(tidyr)
library(viridis)

df_wide = read.csv("../data/raw/dsred_cleavage_fluorescence.csv")
df_full = gather(df_wide, series, intensity, dsred_5ul:lipo_uncleaved_275ul_cu_edta)
df_full$series = factor(df_full$series)

df_full$protein[grepl('dsred_', df_full$series, fixed=TRUE)] = 'Solubilized uncleaved'
df_full$protein[grepl('lipo_cleaved', df_full$series, fixed=TRUE)] = 'Liposomes uncleaved'
df_full$protein[grepl('lipo_uncleaved', df_full$series, fixed=TRUE)] = 'Liposomes cleaved'
df_full$protein = factor(df_full$protein)

df_full$copper = ifelse(grepl('_cu', df_full$series, fixed=TRUE), TRUE, FALSE)
df_full$edta = ifelse(grepl('_edta', df_full$series, fixed=TRUE), TRUE, FALSE)
df_full$copper = factor(df_full$copper)
df_full$edta = factor(df_full$edta)

df_full$volume[grepl('_5ul', df_full$series, fixed=TRUE)] = '5 μl'
df_full$volume[grepl('_100ul', df_full$series, fixed=TRUE)] = '100 μl'
df_full$volume[grepl('_275ul', df_full$series, fixed=TRUE)] = '275 μl'
df_full$volume = factor(df_full$volume)

# Custom facet labels
copper.labs <- c("+Copper", "")
names(copper.labs) <- c(TRUE, FALSE)
edta.labs <- c("+EDTA", "")
names(edta.labs) <- c(TRUE, FALSE)

ggplot(df_full, aes(x = wavelength, y = intensity, color = volume)) +
  geom_point(size = 0.1) +
  geom_line() +
  facet_wrap(
    vars(protein, copper, edta),
    ncol = 3,
    labeller = labeller(copper = copper.labs, edta = edta.labs)
  ) +
  coord_cartesian(xlim=c(570, 700), ylim=c(0, 100)) +
  labs(title = "Fluorescence & quenching of Dsred at 555nm", subtitle = "Liposomes & solubilized", x = "Wavelength [nm]", y = "Intensity") +
  theme_minimal() +
  ggsave('../img/dsred_cleavage_fluorescence.png', width = 20, units = 'cm', dpi = 'print')
