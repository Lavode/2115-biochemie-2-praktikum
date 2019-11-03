library(ggplot2)
library(ggrepel)
library(dplyr)
library(scales)

df = read.csv("../data/bca_assay.csv")

min_max_standard_abs = df %>% 
  filter(type == 'standard') %>% 
  summarize(min = min(absorption), max = max(absorption))

df %>% 
  filter(type == 'sample' & between(absorption, min_max_standard_abs$min, min_max_standard_abs$max))

ggplot(df, aes(x = concentration, y = absorption, color = type)) +
  geom_point() +
  stat_function(fun=function(x)0.81542*x + 0.02701, geom="line", aes(colour="0.81542 * x + 0.02701")) +
  labs(title = "OD562 measurement of BCA assay, sample and standard", x = "Concentration [g/l]", y = "OD562") +
  theme_minimal()

