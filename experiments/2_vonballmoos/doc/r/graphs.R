library(ggplot2)
library(ggrepel)
library(dplyr)
library(scales)

df = read.csv("../data/bca_assay.csv")

ggplot(df, aes(x = concentration, y = absorption, color = type)) +
  geom_point() +
  stat_function(fun=function(x)0.81542*x + 0.02701, geom="line", aes(colour="0.81542 * x + 0.02701")) +
  labs(title = "OD562 measurement of BCA assay, sample and standard", x = "Concentration [g/l]", y = "OD562") +
  theme_minimal()

