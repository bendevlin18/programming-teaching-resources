

rm(list = ls())
setwd(choose.dir())

library(tidyverse)
library(ggpubr)
library(reshape2)

sem <- function(x) sqrt(var(x)/length(x))
df <- read.csv('index_output_for_analysis_200601.csv')

df$X0[is.na(df$X0)] <- 0

df_grouped <- df %>%
  group_by(group, time) %>%
  summarise(sem = sem(X0), X0 = mean(X0))


ggplot(df, aes(x = interaction(group, time), y = X0, color = group, fill = group))+
  geom_jitter()+
  geom_bar(data = df_grouped, stat = 'identity')+
  geom_errorbar(data = df_grouped, aes(ymin = X0 - sem, ymax = X0 + sem), color = 'black', width = 0.1)+
  theme_classic()
