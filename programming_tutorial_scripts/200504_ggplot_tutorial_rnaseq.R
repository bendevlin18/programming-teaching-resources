## ggplot2 tutorial for plotting rnaseq data

## we will be using the Gocke 2016 striatum dataset once again


#start off the script by cleaning up R workstation
rm(list = ls())
#and by importing any necessary packages (if you don't have any of these, install them now)
library(dplyr)
library(reshape2)
library(useful)
library(ggplot2)
library(plotly)

#it can always be useful to set the working directory for this script at the beginning, so that you do 
#not need to give the full filepath everytime you are importing or saving data tables
setwd('H:\\DATA\\rna_seq_datasets\\adult_striatum')

#time to read in our data! this is just the raw csv expression matrix, with a few misnamed columns
df <- read.csv('gocke2016_taxonomy_mouse_striatum_GSE82187.csv')


### I advise AGAINST opening a preview of data this size, especially when there are a lot of variables (columns), R gets angry ###

#to avoid that, we can print out representative portions within the terminal

#head is very bad, not super helpful in this scenario
head(df)

#a different function (corner) that i found in the 'useful' package is better for glancing at this type of data
corner(df)


#returns all of the column names and row names of the dataframe, helpful, but a bit overwhelming
colnames(df)
row.names(df)

#based on what we saw above, we probably don't care much about a few of the variable columns
#let's get rid of them using the subset function
df <- subset(df, select = -c(X, cell.name, experiment, protocol))
corner(df)


#since we might be interested in the types of cells, let's make sure all the names are correct/non-overlapping
unique(df$type)

#get a quick summary of the number of times this conditional is met
summary(df$type == 'Oligodendrocyte')

#we can directly change the values in the 'type' column by indexing to that location and reassigning
df$type[df$type == 'Astrocyte'] = 'Astro'
df$type[df$type == 'neuron'] = 'Neuron'
df$type[df$type == 'Oligodendrocyte'] = 'Oligo'
unique(df$type)
length(unique(df$type))


#now that we are confident are data is barebones and clean, let's use the 'melt' function from the 
#reshape package to get the expression data into tidy format
df_tidy <- melt(df, value.name = 'expression', variable.name = 'gene')

#now that we have tidy data, we can open up the preview and feel less worried about it crashing R :P
View(df_tidy)


#time to group our data, compute summary statistics on specific groups that constitute aesthetics we want to plot
df_grouped <- df_tidy %>%
  group_by(type, gene) %>%
  summarise(mean_expression = mean(expression))

#basic plot of the data we have prepared! this will take a long, long time - we are trying to plot a dot for
#every gene in the dataset
ggplot(df_grouped, aes(x = type, y = mean_expression))+
  geom_jitter()

#colors might help it look prettier...
ggplot(df_grouped, aes(x = type, y = mean_expression, color = type))+
  geom_jitter()

#what if we don't want to group along the x-axis by type? give the aes argument an arbitrary value!
ggplot(df_grouped, aes(x = 1, y = mean_expression, color = type))+
  geom_jitter()



#okay we can't actually glean much info from that crowded scatter plot, let's narrow in on one gene
gene_of_interest = 'Gapdh'

goi <- filter(df_tidy, gene == gene_of_interest)
goi_grouped <- filter(df_grouped, gene == gene_of_interest)


### okay now we have our data in a good format for plotting with ggplot!!

#let's start with a basic bar plot layered with all individidual values as dots
ggplot(goi_grouped, aes(x = type, y = mean_expression, fill = type, color = type)) +
  geom_bar(stat = 'identity', aes(x = type, y = mean_expression, fill = type, color = type))+
  geom_jitter(data = goi, aes(x = type, y = expression), size = 1.5, shape = 21, color = 'black', width = 0.2)+
  theme_classic()

#okay now line!!
ggplot(goi_grouped, aes(x = type, y = mean_expression, fill = type, color = type)) +
  geom_line(aes(x = type, y = mean_expression, fill = type, color = type))+
  geom_jitter(data = goi, aes(x = type, y = expression), size = 1.5, shape = 21, color = 'black', width = 0.2)+
  theme_classic()
#hmmm error

#when plotting a line graph, you need a group variable! which in our case is arbitrary cuz there will only be
#one line to group into
ggplot(goi_grouped, aes(x = type, y = mean_expression, group = 1)) +
  geom_line(size = 1.5, color = 'gray')+
  geom_jitter(data = goi, aes(x = type, y = expression, fill = type, color = type), size = 1.5, shape = 21, color = 'black', width = 0.2)+
  ggtitle(paste(gene, 'Expression'))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
  
  



#how to plot SEM error bars

#first we should create a function that allows us to calculate the standard error while in a dplyr pipe
sem <- function(x) sqrt(var(x)/length(x))

df_grouped <- df_tidy %>%
  group_by(type, gene) %>%
  summarise(sem = sem(expression), mean_expression = mean(expression))

goi <- filter(df_tidy, gene == gene_of_interest)
goi_grouped <- filter(df_grouped, gene == gene_of_interest)

ggplot(goi_grouped, aes(x = type, y = mean_expression, fill = type, color = type)) +
  geom_bar(stat = 'identity', aes(x = type, y = mean_expression, fill = type, color = type))+
  geom_jitter(data = goi, aes(x = type, y = expression), size = 1.5, shape = 21, color = 'black', width = 0.2)+
  geom_errorbar(aes(ymin = mean_expression - sem, ymax = mean_expression + sem), color = 'black', width = .1, size = 1.2)+
  theme_classic()
