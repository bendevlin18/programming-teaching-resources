
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

#head is very bad, not super helpful in this scenario
head(df)


#a different function (corner) that i found in the 'useful' package is better for glancing at this type of data
corner(df)

#returns all of the column names and row names of the dataframe, helpful, but a bit overwhelming
colnames(df)
row.names(df)

df <- subset(df, select = -c(X, cell.name, experiment, protocol))

#since we might be interested in the types of cells, let's make sure all the names are correct/non-overlapping
unique(df$type)


#get a quick summary of the number of times this conditional is met
summary(df$type == 'Astrocyte')


df$type[df$type == 'Astrocyte'] = 'Astro'
df$type[df$type == 'neuron'] = 'Neuron'
df$type[df$type == 'Oligodendrocyte'] = 'Oligo'

length(unique(df$type))


#now that we are confident are data is barebones and clean, let's use the 'melt' function from the 
#reshape package to get the expression data into tidy format
df_tidy <- melt(df, value.name = 'expression', variable.name = 'gene')


#time to group our data, compute summary statistics on specific groups that constitute aesthetics we want to plot
df_grouped <- df_tidy %>%
  group_by(type, gene)%>%
  summarise(mean_expression = mean(expression)) 


#basic plot of the data we have prepared! this will take a long, long time - we are trying to plot a dot for
#every gene in the dataset
ggplot(df_grouped, aes(x = type, y = mean_expression))+
  geom_jitter()


ggplot(df_grouped, aes(x = type, y = mean_expression, color = type))+
  geom_jitter()

ggplot(df_grouped, aes(x = 1, y = mean_expression, color = type))+
  geom_jitter()


#okay we can't actually glean much info from that crowded scatter plot, let's narrow in on one gene
gene_of_interest = 'Cx3cr1'

goi <- filter(df_tidy, gene == gene_of_interest)
goi_grouped <- filter(df_grouped, gene == gene_of_interest)

ggplot(goi_grouped, aes(x = type, y = mean_expression, fill = type, color = 'black'))+
  geom_bar(stat = 'identity', aes(x = type, y = mean_expression, fill = type), color = 'black', size = 4)+
  geom_jitter(data = goi, aes(x = type, y = expression), size = 1.5, shape = 21, color = 'black', width = 0.2)+
  theme_classic()+
  ggtitle(paste(gene_of_interest, 'Expression'))+
  theme(
    plot.title = element_text(hjust = 0.5)
    
  )



