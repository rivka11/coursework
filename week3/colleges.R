#set necessary library
library(tidyverse)
library(varhandle)
library(ggplot2)

# set plot theme
theme_set(theme_bw())

#read data from csv file
colleges <- read_csv('MERGED2013_PP.csv')
colleges <- read.delim('MERGED2013_PP.csv', sep=',', header=TRUE)

#extract necessary columns, and removing rows with null values
filtered_colleges <- 
  colleges %>%
  select(INSTNM, CONTROL, TUITFTE, DEBT_MDN) %>% 
  filter(DEBT_MDN != "PrivacySuppressed", DEBT_MDN != "NULL", TUITFTE != "NULL", CONTROL != 3)

#Changing the column names
colnames(filtered_colleges) <- c("College","Type","Cost","Debt")

#Change data type to appropriate type
filtered_colleges <- unfactor(filtered_colleges) 
filtered_colleges$Type <- as.factor(filtered_colleges$Type)

filtered_colleges <-
  filter(filtered_colleges, Cost <= 30000, Debt <= 30000)

#GGPlot with linear model
regplot <- filtered_colleges %>%
  ggplot(aes(x= Cost, y = Debt, color=Type)) + 
  geom_point() +
  #xlim(0,30000) + 
  #ylim(0,30000) +
  scale_color_discrete(name = "College Type", labels = c("Private", "Public")) +
  stat_smooth(method = "lm")

#plot(regplot)

#Statistical analysis using OLS
OLS = lm(Debt ~ Cost, data=filtered_colleges)
Pval = summary(OLS)$coefficients[2,4]
  
#save data for easy loading in the future
save(regplot, OLS, Pval, filtered_colleges, file='colleges.RData')
