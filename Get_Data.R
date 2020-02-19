library(dplyr)
library(ggplot2)
library(readxl)
library(tidyverse)

#setwd('Desktop/NYCDataScience/3. Data Analysis R/Project/My_Project/Shiny_Project')
getwd()

County_Health_Data <- read_excel("Data/County Health Data.xlsx")
#View(County_Health_Data)

df <- County_Health_Data
#head(df)

colnames(df)[1] = 'Group' #renmae first column

colnames(df)[2] = 'Indicator'
colnames(df)[3] = 'Descriptor'
colnames(df)[4] = 'Source'
colnames(df)[5] = 'Link'

colnames(df)[8:length(colnames(df))] = df[1,8:length(colnames(df)) ] # the first row will be the header
df = df[-1, ] #remove row 1 now
df = df[,-6] #remove column 6 (no info)
df = df[,-length(df)]

#clean up cols before transpose
df1 = df %>%
  select(-Group, -Descriptor, -Source, -Link, -`Data year`)

head(df1)

long <- gather(df1,County,Value,-Indicator)
#long <- melt(df, id.vars=c('Indicator'))
head(long,10)

long1 <- long %>%
  filter(!is.na(Indicator),              #remove na from Indicator col
         Indicator!='Racial Disparity in Infant Mortality Rate', #remove rows with redundancy (from source)
         Indicator!='Racial Disparity in Life Expectancy')

wide <- spread(long1, key=Indicator, value=Value)
#wide <- reshape(long, idvar='variable', timevar='Indicator', direction='wide')
head(wide,10)
view(wide)

#split NC from counties

df_NC <- wide %>%
  filter(County=='NC')

df_Counties <- wide %>%
  filter(County!='NC')

#Factor Tier column 
df_Counties$Tier_Level = 
  factor(df_Counties$Tier, 
         levels = c('1.0','2.0','3.0'),
         labels = c("Tier 1","Tier 2","Tier 3"))

write.csv(df_Counties,'Data/Counties.csv')

names(df_Counties)







