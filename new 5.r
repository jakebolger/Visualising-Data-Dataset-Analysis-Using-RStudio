#install.packages("tidyverse")
#install.packages("reshape2")
#install.packages("plotly")
#install.packages("sqldf")
#install.packages("ggstream")

library(tidyverse)
library(reshape2)
library(plotly)
library(sqldf)
library(ggstream)

#National longitudinal Survey of Youth 1979-2012
#National Longitudinal Survey of Youth (1979 – 2012) is a longitudinal project that follows a sample of American youth born between 1957-64 on 
#various life aspects from 1979 to 2012. 
#The data set provided below is a subset of this database, focusing on variables of 4 main topics: 
#socioeconomic status, employment, education, and marriage. Some recommended statistical analysis techniques to be applied are multiple regression, 
#time series analysis, logistic regression, and ANOVA.

#Find an appropriate dataset. There must be at least 8 attributes (in the final, merged and tidied dataset) and 200 observations.  (4 marks)
df<-read.csv('./data/youth-dataset-long-form.csv')

colnames(df)
#Tidy and / or enhance your dataset by merging. (4 marks)

#youth Dataset, CSV
#

#tidying dataset
#
#finding unique values
#
unique(df$ID)
unique(df$YEAR)
unique(df$YEAR_OF_BIRTH)
unique(df$COUNTRY_OF_BIRTH)
unique(df$SAMPLE_RACE)
unique(df$SAMPLE_SEX)
unique(df$C1DOB_Y)
unique(df$HAVING_HEALTHPLAN)
unique(df$FAMSIZE_)
unique(df$TNFI_)
unique(df$POVSTATUS_)
unique(df$REGION_)
unique(df$MARSTAT_KEY_)
unique(df$WKSUEMP_PCY_)
unique(df$URBAN_RURAL_)
unique(df$JOBSNUM_)
unique(df$NUMCH_)
unique(df$AGE_1STCHILD)
unique(df$EVER_IN_POVERTY)
unique(df$WHEN_IN_POVERTY)
unique(df$INCOME_)
unique(df$INCOME_MAX)
unique(df$HOURS_WORKED_PER_WEEK_)

#getting lengths of attributes
#
length(unique(df$ID))
length(unique(df$YEAR))
length(unique(df$YEAR_OF_BIRTH))
length(unique(df$SAMPLE_SEX))
length(unique(df$JOBSNUM_))
length(unique(df$COUNTRY_OF_BIRTH))
length(unique(df$INCOME_))
length(unique(df$INCOME_MAX))
length(unique(df$TNFI_))


##
length(unique(df$SAMPLE_RACE))
length(unique(df$C1DOB_Y))
length(unique(df$HAVING_HEALTHPLAN))
length(unique(df$FAMSIZE_))
length(unique(df$POVSTATUS_))
length(unique(df$REGION_))
length(unique(df$MARSTAT_KEY_))
length(unique(df$WKSUEMP_PCY_))
length(unique(df$URBAN_RURAL_))
length(unique(df$NUMCH_))
length(unique(df$AGE_1STCHILD))
length(unique(df$EVER_IN_POVERTY))
length(unique(df$WHEN_IN_POVERTY))

#columns have been chosen to merge into new tidied dataset
#
AmericanYouthLifeAspects<-df%>%
  select('ID', 'YEAR', 'YEAR_OF_BIRTH', 'SAMPLE_SEX', 
         'JOBSNUM_', 'COUNTRY_OF_BIRTH', 'TNFI_', 'INCOME_', 'INCOME_MAX')


#merging and creating new file, writing to path and writing csv file.
#
pathname = './data/AmericanYouthLifeAspects.csv'
write_csv(AmericanYouthLifeAspects, pathname)

#checking set
#
str(AmericanYouthLifeAspects)

#tidying and merging complete
#


#Explore your data using charts and code.  (4 marks)
#
#first different time trends were explored
#
#trend graph bar chart for average Income of American youth from 1979-2012 grouped by year of birth between 1957 and 1964 using meant which returns mean of values.
#
YearIncome = sqldf("select YEAR_OF_BIRTH, avg(INCOME_) meant from AmericanYouthLifeAspects group by YEAR_OF_BIRTH")
ggplot(YearIncome, aes(x=YEAR_OF_BIRTH, y=meant, fill = meant))+geom_bar(stat='identity')

str(AmericanYouthLifeAspects)

#next the data was explored using line charts
#

#Selecting and displaying line charts
#
YearIncomeLine = sqldf("select YEAR_OF_BIRTH, avg(INCOME_) meant from AmericanYouthLifeAspects group by YEAR_OF_BIRTH")
ggplot(YearIncomeLine, aes(x=YEAR_OF_BIRTH, y=meant))+geom_line()

YearIncomeLine2 = sqldf("select YEAR_OF_BIRTH, avg(INCOME_MAX) meant from AmericanYouthLifeAspects group by YEAR_OF_BIRTH")
ggplot(YearIncomeLine2, aes(x=YEAR_OF_BIRTH, y=meant))+geom_line()




YearIncomeLine3 = sqldf("select YEAR_OF_BIRTH, JOBSNUM_, '5000' INCOME_ from AmericanYouthLifeAspects group by YEAR_OF_BIRTH")
ggplot(YearIncomeLine3, aes(x=YEAR_OF_BIRTH, y=JOBSNUM_))+geom_line()

YearIncomeLine4 = sqldf("select YEAR_OF_BIRTH, JOBSNUM_, '11000' INCOME_ from AmericanYouthLifeAspects group by YEAR_OF_BIRTH")
ggplot(YearIncomeLine3, aes(x=YEAR_OF_BIRTH, y=JOBSNUM_))+geom_line()

#combining Income line charts datasets
#
df=sqldf("select * from YearIncomeLine3 union select * from YearIncomeLine4")
head(df)
str(df)


ggplot(df, aes(x=YEAR_OF_BIRTH, y=JOBSNUM_)) +
  
  facet_grid(facets = INCOME_~.) +
  geom_line() +theme_classic()


##combining to make combined bar
#
ggplot(df, aes(x=YEAR_OF_BIRTH, y=JOBSNUM_, fill=INCOME_)) +
  
  geom_bar(stat='identity') +theme_classic()


#ROW wise small multiples
#
ggplot(data = df, aes(x = YEAR_OF_BIRTH, y = JOBSNUM_)) + 
  geom_bar(stat='identity') +
  facet_grid(facets = .~INCOME_) +
  theme_classic()


#next comparison charts were used to explore the data
#
#install.packages("ggplot2")
library(ggplot2)

str(AmericanYouthLifeAspects)

#first comparison chart was single variable exploration using a histogram
#
p <- ggplot(AmericanYouthLifeAspects, aes(x=YEAR_OF_BIRTH)) + 
  geom_histogram(bins=8,fill="Purple", color="Blue") +
  geom_freqpoly(binwidth=1,color="Green")
p

#then i used a box plot using single variable exploration
#
ggplot(AmericanYouthLifeAspects, aes(x=as.factor(JOBSNUM_), y=YEAR_OF_BIRTH, fill=JOBSNUM_)) + geom_boxplot()

#Density Plot for maximum income based on sex/gender
#
theme_set(theme_classic())

# Plot
g <- ggplot(AmericanYouthLifeAspects, aes(INCOME_MAX))
g + geom_density(aes(fill=factor(SAMPLE_SEX)), alpha=0.8) + 
  labs(title="Density plot", 
       subtitle="Maximum Income grouped by Gender",
       caption="Source: AmericanYouthLifeAspects",
       x="Maximum Income",
       fill="# Sex")

#next I did simple comparisons using a parts of a whole Treemap for simple comparison exploration
#
# load library
library(ggplot2)
#install.packages("treemap")
library(treemap)

gm1979=sqldf("select COUNTRY_OF_BIRTH, SAMPLE_SEX,
INCOME_/10 INCOME_ from AmericanYouthLifeAspects where YEAR = 1979")
gm1981=sqldf("select COUNTRY_OF_BIRTH, SAMPLE_SEX,
             INCOME_/10 INCOME_ from AmericanYouthLifeAspects where YEAR = 1981")

treemap(gm1979,
        index=c("COUNTRY_OF_BIRTH","SAMPLE_SEX"),
        vSize="INCOME_",
        type="index"
)
#potentially do more complex treemap
#

#Next I explored Multi Distribution comparisons using and overlaid histogram.
#

ggplot(AmericanYouthLifeAspects, aes(x = INCOME_, fill = SAMPLE_SEX)) +   
  # Draw overlaying histogram
  geom_histogram(position = "identity", alpha = 0.2, bins = 50)

##another overlaying plot
#
ggplot(AmericanYouthLifeAspects, aes(x = INCOME_, fill = COUNTRY_OF_BIRTH)) +   
  # Draw overlaying histogram
  geom_histogram(position = "identity", alpha = 0.2, bins = 50)
