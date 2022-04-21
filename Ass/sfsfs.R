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

#National longitudinal?Survey of Youth 1979-2012
#National Longitudinal Survey of Youth (1979 - 2012) is a longitudinal project that follows a sample of American youth born between 1957-64 on 
#various life aspects from 1979 to 2012. 
#The data set provided below is a subset of ?his database, focusing on variables of 4 main topics: 
#socioeconomic status, employment, education, and marriage. Some recommended statistical analysis techniques to be applied are multiple regression, 
#time series analysis, logistic regression, and ANOV?.

#Find an appropriate dataset. There must be at least 8 attributes (in the final, merged and tidied dataset) and 200 observations.  (4 marks)
df<-read.csv('./data/youth-dataset-long-form.csv')

colnames(df)
#Tidy and / or enhance your dataset by merging.?(4 marks)

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
un?que(df$FAMSIZE_)
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
unique(df$WHEN_IN_PO?ERTY)
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
leng?h(unique(df$COUNTRY_OF_BIRTH))
length(unique(df$INCOME_))
length(unique(df$INCOME_MAX))
length(unique(df$TNFI_))


##
length(unique(df$SAMPLE_RACE))
length(unique(df$C1DOB_Y))
length(unique(df$HAVING_HEALTHPLAN))
length(unique(df$FAMSIZE_))
length(unique(d?$POVSTATUS_))
length(unique(df$REGION_))
length(unique(df$MARSTAT_KEY_))
length(unique(df$WKSUEMP_PCY_))
length(unique(df$URBAN_RURAL_))
length(unique(df$NUMCH_))
length(unique(df$AGE_1STCHILD))
length(unique(df$EVER_IN_POVERTY))
length(unique(df$WHEN_IN_P?VERTY))

#columns have been chosen to merge into new tidied dataset
#
AmericanYouthLifeAspects<-df%>%
  select('ID', 'YEAR', 'YEAR_OF_BIRTH', 'SAMPLE_SEX', 
         'JOBSNUM_', 'COUNTRY_OF_BIRTH', 'TNFI_', 'INCOME_', 'INCOME_MAX')


#merging and creating ?ew file, writing to path and writing csv file.
#
pathname = './data/AmericanYouthLifeAspects.csv'
write_csv(AmericanYouthLifeAspects, pathname)

#checking set
#
str(AmericanYouthLifeAspects)

#tidying and merging complete
#


#Explore your data using chart? and code.  (4 marks)
#
#first different time trends were explored
#
#trend graph bar chart for average Income of American youth from 1979-2012 grouped by year of birth between 1957 and 1964 using meant which returns mean of values.
#
YearIncome = sqldf("s?lect YEAR_OF_BIRTH, avg(INCOME_) meant from AmericanYouthLifeAspects group by YEAR_OF_BIRTH")
ggplot(YearIncome, aes(x=YEAR_OF_BIRTH, y=meant, fill = meant))+geom_bar(stat='identity')

str(AmericanYouthLifeAspects)


dfall<-sqldf("select YEAR, YEAR_OF_BIRT?, INCOME_ from AmericanYouthLifeAspects")

dfall$INCOME_=as.factor(dfall$INCOME_)
str(dfall)

ggplot(dfall, aes(x=YEAR, y=YEAR_OF_BIRTH, fill=INCOME_) +
  
  geom_area() +theme_classic()

