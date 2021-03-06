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

##dataset was made smaller so the graphs would work better as to? many observations made the graphs look abnormal. 1979-1999
#
dftidy <- sqldf("select * from AmericanYouthLifeAspects where ID <= '20' ")
str(dftidy)
#selecting years
#
df7999 <- sqldf("select * from dftidy where YEAR between 1979 and 1999")
#now has 310 o?servations
#
str(df7999)

##smaller for 2000-2012
#first * row making it smaller for better looking graph
#
dftidy2 <- sqldf("select * from AmericanYouthLifeAspects where ID <= '50' ")
str(dftidy2)
#selecting years 2000-2012
#
df2012 <- sqldf("select * fro? dftidy2 where YEAR between 2000 and 2012")
#now has 245 observations
#
str(df2012)
#tidying and merging complete
#


#Explore your data using charts and code.  (4 marks)
#
#first different time trends were explored
#
#trend graph bar charts for average In?ome of American youth from 1979-2012 grouped by year.
#79-99
#
YearIncome = sqldf("select YEAR, avg(INCOME_) Average_Income from df7999 group by YEAR")
ggplot(YearIncome, aes(x=YEAR, y=Average_Income, fill = Average_Income))+geom_bar(stat='identity')

#00-?2
#
YearIncome2 = sqldf("select YEAR, avg(INCOME_) Average_Income from df2012 group by YEAR")
ggplot(YearIncome2, aes(x=YEAR, y=Average_Income, fill = Average_Income))+geom_bar(stat='identity')
#
#as you can see there a big difference in the average income?in the time periods. there is an increase every year.
#str(AmericanYouthLifeAspects)

#next the data was explored using line charts
#

#Selecting and displaying line charts for the maxium income over the years 1979-2012
#
YearIncomeLine = sqldf("select YEA?, avg(INCOME_MAX) Maximum_Income from df7999 group by YEAR")
ggplot(YearIncomeLine, aes(x=YEAR, y=Maximum_Income))+geom_line()

YearIncomeLine2 = sqldf("select YEAR, avg(INCOME_MAX) Maximum_Income from df2012 group by YEAR")
ggplot(YearIncomeLine2, aes(x=Y?AR, y=Maximum_Income))+geom_line()




YearIncomeLine3 = sqldf("select YEAR, avg(INCOME_MAX) Maximum_Income, 'FEMALE' SAMPLE_SEX from df7999 group by YEAR")
ggplot(YearIncomeLine3, aes(x=YEAR, y=Maximum_Income))+geom_line()

YearIncomeLine4 = sqldf("select?YEAR, avg(INCOME_MAX) Maximum_Income, 'MALE' SAMPLE_SEX from df2012 group by YEAR")
ggplot(YearIncomeLine4, aes(x=YEAR, y=Maximum_Income))+geom_line()
#combining Income line charts datasets
#
df1=sqldf("select * from YearIncomeLine3 union select * from Yea?IncomeLine4")
head(df1)
str(df1)


ggplot(df1, aes(x=YEAR, y=Maximum_Income)) +
  
  facet_grid(facets = SAMPLE_SEX~.) +
  geom_line() +theme_classic()


##combining to make combined bar
#
ggplot(df1, aes(x=YEAR, y=Maximum_Income, fill=SAMPLE_SEX)) +
  
  ?eom_bar(stat='identity') +theme_classic()


#ROW wise small multiples
#
#ggplot(data = df1, aes(x = YEAR, y = Maximum_Income)) + 
  #geom_bar(stat='identity') +
  #facet_grid(facets = .~SAMPLE_SEX) +
  #theme_classic()


#next comparison charts were used t? explore the data
#
#install.packages("ggplot2")
library(ggplot2)

str(AmericanYouthLifeAspects)

#first comparison chart was single variable exploration using a histogram
#79-99
#
p <- ggplot(df7999, aes(x=YEAR_OF_BIRTH)) + 
  geom_histogram(bins=8,fill="?urple", color="Blue") +
  geom_freqpoly(binwidth=1,color="Green")
p
#2000-2012
#
p <- ggplot(df2012, aes(x=YEAR_OF_BIRTH)) + 
  geom_histogram(bins=8,fill="Purple", color="Blue") +
  geom_freqpoly(binwidth=1,color="Green")
p

#not included
#
#then i used a?box plot using single variable exploration
#
#ggplot(AmericanYouthLifeAspects, aes(x=as.factor(JOBSNUM_), y=YEAR_OF_BIRTH, fill=JOBSNUM_)) + geom_boxplot()
#


#next
#Density Plot for maximum income based on sex/gender
#
theme_set(theme_classic())

# Plot
? <- ggplot(AmericanYouthLifeAspects, aes(INCOME_MAX))
g + geom_density(aes(fill=factor(SAMPLE_SEX)), alpha=0.8) + 
  labs(title="Density plot", 
       subtitle="Maximum Income grouped by Gender",
       caption="Source: AmericanYouthLifeAspects",
       x?"Maximum Income",
       fill="# Sex")

#next I did simple comparisons using a parts of a whole Treemap for simple comparison exploration
#
# load library
library(ggplot2)
#install.packages("treemap")
library(treemap)

gm1979=sqldf("select COUNTRY_OF_BIRTH? SAMPLE_SEX,
INCOME_/10 INCOME_ from AmericanYouthLifeAspects where YEAR = 1979")
gm1981=sqldf("select COUNTRY_OF_BIRTH, SAMPLE_SEX,
             INCOME_/10 INCOME_ from AmericanYouthLifeAspects where YEAR = 1981")

treemap(gm1979,
        index=c("COUNTRY?OF_BIRTH","SAMPLE_SEX"),
        vSize="INCOME_",
        type="index"
)
#potentially do more complex treemap
#

#Next I explored Multi Distribution comparisons using and overlaid histogram.
#

ggplot(AmericanYouthLifeAspects, aes(x = INCOME_, fill = SAMPL?_SEX)) +   
  # Draw overlaying histogram
  geom_histogram(position = "identity", alpha = 0.2, bins = 50)

##another overlaying plot
#
ggplot(AmericanYouthLifeAspects, aes(x = INCOME_, fill = COUNTRY_OF_BIRTH)) +   
  # Draw overlaying histogram
  geom_his?ogram(position = "identity", alpha = 0.2, bins = 50)

#install.packages("ggforce")
library(ggforce)
library(reshape2)

#Develop your story using 3 or more charts.  Charts should be relevant, effective, correct and should portray your story.  
#Your charts ?hould be understandable by readers who have not read your background research.       (12 marks)
#
#chart that outputs the income of females and males between the years 1979-1989.
#
str(AmericanYouthLifeAspects)

#selecting data between 1979 and 1985
#df798? = sqldf("select YEAR, SAMPLE_SEX, '5000' INCOME_ from AmericanYouthLifeAspects group by YEAR_OF_BIRTH")
#df7985 <- sqldf("select * from AmericanYouthLifeAspects where YEAR between 1979 and 1985")
#df7985 <- sqldf("select * from df7989 where COUNTRY_OF_BIR?H = 'IN THE US'")
#head(df7989, n = 30)
str(df7985)

#narrowing down the data
#
#df79 <- sqldf("select * from AmericanYouthLifeAspects where YEAR = '1979'")
#str(df79)
#df79US <- sqldf("select * from df79 where COUNTRY_OF_BIRTH = 'IN THE US'")
#str(df79US)?
#first 200 rows
#
dfID9 <- sqldf("select * from AmericanYouthLifeAspects where ID <= '15' ")
str(dfID9)
#selecting years
#
dfID7999 <- sqldf("select * from dfID9 where YEAR between 1979 and 1999")
str(dfID7999)

# encoded scatterplot for 1979-99
#
ptch = ?gplot(dfID7999, aes(x=YEAR, y=INCOME_, colour = SAMPLE_SEX)) + 
  geom_point() + 
  labs(title = 'Plotting Data from the Year 1979-99 for 
  -income per Year ',
        subtitle = 'Colour coded by GENDER / SEX',
                   x = 'Timeline of Years (1?79-1999)', y='Income per Year',
                   caption='Based on Gender') + theme(panel.background=element_rect(fill='white'), panel.grid.minor = element_line(color = "black",
                                                                            ?                                                      size = 0.5,
                                                                                                                                   linetype = 2), 
                                           ?          legend.text = element_text(colour = "blue", size = rel(.75)), 
                                                      legend.title = element_text(colour = 'black', size = rel(1)),
                                                      plot.title = ?lement_text(colour = "black", size = rel(2)),
                                                      plot.subtitle = element_text(size = rel(1.5)),
                                                      plot.caption = element_text(size = rel(1.5), face='bold?)) + 
  annotate("text",  x = 1993, y = 105000, label = "Highest Income Male", size =4, fontface='italic')+
  geom_circle(aes(x0 = 1993, y0 = 100000, r = 5),
              inherit.aes = FALSE)
ptch
#
#


#first * row making it smaller for better looking gr?ph
#
dfID39 <- sqldf("select * from AmericanYouthLifeAspects where ID <= '42' ")
str(dfID39)
#selecting years 2000-2012
#
dfID2012 <- sqldf("select * from dfID39 where YEAR between 2000 and 2012")
str(dfID2012)
# encoded scatterplot for 2000-2012
#
ptch = ?gplot(dfID2012, aes(x=YEAR, y=INCOME_, colour = SAMPLE_SEX)) + 
  geom_point() + 
  labs(title = 'Plotting Data from the Year 2000-12 for 
  -income per Year ',
       subtitle = 'Colour coded by GENDER / SEX',
       x = 'Timeline of Years (2000-2012)', y?'Income per Year',
       caption='Based on Gender') + theme(panel.background=element_rect(fill='white'), panel.grid.minor = element_line(color = "black",
                                                                                                     ?                 size = 0.5,
                                                                                                                       linetype = 2), 
                                          legend.text = element_text(colour = "blue", size =?rel(.75)), 
                                          legend.title = element_text(colour = 'black', size = rel(1)),
                                          plot.title = element_text(colour = "black", size = rel(2)),
                                      ?   plot.subtitle = element_text(size = rel(1.5)),
                                          plot.caption = element_text(size = rel(1.5), face='bold')) + 
  annotate("text",  x = 2012, y = 355000, label = "Female Highest", size =4, fontface='italic')+
  geo?_circle(aes(x0 = 2012, y0 = 340500, r = 1),
              inherit.aes = FALSE)
ptch
#
#
#removed this section graph didnt work as value son y axis are no linear
#next, i made a 3 line chart 
#
#
#columns have been chosen to merge into new tidied dataset fo? 3line Chart
#
#df3Line <-df%>%
  #select('ID', 'YEAR','SAMPLE_SEX','INCOME_')

#str(df3Line)

#selecting smaller set so chart will work better
#
#dfID9Line <- sqldf("select * from df3Line where ID <= '9' ")
#str(dfID9Line)
#selecting years
#
#dfID7999Line?<- sqldf("select * from dfID9Line where YEAR between 1979 and 1999")
#str(dfID7999Line)

#p<- ggplot() +
  #geom_area(data = dfID7999Line, aes(x = YEAR, y = INCOME_, fill = SAMPLE_SEX), size = 1)+
  #xlab("x axis") +
  #ylab("y axis")
#p

#single variable ?xploration
#
#Density Plot - encoded
#


#for 1979-1999
#
# Plot
g <- ggplot(dfID7999, aes(INCOME_MAX))
g + geom_density(aes(fill=factor(SAMPLE_SEX)), alpha=0.8) + 
  labs(title="Density plot", 
       subtitle="Maximum Income From 1979-1999 Based on Gende?",
       caption="Source: df7999",
       x="Maximum Income",
       y ="Density",
       fill="Gender / Sex") + 
theme(
    panel.background=element_rect(fill='white'),
    panel.grid.minor = element_line(color = "black",
                                ?   size = 0.5,
                                    linetype = 2), legend.text = element_text(colour = "blue", size = rel(.75)),
    legend.title = element_text(colour = 'black', size = rel(1)),
    plot.title = element_text(colour = "black", size = rel(2))?
    plot.subtitle = element_text(size = rel(1.5)),
    plot.caption = element_text(size = rel(1.5), face='bold')
)
#

#for 2000-2012
#
# Plot
g <- ggplot(dfID2012, aes(INCOME_MAX))
g + geom_density(aes(fill=factor(SAMPLE_SEX)), alpha=0.8) + 
  labs(title=?Density plot", 
       subtitle="Maximum Income From 2000-2012 Based on Gender",
       caption="Source: dfID2012",
       x="Maximum Income",
       y ="Density",
       fill="Gender / Sex") + 
  theme(
    panel.background=element_rect(fill='white'),
   ?panel.grid.minor = element_line(color = "black",
                                    size = 0.5,
                                    linetype = 2), legend.text = element_text(colour = "blue", size = rel(.75)),
    legend.title = element_text(colour = 'blac?', size = rel(1)),
    plot.title = element_text(colour = "black", size = rel(2)),
    plot.subtitle = element_text(size = rel(1.5)),
    plot.caption = element_text(size = rel(1.5), face='bold')
  )





#selecting data for overlaying histogram 1979-1999
?
Overlaid7999 <- sqldf("select * from AmericanYouthLifeAspects where YEAR between 1979 and 1999")
str(Overlaid7999)
#multi distribution comparisons, overlaid histogram
#
#1979-1999
#
ggplot(Overlaid7999, aes(x = JOBSNUM_, fill = SAMPLE_SEX)) +   
  # Draw ?verlaying histogram
  geom_histogram(position = "identity", alpha = 0.5, bins = 50) + 
  labs(title="Overlaid Histogram ", 
       subtitle="Number of Jobs Worked for Males & Females ",
       caption="Source: Overlaid7999",
       x="Number of Jobs",
    ?  fill="Gender / Sex") +
  theme(
    panel.background=element_rect(fill='white'),
    panel.grid.minor = element_line(color = "black",
                                    size = 0.5,
                                    linetype = 2), legend.text = element?text(colour = "blue", size = rel(.75)),
    legend.title = element_text(colour = 'black', size = rel(1)),
    plot.title = element_text(colour = "black", size = rel(2)),
    plot.subtitle = element_text(size = rel(1.5)),
    plot.caption = element_text(siz? = rel(1.5), face='bold')
  )


#selecting data for overlaying histogram 2000-2012
#
Overlaid2012 <- sqldf("select * from AmericanYouthLifeAspects where YEAR between 2000 and 2012")
str(Overlaid2012)
#multi distribution comparisons, overlaid histogram
#
#2?00-2012
#
ggplot(Overlaid2012, aes(x = JOBSNUM_, fill = SAMPLE_SEX)) +   
  # Draw overlaying histogram
  geom_histogram(position = "identity", alpha = 0.5, bins = 50) + 
  labs(title="Overlaid Histogram ", 
       subtitle="Number of Jobs Worked for Males?& Females ",
       caption="Source: Overlaid2012",
       x="Number of Jobs",
       fill="Gender / Sex") +
  theme(
    panel.background=element_rect(fill='white'),
    panel.grid.minor = element_line(color = "black",
                                    ?ize = 0.5,
                                    linetype = 2), legend.text = element_text(colour = "blue", size = rel(.75)),
    legend.title = element_text(colour = 'black', size = rel(1)),
    plot.title = element_text(colour = "black", size = rel(2)),
  ? plot.subtitle = element_text(size = rel(1.5)),
    plot.caption = element_text(size = rel(1.5), face='bold')
  )


#Next was a small multiple chart for a multi distribution comparison
#comparing the yaer people were born wioth the max income
#doesnt matte? what yesr you were born all about the year you earned
#1979-2012
#selecting data for small multiple for 1957-60
#
smalldf5760 <- sqldf("select * from AmericanYouthLifeAspects where YEAR_OF_BIRTH between 56 and 60")
str(smalldf5760)
#
ggplot(data=smalldf57?0, aes(x=INCOME_MAX, fill = SAMPLE_SEX, colour=SAMPLE_SEX)) +
  geom_histogram(binwidth=500) +
  facet_wrap(~YEAR_OF_BIRTH) +
  labs(title="Small Multiples ", 
       subtitle="Maximum Income Based on Year they were Born, 1957-1960. ",
       caption="Sour?e: smalldf5760",
       x="Maximum Income") +
  theme(
    panel.background=element_rect(fill='white'),
    panel.grid.minor = element_line(color = "black",
                                    size = 0.5,
                                    linetype = 2), ?egend.text = element_text(colour = "blue", size = rel(.75)),
    legend.title = element_text(colour = 'black', size = rel(1)),
    plot.title = element_text(colour = "black", size = rel(2)),
    plot.subtitle = element_text(size = rel(1.3)),
    plot.capti?n = element_text(size = rel(1.5), face='bold'))
#
#
#
#1961-64
#
smalldf6164 <- sqldf("select * from AmericanYouthLifeAspects where YEAR_OF_BIRTH between 61 and 64")
str(smalldf6164)
#
ggplot(data=smalldf6164, aes(x=INCOME_MAX, fill = SAMPLE_SEX, colour=SA?PLE_SEX)) +
  geom_histogram(binwidth=500) +
  facet_wrap(~YEAR_OF_BIRTH) +
  labs(title="Small Multiples ", 
       subtitle="Maximum Income Based on Year they were Born, 1961-1964. ",
       caption="Source: smalldf5760",
       x="Maximum Income") +
  t?eme(
    panel.background=element_rect(fill='white'),
    panel.grid.minor = element_line(color = "black",
                                    size = 0.5,
                                    linetype = 2), legend.text = element_text(colour = "blue", size =?rel(.75)),
    legend.title = element_text(colour = 'black', size = rel(1)),
    plot.title = element_text(colour = "black", size = rel(1.5)),
    plot.subtitle = element_text(size = rel(1.3)),
    plot.caption = element_text(size = rel(1.5), face='bold'))?
##
#
#2000-2012
#Time Trend using stream graph for Mean income based on gender over the years 2000-2012
dfstream<-sqldf("select SAMPLE_SEX, avg(INCOME_) INCOMEmean, YEAR from dfID7999 group by SAMPLE_SEX,YEAR")
head(dfstream)
#
p3 = ggplot(data=dfstream,a?s(x=YEAR,y=INCOMEmean, colour=SAMPLE_SEX,fill=SAMPLE_SEX)) + 
  geom_stream(alpha = 0.9)+
  labs(title="Stream Graph ", 
       subtitle="Mean Income based for male and females from the years 1979-1999 ",
       caption="Source: dfstream",
       x="Year",?y="Mean Income") +
  theme(
    panel.background=element_rect(fill='white'),
    panel.grid.minor = element_line(color = "black",
                                    size = 0.5,
                                    linetype = 2), legend.text = element_text(?olour = "blue", size = rel(.75)),
    legend.title = element_text(colour = 'black', size = rel(1)),
    plot.title = element_text(colour = "black", size = rel(1.5)),
    plot.subtitle = element_text(size = rel(1.3)),
    plot.caption = element_text(size = ?el(1.5), face='bold'))
p3

##
#2000-2012
#Time Trend using stream graph for Mean income based on gender over the years 2000-2012
dfstream1<-sqldf("select SAMPLE_SEX, avg(INCOME_) INCOMEmean, YEAR from dfID2012 group by SAMPLE_SEX,YEAR")
head(dfstream1)
#
p? = ggplot(data=dfstream1,aes(x=YEAR,y=INCOMEmean, colour=SAMPLE_SEX,fill=SAMPLE_SEX)) + 
  geom_stream(alpha = 0.95)+theme_classic()+
  labs(title="Stream Graph ", 
       subtitle="Mean Income based for male and females from the years 2000-2012 ",
       ?aption="Source: dfstream1",
       x="Year", y="Mean Income") +
  theme(
    panel.background=element_rect(fill='white'),
    panel.grid.minor = element_line(color = "black",
                                    size = 0.5,
                                 ?  linetype = 2), legend.text = element_text(colour = "blue", size = rel(.75)),
    legend.title = element_text(colour = 'black', size = rel(1)),
    plot.title = element_text(colour = "black", size = rel(1.5)),
    plot.subtitle = element_text(size = rel(1?3)),
    plot.caption = element_text(size = rel(1.5), face='bold'))
p3


