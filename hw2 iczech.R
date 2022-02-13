# 1)
# df1 line creates the table dataframe that will be used, with names, states, and sales
df1=data.frame(Name=c('James','Paul','Richards','Marico','Samantha','Ravi','Raghu',
                      'Richards','George','Ema','Samantha','Catherine'),
               State=c('Alaska','California','Texas','North Carolina','California','Texas',
                       'Alaska','Texas','North Carolina','Alaska','California','Texas'),
               Sales=c(14,24,31,12,13,7,9,31,18,16,18,14))

# aggregate gets the sum of sales and groups them by state
aggregate(df1$Sales, by=list(df1$State), FUN=sum)

# according to the output, this attaches the dplyr package which according to the R
# website, is data manipulation tools
library(dplyr)

# this is the dplyr output equivalent of aggregate
df1 %>% group_by(State) %>% summarise(sum_sales = sum(Sales))

# 2)

# read world cup matches csv
df2 = read.csv("C:/Users/iandc/OneDrive/School/UM Flint/Winter 2022/r scripts/WorldCupMatches.csv", header=T)

# a)  Find the size of the data frame. How many rows, how many columns?
dim(df2)

# b) Use summary function to report the statistical summary of your data.
summary(df2)

# c) Find how many unique locations olympics were held at.
unique(df2$City)

# d) Find the average attendance
mean(df2$Attendance, trim=0, na.rm=TRUE)

# e) For each Home Team, what is the total number of goals scored?
aggregate(df2$Home.Team.Goals, by=list(df2$Home.Team.Name), FUN=sum)

# f) What is the average number of attendees for each year? Is there a trend or pattern in the data in that sense?
aggregate(df2['Attendance'], list(df2$Year), mean)

# 3)

# read metabolite csv
df3 = read.csv("C:/Users/iandc/OneDrive/School/UM Flint/Winter 2022/r scripts/metabolite.csv", header=T)

# a) Find how many Alzheimers patients there are in the data set.
df3 %>% count('Alzheimeers')

# b) Determine the number of missing values for each column.
sum(is.na(df3))

# c) Remove the rows which has missing value for the Dopamine column and assign the result to a new data frame
df4 = is.na(df3)
df4

# d) In the new data frame, replace the missing values in the c4-OH-Pro column with the median value of the same column.
