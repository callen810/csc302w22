inflation=read.csv('C:/Users/iandc/OneDrive/School/UM Flint/Winter 2022/r scripts/inflation.csv')
wage=read.csv('C:/Users/iandc/OneDrive/School/UM Flint/Winter 2022/r scripts/minwage.csv')
library(tidyverse)

data1 <- inflation %>% group_by(Year) %>% summarize(m = mean(FPCPITOTLZGUSA))  

data2 <- wage %>% group_by(Year, Federal.Minimum.Wage) %>% summarize(m = mean(Federal.Minimum.Wage))

ggplot() + geom_line(data = data1, aes(x = Year, y = m), color = "blue") + geom_line(data = data2, aes(x = Year, y = m))

## Source: https://bookdown.org/yih_huynh/Guide-to-R-Book/graphing-with-different-datasets.html

## For this code, I had to manipulate one piece of raw data, because in the inflation data, it showed them as year (1/1/XXXX), and the wage data had just
## the year, so I changed in the csv the 1/1/XXXX to just a year