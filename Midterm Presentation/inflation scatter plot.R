inflation=read.csv('inflation.csv')
library(tidyverse)
ggplot()+aes(y=inflation$FPCPITOTLZGUSA,x=inflation$DATE,group="")+geom_point()
