library(maps)
library(mapdata)
library(ggplot2)
library(ggrepel)
library(cowplot)
library(socviz)


state=map_data('state')
cali=subset(state, region=='california')

load(file='wind_turbines.rda')
wind_cali=subset(wind_turbines, t_state=='CA')

ggplot()+geom_polygon(data=cali, aes(x=long, y=lat), fill='red')+
  coord_fixed(1.3)+geom_point(data=wind_cali, aes(x=xlong, y=ylat), color="blue")
