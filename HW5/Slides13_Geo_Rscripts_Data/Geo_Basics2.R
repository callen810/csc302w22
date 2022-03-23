require(maps)
require(mapdata)
library(ggplot2)
library(ggrepel)



global <- map_data("world")
ggplot() + geom_polygon(data = global, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)

ggplot() + 
  geom_polygon(data = global, aes(x=long, y = lat, group = group), fill = NA, color = "red") + 
  coord_fixed(1.3)

gg1 <- ggplot() + 
  geom_polygon(data = global, aes(x=long, y = lat, group = group), fill = "green", color = "blue") + 
  coord_fixed(1.3)
gg1



#xlim and ylim can be manipulated to zoom in or out of the map
#+ xlim(2,100) + ylim(2,100)

textsize<-function(x)
{
  y=matrix(nrow = length(x), ncol = 1)
  for (i in 1:length(x))
  {
    if (x[i]>=70)
      y[i]=2
    else
      y[i]=0
  }
  y
}

coors = read.table('G:/My Drive/DATA/citiesWcoordsv2.csv', sep=',', header = TRUE)

coors$textsize = apply(as.matrix(coors$mycount),1, textsize)

gg1<-ggplot() + 
  geom_polygon(data = global, aes(x=long, y = lat, group = group, fill=group), color = "white") + 
  #coord_fixed(ratio = 1.3)
  coord_fixed(xlim = c(-150, 180.0),  ylim = c(-50, 75), ratio = 1.3)

gg1

gg1 + geom_point(data=coors, aes(lon, lat, size=mycount, position = "jitter"), color="coral1") + 
  ggtitle("") + scale_size(name="Count") + 
  geom_text( data=coors, hjust=0.5, vjust=-0.5, aes(x=lon, y=lat, label=city), colour="gold2", size=coors$textsize ) +
  xlab("") + ylab("")


