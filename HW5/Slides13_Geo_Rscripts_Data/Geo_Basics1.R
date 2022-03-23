library(maps)
library(mapdata)
library(ggplot2)
library(ggrepel)
library(cowplot)
library(socviz)

#if you don't have packages, please install them. You can install all at once as follows:

#install.packages(c("maps", "mapdata", "socviz", "cowplot"))


states <- map_data("state")
arrests <- USArrests

names(arrests) <- tolower(names(arrests))
arrests$region <- tolower(rownames(USArrests))

choro <- merge(states, arrests, sort = FALSE, by = "region")
choro <- choro[order(choro$order), ]
ggplot(choro, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = assault)) +
  coord_map("albers",  lat0 = 45.5, lat1 = 29.5)

#
us_states <- map_data("state") #it's more than 15000 because we need that many inputs
#to draw poylgons on the canvas.

dim(us_states)

#basic
ggplot(data=us_states, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill="white", color="black")

#add color
ggplot(data=us_states, aes(x=long, y=lat, group=group, fill=region)) +
  geom_polygon( size=0.1, color='gray90') + 
  guides(fill="none") #without a legend

#let's add the projection component. 
#Earlier, default ones, were plotted useing Mercator projection
#Now let's introduce coord_map to apply the projectio which will make some adjustments to the earlier
ggplot(data=us_states, aes(x=long, y=lat, group=group, fill=region)) +
  geom_polygon( size=0.1, color='gray90') + 
  coord_map(projection = 'albers', lat0=39, lat1=45)+
  guides(fill="none") #without a legend

#let's visualize some real data
#2016 elections.
election$region = tolower(election$state)
us_states_elec = left_join(us_states, election)
party_colors=c('#2E74C0', '#CB454A')

ggplot(data=us_states_elec, aes(x=long, y=lat, group=group, fill=party)) +
  geom_polygon( size=0.1, color='gray90') + 
  coord_map(projection = 'albers', lat0=39, lat1=45)+
  scale_fill_manual(values = party_colors) #+
  #theme_map()
  #guides(fill="none") #without a legend

#well, colors don't really represent two major political parties in US. So,

