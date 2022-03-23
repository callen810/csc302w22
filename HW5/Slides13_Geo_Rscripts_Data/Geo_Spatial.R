
#install.packages('tidyverse') will bring ggplot2, dplyr, tidyr, forcats
library(tidyr) #help you create tidy data https://github.com/rstudio/cheatsheets/blob/main/tidyr.pdf
library(dplyr)
#***************
#*data manipulation, providing a consistent set of verbs that help you solve the most common data manipulation challenges:
#*mutate() adds new variables that are functions of existing variables
#*select() picks variables based on their names.
#*filter() picks cases based on their values.
#*summarise() reduces multiple values down to a single summary.
#*arrange() changes the ordering of the rows.
#*
library(ggplot2) #visualization library
library(forcats)
#R uses factors to handle categorical variables, variables that have a fixed and 
#known set of possible values. Factors are also helpful for reordering character 
#vectors to improve display. The goal of the forcats package is to provide 
#a suite of tools that solve common problems with factors, including changing the order of levels or the values. Some examples include:

#fct_reorder(): Reordering a factor by another variable.
#fct_infreq(): Reordering a factor by the frequency of values.
#fct_relevel(): Changing the order of a factor by hand.
#fct_lump(): Collapsing the least/most frequent values of a factor into "other".


library(ggmap) #google map
library(statebins) #Create a new ggplot-based "statebin" chart for USA states/territories
library(sf) #Create sf object https://github.com/rstudio/cheatsheets/blob/main/sf.pdf
library(geofacet) #Arrange a sequence of geographical panels into a grid that preserves some geographical orientation
library(ggspatial) #Spatial data plus the power of the ggplot2 framework means easier mapping.

library(colorspace) #provides a broad toolbox for selecting individual colors or color palettes, manipulating these colors, and employing them in various kinds of visualizations

library(cowplot) 
#It provides various features that help with creating publication-quality figures, 
#such as a set of themes, functions to align plots and arrange them into 
#complex compound figures, and functions that make it easy to annotate plots and or mix plots with images. The package was originally written for internal use in my lab, to provide my students and postdocs with the tools to make high-quality figures for their publications.

library(lubridate) #for date time manipulations: https://rawgit.com/rstudio/cheatsheets/main/lubridate.pdf


longs <- -180:-20
lats <- rep(89.9, length(longs))
earth_boundary <- sf::st_sfc(
  sf::st_linestring(
    cbind(longs, lats)
  ),
  crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
)
earth_boundary <- sf::st_transform(earth_boundary, crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
whiteout <- data.frame(
  x = earth_boundary[[1]][, 1],
  y = earth_boundary[[1]][, 2]
)

load('/Volumes/GoogleDrive/My Drive/DATA/US_states_geoms.rda')
load('G:/My Drive/DATA/US_states_geoms.rda')

ggplot(US_states_geoms$true_albers) + 
  geom_sf(fill = "#E69F00B0", color = "black", size = 0.5/.pt) +
  geom_polygon(
    data = whiteout, aes(x, y),
    fill = "white", color = "gray30",
    size = 0.5/.pt
  ) +
  coord_sf(xlim = c(-6721002, 2685733), ylim = c(-1634610, 4888053), expand = FALSE, ndiscr = 1000) +
  scale_x_continuous(name = "longitude", breaks = -20*c(3:10)) +
  scale_y_continuous(name = "latitude", breaks = (1:9)*10) +
  #theme_dviz_grid(font_size = 12, rel_small = 1) +
  theme(
    #plot.background = element_rect(fill = "cornsilk"),
    panel.background = element_rect(fill = "#56B4E950"),
    panel.grid.major = element_line(color = "gray30", size = 0.25),
    axis.ticks = element_line(color = "gray30", size = 0.5/.pt)
  )

#****
#*
brown <- "#deb664"
ggplot(US_states_geoms$us_albers) + 
  geom_sf(fill = brown, color = "black", size = 0.5/.pt) +
  coord_sf(datum = NA, expand = FALSE) +
  #theme_dviz_map() +
  theme(
    #plot.background = element_rect(fill = "cornsilk"),
    plot.margin = margin(6, 6, 1, 1.5) 
  )
#***************
#*
#*
ggplot(US_states_geoms$albers_revised) + 
  geom_sf(fill = brown, color = "black", size = 0.5/.pt) +
  coord_sf(datum = NA, expand = FALSE) +
  #theme_dviz_map() +
  theme(
    #plot.background = element_rect(fill = "cornsilk")
  )

#**********Incorporating Google Maps to create layered geospatial visualization*****************
#*
#*
#*
load('G:/My Drive/DATA/wind_turbines.rda')
load('G:/My Drive/DATA/sfbay_maps.rda')

m_per_deg <- 87832.42967867786
sfbay_scale = data.frame(
  x = -122.83,
  xend = -122.83 + 10000/m_per_deg,
  y = 37.24,
  yend = 37.24,
  label = "10km"
)
sfbay_bbox <- c(left = -122.88, bottom = 37.20, right = -120.88, top = 38.31)
wind_sfbay <- wind_turbines %>%
  filter(
    xlong < sfbay_bbox["right"],
    xlong > sfbay_bbox["left"],
    ylat > sfbay_bbox["bottom"],
    ylat < sfbay_bbox["top"]
  )
shiloh_bbox <- c(left = -121.9, bottom = 38.06, right = -121.71, top = 38.20)
tracy_bbox <- c(left = -121.73, bottom = 37.66, right = -121.55, top = 37.81)

ggmap(sfbay_maps$sfbay_bg)  + 
  inset_ggmap(sfbay_maps$sfbay_lines) +
  geom_point(
    data = wind_sfbay,
    aes(x = xlong, y = ylat),
    size = 0.1,
    color = "#A825A8",
    alpha = 1/3
  ) +
  geom_rect(
    data = data.frame(rbind(t(shiloh_bbox), t(tracy_bbox))),
    aes(xmin = left, xmax = right, ymin = bottom, ymax = top),
    size = 0.5,
    color = "black",
    fill = NA,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = data.frame(x = 0.5*(shiloh_bbox['left'] + shiloh_bbox['right']), y = shiloh_bbox['top'], label = "Shiloh Wind Farm"),
    aes(x, y, label = label),
    hjust = 0.512,
    vjust = -0.51,
    #family = dviz_font_family,
    color = "white",
    size = 11/.pt
  ) +
  geom_text(
    data = data.frame(x = 0.5*(shiloh_bbox['left'] + shiloh_bbox['right']),
                      y = shiloh_bbox['top'], label = "Shiloh Wind Farm"),
    aes(x, y, label = label),
    hjust = 0.5,
    vjust = -0.5,
    #family = dviz_font_family,
    size = 11/.pt
  ) +
  inset_ggmap(sfbay_maps$sfbay_labels) +
  geom_segment(
    data = sfbay_scale,
    aes(x, y, xend = xend, yend = yend),
    size = 1
  ) +
  geom_text(
    data = sfbay_scale,
    aes(0.5*(x+xend), y, label = label),
    hjust = 0.5,
    vjust = -0.5,
    #family = dviz_font_family,
    size = 10/.pt
  ) +
  ggspatial::annotation_north_arrow(
    width = grid::unit(1, "cm"),
    height = grid::unit(1, "cm"),
    pad_x = grid::unit(0.25, "cm"),
    pad_y = grid::unit(0.5, "cm"),
    style = ggspatial::north_arrow_fancy_orienteering(
      line_width = 1,
      text_size = 12,
      #text_family = dviz_font_family
    ),
    location ="tr"
  )

#***********************************************************
#*
#*
m_per_deg <- 87832.42967867786
shiloh_scale = data.frame(
  x = -121.735,
  xend = -121.735 + 2000/m_per_deg,
  y = 38.064,
  yend = 38.064,
  label = "2000m"
)
#bbox <- c(left = -121.9, bottom = 38.06, right = -121.71, top = 38.20)
wind_shiloh <- wind_turbines %>%
  filter(
    xlong < shiloh_bbox["right"],
    xlong > shiloh_bbox["left"],
    ylat > shiloh_bbox["bottom"],
    ylat < shiloh_bbox["top"]
  ) %>%
  mutate(
    name = fct_relevel(fct_collapse(p_name,
                                    `EDF Renewables` = "EDF Renewable V",
                                    `High Winds` = "High Winds",
                                    `Shiloh` = c("Shiloh Wind Project", "Shiloh II", "Shiloh III", "Shiloh IV"),
                                    `Solano` = c("Solano Phase 3", "Solano Phase IIA", "Solano Wind Project", "Solano Wind Project, Phase I", "Solano Wind Project, Phase IA"),
                                    `other` = c("Montezuma", "Montezuma Winds II", "unknown Solano County")
    ), "EDF Renewables", "High Winds", "Shiloh", "Solano", "other"),
    year_range = cut(
      p_year,
      breaks = c(1980, 2000, 2005, 2010, 2015),
      labels = c("before 2000", "2000 to 2004", "2005 to 2009", "2010 to 2014"),
      right = FALSE
    )
  )

p2 <- ggmap(sfbay_maps$shiloh_terrain)  + 
  geom_point(
    data = wind_shiloh,
    aes(x = xlong, y = ylat, fill = year_range, shape = name),
    size = 1.5,
    color = "black", stroke = 0.2
  ) +
  geom_segment(
    data = shiloh_scale,
    aes(x, y, xend = xend, yend = yend),
    size = 1
  ) +
  geom_text(
    data = shiloh_scale,
    aes(0.5*(x+xend), y, label = label),
    hjust = 0.5,
    vjust = -0.5,
    #family = dviz_font_family,
    size = 10/.pt
  ) +
  ggspatial::annotation_north_arrow(
    width = grid::unit(1, "cm"),
    height = grid::unit(1, "cm"),
    pad_x = grid::unit(0.2, "cm"),
    pad_y = grid::unit(0.2, "cm"),
    style = ggspatial::north_arrow_fancy_orienteering(
      line_width = 1,
      text_size = 12,
      #text_family = dviz_font_family
    ),
    location ="bl"
  ) +
  xlab(label = NULL) +
  ylab(label = NULL) +
  scale_fill_viridis_d(
    name = "year built",
    option = "A", end = .95, begin = 0.3, direction = -1,
    guide = guide_legend(
      order = 2,
      reverse = FALSE,
      override.aes = list(shape = 22, size = 4, stroke = 0))
  ) +
  scale_shape_manual(
    name = "project name",
    values = 21:25,
    guide = guide_legend(
      order = 1,
      override.aes = list(
        fill = "grey70",
        size = 2
      )
    )
  ) +
  #theme_dviz_map(12) +
  theme(
    legend.key.width = grid::unit(12, "pt")
  )
p2

#*******************************************************************
#*
#*Population density. Population divided by area
#*
load('G:/My Drive/DATA/US_counties_income.rda')
source('G:/My Drive/R_ScriptsToSource/align_legend.R')


US_counties_income <- mutate(US_counties_income, logdens = log(as.numeric(popdens)*1e6))
ggplot(US_counties_income) + 
  geom_sf(aes(color = logdens, fill = logdens), size = 0.1) + 
  geom_sf(data = US_states_geoms$albers_revised, fill = NA, color = "grey30", size = 0.2) +
  coord_sf(datum = NA, expand = FALSE) +
  scale_x_continuous(limits = c(-4000000, 2300000)) +
  scale_fill_continuous_sequential(
    aesthetics = c("color", "fill"),
    palette = "YlGnBu", rev = TRUE, cmax = 20, c2 = 20, p2 = 1.75,
    name = "population density\n(persons / square km)",
    limits = log(c(0.01, 30000)),
    breaks = log(c(0.01, 1, 100, 10000)),
    labels = c("0.01", "1", "100", "10,000"),
    guide = guide_colorbar(
      frame.colour = "black",
      ticks.colour = "white",
      barwidth = grid::unit(15, "pt"),
      barheight = grid::unit(90, "pt")
    )
  ) +
  #theme_dviz_map(12, rel_small = 1) +
  theme(
    #plot.background = element_rect(fill = "cornsilk"),
    legend.position = c(0, 1),
    legend.justification = c(0, 1),
    legend.spacing.x = grid::unit(3, "pt"),
    legend.title = element_text(hjust = 0.5),
    plot.margin = margin(3, 3, 3, 1.5)
  )
ggdraw(align_legend(p))

#**************************************
#*Discretized incomes (bins)
#*
#*
US_counties_income <- mutate(
  US_counties_income,
  income_bins = cut(
    ifelse(is.na(median_income), 35000, median_income), # hide missing value
    breaks = c(0, 30000, 55000, 80000, 105000, 150000),
    labels = c("< $30k", "$30k to $60k", "$60k to $85k", "$85k to $105k", "> $105k"),
    right = FALSE
  )
)


ggplot(US_counties_income) + 
  geom_sf(aes(color = income_bins, fill = income_bins), size = 0.1) + 
  geom_sf(data = US_states_geoms$albers_revised, fill = NA, color = "grey30", size = 0.2) +
  coord_sf(datum = NA, expand = FALSE) +
  scale_x_continuous(limits = c(-4000000, 2300000)) +
  scale_fill_discrete_sequential(
    aesthetics = c("color", "fill"),
    h1 = -83, h2 = 20, c1 = 30, cmax = 40, c2 = 0, l1 = 20, l2 = 100, p1 = 1, p2 = 1.2, rev = TRUE, 
    name = "median income",
    nmax = 6,
    order = 2:6,
    guide = guide_legend(
      override.aes = list(colour = "white", size = 1),
      reverse = TRUE
    )
  ) +
  #theme_dviz_map(12, rel_small = 1) +
  theme(
    #plot.background = element_rect(fill = "cornsilk"),
    legend.position = c(0, 1),
    legend.justification = c(0, 1),
    legend.spacing.x = grid::unit(3, "pt"),
    legend.spacing.y = grid::unit(3, "pt"),
    legend.title = element_text(hjust = 0.5),
    legend.key.width = grid::unit(18, "pt"),
    legend.key.height = grid::unit(15, "pt"),
    plot.margin = margin(3, 3, 3, 1.5)
  )

#*******************************************************************
#*
#*
#*
#*

load('G:/My Drive/DATA/US_income.rda')
load('G:/My Drive/DATA/US_income_cartogram.rda')

US_income <- mutate(
  US_income,
  income_bins = cut(
    ifelse(is.na(median_income), 25000, median_income), # hide missing value
    breaks = c(0, 40000, 50000, 60000, 70000, 80000),
    labels = c("< $40k", "$40k to $50k", "$50k to $60k", "$60k to $70k", "> $70k"),
    right = FALSE
  )
)

US_income_cartogram$income_bins <- US_income$income_bins

ggplot(US_income_cartogram, aes(fill = income_bins)) + 
  geom_sf(color = "grey30", size = 0.2) + coord_sf(datum = NA, expand = FALSE) +
  scale_x_continuous(limits = c(-3900000, 2500000)) +
  scale_fill_discrete_sequential(
    h1 = -83, h2 = 20, c1 = 30, cmax = 40, c2 = 0, l1 = 20, l2 = 100, p1 = 1, p2 = 1.2, rev = TRUE, 
    name = "median income",
    nmax = 7,
    order = 2:6,
    guide = guide_legend(
      override.aes = list(colour = "white", size = 1),
      reverse = TRUE
    )
  ) +
  #theme_dviz_map(12, rel_small = 1) +
  theme(
    #plot.background = element_rect(fill = "cornsilk"),
    legend.position = c(0, 1),
    legend.justification = c(0, 1),
    legend.spacing.x = grid::unit(3, "pt"),
    legend.spacing.y = grid::unit(3, "pt"),
    legend.title = element_text(hjust = 0.5),
    legend.key.width = grid::unit(18, "pt"),
    legend.key.height = grid::unit(15, "pt"),
    plot.margin = margin(3, 3, 3, 1.5)
  )

#**************************************************************
#*
#*Cartogram w/ squares
#*
#*
filter(US_income, name != "Puerto Rico", GEOID != "11") %>% # remove Puerto Rico and DC
  ggplot(aes(state = name, fill = income_bins)) +
  geom_statebins(
                 lbl_size = 14/.pt) +
  expand_limits(x = -1.3) + # make space for legend
  coord_equal(expand = FALSE) +
  scale_fill_discrete_sequential(
    h1 = -83, h2 = 20, c1 = 30, cmax = 40, c2 = 0, l1 = 20, l2 = 100, p1 = 1, p2 = 1.2, rev = TRUE, 
    name = "median income",
    nmax = 7,
    order = 2:6,
    guide = guide_legend(
      override.aes = list(colour = "white", size = 1),
      reverse = TRUE
    )
  ) +
  #theme_dviz_map(12, rel_small = 1) +
  theme(
    #plot.background = element_rect(fill = "cornsilk"),
    legend.background = element_blank(),
    legend.position = c(0, 1),
    legend.justification = c(0, 1),
    legend.spacing.x = grid::unit(3, "pt"),
    legend.spacing.y = grid::unit(3, "pt"),
    legend.title = element_text(hjust = 0.5),
    legend.key.width = grid::unit(18, "pt"),
    legend.key.height = grid::unit(15, "pt")
  )

#******************************************************
#*
#*Cartogram w/ plots and map-like representation
#*
#*
load('G:/My Drive/DATA/house_prices.rda')

adjust_labels <- as_labeller(
  function(x) {
    case_when(
      x == "New Hampshire" ~ "N. Hampshire",
      x == "District of Columbia" ~ "DC",
      TRUE ~ x
    )
  }
)
house_prices %>% 
  filter(
    date >= ymd("2007-01-01"),
    date <= ymd("2013-05-31")
  ) %>%
  ggplot(aes(date, unemploy_perc)) + 
  geom_area(fill = "#56B4E9", alpha = 0.7) +
  geom_line() + 
  scale_y_continuous(
    name = "unemployment rate",
    limits = c(0, 16), expand = c(0, 0),
    breaks = c(0, 5, 10, 15),
    labels = c("0%", "5%", "10%", "15%")
  ) +
  scale_x_date(
    name = NULL,
    breaks = ymd(c("2008-01-01", "2010-01-01", "2012-01-01")),
    labels = c("'08", "'10", "'12"),
    expand = c(0, 0)
  ) +
  coord_cartesian(clip = "off") +
  facet_geo(~state, grid = "us_state_grid1", labeller = adjust_labels) + #library(geofacet)
  #theme_dviz_grid(12, dviz_font_family_condensed, rel_small = 10/12) +
  theme(
    strip.text = element_text(
      #family = dviz_font_family_condensed,
      margin = margin(3, 3, 3, 3)
    ),
    axis.line.x = element_blank(),
    panel.spacing.x = grid::unit(5, "pt"),
    panel.spacing.y = grid::unit(5, "pt"),
    panel.grid.major = element_line(color = "gray80"),
    panel.background = element_rect(fill = "gray90")
  )
