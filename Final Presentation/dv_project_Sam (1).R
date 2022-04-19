library (ggplot2)
library (tidyverse)
library (ggcorrplot)

data = read.csv('covid_19_data.csv')


data$ObservationDate <- factor(data$ObservationDate, ordered = TRUE)
recent <- max(data$ObservationDate)
data_recent = subset(data, data$ObservationDate == recent)

countries <- data_recent %>%
  group_by(Country.Region) %>%
  summarize(sum(Confirmed), sum(Deaths), sum(Recovered), .groups="drop")

colnames(countries)[1] <- "Country"
colnames(countries)[2] <- "Confirmed"
colnames(countries)[3] <- "Deaths"
colnames(countries)[4] <- "Recovered"

countries <- countries %>%
  mutate('Mortality Rate per 100' = Deaths/Confirmed * 100)

countries <- countries %>%
  arrange(desc(Confirmed))

death_plot <- countries %>%
  top_n(7, Deaths) %>%
  ggplot() + aes(reorder(Country, +Deaths), Deaths, fill = factor(Country)) + 
  geom_col() +
  theme(legend.position = 'none') +
  labs(title = "Deaths", x = "Country")+
  scale_y_continuous(labels=scales::comma)
death_plot

recovered_plot <- countries %>%
  top_n(7, `Recovered`) %>%
  ggplot() + aes(reorder(Country, +`Recovered`), `Recovered`, fill = factor(Country)) + 
  geom_col() +
  theme(legend.position = 'none') +
  labs(title = "Recovered", x = "Country")+
  scale_y_continuous(labels=scales::comma)
recovered_plot

mortality_plot <- countries %>%
  top_n(7, `Mortality Rate per 100`) %>%
  ggplot() + aes(reorder(Country, +`Mortality Rate per 100`), `Mortality Rate per 100`, fill = factor(Country)) + 
  geom_col() +
  theme(legend.position = 'none') +
  labs(title = "Mortality Rate", x = "Country")+
  scale_y_continuous(labels=scales::comma)
mortality_plot

countries2 <- countries %>% select(2:4)
cormat <- round(cor(countries2), 2)

ggcorrplot(cormat,lab=TRUE)
