
#Setup
library(tidyverse)
library(dplyr)
library(ggplot2)
library(countrycode)

#Download Data
fulldata <- readRDS("V-Dem-CPD-Party-V2.rds")

#Filter Data
filtereddata <- fulldata %>%
  select(v2paenname, country_name, year, v2xpa_popul, v2paseatshare)

#Clean Data & Remove Parties with less than 10% of seats in legislature
cleandata <- filtereddata %>%
  filter(!is.na(v2paseatshare) & !is.na(v2xpa_popul) & v2paseatshare >= 10)

#Regression
PopulismRegression <- lm(v2xpa_popul ~ year, data = cleandata)
print(PopulismRegression)
ggplot(cleandata, aes(x = year, y = v2xpa_popul)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatterplot of Populism Level vs. Year", 
       x = "Year", 
       y = "Populism") +
  theme_minimal()

#Caluclating Country Averages
average_populism <- cleandata %>%
  group_by(country_name, year) %>%
  summarize(average_populism = mean(v2xpa_popul, na.rm = TRUE))%>%
  group_by(country_name) %>%
  filter(year == max(year)) %>%
  ungroup()

#Ranking countries by level of populist rhetoric in their political parties
ranked_populism <- average_populism %>%
  mutate(populism_rank = rank(-average_populism, ties.method = "first"))

#Boxplot of Country Averages
ggplot(ranked_populism, aes(y = average_populism)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Average Populism Rankings", 
       y = "Average Populism Score") +
  theme_minimal()

#Creating a variable for continent
bycontinent <- ranked_populism %>%
  mutate(continent = countrycode(country_name, origin = "country.name", 
                                 destination = "continent"),
         continent = ifelse(continent == "Americas", "America", continent))

#Boxplot by Continent
ggplot(bycontinent, aes(x = factor(continent, levels = c("America", "Africa", "Asia", "Europe", "Oceania", "Other")), 
                         y = average_populism)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Average Populism: Americas & Rest of World", x = "", 
       y = "Average Populism Score") +
  theme_minimal()

#Regression of Populism and Seat Share in the Americas
americasdata <- cleandata %>%
  mutate(continent = countrycode(country_name, origin = "country.name", destination = "continent")) %>%
  filter(continent == "Americas")

ggplot(americasdata, aes(x = v2paseatshare, y = v2xpa_popul)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Regression of v2xpa_popul on v2paseatshare",
       x = "v2paseatshare",
       y = "v2xpa_popul") +
  theme(
    plot.title = element_text(size = 18, face = "bold", color = "black", family = "serif"),
    axis.title = element_text(size = 16, face = "bold", color = "black", family = "serif"),
    axis.text = element_text(size = 12, face = "bold", color = "black", family = "serif"),
    axis.line = element_line(color = "black"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )