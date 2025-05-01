
#Setup
library(tidyverse)
library(readxl)
library(readr)
library(kableExtra)
library(knitr)

#Read data files
load("vdem.RData")
Corrup <- read_excel("CPI2024-Results-and-trends.xlsx")
InfLab <- read_csv('EMP_NIFL_SEX_INS_RT_A-20250420T1917.csv')

#Filter VDem Data
vdem2 <- vdem %>%
  filter(year == 2024) %>%
  select(
    CountryName = country_name, YearExclInfLab = year,
    Polyarchy = v2x_polyarchy, LibDem = v2x_libdem,
    ParticipDem = v2x_partipdem, EgalDem = v2x_egaldem,
    FreeFairElect = v2xel_frefair, EqualAccess = v2xeg_eqaccess
  )

#Filter Corrup Data
Corrup2 <- Corrup %>%
  select(
    CountryName = `Corruption Perceptions Index 2024: Global scores`,
    CPIScore = ...4, CPIScoreRank = ...5, CPIScoreStdErr = ...6,
    EconIntelRating = ...13, SnPGloInsightsRating = ...15,
    WEFRating = ...21
  )

#Filter InfLab Data
InfLab2 <- InfLab %>%
  group_by(ref_area.label) %>%
  filter(time == max(time, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(classif1.label == "Institutional sector: Total") %>%
  pivot_wider(names_from = sex.label, values_from = obs_value) %>%
  select(CountryName = ref_area.label, InfLabYear = time, 
         MaleInfLab = Male, FemInfLab = Female, TotInfLab = Total)

#Join Datasets
jointdata <- vdem2 %>%
  left_join(Corrup2, by = "CountryName") %>%
  left_join(InfLab2, by = "CountryName") %>%

#Make all variables on a 0 to 1 scale
  mutate(across(c(CPIScore, CPIScoreStdErr, 
                  SnPGloInsightsRating, EconIntelRating, WEFRating), 
                ~ as.numeric(.x) / 100))

#Write CSV
write_csv(jointdata, "CorruptionPolitEcon.csv")

#Function to make tables
summarize_variable <- function(variable, caption) {
  summary_stats <- summary(variable)
  summary_df <- data.frame(
    Statistic = names(summary_stats),
    Value = round(as.numeric(summary_stats), 3)
  )
  kable(summary_df, caption = caption)
}

