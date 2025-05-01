# Exemplary Code Chunk

The following is a code chunk, written by me, the purpose of which is to clean and reshape data on informal labor employment.  I begin by grouping by country, and then select only 
the values for the most recent year for each country.  I then filter and select only the values for total informal labor employment, leaving out individual sectors.  I then pivot 
and reshape the data from wide to long by creating new columns for "Male", "Female", and "Total" informal labor employment, as opposed to one column identifying which of the three 
each observation is.  Finally, I rename each column to something more legible, and consistent with the other variable names I used.

#Filter InfLab Data
InfLab2 <- InfLab %>%
  group_by(ref_area.label) %>%
  filter(time == max(time, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(classif1.label == "Institutional sector: Total") %>%
  pivot_wider(names_from = sex.label, values_from = obs_value) %>%
  select(CountryName = ref_area.label, InfLabYear = time, 
         MaleInfLab = Male, FemInfLab = Female, TotInfLab = Total)

The result is a tidy dataset with 5 columns, 111 observations, and no missing values.
