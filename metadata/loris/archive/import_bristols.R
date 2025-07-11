bristol <- read.csv(here("metadata/loris/hdz_nutritionData_2025-1-30.csv"), header = T) %>%
  mutate(date = mdy(Date)) %>%
  select(date,
         bristol      = Fecal.Score.for.Culi,
         note_culi    = Notes) %>%
  separate_longer_delim(cols = "bristol", delim = ", ") %>%
  separate_longer_delim(cols = "bristol", delim = ",") %>%
  mutate(bristol_culi        = as.numeric(bristol),
         bristol_warble      = 1, .keep = "unused") %>%
  pivot_longer(
    cols         = starts_with("bristol_"),
    names_prefix = "bristol_",
    names_to     = "subject",
    values_to    = "bristol"
  ) %>%
  mutate(note = if_else(subject == "culi", note_culi, "")) %>%
  distinct(date,
           subject,
           bristol,
           note) %>%
  group_by(date, subject) %>%
  summarize(
    bristol_min  = min(bristol),
    bristol_max  = max(bristol),
    bristol_mean = mean(bristol)
  ) %>% ungroup()

write.table(bristol, here("metadata/loris/bristols.tsv"), row.names = F, sep = "\t")

