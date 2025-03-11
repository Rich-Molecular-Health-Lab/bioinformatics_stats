studbook.living <- read.csv(here("Studbooks/Studbook_Living.csv"), header = T) %>%
  filter(str_detect(Location, "\\w+")) %>%
  select(ID, Sex, Sire, Dam, Event_Type, Date, Location)  %>%
  mutate(Date  = str_squish(str_extract(Date, ".{1,3}[/-].{1,3}[/-]\\d{1,4}"))) %>%
  mutate(Date  = str_remove_all(Date, "\\s")) %>%
  mutate(Date  = str_replace(Date, "-(?=0\\d$)", "-20"),
         Date  = str_replace(Date, "-(?=[5-9]\\d$)", "-19")) %>%
  mutate(Date  = if_else(str_detect(Date, "^\\d{1,}"), dmy(Date), mdy(Date))) %>%
  mutate(across(where(is.character), ~ na_if(., "")),
         across(where(is.character), ~ str_trim(.)),
         across(where(is.character), ~ str_remove_all(., "\n")),
         Location = str_remove_all(Location, "[^\\w]"),
         across(where(is.character), ~ str_remove_all(., "\\s")),
         ID   = as.integer(str_remove_all(ID  , "[^\\d+]"))) %>%
  fill(ID, Sex, Sire, Dam) %>%
  group_by(ID) %>%
  mutate(Event_order = row_number(), Location = str_to_upper(Location)) %>%
  fill(Date, .direction = "up") %>%
  ungroup() %>%
  select(Event_order, ID, Sex, Sire, Dam, Event_Type, Date, Location)

studbook.historic <- read.csv(here("Studbooks/Studbook_Historic.csv"), header = T) %>%
  filter(str_detect(Location, "\\w+")) %>%
  select(ID, Sex, Sire, Dam, Event_Type, Date, Location)  %>%
  mutate(Date  = str_squish(str_extract(Date, ".{1,3}[/-].{1,3}[/-]\\d{1,4}"))) %>%
  mutate(Date  = str_remove_all(Date, "\\s")) %>%
  mutate(Date  = str_replace(Date, "-(?=0\\d$)", "-20"),
         Date  = str_replace(Date, "-(?=[5-9]\\d$)", "-19")) %>%
  mutate(Date  = if_else(str_detect(Date, "^\\d{1,}"), dmy(Date), mdy(Date))) %>%
  mutate(across(where(is.character), ~ na_if(., "")),
         across(where(is.character), ~ str_trim(.)),
         across(where(is.character), ~ str_remove_all(., "\n")),
         Location = str_remove_all(Location, "[^\\w]"),
         across(where(is.character), ~ str_remove_all(., "\\s")),
         ID   = as.integer(str_remove_all(ID  , "[^\\d+]"))) %>%
  fill(ID, Sex, Sire, Dam) %>%
  group_by(ID) %>%
  mutate(Event_order = row_number(), Location = str_to_upper(Location)) %>%
  fill(Date, .direction = "updown") %>%
  ungroup() %>%
  select(Event_order, ID, Sex, Sire, Dam, Event_Type, Date, Location) %>%
  filter(!(ID %in% studbook.living$ID))
