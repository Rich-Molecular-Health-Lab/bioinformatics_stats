here::i_am("metadata/loris/Studbook/LoadStudbookData.R")

loc.extra <- tibble(
  Institution.Name = c(rep("Wild", times = 5), 
                       "Private Owner", 
                       "Virginia Safari Park"),
  Mnemonic = c(
    "Vietnam", 
    "Thailand", 
    "LAOPEOPLESDEMOCRATICREPUBLIC", 
    "Undetermined", 
    "Asia", 
    "Greenwich", 
    "VASAFARI"),
  Country = c("Vietnam", 
              "Thailand", 
              "Laos", 
              rep("Undetermined", times = 2), 
              "United Kingdom", 
              "United States")
)


missing <- tibble(
  ID           = 2638,
  Sex          = "F",
  Status       = "A",
  Sire         = 1121,
  Dam          = 1145,
  OrderLoc     = 1,
  Date         = ymd("2011-06-14"),
  TypeEvent    = "Birth",
  Location     = "MOO",
  NameLoc      = "Aquarium & Rainforest at Moody Gardens",
  Country      = "United States"
)

locations <- bind_rows(read.csv(here(path$AZAstudbooks$institutions$current25)), 
                       read.csv(here(path$AZAstudbooks$institutions$current21)), 
                       read.csv(here(path$AZAstudbooks$institutions$historic21)),
                       loc.extra) %>% clean.locations() %>% enhance.locations()


btp25 <- enframe(BTP, name = "LocAssignment", value = "Details") %>%
  unnest_wider(Details) %>%
  select(-Address) %>%
  unnest_longer(Lorises, indices_to = "ID", values_to = "Indiv") %>%
  unnest_wider(Indiv) %>%
  mutate(across(c(ID, Age, MateID), ~ as.integer(.)),
         CurrentLoc = str_remove_all(str_remove_all(Location, c("\\s")), "-"),
         .keep = "unused") %>%
  rename(Assignment = Note, Note = "Facility Note") %>%
  mutate(DispIcon = case_match(Disposition,
                               "SEND TO"      ~ "'plane-departure'",
                               "HOLD"         ~ "'spinner'",
                               "RECEIVE FROM" ~ "'plane-arrival'"),
         BreedIcon = case_match(Breeding,
                                "DO NOT BREED" ~ "'ban'",
                                "BREED WITH"   ~ "'heart-circle-check'")) %>%
  mutate(Status = case_when(is.na(Assignment) ~ "A",
                            str_detect(Assignment, "Died") ~ "D", 
                            .default = "A"),
         Plan = if_else(str_detect(Assignment, "Excluded"), "Exclude", "Include")) %>%
  left_join(select(locations,
                   LocAbbrev,
                   colorLoc,
                   NameLoc,
                   Country,
                   CurrentLoc = Label), by = join_by(CurrentLoc)) %>%
  select(
    ID,
    Status,
    Sex,
    Age,
    CurrentLoc = LocAbbrev,
    Disposition,
    DispIcon,
    LocAssignment,
    Breeding,
    BreedIcon,
    MateID,
    Plan,
    Assignment,
    Note,
    colorCurrent = colorLoc,
    NameLocCurrent = NameLoc,
    Country
  ) %>% arrange(ID)

alive    <- filter(btp25, Status == "A")     %>% pull(ID)
excluded <- filter(btp25, Plan == "Exclude") %>% pull(ID)

current.institutions <- enframe(BTP, name = "LocAbbrev", value = "Details") %>%
  unnest_wider(Details) %>%
  select(LocAbbrev, NameLoc = Institution, Address) %>%
  left_join(locations, by = join_by(LocAbbrev, NameLoc))


living.25   <- read.csv(here(path$AZAstudbooks$living25))   %>% 
  clean_studbook(., alive, locations)

deaths.24 <- setdiff(pull(living.25, ID), pull(btp25, ID))

births.24 <- filter(btp25, !(ID %in% pull(living.25, ID))) %>%
  mutate(TypeEvent = "Birth",
         OrderLoc  = 1,
         Date      = ymd("2025-1-1")) %>%
  select(
         ID,
         Sex,
         Status,
         Date,
         TypeEvent,
         OrderLoc,
         Location = CurrentLoc,
         NameLoc  = NameLocCurrent,
         Country)
  

living.21   <- read.csv(here(path$AZAstudbooks$living21))   %>% 
  clean_studbook(., alive, locations) %>%
  filter(!(ID %in% pull(living.25, ID)))

deaths.21_24 <- setdiff(pull(living.21, ID), pull(living.25, ID))

historic.21 <- read.csv(here(path$AZAstudbooks$historic21)) %>% 
  clean_studbook(., alive, locations) %>%
  filter(!(ID %in% pull(living.25, ID))) %>%
  filter(!(ID %in% pull(living.21, ID)))

studbook <- bind_rows(living.25, 
                      living.21, 
                      historic.21,
                      births.24) %>%
  mutate(TypeEvent = fct(TypeEvent, 
                         levels = c("Birth", "Transfer", "End")),
         across(c(Sire, Dam), ~ na_if(., 0))) %>%
  arrange(ID, OrderLoc, TypeEvent) %>%
  distinct() %>%
  group_by(ID) %>%
  mutate(StartLoc = if_else(TypeEvent != "End", Date, lag(Date)),
         EndLoc   = if_else(TypeEvent == "End", Date, lead(Date))) %>%
  select(
    ID,
    Status,
    OrderLoc,
    Location,
    TypeEvent,
    StartLoc,
    Date,
    EndLoc,
    NameLoc,
    Country,
    Sex,
    Sire,
    Dam,
    Confirmed_Death,
    Confirmed_Age
  ) %>% ungroup()
