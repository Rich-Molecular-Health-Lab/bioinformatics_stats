
ID == 2108 ~ 12108,
ID == 2717 ~ 12717,
ID == 1047 ~ 1012,
ID == 2645 ~ 1131,
ID == 1045 | ID == 1046 ~ 11045,
ID == 2423 | ID == 2424 ~ 12423

hypotheticals <- tibble(
  Studbook.ID    = rep(c(12108, 12717, 22717, 11045, 12423), each = 2),
  Sex.Type       = rep(c("M", "M", "F", "M", "M")      , each = 2),
  Current.Status = "Deceased",
  Location       = rep(c("IRCHEL", "GREENWICH", "GREENWICH", "POZNAN", "PUBLIC"), each = 2),
  Sire           = 0,
  Dam            = 0,
  Date           = c("10/Apr/1987", 
                     "10/Apr/1988", 
                     rep(c("12/Jun/2019", "12/Jun/2020"), times = 2),
                     "14/Feb/1985",
                     "14/Feb/1988",
                     "01/Jan/1997",
                     "01/Jan/2002"
  ),
  Event.Type     = rep(c("Birth/hatch", "Go Ltf"), times = 5)
)

assign.sire <- need.dads %>% list_assign("2645" = 1131)


mutate(Sire = case_when(
  ID == 2108 ~ 12108,
  ID == 2717 ~ 12717,
  ID == 1047 ~ 1012,
  ID == 2645 ~ 1131,
  ID == 1045 | ID == 1046 ~ 11045,
  ID == 2423 | ID == 2424 ~ 12423,
  .default = Sire
), 
Dam = if_else(ID == 2717, 22717, Dam)) %>%
  
  
  
  
  mutate(Date = if_else(TypeEvent == "Breed" & ID == 12717 | ID == 22717, EndLoc, Date)) %>%
  mutate(Date = if_else(TypeEvent == "Birth" & ID == 12717 | ID == 22717, lead(Date) - years(1), Date),
         StartLoc = if_else(ID == 12717 | ID == 22717, EndLoc - years(1), StartLoc)) %>%