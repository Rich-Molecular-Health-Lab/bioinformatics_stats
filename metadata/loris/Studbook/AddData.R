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