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

life.stats <-  list(
  x     = "A given age class",
  Nx    = "Raw count of individuals alive at the start of an age class",
  Px    = "Proportion of individuals that survive from the beginning of one age class to the next (Age-specific Survival)",
  N0    = "Raw count of individuals born within a given cohort",
  Lx    = "Proportion of individuals who survive from birth to the beginning of a specific age class (Age-specific survivorship)",
  N1    = "Raw count of individuals alive at the end of the first year of life",
  Lx1   = "Proportion of individuals who survive from the start of their second year (N1) to the beginning of a specific age class (Age-specific survivorship)",
  Risk  = "Number of animals alive and potentially contributing to deaths or births during age class (RiskQx will be the same as Nx, Risk Mx excludes non-reproductive individuals)",
  Qx    = "Proportion of animals that die during an age class (Mortality Rate)",
  Mx    = "1/2 the average number of offspring born to animals in an age class (Fecundity).",
  MLE   = "The age at which 50% of individuals alive after first year will die before and 50% of individuals alive after first year will die after",
  Lamda = "The proportional change in population size from one year to the next.",
  T     = "The average time elapsing from one generation to the time the next generation reproduces (Mean Generation Time)."
)