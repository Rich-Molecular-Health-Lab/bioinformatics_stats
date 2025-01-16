health.log <- list(
  culi = tribble(
  ~health_note                   , ~Date,
  "dried blood around anus/tail" , ymd("2023-11-16"),
  "coprophagy observed"          , ymd("2023-12-17"),
  "weight - 451 g"               , ymd("2024-04-14"),
  "left biscuit/gum behind"      , ymd("2024-02-10")
))

health.records <- list(
  culi = list(
    "2023-11-16" = "dried blood around anus/tail",
    "2023-12-17" = "coprophagy observed",
    "2024-02-10" = "left biscuit/gum behind",
    "2024-04-14" = "weight - 451 g"
  )
)