dummy.compilation <- tibble(
  ExtractID       = str_c("TEST", seq(1, 50)),
  Subject         = sample(c(str_c("TEST", seq(1, 5))), size = 50, replace = TRUE),
  Subj_Certainty  = sample(c("yes", "no", "unknown"), size = 50, replace = TRUE),
  CollectionDate  = sample(seq(ymd("2023-01-01"), ymd("2023-12-31"), by = "day"), size = 50, replace = TRUE),
  ExtractConc     = runif(50, min = 10, max = 100),
  steps_remaining = sample(c("sample not extracted", "extract not sequenced", "sample extracted and sequenced"), 
                           size = 50, replace = TRUE) %>% 
    factor(levels = c("sample not extracted", "extract not sequenced", "sample extracted and sequenced")),
  ExtractBox      = str_c("Box_", sample(1:10, size = 50, replace = TRUE))
)

bats          <- config::get(config = "bats")
isolates      <- config::get(config = "isolates")
envir         <- config::get(config = "envir")

write.table(dummy.compilation,
            bats$compilation,
            row.names = F,
            sep = "\t")

write.table(dummy.compilation,
            isolates$compilation,
            row.names = F,
            sep = "\t")

write.table(dummy.compilation,
            envir$compilation,
            row.names = F,
            sep = "\t")