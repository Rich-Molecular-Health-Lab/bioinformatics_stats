colFormat.CollectionDate <- colFormat(date = TRUE)
colFormat.depth          <- colFormat(separators = TRUE)
colFormat.mean_coverage  <- colFormat(percent = TRUE, digits = 1)




colDef.SampleID       <- colDef(name = "Sample"              , aggregate = "count")
colDef.Subject        <- colDef(name = "Subject"             , aggregate = "frequency", filterable = TRUE)
colDef.probiotic      <- colDef(name = "Probiotic"           , aggregate = "unique"   , filterable = TRUE)
colDef.oatgel         <- colDef(name = "Oat Gel"             , aggregate = "unique"   , filterable = TRUE)
colDef.steroid        <- colDef(name = "Steroid (mg/day)"    , aggregate = "unique"   , filterable = TRUE)
colDef.cauliflower    <- colDef(name = "Cauliflower"         , aggregate = "unique"   , filterable = TRUE)
colDef.tomatoes       <- colDef(name = "Tomatoes"            , aggregate = "unique"   , filterable = TRUE)
colDef.broccoli       <- colDef(name = "Broccoli"            , aggregate = "unique"   , filterable = TRUE)
colDef.med_type       <- colDef(name = "Type"                , aggregate = "unique"   , filterable = TRUE)
colDef.med_name       <- colDef(name = "Name"                , aggregate = "unique"   )
colDef.med_dose       <- colDef(name = "Dose (mg/day)"       , aggregate = "unique"   )
colDef.enclosure      <- colDef(name = "Old or New Enclosure", aggregate = "unique"   , filterable = TRUE)
colDef.access         <- colDef(name = "Breeding Access"     , aggregate = "unique"   , filterable = TRUE)
colDef.estrus         <- colDef(name = "In Estrus"           , aggregate = "unique"   , filterable = TRUE)
colDef.pregnant       <- colDef(name = "Pregnant"            , aggregate = "unique"   , filterable = TRUE)
colDef.bristol_mean   <- colDef(name = "Mean Bristol Score"  , aggregate = "mean"     )
colDef.health_note    <- colDef(name = "Health"              , aggregate = "unique"   )
colDef.repro_note     <- colDef(name = "Reproduction"        , aggregate = "unique"   )
colDef.SampleNotes    <- colDef(name = "Sample Collection"   , aggregate = "unique"   )


colDef.CollectionDate <- colDef(name = "Date Collected", format = colFormat.CollectionDate, sticky = "left")




colDef.depth          <- colDef(name = "Read Count"    , aggregate = "frequency", filterable = TRUE, format = colFormat.depth        , aggregated = aggregated.depth        , html = TRUE, style= style.depth)
colDef.mean_coverage  <- colDef(name = "Mean Taxonomic Coverage", aggregate = "frequency"          , format = colFormat.mean_coverage, aggregated = aggregated.mean_coverage, html = TRUE, style= style.mean_coverage)
