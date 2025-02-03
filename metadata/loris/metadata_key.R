metadata_variables <- list(
  sample_context = list(
    basics = list(
      study_day         =  "day of sample collection relative to collection start",
      subject           =  "name of subject sample collected from",
      SampleID          =  "unique id for each sample collected",
      steps_remaining   = "has DNA sample been extracted/purified, library prepped, sequenced, etc?"
      ),
   nutrition = list(
     diet_name         = "code name applied to categorize diet trials administered to Culi (Warble is always baseline)",
     total_mg          = "total weight of daily diet in mg",
     total_kcal        = "total kcal in daily diet"   ,
     total_mg_dry      = "total dry weight of daily diet in mg",
     foods             = "nested tibble with list of food categories and total mass of each (in mg) for a given diet" ,
     proteins          = "nested tibble with masses and relative proportions of proteins in diet" ,  
     fats              = "nested tibble with masses and relative proportions of fats in diet" ,  
     CHOs              = "nested tibble with masses and relative proportions of carbohydrates in diet" ,  
     Ash               = "nested tibble with masses and relative proportions of mineral content in diet" ,    
     vitamins          = "nested tibble with masses and relative proportions of vitamin content in diet" 
     ),
   supplements = list(
     probiotic         = "probiotic supplement administered, represented as proportion of max daily dose over study period (low dose = 5 drops, high dose = 10 drops)",       
     fiber             = "fiber supplement (Metamucil) administered, represented as proportion of max daily dose over study period (low dose = .5 capsule, high dose = 1 capsule)",
     steroid           = "steroid rx (Budesonide) administered, represented as proportion of max daily dose over study period (low dose = .1 mg, high dose = .2 mg)",
     antibiotic        = "antibiotic rx (Metronidazole) administered, represented as proportion of max daily dose over study period (only dose given was 12.5 mg)",
     antidiarrheal     = "antidiarrheal rx (Loperamide) administered, represented as proportion of max daily dose over study period (only dose given was 0.08 mg)"
     ),
   other_by_date_subj = list(
     holding           = "was subject still in the old enclosure or had they transferred to new enclosure? (old/new)",
     pair_access       = "did the pair have complete physical access via shared enclosure? (y/n)", 
     warb_status       = "was warble currently in estrus or pregnant? (estrus/pregnant/anestrus)",
     keeper_note       = "ad lib notes from keepers or direct observations matched by date of note and relevant subject"
     ),
   subject_info = list(
     Subj_Certainty     =  "whether subject identity is confirmed, unknown, suspected",  
     Sex                =  "Sex of subject",
     subject_age        =  "age of subject (in years) on the date of sample collection, relative to birth year in AZA studbook record",
     StudbookID         =  "AZA studbook ID number of subject",
     MotherID           =  "AZA studbook ID number of subject's mother",  
     FatherID           =  "AZA studbook ID number of subject's father",  
     BirthLocation      =  "Institution where subject was born (uses AZA codes)"
     ),
   collection = list(
     CollectionDate     =  "date of sample collection (yyyy-mm-dd)",     
     SampleSet          =  "lab code used to identify different samplesets (loris for this one)",
     SampleCollectedBy  =  "identifier for person or group collecting sample",
     SampleNotes        =  "ad lib notes recorded for sample collection"
     )),
  labwork = list(
    DNAextraction = list(
      ExtractID          =  "unique id for each (DNA) extract",
      ExtractDate        =  "date of DNA extraction/purification",  
      ExtractConc        =  "DNA concentration (in ng/ul) estimated after extraction using Qubit assay and fluorometer",
      ExtractKit         =  "kit used in DNA extraction/purification",  
      ExtractBox         =  "identifier for freezer box where sample is located in the Rich Lab",
      ExtractedBy        =  "person performing DNA extraction/purification", 
      ExtractNotes       =  "ad lib notes recorded for DNA extraction/purification"
      ),
    Sequencing = list(
      SequenceID         =  "unique id for each sequencing run per sample",
      LibPrepDate        =  "date of library preparation",  
      LibPrepWorkflow    =  "lab workflow used for library preparation",      
      LibraryCode        =  "unique id for each pooled library",  
      protocol_group_id  =  "unique id for each sequencing run logged by MinKNOW software",  
      LibPrepKit         =  "ONT kit used for library prep",
      LibraryTube        =  "number applied to tube during Library Prep protocol",
      TemplateVolPrep    =  "volume of extract used in initial step of library prep",
      LibraryBarcode     =  "ONT barcode attached to library before pooling and multiplex sequencing",     
      fragment_type      =  "category applied for calculations during library prep protocol",
      strands            =  "double or single-stranded DNA prepared during library prep protocol",
      Length             =  "target DNA length during library prep protocol",
      InputMassStart     =  "target starting template mass (in ng) during library prep protocol",      
      Conc_QC2           =  "DNA concentration (in ng/ul) estimated at the end of library prep using Qubit assay and fluorometer",
      PoolSamples        =  "were samples barcoded and pooled for multiplexing during library prep?",  
      SampVolPool        =  "volume of sample added to library pool after barcoding during library prep",
      BeadVol            =  "volume of AMPure beads added to pooled library for final cleanup step",
      TotalPoolVol       =  "total volume of pooled library at the end of library prep",
      InputMassFinal     =  "target template mass (in ng) for flow cell loading after library prep",      
      SeqRunID           =  "unique id for each sequencing run",  
      SeqDate            =  "date of library sequencing run",
      SeqDateTime        =  "date and time (Central Time Zone) of library sequencing run",
      FlowCellType       = "type of Flow Cell library was sequenced with",   
      FlowCellSerial     = "serial number of flow cell used in sequencing run",      
      FlongleAdapter     =  "ID of adapter if Flongle Flow Cell was used for sequencing",     
      SeqDevice          =  "MinION device used for sequencing (Angel or Spike)"      ,
      reads_unclassified =  "raw count of unclassified reads after post-sequencing basecalling performed"
      )
    )
  )

nested_metadata <- list(
  foods    = list(
    columns = list(
      food   = "name of food category", 
      mg_fed = "daily mass in diet (mg)"
      ),
    rows = list(
      )
    ),
  proteins = list(
    columns = list(
      nutrient      = "name of protein (total is overall protein value)",
      fed           = "amount in daily diet as raw mass",
      fed_unit      = "unit used to report raw mass",
      relative_fed  = "amount in daily diet relative to overall diet or body mass",
      relative_unit = "transformation or unit applied to report relative amount"
      ),
    rows = list(
      total      = "overall protein values",
      methionine = "Methionine",
      taurine    = "Taurine"
      )
    ),
  fats     = list(
    columns = list(
      nutrient      = "name of protein (total is overall protein value)",
      fed           = "amount in daily diet as raw mass",
      fed_unit      = "unit used to report raw mass",
      relative_fed  = "amount in daily diet relative to overall diet or body mass",
      relative_unit = "transformation or unit applied to report relative amount"
    ),
    rows    = list(
      total      = "overall fat values",
      omega3     = "Omega3",
      omega6     = "Omega6"
    )
  ),
  CHOs     = list(
    columns = list(
      nutrient      = "name of protein (total is overall protein value)",
      fed           = "amount in daily diet as raw mass",
      fed_unit      = "unit used to report raw mass",
      relative_fed  = "amount in daily diet relative to overall diet or body mass",
      relative_unit = "transformation or unit applied to report relative amount"
    ),
    rows    = list(
      total       = "overall carbohydrate values",
      ADF         = "Acid Detergent Fiber",
      NDF         = "Neutral Detergent Fiber",
      TDF         = "Total Dietary Fiber",
      WSC         = "Water-Soluble Carbohydrates",
      crude_fiber = "Crude Fiber",
      starch      = "Starch"
    )
  ),
  Ash      = list(
    columns = list(
      nutrient      = "name of protein (total is overall protein value)",
      fed           = "amount in daily diet as raw mass",
      fed_unit      = "unit used to report raw mass",
      relative_fed  = "amount in daily diet relative to overall diet or body mass",
      relative_unit = "transformation or unit applied to report relative amount"
    ),
    rows    = list(
      total       = "overall mineral values",
      calcium     = "Calcium",
      copper      = "Copper",
      iodine      = "Iodine",
      iron        = "Iron",
      magnesium   = "Magnesium",
      manganese   = "Manganese",
      phosphorus  = "Phosphorus",
      potassium   = "Potassium",
      zinc        = "Zinc"
    )
  ),
  vitamins = list(
    columns = list(
      nutrient      = "name of protein (total is overall protein value)",
      fed           = "amount in daily diet as raw mass",
      fed_unit      = "unit used to report raw mass",
      relative_fed  = "amount in daily diet relative to overall diet or body mass",
      relative_unit = "transformation or unit applied to report relative amount"
    ),
    rows    = list(
      total                   = "overall vitamin values",
      beta_carotene           = "Beta-Carotene",
      choline                 = "Choline",
      folic_acid              = "Folic Acid (Vitamin B9)",
      lycopene                = "Lycopene",
      vit_A                   = "Vitamin A (Retinol, Retinal, Retinoic Acid)",
      vit_B12                 = "Vitamin B12 (Cobalamin)",
      vit_B1_thiamin          = "Vitamin B1 (Thiamin)",
      vit_B2_riboflavin       = "Vitamin B2 (Riboflavin)",
      vit_B3_niacin           = "Vitamin B3 (Niacin)",
      vit_B5_pantothenic_acid = "Vitamin B5 (Pantothenic Acid)",
      vit_B6_pyridoxine       = "Vitamin B6 (Pyridoxine)",
      vit_B7_biotin           = "Vitamin B7 (Biotin)",
      vit_C                   = "Vitamin C (Ascorbic Acid)",
      vit_D3                  = "Vitamin D3 (Cholecalciferol)",
      vit_E                   = "Vitamin E (Tocopherols & Tocotrienols)",
      vit_K                   = "Vitamin K (Phylloquinone & Menaquinone)"
      )
  )
)

nutrition_details <- list(
  proteins = list(
    methionine = list(name = "Methionine"   , context = list("An essential amino acid (must be obtained from diet).", "Important for protein synthesis, liver function, and antioxidant pathways","Found in eggs, fish, meat, and some legumes.")),
    taurine    = list(name = "Taurine"      , context = list("A conditionally essential amino acid for some species", "Critical for eye, heart, and nervous system function", "Naturally found in animal tissues (meat, fish, and insects)—not in plants", "Some species (like strict carnivores) cannot synthesize it efficiently and require dietary supplementation."))
  ),
  fats     = list(
    omega3     = list(name = "Omega-3 Fatty Acids"   , context = list("A class of polyunsaturated fats including ALA (alpha-linolenic acid), EPA (eicosapentaenoic acid), and DHA (docosahexaenoic acid)", "Important for brain function, anti-inflammatory processes, and cardiovascular health", "Found in fish, algae, flaxseeds, and some nuts", "Essential in many mammalian diets for neurological and immune functions")),
    omega6     = list(name = "Omega-6 Fatty Acids"   , context = list("Another class of polyunsaturated fats, including LA (linoleic acid) and AA (arachidonic acid).", "Important for skin health, immune response, and cell membrane function.", "Found in plant oils (soybean, corn, sunflower) and animal fats.", "Omega-6 and omega-3 should be balanced, as excess omega-6 can promote inflammation."))
  ),
  CHOs     = list(
    ADF         = list(name = "Acid Detergent Fiber"       , context = list("Measures cellulose and lignin (the least digestible plant components)", "Important for evaluating forage quality—higher ADF means lower digestibility.")),
    NDF         = list(name = "Neutral Detergent Fiber"    , context = list("Includes hemicellulose, cellulose, and lignin (total plant fiber)", "Indicates bulkiness and gut fill—higher NDF means lower voluntary intake.")),
    TDF         = list(name = "Total Dietary Fiber"        , context = list("The sum of soluble and insoluble fiber in food", "Includes cellulose, hemicellulose, pectins, gums, and resistant starch.")),
    WSC         = list(name = "Water-Soluble Carbohydrates", context = list("Includes simple sugars (glucose, fructose, sucrose) and fructans", "Important for fermentation, energy metabolism, and insulin response.")),
    crude_fiber = list(name = "Crude Fiber"                , context = list("An older method of fiber analysis measuring cellulose and lignin, but underestimates total fiber.")),
    starch      = list(name = "Starch"                     , context = list("A digestible polysaccharide providing a rapid energy source", "Important for balancing energy intake in different species (e.g., ruminants vs. omnivores)."))
  ),
  Ash      = list(
    "Macrominerals"  = list(
      calcium     = list(name = "Calcium (Ca)"   , context = list("Essential for bone health, muscle function, and nerve signaling.", "Found in dairy, leafy greens, bones (in whole prey), and eggshells.", "Calcium-to-phosphorus (Ca:P) ratio is critical—imbalances can cause metabolic bone disease (MBD).")),
      magnesium   = list(name = "Magnesium (Mg)" , context = list("Supports muscle and nerve function, enzyme activation, and bone structure.", "Found in nuts, seeds, whole grains, and dark leafy greens.", "Deficiency can cause muscle cramps, weakness, and metabolic issues.")),
      phosphorus  = list(name = "Phosphorus (P)" , context = list("Works with calcium in bone health and energy metabolism (ATP production).", "Found in meat, fish, nuts, and dairy.", "Excess phosphorus with low calcium can lead to bone disorders (common in meat-heavy diets).")),
      potassium   = list(name = "Potassium (K)"  , context = list("Regulates fluid balance, muscle contractions, and nerve signals.", "Found in bananas, potatoes, beans, and leafy greens.", "Deficiency can cause muscle weakness and heart issues."))
    ),
    "Trace Minerals" = list(
      copper      = list(name = "Copper (Cu)"    , context = list("Essential for iron metabolism, connective tissue formation, and immune function.", "Found in organ meats, shellfish, nuts, and seeds.", "Excess copper can be toxic, especially in species prone to copper accumulation (e.g., some primates and dogs).")),
      iodine      = list(name = "Iodine (I)"     , context = list("Crucial for thyroid hormone production (supports metabolism and growth).", "Found in seafood, seaweed, dairy, and iodized salt.", "Deficiency leads to goiter and thyroid dysfunction.")),
      iron        = list(name = "Iron (Fe)"      , context = list("Essential for oxygen transport (hemoglobin), energy metabolism, and immune function.", "Found in red meat, liver, legumes, and dark leafy greens.", "Heme iron (from animal sources) is more bioavailable than non-heme iron (from plants).", "Excess iron can cause oxidative stress, especially in species adapted to low-iron diets (e.g., some bats and primates).")),
      manganese   = list(name = "Manganese (Mn)" , context = list("Needed for bone development, antioxidant function, and metabolism.", "Found in nuts, whole grains, tea, and leafy greens.", "Deficiency is rare but can affect bone health and enzyme function.")),
      zinc        = list(name = "Zinc (Zn)"      , context = list("Supports immune function, wound healing, and enzyme activity.", "Found in meat, shellfish, legumes, and seeds.", "Deficiency can cause skin lesions, slow healing, and immune suppression."))
    )
  ),
  vitamins = list(
    "Carotenoids & Vitamin Precursors"       = list(
      beta_carotene           = list(name = "Beta-Carotene", context = list("A provitamin A carotenoid, meaning it can be converted into vitamin A in some species.", "Found in carrots, sweet potatoes, and leafy greens.", "Not all species efficiently convert it—carnivores like cats require preformed vitamin A.")),
      lycopene                = list(name = "Lycopene"     , context = list("A carotenoid antioxidant with no vitamin A activity but protective against oxidative stress.", "Found in tomatoes, red peppers, and watermelon.", "Important for skin health and cellular protection."))
    ),
    "Water-Soluble Vitamins (B-Complex & C)" = list(
      choline                 = list(name = "Choline"                      , context = list("Not technically a vitamin, but often grouped with B vitamins.", "Essential for nerve signaling, brain development, and liver function.", "Found in eggs, liver, and soybeans.")),
      folic_acid              = list(name = "Folic Acid (Vitamin B9)"      , context = list("Essential for DNA synthesis, cell division, and fetal development.", "Found in leafy greens, legumes, and liver.")),
      vit_B1_thiamin          = list(name = "Vitamin B1 (Thiamin)"         , context = list("Required for carbohydrate metabolism and nerve function.", "Found in whole grains, pork, and legumes.", "Deficiency can cause neurological issues, particularly in high-fish diets (thiaminase risk).")),
      vit_B2_riboflavin       = list(name = "Vitamin B2 (Riboflavin)"      , context = list("Important for energy production and antioxidant defense.", "Found in dairy, eggs, and lean meats.")),
      vit_B3_niacin           = list(name = "Vitamin B3 (Niacin)"          , context = list("Supports metabolism and DNA repair.", "Found in meat, fish, and whole grains.", "Some species (e.g., cats) require preformed niacin, as they cannot synthesize it efficiently.")),
      vit_B5_pantothenic_acid = list(name = "Vitamin B5 (Pantothenic Acid)", context = list("Essential for coenzyme A (CoA) synthesis, involved in fat and carbohydrate metabolism.", "Found in meat, eggs, and whole grains.")),
      vit_B6_pyridoxine       = list(name = "Vitamin B6 (Pyridoxine)"      , context = list("Important for protein metabolism and neurotransmitter function.", "Found in bananas, poultry, and fish.")),
      vit_B7_biotin           = list(name = "Vitamin B7 (Biotin)"          , context = list("Supports fat metabolism, skin, and coat health.", "Found in eggs, nuts, and liver.", "Raw egg whites contain avidin, which inhibits biotin absorption.")),
      vit_B12                 = list(name = "Vitamin B12 (Cobalamin)"      , context = list("Essential for nerve function and red blood cell production.", "Found only in animal products (meat, eggs, dairy).", "Strict herbivores may need supplementation.")),
      vit_C                   = list(name = "Vitamin C (Ascorbic Acid)"    , context = list("Antioxidant and collagen synthesis vitamin.", "Found in fruits and vegetables (citrus, bell peppers).", "Most mammals synthesize vitamin C, but primates, guinea pigs, and some bats require dietary sources."))
      
    ),
    "Fat-Soluble Vitamins (A, D, E, K)"      = list(
      vit_A                   = list(name = "Vitamin A (Retinol, Retinal, Retinoic Acid)", context = list("Crucial for vision, immune function, and reproduction.", "Found in liver, fish oil, and dairy.", "Toxic in excess, especially in species that accumulate vitamin A (e.g., cats).")),
      vit_D3                  = list(name = "Vitamin D3 (Cholecalciferol)"               , context = list("Regulates calcium and phosphorus metabolism.", "Synthesized in skin via UVB exposure or obtained from fatty fish, liver, and egg yolks.", "Some nocturnal species may require dietary vitamin D3 supplementation.")),
      vit_E                   = list(name = "Vitamin E (Tocopherols & Tocotrienols)"     , context = list("A powerful antioxidant that protects cell membranes.", "Found in nuts, seeds, and plant oils.", "Deficiency can lead to muscle degeneration and immune dysfunction.")),
      vit_K                   = list(name = "Vitamin K (Phylloquinone & Menaquinone)"    , context = list("Essential for blood clotting and bone metabolism.", "Found in leafy greens (K1) and fermented foods (K2).", "Some gut bacteria produce K2, so deficiencies are rare but can occur with certain medications."))
    )
  )
)

ordered_variables = c(
    "study_day",
    "subject",
    "SampleID",
    "steps_remaining",
    "diet_name",
    "total_mg",
    "total_kcal",
    "total_mg_dry",
    "foods",
    "proteins",
    "fats",
    "CHOs",
    "Ash",
    "vitamins",
    "probiotic",
    "fiber",
    "steroid",
    "antibiotic",
    "antidiarrheal",
    "holding",
    "pair_access",
    "warb_status",
    "keeper_note",
    "Subj_Certainty",
    "Sex",
    "subject_age",
    "StudbookID",
    "MotherID",
    "FatherID",
    "BirthLocation",
    "CollectionDate",
    "SampleSet",
    "SampleCollectedBy",
    "SampleNotes",
    "ExtractID",
    "ExtractDate",
    "ExtractConc",
    "ExtractKit",
    "ExtractBox",
    "ExtractedBy",
    "ExtractNotes",
    "SequenceID",
    "LibPrepDate",
    "LibPrepWorkflow",
    "LibraryCode",
    "protocol_group_id",
    "LibPrepKit",
    "LibraryTube",
    "TemplateVolPrep",
    "LibraryBarcode",
    "fragment_type",
    "strands",
    "Length",
    "InputMassStart",
    "Conc_QC2",
    "PoolSamples",
    "SampVolPool",
    "BeadVol",
    "TotalPoolVol",
    "InputMassFinal",
    "SeqRunID",
    "SeqDate",
    "SeqDateTime",
    "FlowCellType",
    "FlowCellSerial",
    "FlongleAdapter",
    "SeqDevice",
    "reads_unclassified"
)

nested_colDefs <- list(
  food              = colDef(name = "Food Items"),
  mg_fed            = colDef(name = "mg per Day"),
  nutrient          = colDef(name = "Category"),
  fed               = colDef(name = "Weight"),
  fed_unit          = colDef(name = "Units"),
  relative_fed      = colDef(name = "Relative Amount"),
  relative_unit     = colDef(name = "Relative Metric")
)
nested_colGroups <- list(
  amount_day  = colGroup(name = "Fed per Day", columns = c("fed", "fed_unit", "relative_fed", "relative_unit"))
)


nested_tbls <- list(
  foods = colDef(name = "Foods", 
                 details = function(index) {
    foods_tbl <- metadata$foods[[index]]
    
    if (inherits(foods_tbl, "tbl_df") && nrow(foods_tbl) > 0) {
      return(reactable(foods_tbl, 
                       columns = list(
                         food   = nested_colDefs$food   ,
                         mg_fed = nested_colDefs$mg_fed 
                       ),
                       fullWidth = FALSE, 
                       compact   = TRUE, 
                       bordered  = TRUE, 
                       striped   = TRUE))
    }
  }),
  proteins = colDef(name = "Proteins",
                    details = function(index) {
    proteins_tbl <- metadata$proteins[[index]]
    
    if (inherits(proteins_tbl, "tbl_df") && nrow(proteins_tbl) > 0) {
      return(reactable(proteins_tbl, 
                       columns = list(
                         nutrient     = nested_colDefs$nutrient     ,
                         fed          = nested_colDefs$fed          ,
                         fed_unit     = nested_colDefs$fed_unit     ,
                         relative_fed = nested_colDefs$relative_fed ,
                         relative_unit= nested_colDefs$relative_unit
                       ),
                       columnGroups = nested_colGroups$amount_day,
                       fullWidth = FALSE, 
                       compact   = TRUE, 
                       bordered  = TRUE, 
                       striped   = TRUE))
    }
  }),
  fats = colDef(name = "Fats",
                details = function(index) {
    fats_tbl <- metadata$fats[[index]]
    
    if (inherits(fats_tbl, "tbl_df") && nrow(fats_tbl) > 0) {
      return(reactable(fats_tbl, 
                       columns = list(
                         nutrient     = nested_colDefs$nutrient     ,
                         fed          = nested_colDefs$fed          ,
                         fed_unit     = nested_colDefs$fed_unit     ,
                         relative_fed = nested_colDefs$relative_fed ,
                         relative_unit= nested_colDefs$relative_unit
                       ),
                       columnGroups = nested_colGroups$amount_day,
                       fullWidth = FALSE, 
                       compact   = TRUE, 
                       bordered  = TRUE, 
                       striped   = TRUE))
    }
  }),
  
  CHOs = colDef(name = "Carbohydrates",
                details = function(index) {
    CHOs_tbl <- metadata$CHOs[[index]]
    
    if (inherits(CHOs_tbl, "tbl_df") && nrow(CHOs_tbl) > 0) {
      return(reactable(CHOs_tbl, 
                       columns = list(
                         nutrient     = nested_colDefs$nutrient     ,
                         fed          = nested_colDefs$fed          ,
                         fed_unit     = nested_colDefs$fed_unit     ,
                         relative_fed = nested_colDefs$relative_fed ,
                         relative_unit= nested_colDefs$relative_unit
                       ),
                       columnGroups = nested_colGroups$amount_day,
                       fullWidth = FALSE, 
                       compact   = TRUE, 
                       bordered  = TRUE, 
                       striped   = TRUE))
    }
  }),
  
    vitamins = colDef(name = "Vitamins",
                      details = function(index) {
    vitamins_tbl <- metadata$vitamins[[index]]
    
    if (inherits(vitamins_tbl, "tbl_df") && nrow(vitamins_tbl) > 0) {
      return(reactable(vitamins_tbl, 
                       columns = list(
                         nutrient     = nested_colDefs$nutrient     ,
                         fed          = nested_colDefs$fed          ,
                         fed_unit     = nested_colDefs$fed_unit     ,
                         relative_fed = nested_colDefs$relative_fed ,
                         relative_unit= nested_colDefs$relative_unit
                       ),
                       columnGroups = nested_colGroups$amount_day,
                       fullWidth = FALSE, 
                       compact   = TRUE, 
                       bordered  = TRUE, 
                       striped   = TRUE))
    }
  })
)

colDefs <- list(
  study_day         = colDef(name = "Study Day"),
  subject           = colDef(name = "Subject"),
  SampleID          = colDef(name = "Sample ID"),
  steps_remaining   = colDef(name = "Analysis Stage"),
  diet_name         = colDef(name = "Diet Phase"),
  total_mg          = colDef(name = "Mass (in mg)"),
  total_kcal        = colDef(name = "kcal"),
  total_mg_dry      = colDef(name = "Dry Mass (in mg)"),
  foods             = nested_tbls$foods   ,
  proteins          = nested_tbls$proteins,
  fats              = nested_tbls$fats    ,
  CHOs              = nested_tbls$CHOs    ,
  Ash               = nested_tbls$Ash     ,
  vitamins          = nested_tbls$vitamins,
  probiotic         = colDef(name = "Probiotic"),
  fiber             = colDef(name = "Fiber"),
  steroid           = colDef(name = "Steroid"),
  antibiotic        = colDef(name = "Antibiotic"),
  antidiarrheal     = colDef(name = "Antidiarrheal"),
  holding           = colDef(name = "Holding Facility"),
  pair_access       = colDef(name = "Full Pair Access"),
  warb_status       = colDef(name = "Warble's Repro Status"),
  keeper_note       = colDef(name = "Keeper Notes"),
  Subj_Certainty    = colDef(name = "ID Certainty"),
  Sex               = colDef(name = "Sex"),
  subject_age       = colDef(name = "Age (years) on Collection Date"),
  StudbookID        = colDef(name = "AZA Studbook ID"),
  MotherID          = colDef(name = "Mother's AZA Studbook ID"),
  FatherID          = colDef(name = "Father's AZA Studbook ID"),
  BirthLocation     = colDef(name = "Birth Institution (AZA Code)"),
  CollectionDate    = colDef(name = "Collection Date"),
  SampleSet         = colDef(name = "Sample Set Code"),
  SampleCollectedBy = colDef(name = "Collected by"),
  SampleNotes       = colDef(name = "Collection Notes"),
  ExtractID         = colDef(name = "ID"),
  ExtractDate       = colDef(name = "Date"),
  ExtractConc       = colDef(name = "DNA Concentration (ng/ul)"),
  ExtractKit        = colDef(name = "Kit"),
  ExtractBox        = colDef(name = "Storage Box"),
  ExtractedBy       = colDef(name = "Performed by"),
  ExtractNotes      = colDef(name = "Notes"),
  SequenceID        = colDef(name = "Sequenced Profile ID"),
  LibPrepDate       = colDef(name = "Date"),
  LibPrepWorkflow   = colDef(name = "Protocol"),
  LibraryCode       = colDef(name = "Pooled Library ID"),
  protocol_group_id = colDef(name = "Sequencing Run ID (MinKNOW)"),
  LibPrepKit        = colDef(name = "Kit"),
  LibraryTube       = colDef(name = "Tube"),
  TemplateVolPrep   = colDef(name = "Extract Volume Added (ul)"),
  LibraryBarcode    = colDef(name = "Barcode ID"),
  fragment_type     = colDef(name = "Fragment Size Code"),
  strands           = colDef(name = "ds or ssDNA"),
  Length            = colDef(name = "Target Length"),
  InputMassStart    = colDef(name = "Starting Target Mass (ng)"),
  Conc_QC2          = colDef(name = "Final Library Concentration (ng/ul)"),
  PoolSamples       = colDef(name = "Samples Pooled?"),
  SampVolPool       = colDef(name = "Volume added to Pool"),
  BeadVol           = colDef(name = "Volume AMPure Beads to Pool"),
  TotalPoolVol      = colDef(name = "Total Pool Volume (ul)"),
  InputMassFinal    = colDef(name = "Final Target Mass (ng)"),
  SeqRunID          = colDef(name = "ID"),
  SeqDate           = colDef(name = "Date"),
  SeqDateTime       = colDef(name = "Date and Time"),
  FlowCellType      = colDef(name = "Flow Cell Type"),
  FlowCellSerial    = colDef(name = "Flow Cell Serial"),
  FlongleAdapter    = colDef(name = "Flongle Adapter Serial"),
  SeqDevice         = colDef(name = "MinION Device"),
  reads_unclassified= colDef(name = "Unclassified Read Count")
)

colGroups <- list(
  basics      = colGroup(name = "Samples"                                     , columns = c("study_day", "subject", "SampleID"), sticky = "left"),
  diet_totals = colGroup(name = "Dietary Total per Day"                       , columns = c("total_mg", "total_kcal", "total_mg_dry")),
  diet_nested = colGroup(name = "Detailed Dietary Totals per Day"             , columns = c("foods", "proteins", "fats", "CHOs", "Ash", "vitamins")),
  supplements = colGroup(name = "Meds/Supplements (as Proportion of Max Dose)", columns = c("probiotic", "fiber", "steroid", "antibiotic", "antidiarrheal")),
  subject     = colGroup(name = "Subject Info"                                , columns = c("Subj_Certainty"  , "Sex", "subject_age", "StudbookID", "MotherID", "FatherID", "BirthLocation")),
  collection  = colGroup(name = "Sample Collection"                           , columns = c(
    "CollectionDate",
    "SampleSet",
    "SampleCollectedBy",
    "SampleNotes")),
  extraction  = colGroup(name = "DNA Extraction/Purification", columns = c(
    "ExtractID",
    "ExtractDate",
    "ExtractConc",
    "ExtractKit",
    "ExtractBox",
    "ExtractedBy",
    "ExtractNotes")),
  libraryprep = colGroup(name = "Library Prep Procedures", columns = c(
    "LibPrepDate",
    "LibPrepWorkflow",
    "LibraryCode",
    "protocol_group_id",
    "LibPrepKit",
    "LibraryTube",
    "TemplateVolPrep",
    "LibraryBarcode",
    "fragment_type",
    "strands",
    "Length",
    "InputMassStart",
    "Conc_QC2",
    "PoolSamples",
    "SampVolPool",
    "BeadVol",
    "TotalPoolVol"
  )),
  sequencing = colGroup(name = "Sequencing Run", columns = c(
    "InputMassFinal",
    "SeqRunID",
    "SeqDate",
    "SeqDateTime",
    "FlowCellType",
    "FlowCellSerial",
    "FlongleAdapter",
    "SeqDevice",
    "reads_unclassified"
  ))
)

metadata_table <- list(
  columns = list(
    study_day         = colDefs$study_day         , 
    subject           = colDefs$subject           , 
    SampleID          = colDefs$SampleID          , 
    steps_remaining   = colDefs$steps_remaining   , 
    diet_name         = colDefs$diet_name         , 
    total_mg          = colDefs$total_mg          , 
    total_kcal        = colDefs$total_kcal        , 
    total_mg_dry      = colDefs$total_mg_dry      , 
    foods             = colDefs$foods             , 
    proteins          = colDefs$proteins          , 
    fats              = colDefs$fats              , 
    CHOs              = colDefs$CHOs              , 
    Ash               = colDefs$Ash               , 
    vitamins          = colDefs$vitamins          , 
    probiotic         = colDefs$probiotic         , 
    fiber             = colDefs$fiber             , 
    steroid           = colDefs$steroid           , 
    antibiotic        = colDefs$antibiotic        , 
    antidiarrheal     = colDefs$antidiarrheal     , 
    holding           = colDefs$holding           , 
    pair_access       = colDefs$pair_access       , 
    warb_status       = colDefs$warb_status       , 
    keeper_note       = colDefs$keeper_note       , 
    Subj_Certainty    = colDefs$Subj_Certainty    , 
    Sex               = colDefs$Sex               , 
    subject_age       = colDefs$subject_age       , 
    StudbookID        = colDefs$StudbookID        , 
    MotherID          = colDefs$MotherID          , 
    FatherID          = colDefs$FatherID          , 
    BirthLocation     = colDefs$BirthLocation     , 
    CollectionDate    = colDefs$CollectionDate    , 
    SampleSet         = colDefs$SampleSet         , 
    SampleCollectedBy = colDefs$SampleCollectedBy , 
    SampleNotes       = colDefs$SampleNotes       , 
    ExtractID         = colDefs$ExtractID         , 
    ExtractDate       = colDefs$ExtractDate       , 
    ExtractConc       = colDefs$ExtractConc       , 
    ExtractKit        = colDefs$ExtractKit        , 
    ExtractBox        = colDefs$ExtractBox        , 
    ExtractedBy       = colDefs$ExtractedBy       , 
    ExtractNotes      = colDefs$ExtractNotes      , 
    SequenceID        = colDefs$SequenceID        , 
    LibPrepDate       = colDefs$LibPrepDate       , 
    LibPrepWorkflow   = colDefs$LibPrepWorkflow   , 
    LibraryCode       = colDefs$LibraryCode       , 
    protocol_group_id = colDefs$protocol_group_id , 
    LibPrepKit        = colDefs$LibPrepKit        , 
    LibraryTube       = colDefs$LibraryTube       , 
    TemplateVolPrep   = colDefs$TemplateVolPrep   , 
    LibraryBarcode    = colDefs$LibraryBarcode    , 
    fragment_type     = colDefs$fragment_type     , 
    strands           = colDefs$strands           , 
    Length            = colDefs$Length            , 
    InputMassStart    = colDefs$InputMassStart    , 
    Conc_QC2          = colDefs$Conc_QC2          , 
    PoolSamples       = colDefs$PoolSamples       , 
    SampVolPool       = colDefs$SampVolPool       , 
    BeadVol           = colDefs$BeadVol           , 
    TotalPoolVol      = colDefs$TotalPoolVol      , 
    InputMassFinal    = colDefs$InputMassFinal    , 
    SeqRunID          = colDefs$SeqRunID          , 
    SeqDate           = colDefs$SeqDate           , 
    SeqDateTime       = colDefs$SeqDateTime       , 
    FlowCellType      = colDefs$FlowCellType      , 
    FlowCellSerial    = colDefs$FlowCellSerial    , 
    FlongleAdapter    = colDefs$FlongleAdapter    , 
    SeqDevice         = colDefs$SeqDevice         , 
    reads_unclassified= colDefs$reads_unclassified
    ),
  columnGroups = list(
    colGroups$basics     ,
    colGroups$diet_totals,
    colGroups$diet_nested,
    colGroups$supplements,
    colGroups$subject    ,
    colGroups$collection ,
    colGroups$extraction,
    colGroups$libraryprep,
    colGroups$sequencing
  )
)
    