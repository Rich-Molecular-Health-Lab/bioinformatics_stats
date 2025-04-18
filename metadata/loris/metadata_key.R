metadata_variables <- list(
  sample_context = list(
    basics = list(
      identifier        =  "unique ID representing deepest processing ID (SequenceID if available, then ExtractID, then SampleID if sample not extracted yet)",
      study_day         =  "day of sample collection relative to collection start",
      subject           =  "name of subject sample collected from",
      steps_remaining   = "Lab analysis stage for samples as one of (1) collected (poop icon), (2) dna extracted (tube icon), (3) microbiome sequenced (dna icon)."
      ),
   nutrition = list(
     diet_name         = "code name applied to categorize diet trials administered to Culi (Warble is always baseline)",
     total_mg          = "Total weight fed per day (expand cell for daily weight by food item/category)",
     total_kcal        = "total kcal in daily diet"   ,
     total_mg_dry      = "total dry weight of daily diet in mg",
     protein_fed       = "Total weight proteins fed per day (expand cell for daily weight of specific proteins)",  
     fat_fed           = "Total weight fats fed per day (expand cell for daily weight of specific fats)",  
     CHO_fed           = "Total weight carbohydrates fed per day (expand cell for daily weight of specific carbohydrates)",  
     mineral_fed       = "Total weight mineral content fed per day (expand cell for specific daily mineral content)",    
     vitamins          = "Expand cell to view table with estimated vitamin content per day" 
     ),
   supplements = list(
     probiotic         = "Probiotic given at 5 or 10 drops/day",       
     fiber             = "0.5 or 1 capsule Metamucil/day",
     steroid           = "Budesonide given 2x/day at 0.1 or 0.2 mg total",
     antibiotic        = "Metronidazole given 2x/day at 12.5 mg total",
     antidiarrheal     = "Loperamide given 2x/day at 0.08 mg total"
     ),
   other_by_date_subj = list(
     bristol_mean      = "Bristol fecal score recorded that day on scale of 1-6 (mean score for days with > 1 observation)",
     holding           = "Was individual in old or new enclosure?",
     pair_access       = "Did pair have shared enclosure access?", 
     warb_status       = "Was Warble in estrus, pregnant, or anestrus cycle?",
     keeper_note       = "Relevant observations or comments from care staff recorded on that day"
     ),
   subject_info = list(
     Subj_Certainty     =  "Certainty of subject ID recorded for fecal sample (confirmed, unknown, suspected)",  
     Sex                =  "Sex of subject",
     subject_age        =  "Age of subject (in years) on the date of sample collection, relative to birth year in AZA studbook record",
     StudbookID         =  "AZA studbook ID number of subject",
     MotherID           =  "AZA studbook ID number of subject's mother",  
     FatherID           =  "AZA studbook ID number of subject's father",  
     BirthLocation      =  "Institution where subject was born (uses AZA codes)"
     ),
   collection = list(
     SampleID           =  "Unique id for each sample collected",
     CollectionDate     =  "Date of sample collection (yyyy-mm-dd)",     
     SampleSet          =  "Lab code used to identify different samplesets (loris for this one)",
     SampleCollectedBy  =  "identifier for person or group collecting sample",
     SampleNotes        =  "Ad lib notes recorded for sample collection"
     )),
  labwork = list(
    DNAextraction = list(
      ExtractID          =  "Unique id for each (DNA) extract",
      ExtractDate        =  "Date of DNA extraction/purification",  
      ExtractConc        =  "DNA concentration (in ng/\u00b5L) estimated after extraction using Qubit assay and fluorometer",
      ExtractKit         =  "Kit used in DNA extraction/purification",  
      ExtractBox         =  "Identifier for freezer box where sample is located in the Rich Lab",
      ExtractedBy        =  "Person performing DNA extraction/purification", 
      ExtractNotes       =  "Lab notes recorded for DNA extraction/purification procedure"
      ),
    LibraryPrep = list(
      LibPrepDate        =  "date of library preparation",  
      LibPrepWorkflow    =  "lab workflow used for library preparation",      
      LibraryCode        =  "unique id for each pooled library",
      LibPrepKit         =  "ONT kit used for library prep",
      LibraryTube        =  "number applied to tube during Library Prep protocol",
      TemplateVolPrep    =  "volume of extract used in initial step of library prep",
      LibraryBarcode     =  "ONT barcode attached to library before pooling and multiplex sequencing",     
      fragment_type      =  "category applied for calculations during library prep protocol",
      strands            =  "double or single-stranded DNA prepared during library prep protocol",
      Length             =  "target DNA length during library prep protocol",
      InputMassStart     =  "target starting template mass (in ng) during library prep protocol",      
      Conc_QC2           =  "DNA concentration (in ng/\u00b5L) estimated at the end of library prep using Qubit assay and fluorometer",
      PoolSamples        =  "were samples barcoded and pooled for multiplexing during library prep?",  
      SampVolPool        =  "volume of sample added to library pool after barcoding during library prep",
      BeadVol            =  "volume of AMPure beads added to pooled library for final cleanup step",
      TotalPoolVol       =  "total volume of pooled library at the end of library prep",
      InputMassFinal     =  "target template mass (in ng) for flow cell loading after library prep"     
      
    ),
    Sequencing = list(
      SequenceID         =  "Unique id for each sequenced profile generated (some samples/extracts have technical replicates with unique IDs)",
      protocol_group_id  =  "unique id for each sequencing run logged by MinKNOW software",  
      SeqRunID           =  "unique id for each sequencing run",  
      SeqDate            =  "date of library sequencing run",
      SeqDateTime        =  "date and time (Central Time Zone) of library sequencing run",
      FlowCellType       = "type of Flow Cell library was sequenced with",   
      FlowCellSerial     = "serial number of flow cell used in sequencing run",      
      FlongleAdapter     =  "ID of adapter if Flongle Flow Cell was used for sequencing",     
      SeqDevice          =  "MinION device used for sequencing (Angel or Spike)"      ,
      reads_unclassified =  "Raw count of reads from sequencing run before filtering and alignment",
      raw_read_count     =  "Raw count of reads from sequencing run before filtering and alignment",
      depth              =  "Raw count of reads after filtering and alignment for each library",
      mean_coverage      =  "Mean proportion of reads that successfully aligned to a reference"
      )
    )
  )

relative_nutrients <- list(
  id_cols = list(
    "identifier"   =  "unique ID representing deepest processing ID (SequenceID if available, then ExtractID, then SampleID if sample not extracted yet)",
    "SampleID"     =  "unique id for each sample collected",
    "ExtractID"    =  "unique id for each (DNA) extract",
    "SequenceID"   =  "unique id for each sequencing run per sample",         
    "subject"      =  "name of subject sample collected from"
  ), 
  raw_vals      = list(
    proteins    = list(
      "methionine"                 = "total amount fed per day in mg",
      "taurine"                    = "total amount fed per day in mg",
      "proteins_total"             = "total amount fed per day in mg"
    ),
    fats        = list(
      "omega3"                     = "total amount fed per day in mg",
      "omega6"                     = "total amount fed per day in mg",
      "fats_total"                 = "total amount fed per day in mg"
    ),
    CHOs        = list(
      "ADF"                        = "total amount fed per day in mg",
      "NDF"                        = "total amount fed per day in mg",
      "TDF"                        = "total amount fed per day in mg",
      "crude_fiber"                = "total amount fed per day in mg",
      "starch"                     = "total amount fed per day in mg",
      "CHOs_total"                 = "total amount fed per day in mg"
      ),     
    Ash         = list(
      "calcium"                    = "total amount fed per day in mg",
      "magnesium"                  = "total amount fed per day in mg",
      "phosphorus"                 = "total amount fed per day in mg",
      "potassium"                  = "total amount fed per day in mg",
      "copper"                     = "total amount fed per day in mg",
      "iodine"                     = "total amount fed per day in mg",
      "iron"                       = "total amount fed per day in mg",
      "manganese"                  = "total amount fed per day in mg",
      "zinc"                       = "total amount fed per day in mg",
      "Ash_total"                  = "total amount fed per day in mg"
      ),     
    vitamins    = list(
      "beta_carotene"              = "total amount fed per day in mg",
      "lycopene"                   = "total amount fed per day in mg",
      "choline"                    = "total amount fed per day in mg",
      "folic_acid"                 = "total amount fed per day in mg",
      "vit_B1_thiamin"             = "total amount fed per day in mg",
      "vit_B2_riboflavin"          = "total amount fed per day in mg",
      "vit_B3_niacin"              = "total amount fed per day in mg",
      "vit_B5_pantothenic_acid"    = "total amount fed per day in mg",
      "vit_B6_pyridoxine"          = "total amount fed per day in mg",
      "vit_B7_biotin"              = "total amount fed per day in mg",
      "vit_B12"                    = "total amount fed per day in mg",
      "vit_C"                      = "total amount fed per day in mg",
      "vit_A"                      = "total amount fed per day in mg",
      "vit_D3"                     = "total amount fed per day in mg",
      "vit_E"                      = "total amount fed per day in mg",
      "vit_K"                      = "total amount fed per day in mg"
      ),
    foods       = list(
      "biscuit"                    = "total amount fed per day in mg",
      "gum_arabic"                 = "total amount fed per day in mg",
      "invertebrates"              = "total amount fed per day in mg",
      "protein_rotate"             = "total amount fed per day in mg",
      "seasonal_veggies"           = "total amount fed per day in mg",
      "HDZ_oatgel"                 = "total amount fed per day in mg"
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