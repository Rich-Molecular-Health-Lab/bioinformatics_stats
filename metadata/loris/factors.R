nutrition_factors <- list(
  foods    = c("biscuit", "HDZ_oatgel", "invertebrates",  "protein_rotate", "gum_arabic", "seasonal_veggies"),
  classes  = c("proteins", "fats", "CHOs", "Ash", "vitamins", "total"),
  proteins = c("methionine", "taurine", "total", "proteins_total"),
  fats     = c("omega3", "omega6", "total", "fats_total"),
  CHOs     = c("ADF", "NDF", "TDF", "WSC", "crude_fiber", "starch", "total", "CHOs_total"),
  Ash      = c("calcium", "magnesium", "phosphorus", "potassium", "copper", "iodine", "iron", "manganese", "zinc", "total", "Ash_total"),
  vitamins = c("beta_carotene", "lycopene", "choline", "folic_acid", "vit_B1_thiamin", "vit_B2_riboflavin", "vit_B3_niacin", "vit_B5_pantothenic_acid", "vit_B6_pyridoxine","vit_B7_biotin", "vit_B12", "vit_C", "vit_A", "vit_D3", "vit_E", "vit_K", "total", "vitamins_total"),
  all      = c("methionine", "taurine", "omega3", "omega6", "ADF", "NDF", "TDF", "WSC", "crude_fiber", "starch", "calcium", "magnesium", "phosphorus", "potassium", "copper", "iodine", "iron", "manganese", "zinc","beta_carotene", "lycopene", "choline", "folic_acid", "vit_B1_thiamin", "vit_B2_riboflavin", "vit_B3_niacin", "vit_B5_pantothenic_acid", "vit_B6_pyridoxine","vit_B7_biotin", "vit_B12", "vit_C", "vit_A", "vit_D3", "vit_E", "vit_K", "total")
)

diet_factors <- c(
  "baseline",
  "oatgel",
  "biscuit_elim",
  "lessBug_moreEgg",
  "seasonals",
  "low_lectin",
  "gum36_veg37_invert27",
  "water31_root31_protein19_gum19"
)

rename_diets <- c(
  "Baseline"                         = "baseline",
  "Oat Gel"                          = "oatgel",
  "Biscuit Elimination"              = "biscuit_elim",
  "Less Bugg/More Egg"               = "lessBug_moreEgg",
  "Seasonal Veggies"                 = "seasonals",
  "Low Lectin"                       = "low_lectin",
  "Gum:Veggies:Invertebrates Ratio"  = "gum36_veg37_invert27",
  "Watery:Root:Protein:Gum Ratio"    = "water31_root31_protein19_gum19"
)

rename_foods <- c(
 "Biscuit"                   = "biscuit", 
 "Oat Gel"                   = "HDZ_oatgel", 
 "Invertebrates"             = "invertebrates",  
 "Protein Rotation"          = "protein_rotate", 
 "Gum Arabic"                =  "gum_arabic", 
  "Seasonal Veggie Rotation" = "seasonal_veggies"
)

rename_nutrients <- c(
 "Methionine"                   = "methionine", 
 "Taurine"                      = "taurine", 
 "Omega-3"                      = "omega3", 
 "Omega-6"                      = "omega6", 
 "Acid Detergent Fiber"         = "ADF", 
 "Neutral Detergent Fiber"      = "NDF", 
 "Total Dietary Fiber"          = "TDF", 
 "Water-Soluble Carbohydrates"  = "WSC", 
 "Crude Fiber"                  = "crude_fiber", 
 "Starch"                       = "starch", 
 "Calcium (Ca)"                 = "calcium", 
 "Magnesium (Mg)"               = "magnesium", 
 "Phosphorus (P)"               = "phosphorus", 
 "Potassium (K)"                = "potassium",
 "Copper (Cu)"                  = "copper", 
 "Iodine (I)"                   = "iodine", 
 "Iron (Fe)"                    = "iron", 
 "Manganese (Mn)"               = "manganese", 
 "Zinc (Zn)"                    = "zinc",
 "Beta-Carotene"                = "beta_carotene", 
 "Lycopene"                     = "lycopene", 
 "Choline"                      = "choline", 
 "Folic Acid (Vitamin B9)"      = "folic_acid",
 "Vitamin B1 (Thiamin)"         = "vit_B1_thiamin", 
 "Vitamin B2 (Riboflavin)"      = "vit_B2_riboflavin", 
 "Vitamin B3 (Niacin)"          = "vit_B3_niacin", 
 "Vitamin B5 (Pantothenic Acid)"= "vit_B5_pantothenic_acid", 
 "Vitamin B6 (Pyridoxine)"      = "vit_B6_pyridoxine",
 "Vitamin B7 (Biotin)"          = "vit_B7_biotin", 
 "Vitamin B12 (Cobalamin)"      = "vit_B12", 
 "Vitamin C (Ascorbic Acid)"    = "vit_C", 
 "Vitamin A"                    = "vit_A", 
 "Vitamin D3 (Cholecalciferol)" = "vit_D3", 
 "Vitamin E"                    = "vit_E", 
 "Vitamin K"                    = "vit_K", 
  "Total"                       = "total"
)

ordered_variables = c(
  "study_day",
  "subject",
  "identifier",
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
  "SampleID",
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

summary_variables <- c(
  "study_day",
  "CollectionDate",
  "Subj_Certainty",
  "subject",
  "identifier",
  "steps_remaining",
  "diet_display",
  "total_mg",
  "protein_fed",
  "fat_fed",
  "CHO_fed",
  "mineral_fed",
  "vitamins",
  "probiotic",
  "fiber",
  "steroid",
  "antibiotic",
  "antidiarrheal",
  "holding",
  "warb_status",
  "pair_access",
  "keeper_note",
  "Sex",
  "subject_age",
  "StudbookID",
  "SampleID",
  "SampleNotes",
  "ExtractID",
  "ExtractDate",
  "ExtractConc",
  "ExtractKit",
  "ExtractBox",
  "ExtractNotes",
  "SequenceID",
  "LibPrepDate",
  "LibPrepWorkflow",
  "LibraryCode",
  "LibPrepKit",
  "LibraryTube",
  "LibraryBarcode",
  "Conc_QC2",
  "SeqDate",
  "FlowCellType",
  "SeqDevice",
  "reads_unclassified",
  "icon_steps_remaining",
  "color_steps_remaining",
  "num_holding",
  "num_warb_status",
  "icon_Subj_Certainty",
  "color_Subj_Certainty",
  "icon_pair_access",
  "color_pair_access",
  "icon_subject",
  "color_subject",
  "num_diet",
  "icon_vitamins"
)
































































factor_cols_upper <- c(
  "StudbookID",
  "MotherID",
  "FatherID",
  "BirthLocation",
  "SampleID",
  "SequenceID",
  "LibraryCode",
  "ExtractID",
  "ExtractKit",
  "Sex",
  "pair_access",
  "identifier"
)

factor_cols <- c(
  "subject",
  "warb_status",
  "LibPrepKit",
  "FlowCellType",
  "SeqDevice",
  "diet_name"
)
