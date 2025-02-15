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

holding_factors <- c(
  "old",
  "new"
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

dose_cols <- c(
  "probiotic",
  "fiber",
  "steroid",
  "antibiotic",
  "antidiarrheal"
)

probiotic_factors <- c(
  "none" = 0.00,
  "low"  = 05.0,
  "max"  = 10.0
)

steroid_factors <- c(
  "none" = 0.00,
  "low"  = 00.1,
  "max"  = 00.2
)

fiber_factors <- c(
  "none" = 0.00,
  "low"  = 00.5,
  "max"  = 01.0
)

antibiotic_factors <- c(
  "none" = 0.00,
  "max"  = 12.5
)

antidiarrheal_factors <- c(
  "none" = 0.00,
  "max"  = 00.08
)


warb_cycle_factors <- c(
  "anestrus",
  "estrus",
  "pregnant"
)

default_factors <- c(
  "day",
  "subject",
  "ExtractKit",
  "ExtractedBy",
  "LibPrepKit",
  "FlowCellType",
  "SeqDevice"
)

steps_remaining_colors <- c(
  "#D2981AFF" = "sample not extracted"          ,
  "#A53E1FFF" = "extract not sequenced"         ,
  "#457277FF" = "sample extracted and sequenced"
)

steps_remaining_icons <- c(
 "poop" = "sample not extracted"           ,
 "vial" = "extract not sequenced"          ,
 "dna"  = "sample extracted and sequenced" 
)


diet_colors <- c(
   "#8C8C8CFF" = "baseline"                      ,
   "#88BDE6FF" = "oatgel"                        ,
   "#FBB258FF" = "biscuit_elim"                  ,
   "#90CD97FF" = "lessBug_moreEgg"               ,
   "#BFA554FF" = "seasonals"                     ,
   "#BC99C7FF" = "low_lectin"                    ,
   "#EDDD46FF" = "gum36_veg37_invert27"          ,
   "#F07E6EFF" = "water31_root31_protein19_gum19"
)

warb_status_colors <- c(
  "#A8A6A7FF" = "anestrus",
  "#B1283AFF" = "estrus",
  "#006A8EFF" = "pregnant"
)

holding_colors <- c(
  "#5773CCFF" = "old",
  "#FFB900FF" = "new"
)

certainty_colors <- c(
  "#CA562CFF" = "no",
  "#B5B991FF" = "yes"
)
certainty_icons <- c(
  "x"     = "no",
  "check" = "yes"
)

pair_access_colors <- c(
  "#E1C473FF" = "n",
  "#2C5724FF" = "y"
)
pair_access_icons <- c(
  "square-minus"     = "no",
  "square-check"     = "yes"
)

subj_colors <- c(
  "#803777FF" = "warble",
  "#216F63FF" = "culi"
)
subj_icons <- c(
  "female"     = "warble",
  "male"       = "culi"
)


alarm_colors        <- c("#CEFF1AFF", "#D8E01BFF", "#DFC11BFF", "#E2A11BFF", "#E37F1BFF", "#E1581AFF", "#DE1A1AFF")
concentration_scale <- c("#FF3200FF", "#E9A17CFF", "#E9E4A6FF", "#1BB6AFFF", "#0076BBFF", "#172869FF")
intake_scale        <- c("#1D457FFF", "#61599DFF", "#C36377FF", "#EB7F54FF", "#F2AF4AFF")

supplement_color <- c("#985A71FF")

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
 "Seasonal Veggie Rotation"  = "seasonal_veggies"
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
  "bristol_min",
  "bristol_max",
  "bristol_mean",
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
  "bristol_mean",
  "diet_name",
  "total_kcal",
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
  "LibraryCode",
  "LibPrepKit",
  "LibraryBarcode",
  "Conc_QC2",
  "SeqDate",
  "FlowCellType",
  "SeqDevice",
  "reads_unclassified",
  "diet_color",
  "icon_steps_remaining",
  "color_steps_remaining",
  "holding_color",
  "warb_status_color",
  "icon_Subj_Certainty",
  "color_Subj_Certainty",
  "icon_pair_access",
  "color_pair_access",
  "icon_subject",
  "color_subject",
  "icon_vitamins"
)

microeco_cols <- c(
  "study_day",
  "subject",
  "identifier",
  "diet_name",
  "total_mg",
  "total_kcal",
  "total_mg_dry",
  "probiotic",
  "fiber",
  "steroid",
  "antibiotic",
  "antidiarrheal",
  "bristol_mean",
  "holding",
  "pair_access",
  "warb_status",
  "keeper_note",
  "Subj_Certainty",
  "Sex",
  "subject_age",
  "SampleID",
  "CollectionDate",
  "SampleNotes",
  "ExtractID",
  "ExtractDate",
  "ExtractConc",
  "ExtractKit",
  "ExtractedBy",
  "ExtractNotes",
  "SequenceID",
  "LibPrepDate",
  "LibraryCode",
  "LibPrepKit",
  "LibraryBarcode",
  "Conc_QC2",
  "SeqDate",
  "FlowCellType",
  "FlowCellSerial",
  "FlongleAdapter",
  "SeqDevice",
  "reads_unclassified",
  "protein_fed",
  "fat_fed",
  "CHO_fed", 
  "mineral_fed"     
)

date.vars <- c(
  "CollectionDate",
  "ExtractDate",
  "LibPrepDate",
  "SeqDate"
)

yn.vars <- c(
  "pair_access",
  "Subj_Certainty"
)

ids <- c(
  "identifier",
  "SampleID",
  "ExtractID",
  "SequenceID",
  "LibraryCode"
)

coverage_table_cols <- c(
  "study_day",
  "Subject",
  "identifier",
  "raw_read_count" = "reads_unclassified",
  "depth",
  "mean_coverage",
  "diet_name",
  "total_mg",
  "total_kcal",
  "total_mg_dry",
  "bristol_mean",
  "protein_fed",
  "fat_fed",
  "CHO_fed", 
  "mineral_fed",
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
  "CollectionDate",
  "SampleID",
  "SampleNotes",
  "ExtractID",
  "ExtractDate",
  "ExtractConc",
  "ExtractKit",
  "ExtractedBy",
  "ExtractNotes",
  "SequenceID",
  "LibPrepDate",
  "LibraryCode",
  "LibPrepKit",
  "LibraryBarcode",
  "Conc_QC2",
  "SeqDate",
  "FlowCellType",
  "FlowCellSerial",
  "FlongleAdapter",
  "SeqDevice"
)

depth_table_cols <- c(
  "study_day",
  "subject",
  "identifier",
  "diet_name",
  "bristol_mean",
  "depth",
  "mean_coverage",
  "ExtractConc",
  "ExtractDate",
  "ExtractedBy",
  "ExtractNotes",
  "LibPrepDate",
  "LibraryCode",
  "Conc_QC2",
  "FlowCellType",
  "SeqDevice",
  "ExtractID",
  "ExtractKit",
  "SequenceID",
  "LibPrepKit",
  "LibraryBarcode",
  "SeqDate",
  "FlowCellSerial",
  "FlongleAdapter",
  "CollectionDate",
  "SampleID",
  "SampleNotes",
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
  "pair_access",
  "warb_status",
  "keeper_note",
  "Subj_Certainty",
  "Sex",
  "subject_age",
  "diet_color",
  "holding_color",
  "warb_status_color",
  "color_Subj_Certainty",
  "icon_Subj_Certainty",
  "color_pair_access",
  "icon_pair_access",
  "color_subject",
  "icon_subject"
  )

sample_table_cols <- c(
  "study_day",
  "subject",
  "identifier",
  "diet_name",
  "total_mg",
  "total_kcal",
  "total_mg_dry",
  "probiotic",
  "fiber",
  "steroid",
  "antibiotic",
  "antidiarrheal",
  "bristol_mean",
  "holding",
  "pair_access",
  "warb_status",
  "keeper_note",
  "Subj_Certainty",
  "Sex",
  "subject_age",
  "SampleID",
  "CollectionDate",
  "SampleNotes",
  "ExtractID",
  "ExtractDate",
  "ExtractKit",
  "ExtractedBy",
  "ExtractNotes",
  "SequenceID",
  "LibraryCode",
  "LibPrepKit",
  "FlowCellType",
  "FlowCellSerial",
  "FlongleAdapter",
  "SeqDevice",
  "protein_fed",
  "fat_fed",
  "CHO_fed", 
  "mineral_fed"   
)

sample_merge_cols <- c(
  "study_day",
  "subject",
  "subject_day",
  "diet_name",
  "total_mg",
  "total_kcal",
  "total_mg_dry",
  "probiotic",
  "fiber",
  "steroid",
  "antibiotic",
  "antidiarrheal",
  "bristol_mean",
  "holding",
  "pair_access",
  "warb_status",
  "keeper_note",
  "Subj_Certainty",
  "Sex",
  "subject_age",
  "CollectionDate",              
  "protein_fed"                ,
  "fat_fed"                    ,
  "CHO_fed"                    ,
  "mineral_fed"                ,
  "Omega_3"                    ,
  "Omega_6"                    ,
  "Methionine"                 ,
  "Taurine"                    ,
  "Calcium_Ca"                 ,
  "Magnesium_Mg"               ,
  "Phosphorus_P"               ,
  "Potassium_K"                ,
  "Copper_Cu"                  ,
  "Iodine_I"                   ,
  "Iron_Fe"                    ,
  "Manganese_Mn"               ,
  "Zinc_Zn"                    ,
  "Acid_Detergent_Fiber"       ,
  "Neutral_Detergent_Fiber"    ,
  "Total_Dietary_Fiber"        ,
  "Water_Soluble_Carbohydrates",
  "Crude_Fiber"                ,
  "Starch"                     ,
  "Beta_Carotene"              ,
  "Lycopene"                   ,
  "Choline"                    ,
  "Folic_Acid_Vitamin_B9"      ,
  "Vitamin_B1_Thiamin"         ,
  "Vitamin_B2_Riboflavin"      ,
  "Vitamin_B3_Niacin"          ,
  "Vitamin_B5_Pantothenic_Acid",
  "Vitamin_B6_Pyridoxine"      ,
  "Vitamin_B7_Biotin"          ,
  "Vitamin_B12_Cobalamin"      ,
  "Vitamin_C_Ascorbic_Acid"    ,
  "Vitamin_A"                  ,
  "Vitamin_D3_Cholecalciferol" ,
  "Vitamin_E"                  ,
  "Vitamin_K"                  ,
  "Biscuit"                    ,
  "Gum_Arabic"                 ,
  "Invertebrates"              ,
  "Protein_Rotation"           ,
  "Seasonal_Veggie_Rotation"   ,
  "Oat_Gel" 
)

sample_table_numeric <- c(
  "study_day",
  "total_mg",
  "total_kcal",
  "total_mg_dry",
  "bristol_mean",
  "protein_fed"                ,
  "fat_fed"                    ,
  "CHO_fed"                    ,
  "mineral_fed"                ,
  "Omega_3"                    ,
  "Omega_6"                    ,
  "Methionine"                 ,
  "Taurine"                    ,
  "Calcium_Ca"                 ,
  "Magnesium_Mg"               ,
  "Phosphorus_P"               ,
  "Potassium_K"                ,
  "Copper_Cu"                  ,
  "Iodine_I"                   ,
  "Iron_Fe"                    ,
  "Manganese_Mn"               ,
  "Zinc_Zn"                    ,
  "Acid_Detergent_Fiber"       ,
  "Neutral_Detergent_Fiber"    ,
  "Total_Dietary_Fiber"        ,
  "Water_Soluble_Carbohydrates",
  "Crude_Fiber"                ,
  "Starch"                     ,
  "Beta_Carotene"              ,
  "Lycopene"                   ,
  "Choline"                    ,
  "Folic_Acid_Vitamin_B9"      ,
  "Vitamin_B1_Thiamin"         ,
  "Vitamin_B2_Riboflavin"      ,
  "Vitamin_B3_Niacin"          ,
  "Vitamin_B5_Pantothenic_Acid",
  "Vitamin_B6_Pyridoxine"      ,
  "Vitamin_B7_Biotin"          ,
  "Vitamin_B12_Cobalamin"      ,
  "Vitamin_C_Ascorbic_Acid"    ,
  "Vitamin_A"                  ,
  "Vitamin_D3_Cholecalciferol" ,
  "Vitamin_E"                  ,
  "Vitamin_K"                  ,
  "Biscuit"                    ,
  "Gum_Arabic"                 ,
  "Invertebrates"              ,
  "Protein_Rotation"           ,
  "Seasonal_Veggie_Rotation"   ,
  "Oat_Gel" 
)

dataset_types <- c("tax", "function")
tax_levels    <- c("species", "genus", "family", "order", "class", "phylum")
func_levels   <- c("kegg", "fpt", "njc")
all_levels    <- c("species", "genus", "family", "order", "class", "phylum", "kegg", "fpt", "njc")
datasets      <- c("main", "culi", "warb")

env_cols <- c(
  "subject",
  "subject_day",
  "diet_name",
  "probiotic",
  "fiber",
  "steroid",
  "antibiotic",
  "antidiarrheal",
  "holding",
  "pair_access",
  "warb_status",
  "study_day",
  "total_mg",
  "total_kcal",
  "total_mg_dry",
  "bristol_mean",
  "protein_fed"                ,
  "fat_fed"                    ,
  "CHO_fed"                    ,
  "mineral_fed"                ,
  "Omega_3"                    ,
  "Omega_6"                    ,
  "Methionine"                 ,
  "Taurine"                    ,
  "Calcium_Ca"                 ,
  "Magnesium_Mg"               ,
  "Phosphorus_P"               ,
  "Potassium_K"                ,
  "Copper_Cu"                  ,
  "Iodine_I"                   ,
  "Iron_Fe"                    ,
  "Manganese_Mn"               ,
  "Zinc_Zn"                    ,
  "Acid_Detergent_Fiber"       ,
  "Neutral_Detergent_Fiber"    ,
  "Total_Dietary_Fiber"        ,
  "Water_Soluble_Carbohydrates",
  "Crude_Fiber"                ,
  "Starch"                     ,
  "Beta_Carotene"              ,
  "Lycopene"                   ,
  "Choline"                    ,
  "Folic_Acid_Vitamin_B9"      ,
  "Vitamin_B1_Thiamin"         ,
  "Vitamin_B2_Riboflavin"      ,
  "Vitamin_B3_Niacin"          ,
  "Vitamin_B5_Pantothenic_Acid",
  "Vitamin_B6_Pyridoxine"      ,
  "Vitamin_B7_Biotin"          ,
  "Vitamin_B12_Cobalamin"      ,
  "Vitamin_C_Ascorbic_Acid"    ,
  "Vitamin_A"                  ,
  "Vitamin_D3_Cholecalciferol" ,
  "Vitamin_E"                  ,
  "Vitamin_K"                  ,
  "Biscuit"                    ,
  "Gum_Arabic"                 ,
  "Invertebrates"              ,
  "Protein_Rotation"           ,
  "Seasonal_Veggie_Rotation"   ,
  "Oat_Gel" 
)

source(here("metadata/loris/reactable_rendering.R"))