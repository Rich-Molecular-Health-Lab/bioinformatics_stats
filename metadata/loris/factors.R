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

icon_cols <- list(
  steps_remaining = c(
    
  )
)
