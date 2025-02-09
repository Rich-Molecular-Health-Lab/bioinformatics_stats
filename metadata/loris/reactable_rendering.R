colGroups <- list(
  
  colGroup(name = "Amount Fed Daily", columns = c(       
    "total_mg"    ,      
    "total_kcal"  ,       
    "total_mg_dry",       
    "protein_fed" ,      
    "fat_fed"     ,       
    "CHO_fed"     ,       
    "mineral_fed" ,
    "vitamins"
  )
  ),
  
  colGroup(name = "Daily Doses of Meds/Supplements (Normalized)", columns = c(       
    "probiotic"   ,  
    "fiber"       ,  
    "steroid"     ,  
    "antibiotic"  ,  
    "antidiarrheal"
  )
  ),
  
  colGroup(name = "Environmental Variables", columns = c(   
    "holding"        ,    
    "pair_access"    ,    
    "warb_status"    ,    
    "keeper_note"      
  )
  ),
  
  colGroup(name = "Subject ID and Studbook Data", columns = c(
    "Subj_Certainty"  ,   
    "Sex"             ,  
    "subject_age"     ,   
    "StudbookID"      ,   
    "MotherID"        ,  
    "FatherID"        ,   
    "BirthLocation"      
  )
  ),
  
  
  colGroup(name = "Sample Collection", columns = c(
    "SampleID"          ,
    "CollectionDate"     ,
    "SampleSet"          ,
    "SampleCollectedBy" ,
    "SampleNotes"        
  )
  ),
  
  colGroup(name = "DNA Extraction", columns = c(
    "ExtractID"     ,     
    "ExtractDate"   ,    
    "ExtractConc"   ,     
    "ExtractKit"    ,     
    "ExtractBox"    ,    
    "ExtractedBy"   ,     
    "ExtractNotes"  
  )
  ),
  
  colGroup(name = "Library Prep", columns = c(
    "SequenceID"        ,
    "LibPrepDate"       ,
    "LibPrepWorkflow"   ,
    "LibraryCode"       ,
    "protocol_group_id" ,
    "LibPrepKit"        ,
    "LibraryTube"       ,
    "TemplateVolPrep"   ,
    "LibraryBarcode"    ,
    "fragment_type"     ,
    "strands"           ,
    "Length"            ,
    "InputMassStart"    ,
    "Conc_QC2"          ,
    "PoolSamples"       ,
    "SampVolPool"       ,
    "BeadVol"           ,
    "TotalPoolVol"      ,
    "InputMassFinal"    
  )
  ),
  
  colGroup(name = "DNA Sequencing", columns = c(
    "SeqDate"          ,  
    "SeqDateTime"      , 
    "FlowCellType"     ,  
    "FlowCellSerial"   ,  
    "FlongleAdapter"   , 
    "SeqDevice"        ,  
    "reads_unclassified"
  )
  )
)

subtab_div <- function(data_sub) {
  tags$div(style = "position: relative; margin-left: 450px; width: max-content; 
                                background: white; padding: 10px; border: 1px solid #ccc; 
                                box-shadow: 2px 2px 10px rgba(0,0,0,0.2);", data_sub)
}



nutrient_cell <- function(data) {
  data_bars(data, 
            round_edges   = TRUE, 
            fill_color    = intake_scale,
            text_position = "above", 
            number_fmt    = label_number(scale = .001, scale_cut = cut_si("g")),
            box_shadow    = TRUE)
}

concentration_cell <- function(data) {
  data_bars(data, 
            round_edges   = TRUE, 
            fill_color    = concentration_scale,
            text_position = "above", 
            number_fmt    = label_number(suffix = " ng/\u00b5L"),
            box_shadow    = TRUE)
}

supplement_cell <- function(data, icon) {
  icon_assign(data, icon = icon, icon_size = 13, fill_color = supplement_color, empty_opacity = 0)
}

subj_certain_cell <- function(data) {
  icon_sets(data, icon_ref = "icon_Subj_Certainty", icon_color_ref = "color_Subj_Certainty", icon_position = "over", tooltip = TRUE)
}

subj_cell <- function(data) {
  icon_sets(data, icon_ref = "icon_subject", icon_color_ref = "color_subject")
}

steps_remain_cell <- function(data) {
  icon_sets(data, icon_ref = "icon_steps_remaining", icon_color_ref = "color_steps_remaining", icon_position = "over", tooltip = TRUE)
}

bristol_cell <- function(data) {
  gauge_chart(data, show_min_max = TRUE, fill_color = alarm_colors)
}

pair_access_cell <- function(data) {
  icon_sets(data, icon_ref = "icon_pair_access", icon_color_ref = "color_pair_access", icon_position = "over", tooltip = TRUE)
}

sex_cell <- function(data) {
  icon_sets(data, icon_ref = "icon_subject", icon_color_ref = "color_subject")
}

reads_unclass_cell <- function(data) {
  data_bars(data, 
            fill_color = alarm_colors,
            round_edges = TRUE, 
            text_position = "outside-end")
}

food_subtab <- function(index) {
  data_sub <- foods_expand[foods_expand$identifier == metadata_summary$identifier[index], ]
  food_sub <-  reactable(data_sub,
                         theme     = flatly(),
                         fullWidth = FALSE,
                         columns   = list(
                           identifier = colDef(show = FALSE),
                           food       = colDef(name = "Food"),
                           mg_fed     = colDef(name = "Amount Daily", cell = nutrient_cell(data_sub))
                         )
  )
  subtab_div(food_sub)
}


nutrient_details <- function(index, data) {
  data_sub <- data[data$identifier == metadata_summary$identifier[index], ]
  table_sub <-  reactable(
    data_sub,
    theme     = flatly(),
    fullWidth = FALSE,
    columns   = list(
      identifier = colDef(show = FALSE),
      nutrient   = colDef(name = "Nutrient"),
      fed        = colDef(name = "Amount Daily", cell = nutrient_cell(data_sub)),
      fed_unit     = colDef(show = FALSE),
      relative_fed = colDef(name = "Prop Daily", cell = merge_column(data_sub, merged_name = "relative_unit")),
      relative_unit = colDef(show = FALSE)
    )
  )
  subtab_div(table_sub)
}

expand_subtables <- function(df, col) {
  df %>% 
    select(identifier, all_of(col)) %>% 
    unnest(col) %>% 
    mutate(relative_fed = if_else(relative_unit == "proportion", round(relative_fed*100, 2), round(relative_fed, 2)),
           relative_unit = if_else(
             relative_unit == "proportion", "%",
             str_replace_all(relative_unit, "_", "/")),
           nutrient = fct_recode(nutrient, !!!rename_nutrients))
}


tippy_identifier <- function() {
  tippy("ID", paste0(metadata_variables$sample_context$basics$identifier))
}

tippy_study_day <- function() {
  tippy("Day", paste0(metadata_variables$sample_context$basics$study_day))
}

tippy_subject <- function() {
  tippy("Subj", paste0(metadata_variables$sample_context$basics$subject))
}

tippy_lab_stage <- function() {
  tippy("Stage", paste0(metadata_variables$sample_context$basics$steps_remaining))
}

tippy_diet <- function() {
  tippy("Diet", paste0(metadata_variables$sample_context$nutrition$diet_name))
}

tippy_total_mg <- function() {
  tippy("Total Weight", paste0(metadata_variables$sample_context$nutrition$total_mg))
}
tippy_total_kcal <- function() {
  tippy("kcal", paste0(metadata_variables$sample_context$nutrition$total_kcal))
}
tippy_total_mg_dry <- function() {
  tippy("Dry Weight", paste0(metadata_variables$sample_context$nutrition$total_mg_dry))
}

tippy_protein <- function() {
  tippy("Protein", paste0(metadata_variables$sample_context$nutrition$protein_fed))
}
tippy_fat <- function() {
  tippy("Fat", paste0(metadata_variables$sample_context$nutrition$fat_fed))
}
tippy_chos <- function() {
  tippy("Carbs", paste0(metadata_variables$sample_context$nutrition$CHO_fed))
}
tippy_ash <- function() {
  tippy("Minerals", paste0(metadata_variables$sample_context$nutrition$mineral_fed))
}
tippy_vitamins <- function() {
  tippy("Vitamins", paste0(metadata_variables$sample_context$nutrition$vitamins))
}

tippy_probiotic <- function() {
  tippy("Probiotic", paste0(metadata_variables$sample_context$supplements$probiotic))
}

tippy_fiber <- function() {
  tippy("Fiber", paste0(metadata_variables$sample_context$supplements$fiber))
}

tippy_steroid <- function() {
  tippy("Steroid", paste0(metadata_variables$sample_context$supplements$steroid))
}

tippy_antibiotic <- function() {
  tippy("Antibiotic", paste0(metadata_variables$sample_context$supplements$antibiotic))
}

tippy_antidiar <- function() {
  tippy("Antidiarr.", paste0(metadata_variables$sample_context$supplements$antidiarrheal))
}

tippy_bristol <- function() {
  tippy("Bristol", paste0(metadata_variables$sample_context$other_by_date_subj$bristol_mean))
}

tippy_pair_access <- function() {
  tippy("Access", paste0(metadata_variables$sample_context$other_by_date_subj$pair_access))
}

tippy_enclosure <- function() {
  tippy("Enclosure", paste0(metadata_variables$sample_context$other_by_date_subj$holding))
}

tippy_warb_status <- function() {
  tippy("Warble Status", paste0(metadata_variables$sample_context$other_by_date_subj$warb_status))
}

tippy_keeper_note <- function() {
  tippy("Keeper Note", paste0(metadata_variables$sample_context$other_by_date_subj$keeper_note))
}

tippy_subj_certain <- function() {
  tippy("Conf", paste0(metadata_variables$sample_context$subject_info$Subj_Certainty))
}

tippy_subj_age <- function() {
  tippy("Age", paste0(metadata_variables$sample_context$subject_info$subject_age))
}

tippy_studbook <- function() {
  tippy("AZA ID", paste0(metadata_variables$sample_context$subject_info$StudbookID))
}

tippy_sampID <- function() {
  tippy("ID", paste0(metadata_variables$sample_context$collection$SampleID))
}

tippy_collectNotes <- function() {
  tippy("Note", paste0(metadata_variables$sample_context$collection$SampleNotes))
}


tippy_collectDate <- function() {
  tippy("Date", paste0(metadata_variables$sample_context$collection$CollectionDate))
}

tippy_extractID <- function() {
  tippy("ID", paste0(metadata_variables$labwork$DNAextraction$ExtractID))
}
tippy_extractDate <- function() {
  tippy("Date", paste0(metadata_variables$labwork$DNAextraction$ExtractDate))
}
tippy_extractConc <- function() {
  tippy("Conc", paste0(metadata_variables$labwork$DNAextraction$ExtractConc))
}
tippy_extractKit <- function() {
  tippy("Kit", paste0(metadata_variables$labwork$DNAextraction$ExtractKit))
}
tippy_extractBox <- function() {
  tippy("Box", paste0(metadata_variables$labwork$DNAextraction$ExtractBox))
}
tippy_extractNotes <- function() {
  tippy("Note", paste0(metadata_variables$labwork$DNAextraction$ExtractNotes))
}


tippy_seqID <- function() {
  tippy("ID", paste0(metadata_variables$labwork$Sequencing$SequenceID))
}
tippy_libprepDate <- function() {
  tippy("Date", paste0(metadata_variables$labwork$LibraryPrep$LibPrepDate))
}
tippy_libprepID <- function() {
  tippy("Pooled Library", paste0(metadata_variables$labwork$LibraryPrep$LibraryCode))
}
tippy_libprepKit <- function() {
  tippy("Kit", paste0(metadata_variables$labwork$LibraryPrep$LibPrepKit))
}
tippy_barcode <- function() {
  tippy("Barcode", paste0(metadata_variables$labwork$LibraryPrep$LibraryBarcode))
}


tippy_finalConc <- function() {
  tippy("Final Conc", paste0(metadata_variables$labwork$LibraryPrep$Conc_QC2))
}

tippy_seqDate <- function() {
  tippy("Date", paste0(metadata_variables$labwork$Sequencing$SeqDate))
}

tippy_flowcell <- function() {
  tippy("Flow Cell", paste0(metadata_variables$labwork$Sequencing$FlowCellType))
}

tippy_seqDevice <- function() {
  tippy("Seq Device", paste0(metadata_variables$labwork$Sequencing$SeqDevice))
}

tippy_reads_unclass <- function() {
  tippy("Unclass. Reads", paste0(metadata_variables$labwork$Sequencing$reads_unclassified))
}

