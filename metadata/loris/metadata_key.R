metadata_key <- list(
  LibraryCode     =  "unique id for each pooled library",  
  SequenceID      =  "unique id for each sequencing run per sample",
  SampleID        =  "unique id for each sample collected",
  ExtractID       =  "unique id for each (DNA) extract",
  study_day       =  "day of sample collection relative to collection start",
  subj_day        =  "first initial of subject name with collection day for cleaner ids in visualization",
  CollectionDate  =  "date of sample collection (yyyy-mm-dd)",     
  Subject         =  "name of subject sample collected from",
  Subj_Certainty  =  "whether subject identity is confirmed, unknown, suspected",     
  probiotic       =  "was subject currently administered standard probiotic dose for trial?",
  oatgel          =  "was subject currently administered special oatgel for trial?",
  steroid         =  "daily dose of Budesonide in mg administered for trial phase",
  cauliflower     =  "was cauliflower still included as a rotating diet option?",  
  tomatoes        =  "was tomato still included as a rotating diet option?",
  broccoli        =  "was broccoli still included as a rotating diet option?",
  med_type        =  "medication type being administered (other than steroid for diet trial)",
  med_name        =  "medication name being administered",
  med_dose        =  "daily dose of medication being administered",
  dose_units      =  "units for dose of medication",
  enclosure       =  "was subject still in the old enclosure or had they transferred to new enclosure?",
  access          =  "did subjects have physical access to each other or was shift door closed?",
  estrus          =  "was subject showing signs of estrus?",
  pregnant        =  "was subject pregnant?",
  bristol_mean    =  "mean bristol fecal score on collection day",   
  bristol_min     =  "minimum bristol fecal score on collection day",     
  bristol_max     =  "maximum bristol fecal score on collection day",     
  health_note     =  "ad lib notes related to subject health recorded on collection day",  
  repro_note      =  "ad lib notes related to subject reproductive status recorded on collection day",   
  SampleNotes     =  "ad lib notes recorded for sample collection",    
  ExtractDate     =  "date of DNA extraction/purification",  
  ExtractKit      =  "kit used in DNA extraction/purification",
  ExtractedBy     =  "person performing DNA extraction/purification", 
  ExtractNotes    =  "ad lib notes recorded for DNA extraction/purification",     
  LibPrepDate     =  "date of library preparation",  
  LibPrepWorkflow = "lab workflow used for library preparation",      
  LibPrepKit      =  "ONT kit used for library prep",
  LibraryBarcode  =  "ONT barcode attached to library before pooling and multiplex sequencing",     
  SeqDate         =  "date of library sequencing run",
  FlowCellType    =  "type of Flow Cell library was sequenced with",   
  FlongleAdapter  =  "ID of adapter if Flongle Flow Cell was used for sequencing",     
  SeqDevice       =  "MinION device used for sequencing (Angel or Spike)"       
)

metadata.cols <- list(
  IDs = c(
    "LibraryCode",
    "SequenceID", 
    "SampleID",   
    "ExtractID"
  ),
  Extractions = c(
    "ExtractDate",
    "ExtractKit",
    "ExtractedBy",
    "ExtractNotes"
  ),
  LibraryPrep = c(
    "LibPrepDate",
    "LibPrepWorkflow",
    "LibPrepKit",
    "LibraryBarcode"
  ),
  Sequencing = c(
    "SeqDate",
    "FlowCellType",
    "FlongleAdapter",
    "SeqDevice"
  ),
  SubjectInfo = c(
    "Subject",
    "Subj_Certainty"
    
  )
)

tech_vars <- c(
  "LibraryCode",
  "ExtractDate",
  "ExtractKit",
  "ExtractedBy",
  "ExtractNotes",
  "LibPrepDate",
  "LibPrepWorkflow",
  "LibPrepKit",
  "LibraryBarcode",
  "SeqDate",
  "FlowCellType",
  "FlongleAdapter",
  "SeqDevice"
)


ids <- c(
  "Subject",
  "LibraryCode",
  "SequenceID", 
  "SampleID",   
  "ExtractID"
)

binary.vars <- c(
  "Subject", 
  "Subj_Certainty",
  "probiotic", 
  "oatgel",    
  "cauliflower", 
  "tomatoes",    
  "broccoli", 
  "enclosure",
  "access",   
  "estrus",   
  "pregnant"
)

replace.unknowns <- c(
  "Subj_Certainty",
  "probiotic", 
  "oatgel",    
  "cauliflower", 
  "tomatoes",    
  "broccoli", 
  "access"
)

date.vars <- c(
  "CollectionDate",
  "ExtractDate",
  "LibPrepDate",
  "SeqDate"
)

continuous.vars <- c(
  "steroid",
  "med_dose",
  "bristol_mean",
  "bristol_min", 
  "bristol_max"
)

ordinal.vars <- c(
  "study_day"
)

categorical.vars <- c(
  "LibraryCode",
  "med_type",
  "med_name",
  "ExtractKit", 
  "ExtractedBy",
  "LibPrepWorkflow", 
  "LibPrepKit",  
  "FlowCellType",   
  "FlongleAdapter", 
  "SeqDevice"     
)

qualitative.vars <- c(
  "health_note",
  "repro_note", 
  "SampleNotes",
  "ExtractNotes"
)


yes.no.factors <- c("yes", "no", "unknown")
subject.factor <- c("culi", "warble", "unknown")
