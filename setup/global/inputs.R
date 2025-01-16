# /setup/global/inputs.R

lab.members <- list(
  "Azadmanesh, Shayda"      = "Shayda Azadmanesh (SA)", 
  "Gill, Dedric"            = "Dedric Gill (DG)", 
  "Miller, Joel"            = "Joel Miller (JM)",
  "Raad, Thomas"            = "Thomas Raad (TR)",
  "Rich, Alicia"            = "Alicia Rich (AMR)",
  "Segura-Palacio, Trineca" = "Trineca Segura-Palacio (TSP)",
  "Stout, Anthony"          = "Anthony Stout (AS)",
  "Wagstaff, Cate"          = "Cate Wagstaff (CW)",
  "Other"                   = "Other Not-Listed (OTH)",
  "NA"                      = "Not Applicable (NA)"
)

samplesets <- list(
                          "Omaha Zoo Pygmy Lorises"  = "loris"  , 
                          "UNO Marmosets"            = "marmoset", 
                          "Wild North American Bats" = "bats",
                          "North American eDNA"      = "envir",
                          "Bacterial Isolates"       = "isolates"
)

input_options <- list(
  "Wide distribution of fragment sizes"         = 1,
  "Amplicons or very short fragments of < 1 kb" = 2,
  "Amplicons or short fragments of 1-5 kb"      = 3,
  "Medium fragments of 5-10 kb"                 = 4,
  "Fragments longer than 10 kb"                 = 5
)

protocol_options <- list(
  "LSK114_IsolateGenome" = "lsk",
  "RBK26_16S_Barcoding"  = "rapid16s",
  "LSK114_mtGenome"      = "lsk",
  "LSK114_16S"           = "lsk16s"
)

kits <- list(
  SQKLSK114 = list(
    DCS = "DNA Control Strand", 
    LA  = "Ligation Adapter", 
    LNB = "Ligation Buffer", 
    LFB = "Long Fragment Buffer", 
    SFB = "Short Fragment Buffer",
    AXP = "AMPure XP Beads", 
    SB  = "Sequencing Buffer", 
    EB  = "Elution Buffer", 
    LIB = "Library Beads", 
    LIS = "Library Solution", 
    FCF = "Flow Cell Flush", 
    FCT = "Flow Cell Tether"
  ),
  SQK16S114 = list(
    RA    = "Rapid Adapter",
    ADB   = "Adapter Buffer",
    AXP   = "AMPure XP Beads",
    EB    = "Elution Buffer",
    EDTA  = "EDTA",
    SB    = "Sequencing Buffer",
    LIB   = "Library Beads",
    LIS   = "Library Solution",
    FCF   = "Flow Cell Flush", 
    FCT   = "Flow Cell Tether"
  ),
  NEBE7672S = list(
    M6630 = "NEBNext® FFPE DNA Repair Mix",
    E7363 = "NEBNext® FFPE DNA Repair Buffer v2",
    E7646 = "NEBNext® Ultra II End Prep Enzyme Mix",
    M0467 = "Salt-T4® DNA Ligase"
  )
)

equipment <- list(
  Spin     = "Microfuge",
  Invert   = "Hula mixer",
  Mix      = "Vortex mixer",
  Beads    = "Magnetic rack",
  PCR      = "Thermal cycler",
  QC       = "Qubit fluorometer",
  Sequence = "MinION",
  Chill    = "Freezy blocks"
)

reagents <- list(
  M0533  = "LongAmp Hot Start Taq 2X Master Mix ",
  BSA    = "Bovine Serum Albumin",
  Q32851 = "Qubit dsDNA HS Assay Kit",
  ETOH   = "Freshly prepared 80% ethanol in nuclease-free water"
)

tubes <- list(
  Q32856 = "Qubit™ Assay Tubes",
  Epp1.5 = "1.5 ml Eppendorf DNA LoBind tubes",
  PCR0.2 = "0.2 ml thin-walled PCR tubes"
)

flowcells <- list(
  MIN114  = "MinION Flow Cell",
  FLG114  = "Flongle Flow Cell"
)

supplies_rap16s <- list(
  Kits      = keep_at(kits, "SQK16S114"),
  Equipment = equipment,
  Reagents  = reagents,
  Tubes     = tubes
)

supplies_lsk <- list(
  Kits      = discard_at(kits, "SQK16S114"),
  Equipment = equipment,
  Reagents  = discard_at(reagents, "M0533"),
  Tubes     = tubes
)

subjects <- list(
  marmoset = list(
   HAM  = "Hamlet",
   HER  = "Hera",
   JAR  = "JarJar BINKS",
   OPH  = "Ophelia",
   KUB  = "Kubo",
   KOR  = "Korra",
   WOL  = "Wolverine",
   IRI  = "Iris",
   GOO  = "Goose",
   LAM  = "Lambchop",
   FRA  = "Franc",
   IVY  = "Ivy",
   CHA  = "Charles",
   PAD  = "Padme",
   BUB  = "Bubblegum",
   GRO  = "Grogu",
   MAR  = "Marshmallow",
   BUD  = "Buddy",
   JOA  = "Joans",
   HEN  = "Henry",
   GIN  = "Ginger"
  ),
  loris = list(
    WARB = "Warble",
    CULI = "Culi"
  ),
  bats = list(
    UNK = "Unknown"
  ),
  envir = list(
    UNK = "Unknown"
  ),
  isolates = list(
    UNK = "Unknown"
  )
)


seqruns <- list(
  loris     = as.list(paste0("hdz", 1:18)),
  marmoset  = as.list(sprintf("cm%03d", 1:10))
)




