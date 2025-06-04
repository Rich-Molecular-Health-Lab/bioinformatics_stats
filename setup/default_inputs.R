samplesets <- list(
  "Omaha Zoo Pygmy Lorises"  = "loris"  , 
  "UNO Marmosets"            = "marmoset", 
  "Wild North American Bats" = "bats",
  "North American eDNA"      = "envir",
  "Bacterial Isolates"       = "isolates"
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
  marmoset  = as.list(sprintf("cm%03d", 1:10)),
  isolates  = as.list(paste0("salci", 1))
)

colors <- list(
  f = "#D53288FF",
  m = "#3F459BFF",
  u = "#21B14BFF",
  sire = "#3F459B33",
  dam  = "#D5328833",
  emph = "#DC8045FF",
  seq  = "rcartocolor::Sunset",
  div  = "rcartocolor::Temps",
  rand = "khroma::stratigraphy"
)



