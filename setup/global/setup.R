global           <- config::get()
micro            <- config::get(config = "microbiome")
read_processing  <- config::get(config = "read_processing")
notebooks        <- config::get(config = "notebooks")
loris            <- config::get(config = "loris")
marm             <- config::get(config = "marmoset")

day1        <- loris$day1

source(paste0(global$functions))
source(paste0(global$packages))
source(paste0(global$inputs))
