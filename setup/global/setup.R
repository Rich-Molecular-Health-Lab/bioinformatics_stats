global           <- config::get(config = "default")
swan             <- config::get(config = "swan")
micro            <- config::get(config = "microbiome")
notebooks        <- config::get(config = "notebooks")
loris            <- config::get(config = "loris")
marm             <- config::get(config = "marmoset")
methods_16s      <- config::get(config = "methods_16s")

day1        <- loris$day1

source(paste0(global$packages))
source(paste0(global$conflicts))
source(paste0(global$functions))
source(paste0(global$inputs))
