required_packages <- c(
  "agricolae",
  "aplot",
  "Boruta",
  "brms",
  "caret",
  "DESeq2",
  "edgeR",
  "FSA",
  "ggalluvial",
  "GGally",
  "ggdendro",
  "ggtern",
  "ggnested",
  "glmmTMB",
  "GUniFrac",
  "igraph",
  "job",
  "MASS",
  "mecodev",
  "mecoturn",
  "metagenomeS",
  "multiROC",
  "networkD3",
  "NST",
  "parallel",
  "patchwork",
  "performance",
  "picante",
  "png",
  "poweRlaw",
  "randomFores",
  "rfPermute",
  "rgexf",
  "rsample",
  "seqinr",
  "SRS",
  "TreeTools"
)

if (!exists("packages_checked")) {
  install_missing_packages(required_packages)
  packages_checked <- TRUE
}



install.packages("file2meco"     , repos = BiocManager::repositories())
install.packages("MicrobiomeStat", repos = BiocManager::repositories())
install.packages("WGCNA"         , repos = BiocManager::repositories())
BiocManager::install("ggtree")
BiocManager::install("metagenomeSeq")
BiocManager::install("ALDEx2")
BiocManager::install("ANCOMBC")

# For linux or mac
install.packages("doMC")
# Then install the following packages
install.packages("lokern")
install.packages("monomvn")
install.packages("pspline")
devtools::install_github('csb5/beem')
