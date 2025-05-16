# Install BiocManager if not already installed
if (!require("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}

# Function to safely install and load packages
install_and_load <- function(pkg, bioc = FALSE) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    message(sprintf("Installing package: %s", pkg))
    if (bioc) {
      BiocManager::install(pkg, update = FALSE, ask = FALSE)
    } else {
      install.packages(pkg)
    }
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      stop(sprintf("Failed to install package: %s", pkg))
    }
  }
  message(sprintf("Successfully loaded package: %s", pkg))
}

# CRAN packages
cran_packages <- c(
  "shiny",
  "shinydashboard",
  "DT",
  "plotly",
  "dplyr",
  "igraph",
  "ggplot2"
)

# Bioconductor packages
bioc_packages <- c(
  "clusterProfiler",
  "enrichplot",
  "limma",
  "msigdbr",
  "org.Hs.eg.db",
  "GO.db",
  "KEGG.db",
  "reactome.db",
  "AnnotationDbi"
)

# First install and load BiocManager
install_and_load("BiocManager")

message("\nInstalling and loading CRAN packages...")
# Install and load CRAN packages
for (pkg in cran_packages) {
  install_and_load(pkg, bioc = FALSE)
}

message("\nInstalling and loading Bioconductor packages...")
# Install and load Bioconductor packages
for (pkg in bioc_packages) {
  install_and_load(pkg, bioc = TRUE)
}

message("\nAll packages installed and loaded successfully!")

# Source utility functions
source("utils/helpers.R")

# Source UI modules
source("ui/ui_crispr_analysis.R")

# Source server modules
source("server/server_crispr_analysis.R") 