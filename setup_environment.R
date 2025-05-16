if (!require("renv")) install.packages("renv")

renv::init()

# Install BiocManager if not already installed
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

# Install Bioconductor packages
BiocManager::install(c("clusterProfiler", "enrichplot", "org.Hs.eg.db"))

# Install CRAN packages
packages <- c(
  "shiny",
  "bslib",
  "tidyverse",
  "msigdbr",
  "plotly",
  "viridis",
  "writexl",
  "shinyjs",
  "DT"
)

renv::install(packages)

renv::snapshot() 