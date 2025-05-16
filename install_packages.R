# Function to check if a package is installed
is_installed <- function(pkg) {
  return(pkg %in% rownames(installed.packages()))
}

# Install BiocManager if not already installed
if (!is_installed("BiocManager")) {
  cat("Installing BiocManager...\n")
  install.packages("BiocManager")
}
library(BiocManager)

# List of required packages
cran_packages <- c(
  "shiny",
  "shinydashboard",
  "DT",
  "plotly",
  "dplyr",
  "igraph",
  "ggplot2"
)

bioc_packages <- c(
  "clusterProfiler",
  "enrichplot",
  "limma",
  "msigdbr",
  "org.Hs.eg.db",
  "GO.db",
  "KEGG.db",
  "reactome.db"
)

# Check and install CRAN packages
cat("\nChecking CRAN packages...\n")
for (pkg in cran_packages) {
  if (!is_installed(pkg)) {
    cat(sprintf("Installing %s...\n", pkg))
    install.packages(pkg)
  } else {
    cat(sprintf("%s is already installed\n", pkg))
  }
}

# Check and install Bioconductor packages
cat("\nChecking Bioconductor packages...\n")
for (pkg in bioc_packages) {
  if (!is_installed(pkg)) {
    cat(sprintf("Installing %s...\n", pkg))
    BiocManager::install(pkg, update = FALSE)
  } else {
    cat(sprintf("%s is already installed\n", pkg))
  }
}

# Verify all packages can be loaded
cat("\nVerifying package loading...\n")
all_packages <- c(cran_packages, bioc_packages)
for (pkg in all_packages) {
  tryCatch({
    library(pkg, character.only = TRUE)
    cat(sprintf("Successfully loaded %s\n", pkg))
  }, error = function(e) {
    cat(sprintf("ERROR loading %s: %s\n", pkg, e$message))
  })
}

cat("\nPackage installation and verification complete!\n") 