# First, ensure BiocManager is installed
if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}

# Set Bioconductor version
BiocManager::install(version = "3.19", ask = FALSE)

# Install required annotation packages first
BiocManager::install(c(
    "AnnotationDbi",
    "org.Hs.eg.db",
    "GO.db",
    "KEGG.db",
    "reactome.db"
), force = TRUE, ask = FALSE)

# Install other Bioconductor packages
BiocManager::install(c(
    "clusterProfiler",
    "enrichplot",
    "limma",
    "msigdbr"
), force = TRUE, ask = FALSE)

# Install CRAN packages
install.packages(c(
    "shiny",
    "shinydashboard",
    "DT",
    "plotly",
    "dplyr",
    "igraph",
    "ggplot2"
))

# Verify installations
packages <- c(
    "org.Hs.eg.db",
    "GO.db",
    "KEGG.db",
    "reactome.db",
    "clusterProfiler",
    "enrichplot",
    "limma",
    "msigdbr",
    "AnnotationDbi",
    "shiny",
    "shinydashboard",
    "DT",
    "plotly",
    "dplyr",
    "igraph",
    "ggplot2"
)

# Check each package
cat("\nVerifying package installations:\n")
for (pkg in packages) {
    if (requireNamespace(pkg, quietly = TRUE)) {
        cat(sprintf("✓ %s installed successfully\n", pkg))
    } else {
        cat(sprintf("✗ %s NOT installed\n", pkg))
    }
} 