# First ensure BiocManager is installed and loaded
if (!require("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}
library(BiocManager)

# Function to safely install and verify a package
install_and_verify <- function(pkg) {
    cat(sprintf("\nChecking %s...\n", pkg))
    
    # Check if installed
    if (!pkg %in% rownames(installed.packages())) {
        cat(sprintf("Installing %s...\n", pkg))
        BiocManager::install(pkg, update = FALSE, ask = FALSE)
    } else {
        cat(sprintf("%s is already installed\n", pkg))
    }
    
    # Verify installation
    if (pkg %in% rownames(installed.packages())) {
        cat(sprintf("Successfully installed %s\n", pkg))
        
        # Try loading the package
        tryCatch({
            library(pkg, character.only = TRUE)
            cat(sprintf("Successfully loaded %s\n", pkg))
        }, error = function(e) {
            cat(sprintf("ERROR loading %s: %s\n", pkg, e$message))
        })
    } else {
        cat(sprintf("Failed to install %s\n", pkg))
    }
}

# Install required annotation packages
required_packages <- c(
    "AnnotationDbi",
    "GO.db",
    "org.Hs.eg.db"
)

# Install and verify each package
for (pkg in required_packages) {
    install_and_verify(pkg)
}

# Final verification
cat("\nFinal verification:\n")
library(GO.db)
cat("GO.db loaded successfully!\n") 