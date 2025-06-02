# CBGE App - Technical Guide

This guide will help you set up and run the CBGE App on your local computer.

## Prerequisites

- Windows or macOS operating system
- Internet connection

## 1. Install Required Software

### Install R

#### For Windows:
1. Go to [R Project Website](https://cran.r-project.org/)
2. Click on "Download R for Windows"
3. Click on "base"
4. Click on "Download R-X.X.X for Windows" (where X.X.X is the latest version)
5. Run the downloaded installer
6. Follow the installation wizard (use default settings)

#### For macOS:
1. Go to [R Project Website](https://cran.r-project.org/)
2. Click on "Download R for macOS"
3. Download and install the latest version for your Mac

### Install RStudio

#### For Windows:
1. Go to [RStudio Website](https://posit.co/download/rstudio-desktop/)
2. Download RStudio Desktop (free version)
3. Run the downloaded installer
4. Follow the installation wizard (use default settings)

#### For macOS:
1. Go to [RStudio Website](https://posit.co/download/rstudio-desktop/)
2. Download RStudio Desktop (free version)
3. Install the downloaded package

## 2. Clone the Repository

### For Windows:
1. Open Command Prompt (you can find it by searching "cmd" in the Start menu)
2. Navigate to where you want to store the project:
   ```bash
   cd C:\Users\YourUsername\Documents
   ```
3. Clone the repository:
   ```bash
   git clone https://github.com/NBISweden/cbge-main.git
   ```
4. Navigate into the project folder:
   ```bash
   cd cbge-main
   ```

### For macOS:
1. Open Terminal (you can find it using Spotlight search - press Cmd + Space and type "Terminal")
2. Navigate to where you want to store the project:
   ```bash
   cd ~/Documents
   ```
3. Clone the repository:
   ```bash
   git clone https://github.com/NBISweden/cbge-main.git
   ```
4. Navigate into the project folder:
   ```bash
   cd cbge-main
   ```

## 3. Set Up the R Environment

1. Open RStudio
2. Go to File > Open Project
3. Navigate to the `cbge-main` folder and select `cbge-main.Rproj`
4. In the R console (bottom left panel), run:
   ```r
   install.packages("renv")
   renv::restore()
   ```
   - This will install all required packages
   - It might take a few minutes
   - If asked to install packages, type 'y' and press Enter

## 4. Run the App

1. In RStudio, open the file `app/app.R`
2. Click the "Run App" button in the top right of the editor
   - Or run this command in the console:
   ```r
   shiny::runApp("app")
   ```

## Troubleshooting

If you encounter any issues:

1. **Package Installation Errors:**
   - Make sure you have the latest version of R
   - Try running `renv::restore()` again
   - On Windows, you might need to install [Rtools](https://cran.r-project.org/bin/windows/Rtools/) if you get compilation errors

2. **App Not Starting:**
   - Check if all packages are installed by running:
   ```r
   renv::status()
   ```
   - If there are missing packages, run:
   ```r
   renv::restore()
   ```

3. **RStudio Issues:**
   - Try restarting RStudio
   - Make sure you're opening the project through the `.Rproj` file
   - On Windows, run RStudio as administrator if you encounter permission issues

## Need Help?

If you encounter any issues not covered here, please:
1. Take a screenshot of the error message
2. Note what step you were on
3. Contact rasools@chalmers.se

## Additional Information

- The app uses the `renv` package for dependency management
- All required packages are listed in the `renv.lock` file
- The main application code is located in the `app` directory
