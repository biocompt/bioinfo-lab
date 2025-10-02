# install_packages.R

basic_packages <- c(
  "BiocManager", "devtools", "qs2"
)

installed <- rownames(installed.packages())
for (pkg in basic_packages) {
  if (!(pkg %in% installed)) {
    install.packages(pkg, repos = "http://cran.us.r-project.org", dependencies = TRUE)
  }
}

install.packages("https://cran.r-project.org/src/contrib/Archive/MASS/MASS_7.3-59.tar.gz")
devtools::install_github("kogalur/varPro")

required_packages <- c(
  "parallel", "randomForestSRC", "caret", "dplyr", "Hmisc"
)

installed <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!(pkg %in% installed)) {
    BiocManager::install(pkg)
  }
}



cat("All required packages are installed.\n")