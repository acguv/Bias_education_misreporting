# Setup

options(repos = c("CRAN" = "https://packagemanager.posit.co/cran/2025-02-28"))

install.packages("renv")
library("renv")

renv::init()

# To keep track not just the packages it “thinks” you use, but all packages you install
# while working with the current project. To do that, run the folliwing code in the R console:

renv::settings$snapshot.type(value = "all")
renv::settings$ppm.enabled(value = TRUE)

install.packages(c("bit", "broom", "cli", "cpp11", "curl", "fs", "generics", "ggplot2","jsonlite",
                    "knitr", "mime", "pillar", "ps", "ragg", "Rdpack", "readxl",
                   "rlang", "sass", "stringi","tinytex", "tzdb", "xfun", "xml2"))

renv::snapshot()
renv::status()
