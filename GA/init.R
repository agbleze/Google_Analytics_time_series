# init.R
#
# Example R code to install packages if not already installed
#

my_packages = c("shiny", "shinydashboard", "shinycssloaders", "shinyanimate", "shinydashboardPlus",
                "shinyEffects", "shinybusy", "shinyBS", "magrittr", "waiter", "fresh", "readr", "forecast",
                "tidyverse", "ggplot2", "fpp2", "lubridate", "GGally", "dplyr", "magrittr", "labelled", "gtsummary",
                "bfast", "ggstatsplot", "googleVis", "formattable", "fontawesome")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}
invisible(sapply(my_packages, install_if_missing))