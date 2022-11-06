library("grDevices")
if (.Platform$OS.type == "windows") {
  # don't use curl as it makes trouble on some peoples' windows machines
  options(renv.download.override = utils::download.file)
}
options(renv.config.install.transactional = FALSE)
options(renv.config.repos.override = "https://cran.microsoft.com/snapshot/2022-10-16/")
source("renv/activate.R")
suppressWarnings(require("checkmate", quietly = TRUE))
suppressWarnings(require("data.table", quietly = TRUE))
