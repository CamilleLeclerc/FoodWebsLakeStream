library("devtools")
options(devtools.name = "Alain Danet",
  devtools.desc.author = "person('Alain', 'Danet',
  email='alain.danet@mnhn.fr', role = c('aut', 'cre'))",
  devtools.desc.license = "MIT + file LICENSE"
)

create_package("~/Documents/post-these/mnhn/warmpond")
use_gpl3_license()
use_readme_rmd()
use_vignette("intro")
use_package_doc()
use_testthat()

#data
use_data()
use_data_raw()

# versioning and set up package checking
use_git()

# To copy data for Camille
mypath <- rprojroot::find_package_root_file
dir.create(mypath("data_for_camille"))

raw_files <- list.files(c(mypath("data-raw")), ".csv$", full.names = TRUE)
new_folder <- mypath("data_for_camille", "data-raw")
dir.create(new_folder)
file.copy(raw_files, new_folder)

data_files <- list.files(c(mypath("data")), ".rda$", full.names = TRUE)
new_data_folder <- mypath("data_for_camille", "data")
dir.create(new_data_folder)
file.copy(data_files, new_data_folder)

dir_to_zip <- dir(mypath("data_for_camille"), full.names = TRUE, recursive = T)

getwd()
zip::zip(zipfile = "data_for_camille.zip", files = "data_for_camille/", recurse = T)


# 
usethis::use_vignette("statistical_analysis")
#use_github_action_check_standard()
#use_coverage("codecov")
#usethis::use_github_action("test-coverage")
#usethis::use_github_action("pkgdown")
#use_cran_comments()

## use pkgdown
#pkgdown::build_site()
#usethis::use_git_ignore("docs")
#usethis::use_git_ignore("inst/docs")

##test
#devtools::load_all()
#devtools::test()

## build package 
#attachment::att_to_description()
#devtools::check()
#devtools::install()
#devtools::build_readme()
#pkgdown::build_site()


cat_bib_file <- function(dir = NULL, outfile = NULL) {
  path_to_bib_files <- list.files(
    dir,
    pattern = "\\.bib$",
    full.names = TRUE
  )

  combined_bib <- ""
  for (path_to_bib_file in path_to_bib_files) {

    fileCon <- file(path_to_bib_file)
    content <- readLines(fileCon)
    close(fileCon)

    combined_bib <-
      paste0(
        combined_bib, "\n", "\n",
        trimws(paste0(content, collapse = "\n"))
      )

  }
  cat(combined_bib, file=paste0(dir, "/", outfile), "\n")
}
cat_bib_file(dir = mypath("doc"), outfile = "references.bib")
