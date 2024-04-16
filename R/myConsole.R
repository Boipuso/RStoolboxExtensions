library(devtools)
library(usethis)

# initializing a github repo
use_github(
  organisation = NULL,
  private = TRUE,
  protocol = git_protocol(),
  host = NULL,
)

# setting up vignette file
use_vignette("Vignette")
