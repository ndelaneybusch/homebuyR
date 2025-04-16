## Add helper functions ----
golem::use_utils_ui(with_test = TRUE)
golem::use_utils_server(with_test = TRUE)

## Use git ----
usethis::use_git()
## Sets the remote associated with 'name' to 'url'
usethis::use_git_remote(
  name = "origin",
  url = "https://github.com/<OWNER>/<REPO>.git"
)

# You're now set! ----

# go to dev/02_dev.R
rstudioapi::navigateToFile("dev/02_dev.R")
