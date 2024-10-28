
# Potentially need to this to access private repo
#set config
# usethis::use_git_config(user.name = "lstoetze", user.email = "lukas.stoetzer@gmail.com")

#Go to github page to generate token
# usethis::create_github_token() 

#paste your PAT into pop-up that follows...
# credentials::set_github_pat()

# library(devtools)
# install_github("lstoetze/propsdid", subdir="pkg")

# Packages
library(propsdid)
library(xtable)
library(tidyverse)
library(kableExtra)
library("foreach")
library("doParallel")

