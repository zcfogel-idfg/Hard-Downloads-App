############################################################
### Necessary Packages for Hard Download Check Shiny App ###
############################################################

if(!require('bslib')) install.packages('bslib')
library(bslib)
library(shiny)
library(shinythemes)
library(glue)
if(!require('DT')) install.packages('DT')
library(DT)
if(!require('shinybusy')) install.packages('shinybusy')
library(shinybusy)
if(!require('shinyBS')) install.packages('shinyBS')
library(shinyBS)
# install.packages("remotes")
# remotes::install_github("daattali/shinycssloaders")
