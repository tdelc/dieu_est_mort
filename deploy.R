# Version Shiny app.io
# install.packages(c('rsconnect','datasets','doBy','rdrop2','shiny','DT','devtools'))
# devtools::install_github("coolbutuseless/lipsum")

library(rsconnect)
# library(lipsum)
library(googlesheets4)
library(googledrive)
library(shinyjs)
library(V8)
library(rdrop2)

# setwd('E:/Drive/GN/Dieu est mort/Informatique/dieu')

# token <- readRDS('google_token')
# gs_auth(token)

# sheet <- gs_title("Indices")
# info_indices <- data.frame(gs_read_csv(sheet,ws=1))
# indices_dispo <- data.frame(gs_read_csv(sheet,ws=2))
# donnees_joueurs <- data.frame(gs_read_csv(sheet,ws=3))
# 
# save(info_indices,file='info_indices')
# save(indices_dispo,file='indices_dispo')
# save(donnees_joueurs,file='donnees_joueurs')

rsconnect::deployApp(account='tdelc',server = "shinyapps.io",
                     appName="dieu_new",launch.browser = FALSE)
Y
