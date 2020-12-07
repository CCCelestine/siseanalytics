usethis::use_package('magrittr')
usethis::use_package('stats')
usethis::use_package('questionr')
usethis::use_package('ggplot2')
usethis::use_package('ggpubr')
usethis::use_package('dplyr')
usethis::use_package('readxl')
usethis::use_build_ignore("devtools_history.R")

df_test<-read_xlsx("D:/Documents/M2 SISE/Programmation R/Projet/OneDrive_1_07-12-2020/df_test.xlsx")
setwd("D:/Documents/M2 SISE/Programmation R/Projet/version_finale/siseanalytics/")
usethis::use_data(df_test)
fromage=read.table("D:/Documents/M2 SISE/Programmation R/Projet/fromage.txt",header=T,row.names = 1,sep = "\t",dec=".")
setwd("D:/Documents/M2 SISE/Programmation R/Projet/version_finale/siseanalytics/")
usethis::use_data(fromage)

mydir <- "D:/Documents/M2 SISE/Programmation R/Projet/version_finale/"
mypackage <- "siseanalytics"
path <- file.path(mydir, mypackage)
setwd(path)
document()
