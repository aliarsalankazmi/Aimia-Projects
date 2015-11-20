##############################
###### Ali Arsalan Kazmi
###### AliArsalan.Kazmi@aimia.com
###### 31/05/2015
##############################




### Load/Install required libraries
libs <- c('readr', 'dplyr', 'lubridate', 'ggplot2', 'ggvis', 'tidyr', 'reshape2', 
          'extrafont', 'scales')

libsLoaded <- sapply(libs, library, character.only = TRUE, logical.return = TRUE, 
                     quietly = TRUE)

installed <- sapply(libs[!libsLoaded], install.packages)

