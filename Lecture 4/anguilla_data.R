library(dismo)
library(dplyr)
library(purrr)

data("Anguilla_grids")
data("Anguilla_test")
data("Anguilla_train")

anguilla = rbind(select(Anguilla_train,-Site, presence = Angaus), 
                 rename(Anguilla_test, presence = Angaus_obs)) %>%
  select(-SegTSeas, -SegLowFlow, -USAvgT) %>%
  na.omit() 

save(anguilla, file="anguilla.Rdata")
