library(dplyr)

dat = read_sav("./data/ehs_stock/spss/physical_19plus20_eul.sav")

dat %>%
  group_by(EPceeb12e) %>%
  summarise(count = n())
