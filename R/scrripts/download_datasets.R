download.file("https://github.com/lbraglia/suanselete3/blob/master/data/bladder.rda?raw=true",
              "data/bladder_orig.rda")
download.file("https://github.com/lbraglia/suanselete3/blob/master/data/addicts.rda?raw=true",
              "data/addicts_orig.rda")

saveRDS(load("data/addicts.rda"), "data/addicts.rds")
saveRDS(load("data/bladder.rda"), "data/bladder.rds")

           