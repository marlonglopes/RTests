
file.sources = list.files(c("C:/Users/lopesma.AUTH/r-packages/statisticalModeling/R"), pattern="*.R$", full.names=TRUE,  ignore.case=TRUE)
sapply(file.sources,source,.GlobalEnv)
