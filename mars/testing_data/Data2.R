data2 <- read_excel("ENB2012_data.xlsx")
names(data2) <- c("x1",paste0("x",2:(ncol(data2)-1)))
names(data2)[ncol(data2)] <- "y"
#data2 <- data2[sample(nrow(data1), 200),]
usethis::use_data(data2, overwrite = TRUE)
