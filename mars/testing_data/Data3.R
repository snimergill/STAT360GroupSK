data3 <- read_excel("Folds5x2_pp.xlsx")

names(data3) <- c("x1",paste0("x",2:(ncol(data3)-1)))
names(data3)[ncol(data3)] <- "y"

usethis::use_data(data3, overwrite = TRUE)
