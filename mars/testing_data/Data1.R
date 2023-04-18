data1 <- read.csv("fertility_Diagnosis.txt",header = FALSE)
names(data1) <- c("x1",paste0("x",2:(ncol(data1)-1))) #https://stackoverflow.com/questions/68271410/i-want-to-combine-columns-with-combination-results
names(data1)[ncol(data1)] <- "y"

#data <- data1[sample(nrow(data1), 200),]
usethis::use_data(data1, overwrite = TRUE)

