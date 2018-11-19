install.packages("openssl", repos = "https://cran.ism.ac.jp")  
library(openssl)  

list <- list.files(recursive=T)
D <- NULL
i <- 1
while(i <= length(list)){
path <- list[i]
info <- file.info(path)
mtime <- round(unclass(info[[4]]))
size <- info[[1]]
md5 <- sub(":", "", md5(file(path)))
D <- rbind(D, c(path, mtime, size, md5))
i <- i + 1
}
write.table(D, "mokuroku.csv", row.names = FALSE, col.names = FALSE, sep=",")
