# 出現するタイル数の確率
# The probability of the number of tiles on the window
# the pixels of the window
# let L1_input, L2_input > T
L1_input <- 375 /2
L2_input <- 31050 /2
T <- 256 # fixed tile size
L1.min <- ceiling(L1_input/T)
L1.max <- ceiling(L1_input/T) + 1
L2.min <- ceiling(L2_input/T)
L2.max <- ceiling(L2_input/T) + 1
L1 <- L1_input - T*(L1.min - 1)
L2 <- L2_input - T*(L2.min - 1)

if (L1 >= T/2){
p.L1.min <- (2*T - 2*L1)/T
p.L1.max <- (2*L1 - T)/T
}else if(L1 < T/2){
p.L1.min <- (T - 2*L1)/T
p.L1.max <- (2*L1)/T
}

if (L2 >= T/2){
p.L2.min <- (2*T - 2*L2)/T
p.L2.max <- (2*L2 - T)/T
}else if(L2 < T/2){
p.L2.min <- (T - 2*L2)/T
p.L2.max <- (2*L2)/T
}

row1 <-c(p.L1.min, p.L1.max)*p.L2.min
row2 <-c(p.L1.min, p.L1.max)*p.L2.max
res <- rbind(row1, row2)
colnames(res) <- as.character(c(L1.min, L1.max))
rownames(res) <- as.character(c(L2.min, L2.max))
res

sum(p.L1.min, p.L1.max, p.L2.min, p.L2.max)
sum(row1, row2)




