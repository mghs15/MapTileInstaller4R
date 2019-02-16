# 出現するタイル数の確率
# The probability of the number of tiles on the window
# the pixels of the window
# let L1_input, L2_input > T
L1_input <- 375 #Horizontal
L2_input <- 550 #Vertical 
T <- 256 # fixed tile size

L1.min <- ceiling(L1_input/T)
L1.max <- ceiling(L1_input/T) + 1
L2.min <- ceiling(L2_input/T)
L2.max <- ceiling(L2_input/T) + 1

L1 <- L1_input/2 - T*(ceiling(L1_input/(T*2)) - 1) # Half of Window
L2 <- L2_input/2 - T*(ceiling(L2_input/(T*2)) - 1) # Half of Window

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

# results table
row1 <-c(p.L1.min, p.L1.max)*p.L2.min
row2 <-c(p.L1.min, p.L1.max)*p.L2.max
res <- rbind(row1, row2)
colnames(res) <- as.character(c(L1.min, L1.max))
rownames(res) <- as.character(c(L2.min, L2.max))
res

# results summary
res2 <- c(row1, row2)
fc_num <- c(c(L1.min, L1.max)*L2.min, c(L1.min, L1.max)*L2.max)
fc <- factor(fc_num)
res_e <- tapply(res2, fc, sum)
res_e

apNum <- sum(res_e*fc_num)
apNum # estimated number of tiles on the window タイルの表示枚数の期待値

# check the sum of probability
sum(p.L1.min, p.L1.max, p.L2.min, p.L2.max)
sum(row1, row2)


###########################################################################
#Kensyo
i <- 1
D <- NULL
E <- NULL
rep <- 10000
while(i < rep){
set.seed(i)
n <- 1000
x1 <- runif(n, -180, 180)
y1 <- runif(n, -80, 80)
z1 <- 16
tX <- ((2^(z1-1))+(x1/180)*(2^(z1-1)))
tY <- (((log(tan(85.051129*(pi/180)/2+pi/4))-log(tan(y1*(pi/180)/2+pi/4)))/log(tan(85.051129*(pi/180)/2+pi/4)))*(2^(z1-1)))
tX1 <- tX + L1_input/T/2; 
tY1 <- tY + L2_input/T/2; 
tX2 <- tX - L1_input/T/2; 
tY2 <- tY - L2_input/T/2
# print(c(tX1, tX2, tY1, tY2))
tX1 <- floor(tX1); tY1 <- floor(tY1); tX2 <- floor(tX2); tY2 <- floor(tY2)
# print(c(tX1, tX2, tY1, tY2))
X_L <- tX1 - tX2 + 1
Y_L <- tY1 - tY2 + 1
kensyo <- X_L*Y_L 
hist(kensyo)
fc_ken <- factor(kensyo)
kensyo_e <- tapply(kensyo, fc_ken, length)
D <- rbind(D, kensyo_e)

# test of goodness of fit (chi-squared test)
chi <- chisq.test(x=kensyo_e, p=res_e)
E <- c(E, chi$p.value)

i <- i+1
}

apply(D, 2, mean)/n

col <- ifelse(E <= 0.05, "red", "black")
plot(E, col=col)
abline(h=0.05, col="blue")
abline(h=0.01, col="red")
length(subset(E, E < 0.05))/rep
# rep=10000, n=10000, alpha=0.05 then 5.06% are significant (p < 0.05) 

