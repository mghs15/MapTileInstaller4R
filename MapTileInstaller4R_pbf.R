library(png)
library(RCurl)
library("curl")
original.wd <- getwd()
setwd(original.wd)

# 作業ディレクトリの設定
wd <- "C:/Users/*****/Desktop/Tile" # お好きなパスを
setwd(wd)

#  ----------------
#  |      y1      |
#  |              |
#  | x1        x2 |
#  |              |
#  |      y2      |
#  ----------------
#
#読み込みたい範囲とタイルURLの入力
x1 <- -0.5
x2 <- 0
y1 <- 51
y2 <- 51.5
z1 <- 2
z2 <- 14
folder <- "os_test0209_curl2"
tile.name <- "https://s3-eu-west-1.amazonaws.com/tiles.os.uk/data/vector/open-zoomstack" # Ordnance Survey
ext <- ".pbf" #拡張子
# https://www.ordnancesurvey.co.uk/business-and-government/products/os-open-zoomstack.html
  # Contains OS data c Crown copyright and database right 2019
  # Where you use Code-Point Open data, you must also use the following attribution statements:
  # Contains Royal Mail data c Royal Mail copyright and Database right 2019
  # Contains National Statistics data c Crown copyright and database right 2019
# 入力終わり

# 以下演算
Z1 <- min(z1, z2)
Z2 <- max(z1, z2)
X1 <- min(x1, x2)
X2 <- max(x1, x2)
Y1 <- max(y1, y2) #x,zと逆であるので注意
Y2 <- min(y1, y2) #x,zと逆であるので注意
dir.create(folder) 

k <- Z1
while(k <= Z2){
zl <- k
#log.lat. to tile XY
tile.X1 <- floor((2^(zl-1))+(X1/180)*(2^(zl-1)))
tile.X2 <- floor((2^(zl-1))+(X2/180)*(2^(zl-1)))
tile.Y1 <- floor(((log(tan(85.051129*(pi/180)/2+pi/4))-log(tan(Y1*(pi/180)/2+pi/4)))/log(tan(85.051129*(pi/180)/2+pi/4)))*(2^(zl-1)))
tile.Y2 <- floor(((log(tan(85.051129*(pi/180)/2+pi/4))-log(tan(Y2*(pi/180)/2+pi/4)))/log(tan(85.051129*(pi/180)/2+pi/4)))*(2^(zl-1)))
z.wd <- paste(wd, "/", folder, sep="")
setwd(z.wd)
dir.create(as.character(zl)) #Zのフォルダを作成
tx <- tile.X1
	while(tx <= tile.X2){
	x.wd <- paste(z.wd, "/", zl, sep="")
	setwd(x.wd)
	dir.create(as.character(tx))
	ty <- tile.Y1
	while(ty <= tile.Y2){
		y.file <- paste(x.wd, "/", tx, "/", ty, ext, sep="")
		url <- paste(tile.name, "/", zl, "/", tx, "/", ty, ext, sep="")
		tryCatch(
		{
#		curl_download(url, y.file) # library("curl")
		download.file(url, y.file, mode="wb")
#		tile_image <-  readPNG(getURLContent(url))
#		writePNG(tile_image, y.file)
		}, error = function(e){}
		)
		ty <- ty+1
		}#yのwhileを閉じる
	tx <- tx+1
	}#xのwhileを閉じる
k <- k+1
}#zのwhileを閉じる

#########################################################################
#目標ディレクトリの設定
setwd <- wd
folder
sch_wd <- paste(wd, folder, sep="/")

#ファイル情報の取得
listf <- list.files(sch_wd, recursive = TRUE, full.names = TRUE) #フルパス
listf <- listf[-length(listf)]#余分なファイルを除く
infof <- file.info(listf); head(infof)

#タイル座標の取得
listtile <- list.files(sch_wd, recursive = TRUE) #タイルのパスのみ
listtile <- listtile[-length(listtile)]#余分なファイルを除く
zxy <- strsplit(listtile, "/")
i <- 1
D <- NULL
while(i <= length(listtile)){
	str <- zxy[[i]]
	C <- c(str[1], str[2], str[3])
	D <- rbind(D, C)
	i <- i + 1}
colnames(D) <- c("z", "x", "y.ext")

#すべての情報を合わせる
info <- cbind(infof, D)
info[,"size"] <- info[,"size"]/1000 #kbへ変換
##############################################

#全体のヒストグラム
sizef <- info[,"size"]
hist(sizef, breaks = 100, xlab="size (kb)")
# hist <- hist(input[,"size"], breaks = seq(0, max, 1000), xlab="size (byte)", main="Histgram of Tile Size at each ZL")

#ディレクトリごとのヒストグラム
input <- info
max <- (floor(max(input[,"size"])/100)+1)*100
i <- 1
col <- c("black", "black", "black", "black", "black", "blue", "pink", "orange", "red", "green", "yellow", "grey", "purple", "brown")
dir <- "z" #調べたいディレクトリの列名
unique <- c("2" , "3" , "4",  "5",  "6",  "7",  "8",  "9" , "10", "11", "12", "13" ,"14")
# unique <- as.character(unique(input[,dir]))
par(mfcol = c(3,5), oma = c(0, 0, 0, 0)) # mfcol で指定した場合は列順に，mfrow で指定した場合は行順にグラフが描かれる
hist(sizef, breaks = seq(0, max, 50), xlab="size (kb)", main="Total", freq = FALSE)
while(i <= length(unique)){
	bool <- (input[,dir] == unique[i])
	C <- subset(input, bool)[,"size"]
	hist(C, breaks = seq(0, max, 50), col=col[i], main=unique[i], xlab="size [byte]", freq = FALSE) # add=T
	lines(density(C), col = "orange", lwd = 2)
	print(paste("ZL=", unique[i], ", mean=", mean(C), ", sd=", sd(C), ", n=", length(C), sep=""))
	input <- subset(input, !bool)
	i <- i + 1
}
par(mfcol = c(1,1), oma = c(0, 0, 0, 0))
############################################
# info <- info[-c(length(info[,1])-1, length(info[,1])),]

C <- subset(info, info[,"z"]=="14")
vx <- as.numeric(as.character(C[,"x"])) 
stry <- as.character(C[,"y.ext"]) #文字列分割
vy <- as.numeric(unlist(strsplit(stry, ".pbf")))
vy <- rev(vy) # タイル座標なので逆転させる
val <- C[, "size"]

plot(vx, vy, pch=16, col = gray(val/max(val)))


#########################################################################
#目標ディレクトリの設定2
setwd <- wd
folder <- c("os_test0209_curl2", "osm_test0209_curl2")
sch_wd <- paste(wd, folder[2], 14, sep="/")

#ファイル情報の取得
listf <- list.files(sch_wd, recursive = TRUE, full.names = TRUE) #フルパス
listf <- listf[-length(listf)]#余分なファイルを除く
infof <- file.info(listf); head(infof)

#タイル座標の取得
listtile <- list.files(sch_wd, recursive = TRUE) #タイルのパスのみ
listtile <- listtile[-length(listtile)]#余分なファイルを除く
zxy <- strsplit(listtile, "/")
i <- 1
D <- NULL
while(i <= length(listtile)){
	str <- zxy[[i]]
	C <- c(str[1], str[2])
#	C <- c(str[1], str[2], str[3])
	D <- rbind(D, C)
	i <- i + 1}
colnames(D) <- c("x", "y.ext") # colnames(D) <- c("z", "x", "y.ext")

#すべての情報を合わせる
info <- cbind(infof, D)
info[,"size"] <- info[,"size"]/1000 #kbへ変換
head(info)
##############################################

#全体のヒストグラム
sizef <- info[,"size"]
hist(sizef, breaks = 100, xlab="size (kb)")
# hist <- hist(input[,"size"], breaks = seq(0, max, 1000), xlab="size (byte)", main="Histgram of Tile Size at each ZL")

#ディレクトリごとのヒストグラム
input <- info
max <- (floor(max(input[,"size"])/100)+1)*100
i <- 1
# col <- c("black", "black", "black", "black", "black", "blue", "pink", "orange", "red", "green", "yellow", "grey", "purple", "brown")
dir <- "x" #調べたいディレクトリの列名
# unique <- c("2" , "3" , "4",  "5",  "6",  "7",  "8",  "9" , "10", "11", "12", "13" ,"14")
unique <- as.character(unique(input[,dir]));print(unique);length(unique)+1
par(mfcol = c(5,5), oma = c(0, 0, 0, 0)) # mfcol で指定した場合は列順に，mfrow で指定した場合は行順にグラフが描かれる
hist(sizef, breaks = seq(0, max, 5), xlab="size (kb)", main="Total", freq = FALSE)
lines(density(C), col = "red", lwd = 2)
while(i <= length(unique)){
	bool <- (input[,dir] == unique[i])
	C <- subset(input, bool)[,"size"]
	hist(C, breaks = seq(0, max, 5), col=col[i], main=unique[i], xlab="size [byte]", freq = FALSE) # add=T
	lines(density(C), col = "orange", lwd = 2)
	print(paste("ZL=", unique[i], ", mean=", mean(C), ", sd=", sd(C), ", n=", length(C), sep=""))
	input <- subset(input, !bool)
	i <- i + 1
}
par(mfcol = c(1,1), oma = c(0, 0, 0, 0))

#########################################################################

#ディレクトリごとのヒストグラム
input <- info
max <- (floor(max(input[,"size"])/100)+1)*100
i <- 1
col <- c("black", "black", "black", "black", "black", "blue", "pink", "orange", "red", "green", "yellow", "grey", "purple", "brown")
dir <- "z" #調べたいディレクトリの列名
unique <- c("1", "2" , "3" , "4",  "5",  "6",  "7",  "8",  "9" , "10", "11", "12", "13" ,"14")#調べたいディレクトリの要素名（ZLなど）
# unique <- as.character(unique(infof[,dir]))
par(mfcol = c(3,5), oma = c(0, 0, 0, 0)) # mfcol で指定した場合は列順に，mfrow で指定した場合は行順にグラフが描かれる
hist(sizef, breaks = seq(0, max, 10), xlab="size (kb)", main="Total", freq = FALSE, type="n")
while(i <= length(unique)){
	bool <- (input[,dir] == unique[i])
	C <- subset(input, bool)[,"size"]
	hist(C, breaks = seq(0, max, 10), col=col[i], main=unique[i], xlab="size [byte]", freq = FALSE) # add=T
	print(paste("ZL=", unique[i], ", mean=", mean(C), ", sd=", sd(C), ", n=", length(C), sep=""))
	input <- subset(input, !bool)
	i <- i + 1
}
par(mfcol = c(1,1), oma = c(0, 0, 0, 0))






