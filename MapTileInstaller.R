# 必要なパッケージの読み込みなど
library(png)
library(RCurl)
original.wd <- getwd()
setwd(original.wd)


# 作業ディレクトリの設定
wd <- "C:/Users/******/Desktop/Tile" # お好きなパスを
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
#屋久島の場合
x1 <- 132.182380
x2 <- 131.185
y1 <- 35.081611
y2 <- 35.335817
#日本全体の場合
x1 <- 154.907227
x2 <- 122.080078
y1 <- 46.528635
y2 <- 19.683970
#ズームレベル指定
z1 <- 14
z2 <- 15
folder <- "test6" #フォルダ名
tile.name <- "https://cyberjapandata.gsi.go.jp/xyz/std" # とりあえず、地理院タイルを読み込みます。
# 地理院タイルの利用規約はこちら　http://www.gsi.go.jp/kikakuchousei/kikakuchousei40182.html
# 入力終わり

# 以下演算
Z1 <- min(z1, z2)
Z2 <- max(z1, z2)
X1 <- min(x1, x2)
X2 <- max(x1, x2)
Y1 <- max(y1, y2) #x,zと逆であるので注意
Y2 <- min(y1, y2) #x,zと逆であるので注意
dir.create(folder) #格納フォルダを作成
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
dir.create(as.character(zl)) # Zのフォルダを作成
tx <- tile.X1
	while(tx <= tile.X2){
	x.wd <- paste(z.wd, "/", zl, sep="")
	setwd(x.wd)
	dir.create(as.character(tx))
	ty <- tile.Y1
	while(ty <= tile.Y2){
		y.file <- paste(x.wd, "/", tx, "/", ty, ".png", sep="")
		url <- paste(tile.name, "/", zl, "/", tx, "/", ty, ".png", sep="")
		tryCatch(
		{
		tile_image <-  readPNG(getURLContent(url))
		writePNG(tile_image, y.file)
		}, error = function(e){}
		)
		ty <- ty+1
		}#yのwhileを閉じる
	tx <- tx+1
	}#xのwhileを閉じる
k <- k+1
}#zのwhileを閉じる
