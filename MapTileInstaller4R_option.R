library(png)
library(RCurl)
original.wd <- getwd()
setwd(original.wd)

#作業用フォルダ名
folder <- "demarea"

Tile.LIST <- c("8/215/108",
"8/215/109",
"8/215/110",
"8/216/108",
"8/216/109",
"8/216/110",
"8/217/109",
"8/218/107",
"8/218/108",
"8/219/101",
"8/219/102",
"8/219/103",
"8/219/104",
"8/219/105",
"8/219/106",
"8/219/107",
"8/219/108",
"8/220/101",
"8/220/102",
"8/220/103",
"8/220/104",
"8/220/105",
"8/220/106",
"8/220/107",
"8/221/101",
"8/221/102",
"8/221/103",
"8/221/104",
"8/221/105",
"8/221/108",
"8/221/109",
"8/221/110",
"8/221/99",
"8/222/100",
"8/222/101",
"8/222/102",
"8/222/103",
"8/223/100",
"8/223/101",
"8/223/102",
"8/224/100",
"8/224/101",
"8/224/102",
"8/224/113",
"8/224/99",
"8/225/100",
"8/225/101",
"8/225/102",
"8/225/98",
"8/225/99",
"8/226/100",
"8/226/101",
"8/226/102",
"8/226/98",
"8/226/99",
"8/227/100",
"8/227/101",
"8/227/102",
"8/227/103",
"8/227/104",
"8/227/105",
"8/227/93",
"8/227/94",
"8/227/95",
"8/227/96",
"8/227/97",
"8/227/98",
"8/227/99",
"8/228/100",
"8/228/107",
"8/228/108",
"8/228/109",
"8/228/110",
"8/228/91",
"8/228/92",
"8/228/93",
"8/228/94",
"8/228/95",
"8/228/96",
"8/228/97",
"8/228/98",
"8/228/99",
"8/229/107",
"8/229/108",
"8/229/91",
"8/229/92",
"8/229/93",
"8/229/94",
"8/229/95",
"8/229/97",
"8/230/92",
"8/230/93",
"8/230/94",
"8/231/92",
"8/231/93",
"8/231/94",
"8/232/91",
"8/232/92",
"8/232/93",
"8/233/91",
"8/233/92",
"8/237/110")

#タイル種類の記述
tile.name <- "https://cyberjapandata.gsi.go.jp/xyz/std" 

#タイルリストの成形（タイルのXYZ座標を行列の形に整理）
tile.list <- strsplit(Tile.LIST , "/")

list.z <- NULL
list.x <- NULL
list.y <- NULL
i <- 1
while(i <= length(tile.list)){
	list.z <- c(list.z, as.integer(tile.list[[i]][1]))
	list.x <- c(list.x, as.integer(tile.list[[i]][2]))
	list.y <- c(list.y, as.integer(tile.list[[i]][3]))
	i <- i+1
}
tileset <- NULL
tileset <- cbind(tileset, list.z)
tileset <- cbind(tileset, list.x)
tileset <- cbind(tileset, list.y)

#タイル番号の抽出
zl.V <- tileset[,1]
tx.V <- tileset[,2]
ty.V <- tileset[,3]

#ディレクトリの設定
wd <- original.wd
z.wd <- paste(wd, "/", folder, sep="") #　作業用ディレクトリに移動 
dir.create(z.wd)
setwd(z.wd) # ディレクトリをZに合わせて変更
dir.create(as.character(rownames(table(zl)))) # Zのフォルダを作成（rownames(table(zl))は頭悪いので何とかしたい）

i <- 1
while(i <= length(tile.list)){
zl <- zl.V[i]
tx <- tx.V[i]
ty <- ty.V[i]
x.wd <- paste(z.wd, "/", zl, sep="")
setwd(x.wd) #　作業用ディレクトリに移動
tryCatch(
	{
	dir.create(as.character(tx))  # Xのフォルダを作成
	}, error = function(e){}
	)
y.file <- paste(x.wd, "/", tx, "/", ty, ".png", sep="")
url <- paste(tile.name, "/", zl, "/", tx, "/", ty, ".png", sep="")
tryCatch(
	{
	tile_image <-  readPNG(getURLContent(url))
	writePNG(tile_image, y.file)
	}, error = function(e){}
	)
i <- i+1
}


