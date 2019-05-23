#ダウンロード
library(png)
library(RCurl)
library("curl")
original.wd <- getwd()
setwd(original.wd)

# 作業ディレクトリの設定
wd <- paste(original.wd, "/tiles", sep="") # お好きなパスを
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
x1 <- 141.303331
x2 <- 141.322299
y1 <- 43.051219
y2 <- 43.043049
z1 <- 18
z2 <- 18
folder <- "fgd_dem_maruyama"
tile.name <- "https://cyberjapandata.gsi.go.jp/xyz/experimental_dem10b" # Ordnance Survey
ext <- ".geojson" #拡張子
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
		download.file(url, y.file, mode="wb")
		}, error = function(e){}
		)
		ty <- ty+1
		}#yのwhileを閉じる
	tx <- tx+1
	}#xのwhileを閉じる
k <- k+1
}#zのwhileを閉じる

#########################################################################

library(geojsonR)
library(rgdal)

target_wd <- paste(wd, folder, sep="/")
files = list.files(target_wd,recursive=T, full.names=T)
files0 = list.files(target_wd,recursive=T)

i = 1
NN = length(files)
while(i <= NN){

filename = files[i]
file_js = FROM_GeoJson(url_file_string = filename)

j = 1
N = length(file_js$features)
while(j <= N){
  file_js$features[[j]]$geometry$coordinates = c(file_js$features[[j]]$geometry$coordinates, file_js$features[[j]]$properties$alti)
  j = j + 1
}

head = ', { "type": "Feature","geometry": {"type": "Point", "coordinates": ['
head0 = '{ "type": "Feature","geometry": {"type": "Point", "coordinates": ['

j = 1
geocoord = file_js$features[[j]]$geometry$coordinates
file_js_geo = paste(head0, geocoord[1], ",", geocoord[2], ",", geocoord[3], ']}, "properties": {"class":"DEMPt","type":"その他","alti":', geocoord[3], "}}", sep="")
j = j + 1
while(j <= N){
  geocoord = file_js$features[[j]]$geometry$coordinates
  file_js_geo = paste(file_js_geo, head, geocoord[1], ",", geocoord[2], ",", geocoord[3], ']}, "properties": {"class":"DEMPt","type":"その他","alti":', geocoord[3], "}}", sep="")
  j = j + 1
}

file_js_full = paste('{ "type": "FeatureCollection","features": [', file_js_geo, "]}", sep="")

setwd(wd)
outfolder = "out_003"
dir.create(paste(wd, outfolder, sep="/"))
setwd(paste(wd, outfolder, sep="/"))
outfiles = strsplit(files0[i] , "/")[[1]]
z.wd <- outfiles[1]
dir.create(as.character(z.wd))
setwd(z.wd)
x.wd <- outfiles[2]
dir.create(as.character(x.wd))
setwd(x.wd)
write(file_js_full, outfiles[3])
setwd(wd)

i = i + 1
}

########################################################################
coord2 = file_js$features[[117]]$geometry$coordinates
file_js$features[[117]]$geometry$coordinates = c(file_js$features[[117]]$geometry$coordinates, file_js$features[[117]]$properties$alti)




