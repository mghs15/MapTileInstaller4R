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

############################################################################################
### Spatial Analysis ####                   
############################################################################################
library(spdep); library(maptools); library(gstat); library(sp)
library(raster); library(rgdal); library(automap)

# Use dataset named as "info", including tile xyz ("x", "y", "z") and "size".
# info <- info[-c(length(info[,1])-1, length(info[,1])),]

zl <- "14"
C <- subset(info, info[,"z"]==zl)
vx <- as.numeric(as.character(C[,"x"])) 
stry <- as.character(C[,"y.ext"]) #文字列分割
vy <- as.numeric(unlist(strsplit(stry, ".pbf")))
value <- C[, "size"]

plot(vx, vy, pch=16, col = gray(value/max(value)))

df <- cbind(vx, vy, value); colnames(df) <- c("x", "y", "value") # 上のデータをまとめる
plot(df); head(df)
length(unique(df[,"x"])); length(unique(df[,"y"]))

########################
# Transform
# 元データをspに変換(このテキストの上の方参照)
dat <- as.data.frame(df) # サンプリング点データ(x, y, value)
coordinates(dat) = ~x+y
spplot(dat)
#dev.new()
#writeOGR(dat, "dat.geojson", layer = "value", driver = "GeoJSON")

# または、GeoJSONを読み込む 
# dat <- readOGR("dat.geojson")

# Raster of Tiles 
datg <- dat
gridded(datg) = TRUE
tile.raster <- raster(datg)
plot(tile.raster)
# writeRaster(tile.raster, "Tile_Raster.tiff", overwrite=TRUE, format="GTiff") # GeoTiff（拡張子.tif）でタイルをラスタとして出力。

########################
# Tile Grid Raster (use "zl", and "vx", "vy" and "value" used to create "df")
# Convert Tile x y to lon lat
vz <- as.numeric(zl)  #zoom level
lon_1 <- vx*360/(2^vz) - 180
lat_1 <- atan(sinh(pi - vy*2*pi/(2^vz)))*180/pi
lon_2 <- (vx+1)*360/(2^vz) - 180
lat_2 <- atan(sinh(pi - (vy+1)*2*pi/(2^vz)))*180/pi
lon <- (lon_1 + lon_2)/2; lat <- (lat_1 + lat_2)/2
plot(lon, lat, pch=16, col = gray(value/max(value)))

dfg <- cbind(lon, lat, value) 
colnames(dfg) <- c("x", "y", "value")
plot(dfg); head(dfg)
length(unique(dfg[,"x"])); length(unique(dfg[,"y"]))

####
# to Spatial Data
datLL <- as.data.frame(dfg) 
coordinates(datLL) = ~x+y
datLL@proj4string <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
# writeOGR(datLL, "datLL.geojson", layer = "value", driver = "GeoJSON")

# 投影法の変換（to UTM）
zone <- 30; new.crs <- CRS(paste("+proj=utm +zone=", zone, " +datum=WGS84 +units=m", sep=""))
datLL <- spTransform(datLL, CRS=new.crs) 
spplot(datLL)

gridded(datLL) = TRUE
tile.raster <- raster(datLL)
plot(tile.raster)
writeRaster(tile.raster, "Tile_Raster.tiff", overwrite=TRUE, format="GTiff") # GeoTiff（拡張子.tif）でタイルをラスタとして出力。

datg # 

########################
# Moran

coords.df <- cbind(df[,"x"], df[,"y"])
nb <- nb2listw(knn2nb(knearneigh(coords.df, k=2)))
plot(nb, coords.df)
moran.test(df[,"value"], nb)

coords.df <- cbind(df[,"x"], df[,"y"])
nb <- nb2listw(dnearneigh(coords.df, 0, 1))
plot(nb, coords.df)
moran.test(df[,"value"], nb)

########################
# Kriging
datLL_noref <- as.data.frame(dfg) # use parameters of datLL
coordinates(datLL_noref) = ~x+y
dat.k <- datLL_noref # no CRS ref
# dat.k <- datLL

# グリッド grid (newdata)
coord <- coordinates(dat.k)
x.grid <- seq(min(coord[,1]), max(coord[,1]), length=100)
y.grid <- seq(min(coord[,2]), max(coord[,2]), length=100)
xy.grid <- expand.grid(x.grid, y.grid)
vxg <- seq(min(coord[,1]), max(coord[,1]), length=nrow(xy.grid))
vyg <- seq(min(coord[,2]), max(coord[,2]), length=nrow(xy.grid))
grid <- as.data.frame(cbind(xy.grid, vxg, vyg))
colnames(grid) <- c("x","y", colnames(grid)[-c(1:2)])
gridded(grid) = ~x+y
# grid@proj4string <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# 普通クリギング　Ordinary kriging
kriging_o <- autoKrige(value~1, dat.k, grid, model = c("Sph", "Exp", "Gau", "Lin")) # , model = c("Sph", "Exp", "Gau", "Lin")
dev.new(); plot(kriging_o)
parameters_o <- kriging_o$var_model
krig_o <- kriging_o[[1]]
krig_o@proj4string <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # if needed when use lat lon for Kriging
r_o <- raster(krig_o["var1.pred"])
r_o_sd <- raster(krig_o["var1.stdev"])

plot(r_o);contour(r_o, col="white", add=T)
plot(r_o_sd);contour(r_o_sd, col="white", add=T)

# 普遍クリギング Universal kriging
kriging_u <- autoKrige(value~x+y, dat.k, grid, model = c("Sph", "Exp", "Gau", "Lin")) # , model = c("Sph", "Exp", "Gau", "Lin")
dev.new(); plot(kriging_u)
parameters_u <- kriging_u$var_model
krig_u <- kriging_u[[1]]
krig_u@proj4string <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # if needed when use lat lon for Kriging
r_u <- raster(krig_u["var1.pred"])
r_u_sd <- raster(krig_u["var1.stdev"])

plot(r_u);contour(r_u, col="white", add=T)
plot(r_u_sd);contour(r_u_sd, col="white", add=T)

#########################################################################
# クリギング結果の出力 Output result
# 投影変換
r_o_ll <- projectRaster(r_o, crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
plot(r_o_ll, main=strsplit(projection(r_o_ll)," ")[[1]][c(1:2)])
r_u_ll <- projectRaster(r_u, crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
plot(r_u_ll, main=strsplit(projection(r_u_ll)," ")[[1]][c(1:2)])
# 出力
writeRaster(r_o_ll, "krig_pred_190202_o_1.tiff", overwrite=TRUE, format="GTiff") # GeoTiff（拡張子.tif）で予測値を出力。
writeRaster(r_u_ll, "krig_pred_190202_u_1.tiff", overwrite=TRUE, format="GTiff") # GeoTiff（拡張子.tif）で予測値を出力。

# On Leaflet
dat.k.out <- dat.k
dat.k.out@proj4string <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
TilePos <- coordinates(dat.k.out)
tile_zoomlevel <- 1 + 10/vz

pal.o <- colorNumeric(c("white",  "orange"), values(r_o_ll),  na.color = "transparent")
pal.u <- colorNumeric(c("white",  "green"), values(r_u_ll),  na.color = "transparent")
atr_gsi <- "<a href='http://maps.gsi.go.jp/development/ichiran.html' target='_blank'>GSI-Tiles</a>"
map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% setView(lng=0,lat=51,zoom=7) %>%
	# add tiles
	addTiles("http://cyberjapandata.gsi.go.jp/xyz/pale/{z}/{x}/{y}.png", attribution = atr_gsi, group = "GSI Tiles") %>%
	addTiles(group = "OpenStreetMap") %>%
	# add original data
	addCircleMarkers(TilePos[,"x"], TilePos[,"y"], group = "TilePos", color= "blue", stroke=FALSE, radius = tile_zoomlevel) %>% 
	# add kriging results (raster)
	addRasterImage(r_o_ll, colors = pal.o, opacity = 0.8, group = "OrdKri") %>% 
	addLegend(pal = pal.o, values = values(r_o_ll), title = "size <br> [kb] <br> Ord", group = "OrdKri", position="topleft") %>%
	addRasterImage(r_u_ll, colors = pal.u, opacity = 0.8, group = "UniKri") %>% 
	addLegend(pal = pal.u, values = values(r_u_ll), title = "size <br> [kb] <br> Uni", group = "UniKri", position="topleft") %>%
	# set tiles and raster on maps
	addLayersControl(
	  baseGroups = c("OpenStreetMap", "GSI Tiles"),
	  overlayGroups = c("TilePos", "OrdKri", "UniKri"),
	  position="topright",
	  options=layersControlOptions(collapsed = TRUE)
	) %>%
	# add scale bar
	addScaleBar(position="bottomleft", options=scaleBarOptions(imperial = FALSE))
map


# Simple
library("leaflet"); library("tidyr")
pal.k <- colorNumeric(c("white",  "orange"), values(r_u_ll),  na.color = "transparent")
map_s <- leaflet(dat.k) %>% addTiles() %>% setView(lng=0,lat=51,zoom=7) %>%
	addScaleBar(position="bottomleft", options=scaleBarOptions(imperial = FALSE)) %>%
	addRasterImage(r_u_ll, colors = pal.k, opacity = 0.8) %>% 
	addLegend(pal = pal.k, values = values(r_u_ll), title = "value <br> [kb]")
map_s

##################################################################################################################################

#########################################################################
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





