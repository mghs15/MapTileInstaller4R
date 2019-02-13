library(png)
library(RCurl)
library("curl")
original.wd <- getwd()
setwd(original.wd)

# ��ƃf�B���N�g���̐ݒ�
wd <- "C:/Users/******/Desktop/Tile" # ���D���ȃp�X��
setwd(wd)

#  ----------------
#  |      y1      |
#  |              |
#  | x1        x2 |
#  |              |
#  |      y2      |
#  ----------------
#
#�ǂݍ��݂����͈͂ƃ^�C��URL�̓���
x1 <- -0.5
x2 <- 0
y1 <- 51
y2 <- 51.5
z1 <- 2
z2 <- 14
folder <- "os_test0209_curl2"
tile.name <- "https://s3-eu-west-1.amazonaws.com/tiles.os.uk/data/vector/open-zoomstack" # Ordnance Survey
ext <- ".pbf" #�g���q
# https://www.ordnancesurvey.co.uk/business-and-government/products/os-open-zoomstack.html
  # Contains OS data c Crown copyright and database right 2019
  # Where you use Code-Point Open data, you must also use the following attribution statements:
  # Contains Royal Mail data c Royal Mail copyright and Database right 2019
  # Contains National Statistics data c Crown copyright and database right 2019
# ���͏I���

# �ȉ����Z
Z1 <- min(z1, z2)
Z2 <- max(z1, z2)
X1 <- min(x1, x2)
X2 <- max(x1, x2)
Y1 <- max(y1, y2) #x,z�Ƌt�ł���̂Œ���
Y2 <- min(y1, y2) #x,z�Ƌt�ł���̂Œ���
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
dir.create(as.character(zl)) #Z�̃t�H���_���쐬
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
		curl_download(url, y.file) # library("curl")
#		download.file(url, y.file)
#		tile_image <-  readPNG(getURLContent(url))
#		writePNG(tile_image, y.file)
		}, error = function(e){}
		)
		ty <- ty+1
		}#y��while�����
	tx <- tx+1
	}#x��while�����
k <- k+1
}#z��while�����

#########################################################################
# For PNG
# �ǂݍ��݂����͈͂ƃ^�C��URL�̓���
x1 <- -0.5
x2 <- 0
y1 <- 51
y2 <- 51.5
z1 <- 2
z2 <- 14
folder <- "osm_test0209_curl2"
tile.name <- "https://b.tile.openstreetmap.org"
ext <- ".png" #�g���q
# ���͏I���

# �ȉ����Z
Z1 <- min(z1, z2)
Z2 <- max(z1, z2)
X1 <- min(x1, x2)
X2 <- max(x1, x2)
Y1 <- max(y1, y2) #x,z�Ƌt�ł���̂Œ���
Y2 <- min(y1, y2) #x,z�Ƌt�ł���̂Œ���
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
dir.create(as.character(zl)) #Z�̃t�H���_���쐬
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
		curl_download(url, y.file) # library("curl")
		}, error = function(e){}
		)
		ty <- ty+1
		}#y��while�����
	tx <- tx+1
	}#x��while�����
k <- k+1
}#z��while�����


#########################################################################
#�ڕW�f�B���N�g���̐ݒ� ��ʂ���
setwd <- wd
folder
sch_wd <- paste(wd, folder, sep="/")

#�t�@�C�����̎擾
listf <- list.files(sch_wd, recursive = TRUE, full.names = TRUE) #�t���p�X
listf <- listf[-length(listf)]#�]���ȃt�@�C��������
infof <- file.info(listf); head(infof)

#�^�C�����W�̎擾
listtile <- list.files(sch_wd, recursive = TRUE) #�^�C���̃p�X�̂�
listtile <- listtile[-length(listtile)]#�]���ȃt�@�C��������
zxy <- strsplit(listtile, "/")
i <- 1
D <- NULL
while(i <= length(listtile)){
	str <- zxy[[i]]
	C <- c(str[1], str[2], str[3])
	D <- rbind(D, C)
	i <- i + 1}
colnames(D) <- c("z", "x", "y.ext")

#���ׂĂ̏������킹��
info <- cbind(infof, D)
info[,"size"] <- info[,"size"]/1000 #kb�֕ϊ�
##############################################

#�S�̂̃q�X�g�O����
sizef <- info[,"size"]
hist(sizef, breaks = 100, xlab="size (kb)")
# hist <- hist(input[,"size"], breaks = seq(0, max, 1000), xlab="size (byte)", main="Histgram of Tile Size at each ZL")

#�f�B���N�g�����Ƃ̃q�X�g�O����
input <- info
max <- (floor(max(input[,"size"])/100)+1)*100
i <- 1
col <- c("black", "black", "black", "black", "black", "blue", "pink", "orange", "red", "green", "yellow", "grey", "purple", "brown")
dir <- "z" #���ׂ����f�B���N�g���̗�
unique <- c("2" , "3" , "4",  "5",  "6",  "7",  "8",  "9" , "10", "11", "12", "13" ,"14")
# unique <- as.character(unique(input[,dir]))
par(mfcol = c(3,5), oma = c(0, 0, 0, 0)) # mfcol �Ŏw�肵���ꍇ�͗񏇂ɁCmfrow �Ŏw�肵���ꍇ�͍s���ɃO���t���`�����
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

#########################################################################
#�ڕW�f�B���N�g���̐ݒ�2 ZL��
setwd <- wd
folder <- c("os_test0209_curl2", "osm_test0209_curl2")
sch_wd <- paste(wd, folder[2], 14, sep="/")

#�t�@�C�����̎擾
listf <- list.files(sch_wd, recursive = TRUE, full.names = TRUE) #�t���p�X
listf <- listf[-length(listf)]#�]���ȃt�@�C��������
infof <- file.info(listf); head(infof)

#�^�C�����W�̎擾
listtile <- list.files(sch_wd, recursive = TRUE) #�^�C���̃p�X�̂�
listtile <- listtile[-length(listtile)]#�]���ȃt�@�C��������
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

#���ׂĂ̏������킹��
info <- cbind(infof, D)
info[,"size"] <- info[,"size"]/1000 #kb�֕ϊ�
head(info)
##############################################

#�S�̂̃q�X�g�O����
sizef <- info[,"size"]
hist(sizef, breaks = 100, xlab="size (kb)")
# hist <- hist(input[,"size"], breaks = seq(0, max, 1000), xlab="size (byte)", main="Histgram of Tile Size at each ZL")

#�f�B���N�g�����Ƃ̃q�X�g�O����
input <- info
max <- (floor(max(input[,"size"])/100)+1)*100
i <- 1
# col <- c("black", "black", "black", "black", "black", "blue", "pink", "orange", "red", "green", "yellow", "grey", "purple", "brown")
dir <- "x" #���ׂ����f�B���N�g���̗�
# unique <- c("2" , "3" , "4",  "5",  "6",  "7",  "8",  "9" , "10", "11", "12", "13" ,"14")
unique <- as.character(unique(input[,dir]));print(unique);length(unique)+1
par(mfcol = c(5,5), oma = c(0, 0, 0, 0)) # mfcol �Ŏw�肵���ꍇ�͗񏇂ɁCmfrow �Ŏw�肵���ꍇ�͍s���ɃO���t���`�����
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

#�f�B���N�g�����Ƃ̃q�X�g�O����
input <- info
max <- (floor(max(input[,"size"])/100)+1)*100
i <- 1
col <- c("black", "black", "black", "black", "black", "blue", "pink", "orange", "red", "green", "yellow", "grey", "purple", "brown")
dir <- "z" #���ׂ����f�B���N�g���̗�
unique <- c("1", "2" , "3" , "4",  "5",  "6",  "7",  "8",  "9" , "10", "11", "12", "13" ,"14")#���ׂ����f�B���N�g���̗v�f���iZL�Ȃǁj
# unique <- as.character(unique(infof[,dir]))
par(mfcol = c(3,5), oma = c(0, 0, 0, 0)) # mfcol �Ŏw�肵���ꍇ�͗񏇂ɁCmfrow �Ŏw�肵���ꍇ�͍s���ɃO���t���`�����
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





