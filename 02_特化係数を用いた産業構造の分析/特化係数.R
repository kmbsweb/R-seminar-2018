install.packages("ggrepel")
install.packages("ggmap")
install.packages("gridExtra")
library(ggplot2)
library(ggrepel)
library(gridExtra) 
##session→working directory
setwd("C:/Users/mkeig/Desktop/project2")
st_waka <- read.csv("stat_wakayama.csv", header=T,as.is = T)

##特化係数
st_waka_rate <- data.frame(st_waka[,1:3],(st_waka[,5:24]/st_waka[,4]*100))

#データ格納用の引数を用意
LQdata <- NULL

#全データを処理
for(i in 1:31){
  #データの抽出
  data1 <- st_waka_rate[i,4:23]/st_waka_rate[1,4:23] 
  #結合
  LQdata <- rbind(LQdata,data1)
  }

LQdata <-  data.frame(st_waka[,1:3],LQdata)    

##和歌山市のLQ（特化係数）に注目する
city <- data.frame(t(st_waka[,-1:-4])[,2],t(LQdata)[-1:-3,2])


colnames(city) <- c("従業者数","LQ")
city$従業者数log <- log(city$従業者数)
city$LQ <- as.numeric(format(city$LQ))

colnames(city)
city$label <- rownames(city)
windowsFonts("MEI"=windowsFont("Meiryo"))

ggplot(city, aes(LQ, 従業者数log, label = label)) +
  geom_vline(xintercept = 1, color="gray") +
  geom_point(color = "red") +
  geom_text_repel(point.padding = NA, size=2.5,family = "MEI") +
  labs(title="和歌山市")+
  theme_classic(base_size = 9, base_family = "MEI")


lqplot <- function(x,title){
  city <- data.frame(t(st_waka[,-1:-4])[,x],t(LQdata)[-1:-3,x])
  
  colnames(city) <- c("従業者数","LQ")
  city$従業者数log <- log(city$従業者数)
  city$LQ <- as.numeric(format(city$LQ))
  
  colnames(city)
  city$label <- rownames(city)
  windowsFonts("MEI"=windowsFont("Meiryo"))
  
  A <-ggplot(city, aes(LQ, 従業者数log, label = label)) +
    geom_vline(xintercept = 1, color="gray") +
    geom_point(color = "red") +
    labs(title=title)+
    geom_text_repel(point.padding = NA, size=2.2,family = "MEI") +
    theme_classic(base_size = 12, base_family = "MEI") 

  return(A)
  }

grid.arrange(lqplot(2,"和歌山市"),lqplot(3,"海南市"),
             lqplot(4,"橋本市"),lqplot(7,"田辺市"),
             lqplot(8,"新宮市"),lqplot(31,"串本町"),
             nrow=3, ncol=2)


##専門化係数の算出
SQdata <- data.frame(abs(1-LQdata[,4:23])) 
SQdata2 <- data.frame(LQdata[,3],apply(SQdata, 1, sum))

install.packages("raster")
install.packages("ggthemes")
install.packages("dplyr")
library(raster)
library(ggthemes)
library(dplyr)
m <- getData("GADM", country="Japan", level=2)
m <- m[m$NAME_1 %in% c("Wakayama"),]

##ggplotで使えるように加工
##解体するためのfunctionを作成
shptogf <- function(shp){
  shp1 <- fortify(shp)
  shp2 <- data.frame(rownames(shp@data),shp@data)
  colnames(shp2)[1] <- c("id")
  shp3 <- left_join(shp1, shp2, by ="id")
  return(shp3)
}
Wakayama <- shptogf(m)

##shapefileとcsvをmerge
SC <- read.csv("SC.csv", header=T,as.is = T)
WakayamaF <- left_join(Wakayama, SC,by ="NL_NAME_2")

ggplot() +
  geom_polygon(data=WakayamaF, aes(x=long, y=lat, group=group,fill=SC)) +
  scale_fill_continuous(low="snow2",high = "darkred")+
  labs(title="専門化係数") +
  theme_map() 


LQ <- LQdata[,-1:-2]

#ユークリッドの距離を使用
d <- dist(LQ, method="euclidean") 
#ward法による階層的クラスタリング
pfit <- hclust(d, method="ward.D2")

# デンドログラムの表示
plot(pfit,LQ$市区町村名, hang=-1)
rect.hclust(pfit, k=4, border="red")

B <- cutree(pfit, k=4)
B <- as.data.frame(B)
SC <- cbind(SC,B)

cols <- c("1" = "thistle", "2" = "lightskyblue2", 
          "3" = "lightseagreen","4" = "dodgerblue2")
WakayamaF <- left_join(Wakayama, SC,by ="NL_NAME_2")

ggplot() +
  geom_polygon(data=WakayamaF, aes(x=long, y=lat, group=group,fill=as.character(B)), color="grey40") +
  scale_fill_manual(values=cols ) +
  labs(title="クラスター") +
  theme_map() 


##付録1
grid.arrange(lqplot(6,"御坊市"),lqplot(24,"白浜町"),
             lqplot(5,"有田市"),lqplot(27,"那智勝浦町"),
             nrow=2, ncol=2)

grid.arrange(lqplot(9,"紀の川市"),lqplot(29,"古座川町"),
             lqplot(12,"かつらぎ町"),lqplot(16,"広川町"),
             nrow=2, ncol=2)


##付録2
install.packages("rpivotTable")
#パッケージの読み込み
library("rpivotTable")

###データ例の作成#####
n <- 100
TestDF <- data.frame(Group = sample(paste0("Group", 1:10), n, replace = TRUE),
                     pop = sample(1:10, n, replace = TRUE),
                     Data2 = rnorm(n),
                     Data3 = sample(1:10, n, replace = TRUE),
                     Data4 = sample(1:10, n, replace = TRUE))
####列数が長くなりすぎるとよくない


#ピボットテーブルのプロット
rpivotTable(TestDF, rows = colnames(TestDF[1]), cols = colnames(TestDF[4]), width = "100%", height = "400px")
st_waka2 <- melt(st_waka)
rpivotTable(test,width = "100%", height = "400px")
head(mtcars)
