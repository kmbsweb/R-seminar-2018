##今回必要なパッケージ
library(ggmap)
library(ggplot2)
library(rgdal)
library(maptools)
library(dplyr)
library(gridExtra) 
library(ggthemes)

#お気に入りの都市のデータを取得
#zoomでサイズ調整が可能 
#level3:continent,level10:city, level21:building
#map type:"satellite","hybrid"
Kobe <- get_map(location = "Nadaku, Kobe, Japan", maptype = "terrain", source = "google", zoom = 14)
ggmap(Kobe)

##working directoryを指定
##自分自身の作業フォルダを指定
##session→working directory
setwd("C:/Users/mkeig/Desktop/project1")
cv_store <- read.csv("cv_store2.csv", header=T, fileEncoding="Shift_JIS",as.is = T)
summary(cv_store)
Nishiku <- get_stamenmap(bbox = c(left = 135.468, bottom = 34.665, right = 135.50, top = 34.698), 
                         maptype = c("toner-lite"), zoom = 15)
ggmap(Nishiku) + geom_point(data = cv_store, aes(x = fX, y = fY))

##brash up!
colnames(cv_store)
table(cv_store$チェーン店舗)
##色を指定する
##http://www.okadajp.org/RWiki/?%E8%89%B2%E8%A6%8B%E6%9C%AC
cols <- c("サンクス" = "red", "セブンイレブン" = "palegreen4", "デイリーヤマザキ" = "tomato2",
          "ファミリーマート" = "darkslategray3","ミニストップ"="gold","ローソン"="dodgerblue3")
ggmap(Nishiku) +
  geom_point(data = cv_store, aes(x = fX, y = fY, color=チェーン店舗), size=2)+
  scale_colour_manual(values = cols)+
  theme_map(base_family="HiraKakuProN-W3") + 
  theme(legend.position="right") +
  labs(x="", y="", fill="") 

##全体の傾向
ggmap(Nishiku) +
  stat_bin2d(data = cv_store, aes(x = fX, y = fY, fill = ..density.., alpha = ..density..), bins = 18, size = 1.5) + 
  scale_fill_gradient('CV_store\nDensity', low = 'blue', high = 'orange')+
  theme_map()+
  theme(legend.position="right") 


## コロプレス地図作成
## shpファイルの読み込み
Osaka <- readOGR(dsn=".", layer="24区画像",encoding = "utf-8")
centroids.df <- as.data.frame(coordinates(Osaka))
ct.df <- data.frame(centroids.df,Osaka@data$ATTR2)
colnames(ct.df) <- c("Longitude", "Latitude" ,"name")

##ggplotで使えるように加工
##解体するためのfunctionを作成
shptogf <- function(shp){
  shp1 <- fortify(shp)
  shp2 <- data.frame(rownames(shp@data),shp@data)
  colnames(shp2)[1] <- c("id")
  shp3 <- left_join(shp1, shp2, by ="id")
  return(shp3)
  }
Osaka3 <- shptogf(Osaka)


##公園データを取り込み
park <- read.csv("Osaka_city_park.csv", header=T, as.is = T)
##世帯あたりの公園の数
park$park_rate <- park$park/park$HH

##shapefileとcsvをmerge
colnames(park) <- c("ATTR2", "pop" ,"pop_den", "HH", "park", "park_rate")
OsakaF <- left_join(Osaka3, park,by ="ATTR2")

##RColorBrewer palette chartで検索!!
A <- ggplot() +
  geom_polygon(data=OsakaF, aes(x=long, y=lat, group=group,fill=park)) +
  scale_fill_continuous(low="snow2",high = "springgreen4")+
  geom_text(data=ct.df,aes(label = name, x = Longitude, y = Latitude),
            size=3, family = "HiraKakuPro-W3")+  
  labs(title="大阪市内における公園の数") +
  theme_map(base_family="HiraKakuProN-W3") 

B <- ggplot() +
  geom_polygon(data=OsakaF, aes(x=long, y=lat, group=group,fill=park_rate)) +
  scale_fill_continuous(low="snow2",high = "springgreen4")+
  geom_text(data=ct.df,aes(label = name, x = Longitude, y = Latitude),
            size=3, family = "HiraKakuPro-W3")+  
  labs(title="大阪市内における公園の数（世帯比）",
       caption = "source:地図情報サイト「マップナビおおさか」") +
  theme_map(base_family="HiraKakuProN-W3") 


grid.arrange(A, B, nrow=1, ncol=2)

sessionInfo()
#other attached packages:
#[1] ggthemes_3.4.2 gridExtra_2.3  maptools_0.9-2 dplyr_0.7.4    rgdal_1.2-18  
#[6] sp_1.2-7       ggmap_2.6.1    ggplot2_2.2.1 