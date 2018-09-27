install.packages("leaflet")
install.packages("leaflet.extras")
library(leaflet)
library(leaflet.extras)

##Osakaを中心地に設定,zoom=の数値で拡大率を変更可能。
leaflet() %>% 
  addTiles() %>% 
  setView(lng=135.519283,lat=34.688620,zoom=8)

##地図の種類を変更
##eg."Esri","OSM" etc...
leaflet() %>% 
  addProviderTiles("CartoDB") %>%
  setView(lng=135.519283,lat=34.688620,zoom=8)

#githubからデータ読込
highschool <- read.csv("https://raw.githubusercontent.com/kmbsweb/R-seminar-2018/master/05_%E7%99%BA%E8%A1%A8/highschool_infomation.csv",
                       header=T, fileEncoding="Shift_JIS",stringsAsFactors = FALSE)

#列名を変更する
colnames(highschool) <- c("name","address","lat","lng","class")

##leafletにプロット
##クラスターで表示
leaflet(highschool) %>% 
  addProviderTiles("CartoDB") %>%
  addMarkers(~lng, ~lat, label=~paste0(name),
             clusterOptions = markerClusterOptions())

##コントロールパネルを追加
leaflet() %>% 
  addProviderTiles("CartoDB") %>%
  addCircleMarkers(data = subset(highschool,highschool$class==0), 
                   ~lng, ~lat, radius =1.5,label = ~name,
                   color = "red", group = "0") %>% 
  addCircleMarkers(data = subset(highschool,highschool$class==1), 
                   ~lng, ~lat, radius =1.5,label = ~name,
                   color = "blue", group = "1") %>% 
  addCircleMarkers(data = subset(highschool,highschool$class==2), 
                   ~lng, ~lat, radius =1.5,label = ~name,
                   color = "green", group = "2") %>% 
  addLayersControl(baseGroups = c("0", "1", "2"),position ="topleft")
                   
 
#色の追加
pal <- colorFactor(c("navy", "red","green"), domain = c("2", "1","0"))
#search boxを追加
leaflet(df) %>% addTiles() %>%
  addCircleMarkers(
    data=highschool, lng = ~lng, lat = ~lat, 
    color = ~pal(class),
    stroke = FALSE, fillOpacity = 0.5,
    popup = ~name,group ='highschool') %>%
  addSearchFeatures(targetGroups = 'highschool')





