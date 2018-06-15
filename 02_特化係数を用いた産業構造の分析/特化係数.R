install.packages("ggrepel")
install.packages("ggmap")
install.packages("gridExtra")
library(ggplot2)
library(ggrepel)
library(gridExtra) 
##sessionÅ®working directory
setwd("C:/Users/mkeig/Desktop/project2")
st_waka <- read.csv("stat_wakayama.csv", header=T,as.is = T)

##ì¡âªåWêî
st_waka_rate <- data.frame(st_waka[,1:3],(st_waka[,5:24]/st_waka[,4]*100))

#ÉfÅ[É^äiî[ópÇÃà¯êîÇópà”
LQdata <- NULL

#ëSÉfÅ[É^Çèàóù
for(i in 1:31){
  #ÉfÅ[É^ÇÃíäèo
  data1 <- st_waka_rate[i,4:23]/st_waka_rate[1,4:23] 
  #åãçá
  LQdata <- rbind(LQdata,data1)
  }

LQdata <-  data.frame(st_waka[,1:3],LQdata)    

##òaâÃéRésÇÃLQÅiì¡âªåWêîÅjÇ…íçñ⁄Ç∑ÇÈ
city <- data.frame(t(st_waka[,-1:-4])[,2],t(LQdata)[-1:-3,2])


colnames(city) <- c("è]ã∆é“êî","LQ")
city$è]ã∆é“êîlog <- log(city$è]ã∆é“êî)
city$LQ <- as.numeric(format(city$LQ))

colnames(city)
city$label <- rownames(city)
windowsFonts("MEI"=windowsFont("Meiryo"))

ggplot(city, aes(LQ, è]ã∆é“êîlog, label = label)) +
  geom_vline(xintercept = 1, color="gray") +
  geom_point(color = "red") +
  geom_text_repel(point.padding = NA, size=2.5,family = "MEI") +
  labs(title="òaâÃéRés")+
  theme_classic(base_size = 9, base_family = "MEI")


lqplot <- function(x,title){
  city <- data.frame(t(st_waka[,-1:-4])[,x],t(LQdata)[-1:-3,x])
  
  colnames(city) <- c("è]ã∆é“êî","LQ")
  city$è]ã∆é“êîlog <- log(city$è]ã∆é“êî)
  city$LQ <- as.numeric(format(city$LQ))
  
  colnames(city)
  city$label <- rownames(city)
  windowsFonts("MEI"=windowsFont("Meiryo"))
  
  A <-ggplot(city, aes(LQ, è]ã∆é“êîlog, label = label)) +
    geom_vline(xintercept = 1, color="gray") +
    geom_point(color = "red") +
    labs(title=title)+
    geom_text_repel(point.padding = NA, size=2.2,family = "MEI") +
    theme_classic(base_size = 12, base_family = "MEI") 

  return(A)
  }

grid.arrange(lqplot(2,"òaâÃéRés"),lqplot(3,"äCìÏés"),
             lqplot(4,"ã¥ñ{és"),lqplot(7,"ìcï”és"),
             lqplot(8,"êVã{és"),lqplot(31,"ã¯ñ{í¨"),
             nrow=3, ncol=2)


##êÍñÂâªåWêîÇÃéZèo
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

##ggplotÇ≈égÇ¶ÇÈÇÊÇ§Ç…â¡çH
##âëÃÇ∑ÇÈÇΩÇﬂÇÃfunctionÇçÏê¨
shptogf <- function(shp){
  shp1 <- fortify(shp)
  shp2 <- data.frame(rownames(shp@data),shp@data)
  colnames(shp2)[1] <- c("id")
  shp3 <- left_join(shp1, shp2, by ="id")
  return(shp3)
}
Wakayama <- shptogf(m)

##shapefileÇ∆csvÇmerge
SC <- read.csv("SC.csv", header=T,as.is = T)
WakayamaF <- left_join(Wakayama, SC,by ="NL_NAME_2")

ggplot() +
  geom_polygon(data=WakayamaF, aes(x=long, y=lat, group=group,fill=SC)) +
  scale_fill_continuous(low="snow2",high = "darkred")+
  labs(title="êÍñÂâªåWêî") +
  theme_map() 


LQ <- LQdata[,-1:-2]

#ÉÜÅ[ÉNÉäÉbÉhÇÃãóó£Çégóp
d <- dist(LQ, method="euclidean") 
#wardñ@Ç…ÇÊÇÈäKëwìIÉNÉâÉXÉ^ÉäÉìÉO
pfit <- hclust(d, method="ward.D2")

# ÉfÉìÉhÉçÉOÉâÉÄÇÃï\é¶
plot(pfit,LQ$ésãÊí¨ë∫ñº, hang=-1)
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
  labs(title="ÉNÉâÉXÉ^Å[") +
  theme_map() 


##ïtò^1
grid.arrange(lqplot(6,"å‰ñVés"),lqplot(24,"îíïlí¨"),
             lqplot(5,"óLìcés"),lqplot(27,"ìﬂíqèüâYí¨"),
             nrow=2, ncol=2)

grid.arrange(lqplot(9,"ãIÇÃêÏés"),lqplot(29,"å√ç¿êÏí¨"),
             lqplot(12,"Ç©Ç¬ÇÁÇ¨í¨"),lqplot(16,"çLêÏí¨"),
             nrow=2, ncol=2)


##ïtò^2
install.packages("rpivotTable")
#ÉpÉbÉPÅ[ÉWÇÃì«Ç›çûÇ›
library("rpivotTable")

###ÉfÅ[É^ó·ÇÃçÏê¨#####
n <- 100
TestDF <- data.frame(Group = sample(paste0("Group", 1:10), n, replace = TRUE),
                     pop = sample(1:10, n, replace = TRUE),
                     Data2 = rnorm(n),
                     Data3 = sample(1:10, n, replace = TRUE),
                     Data4 = sample(1:10, n, replace = TRUE))
####óÒêîÇ™í∑Ç≠Ç»ÇËÇ∑Ç¨ÇÈÇ∆ÇÊÇ≠Ç»Ç¢


#ÉsÉ{ÉbÉgÉeÅ[ÉuÉãÇÃÉvÉçÉbÉg
rpivotTable(TestDF, rows = colnames(TestDF[1]), cols = colnames(TestDF[4]), width = "100%", height = "400px")
st_waka2 <- melt(st_waka)
rpivotTable(test,width = "100%", height = "400px")
head(mtcars)
