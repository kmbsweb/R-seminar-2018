#徳島のラーメン店の分布について
library(ggplot2)
library(ggmap)
geo_r <-read.csv("ramen.csv", header=T, fileEncoding="Shift_JIS",stringsAsFactors = FALSE)
loc<-c(134.55076,34.07464)
#徳島駅の位置情報（経度,緯度）

N<-ggmap(get_map(location = loc,zoom=12,source = 'google')) + xlab('') + ylab('')
N+geom_point(data=geo_r,aes(x=geo_r$fX,y=geo_r$fY),col="black")
N+stat_density2d(aes(x=geo_r$fX,y=geo_r$fY, fill = ..level.., alpha = ..level..),
                 bins = 30, geom = "polygon",
                 data=geo_r) + scale_fill_gradient(low = "navy blue", high = "red")

##食べログの評価と駅、港からの距離は？アクセスが悪い場所=高評価だったり？
##予想としては、やはり駅に近いところ（アクセスの良い場所）がよく利用されることから、、、
##美味しい店も集まって来やすいのでは？（競争的な意味でも）

summary(geo_r[,11:12])

top<-subset(geo_r,geo_r$口コミ数>90)
P<-ggplot(geo_r,aes(x=geo_r$食べログ評価,y=geo_r$口コミ数))+
  geom_point(col="#FF6699",size=3)+
  theme_bw(base_family = "HiraKakuProN-W3")+
  geom_smooth(method="lm",se=F,color="black",size=1)+
  geom_text(data=top,aes(x=top$食べログ評価,y=top$口コミ数,label=top$店名),size=3,vjust=2,hjust=.5,col="black",family = "HiraKakuPro-W3")
P
cor.test(x=geo_r$食べログ評価,y=geo_r$口コミ数)
#外れ値を除外し、口コミ数200以下でplotしてみる
geo_200<-subset(geo_r,geo_r$口コミ数<200)
P200<-ggplot(geo_200,aes(x=geo_200$食べログ評価,y=geo_200$口コミ数))+
  geom_point(col="#FF6699",size=3)+
  theme_bw(base_family = "HiraKakuProN-W3")+
  geom_smooth(method="lm",se=F,color="black",size=1)
P200
summary(geo_200[,11:12])
cor.test(x=geo_200$食べログ評価,y=geo_200$口コミ数)

#histgram
par(family = "HiraKakuProN-W3")
par(mar=c(5, 5, 4, 2) + 0.1)
hist(geo_r$食べログ評価,col ="#0000ff40" , border = "#0000ff",cex.lab=2,cex.axis=2,breaks = 30,main ="",xlab ="",cex.main=1)
hist(geo_200$食べログ評価,col ="#0000ff40" , border = "#0000ff",cex.lab=2,cex.axis=2,breaks = 30,main ="",xlab ="",cex.main=1)

##区分を作成
library("fancycut")
geo_r$評価区分 <- fancycut(x = geo_r[,11],
                     "3.0~" = "[3.0,3.1)","3.1~" = "[3.1,3.2)","3.2~" = "[3.2,3.3)",
                     "3.3~" = "[3.3,3.58]",
                     unmatched.bucket ='other')

N+geom_point(data=geo_r,aes(x=geo_r$fX,y=geo_r$fY,
                              colour=geo_r$評価区分,
                              group=geo_r$評価区分),size=2.5)+
  theme_bw(base_family = "HiraKakuProN-W3")

geo0<-subset(geo_r,geo_r$評価区分=="3.0~")
geo1<-subset(geo_r,geo_r$評価区分=="3.1~")
geo2<-subset(geo_r,geo_r$評価区分=="3.2~")
geo3<-subset(geo_r,geo_r$評価区分=="3.3~")

Np0<-N+geom_point(data=geo0,aes(x=geo0$fX,y=geo0$fY),col="black")
Nd0<-N+stat_density2d(aes(x=geo0$fX,y=geo0$fY, fill = ..level.., alpha = ..level..),
                 bins = 30, geom = "polygon",
                 data=geo0) + scale_fill_gradient(low = "navy blue", high = "red")+
  labs(title="評価3.0")+
  theme_bw(base_family = "HiraKakuProN-W3")

Np1<-N+geom_point(data=geo1,aes(x=geo1$fX,y=geo1$fY),col="black")
Nd1<-N+stat_density2d(aes(x=geo1$fX,y=geo1$fY, fill = ..level.., alpha = ..level..),
                  bins = 30, geom = "polygon",
                  data=geo1) + scale_fill_gradient(low = "navy blue", high = "red")+
  labs(title="評価3.1")+
  theme_bw(base_family = "HiraKakuProN-W3")

Np2<-N+geom_point(data=geo2,aes(x=geo2$fX,y=geo2$fY),col="black")
Nd2<-N+stat_density2d(aes(x=geo2$fX,y=geo2$fY, fill = ..level.., alpha = ..level..),
                  bins = 30, geom = "polygon",
                  data=geo2) + scale_fill_gradient(low = "navy blue", high = "red")+
  labs(title="評価3.2")+
  theme_bw(base_family = "HiraKakuProN-W3")

Np3<-N+geom_point(data=geo3,aes(x=geo3$fX,y=geo3$fY),col="black")
Nd3<-N+stat_density2d(aes(x=geo3$fX,y=geo3$fY, fill = ..level.., alpha = ..level..),
                  bins = 30, geom = "polygon",
                  data=geo3) + scale_fill_gradient(low = "navy blue", high = "red")+
  labs(title="評価3.3以上")+
  theme_bw(base_family = "HiraKakuProN-W3")
summary(geo_r)
#map分割表示
library(gridExtra)
grid.arrange(Np0,Np1,Np2,Np3)
grid.arrange(Nd0,Nd1,Nd2,Nd3)


#-------------------------------------------------------

##距離計測
##徳島駅から
toku<-geo_r[,7:8]
ts<-data.frame(fX=134.55076,fY=34.07464)
toku_s<-as.matrix(rbind(ts,toku))
#徳島港から
tp<-data.frame(fX=134.58419,fY=34.05644)
toku_p<-as.matrix(rbind(tp,toku))
#ICから
tic<-data.frame(fX=134.57689,fY=34.10278)
toku_ic<-as.matrix(rbind(tic,toku))

#各地点からの距離算出
library("geosphere")
distances_s <- distm(toku_s, fun = distGeo)
distances_p <- distm(toku_p, fun = distGeo)
distances_ic <- distm(toku_ic, fun = distGeo)

toku_s<-as.data.frame(distances_s[2:88,1])
colnames(toku_s)<-"距離（駅）"
toku_p<-as.data.frame(distances_p[2:88,1])
colnames(toku_p)<-"距離（港）"
toku_ic<-as.data.frame(distances_ic[2:88,1])
colnames(toku_ic)<-"距離（IC）"

#m→km変換
toku_s<-round(toku_s/1000,2)
toku_p<-round(toku_p/1000,2)
toku_ic<-round(toku_ic/1000,2)
summary(toku_s)
hist(toku_s$`距離（駅）`,col ="#0000ff40" , border = "#0000ff",cex.lab=2,cex.axis=2,breaks = 30,main ="",xlab ="",cex.main=1)
summary(toku_p)
hist(toku_p$`距離（港）`,col ="#0000ff40" , border = "#0000ff",cex.lab=2,cex.axis=2,breaks = 30,main ="",xlab ="",cex.main=1)
summary(toku_ic)
hist(toku_ic$`距離（IC）`,col ="#0000ff40" , border = "#0000ff",cex.lab=2,cex.axis=2,breaks = 30,main ="",xlab ="",cex.main=1)

#元のデータと合わせる
geo_r<-cbind(geo_r,toku_s,toku_p,toku_ic)
#分布確認
geo_r1<-data.frame(geo_r[,11:12],geo_r[,14:16])
library(PerformanceAnalytics)
par(family = "HiraKakuProN-W3")
chart.Correlation(geo_r1)
##結果、港と関係はないが、駅(5%有意)、IC(10%有意)の弱い逆相関がある
##駅からの距離と口コミ数が弱い逆相関(5%有意)
##つまり、口コミする客（徳島に入ってくる人間）の多くは駅からアクセスすることが多いのでは？
##plotの各地点からの距離が近い店舗が固まっている部分が存在している・・・

##駅・港・ICからの距離ごとに、各評価区分別にplotしていく
##距離区分として、全て1km未満・1km-2km未満・2km以上
##1km未満=徒歩で抵抗感のない距離
##2km未満=自転車で抵抗感のない距離
##2km以上=その他

#----------------------------------------------

##駅
cut_s<-data.frame(geo_r$店名,geo_r$`距離（駅）`)
colnames(cut_s)<-c("店名","距離（駅）")
max(cut_s$`距離（駅）`)
cut_s$距離区分s <- fancycut(x = cut_s[,2],
                     "1km未満" = "[0,1)","1km~2km" = "[1,2)","2km~" = "[2,8.16]",
                     unmatched.bucket ='other')
#join
#inner_joinによって符合している項目を対象としてデータをマッチングできる
#byの後ろはkeyとなる変数名
#joinすることで何故か重複した行が出現するので、distinctによって重複分を除外
#.keep_all = Tによってbyで指定した変数列以外が除外されることを防ぐ
library(dplyr)
#駅3.0~
geo0_S<-inner_join(geo0,cut_s,by="店名")
geo0_S<-distinct(geo0_S,geo0_S$店名,geo0_S$所在地,.keep_all = T)
geo0_S<-geo0_S[,1:15]
N0_s<-N+geom_point(data=geo0_S,aes(x=geo0_S$fX,y=geo0_S$fY,
                             colour=geo0_S$距離区分s,
                             group=geo0_S$距離区分s),size=2.5)+
  labs(title="評価3.0")+
  theme_bw(base_family = "HiraKakuProN-W3")
#駅3.1~
geo1_S<-inner_join(geo1,cut_s,by="店名")
geo1_S<-distinct(geo1_S,geo1_S$店名,geo1_S$所在地,.keep_all = T)
geo1_S<-geo1_S[,1:15]
N1_s<-N+geom_point(data=geo1_S,aes(x=geo1_S$fX,y=geo1_S$fY,
                                colour=geo1_S$距離区分s,
                                group=geo1_S$距離区分s),size=2.5)+
  labs(title="評価3.1")+
  theme_bw(base_family = "HiraKakuProN-W3")
#駅3.2~
geo2_S<-inner_join(geo2,cut_s,by="店名")
geo2_S<-distinct(geo2_S,geo2_S$店名,geo2_S$所在地,.keep_all = T)
geo2_S<-geo2_S[,1:15]
N2_s<-N+geom_point(data=geo2_S,aes(x=geo2_S$fX,y=geo2_S$fY,
                                colour=geo2_S$距離区分s,
                                group=geo2_S$距離区分s),size=2.5)+
  labs(title="評価3.2")+
  theme_bw(base_family = "HiraKakuProN-W3")
#駅3.3~
geo3_S<-inner_join(geo3,cut_s,by="店名")
geo3_S<-distinct(geo3_S,geo3_S$店名,geo3_S$所在地,.keep_all = T)
geo3_S<-geo3_S[,1:15]
N3_s<-N+geom_point(data=geo3_S,aes(x=geo3_S$fX,y=geo3_S$fY,
                                colour=geo3_S$距離区分s,
                                group=geo3_S$距離区分s),size=2.5)+
  labs(title="評価3.3")+
  theme_bw(base_family = "HiraKakuProN-W3")

grid.arrange(N0_s,N1_s,N2_s,N3_s)
駅3.0<-summary(geo0_S$距離区分s)
駅3.1<-summary(geo1_S$距離区分s)
駅3.2<-summary(geo2_S$距離区分s)
駅3.3<-summary(geo3_S$距離区分s)
駅3.0<-駅3.0/41*100
駅3.1<-駅3.1/13*100
駅3.2<-駅3.2/11*100
駅3.3<-駅3.3/22*100
list_s<-data.frame(駅3.0,駅3.1,駅3.2,駅3.3)
list_s
##駅からの区分では、最高評価分の1km未満や1km~2km未満の割合が高い（他と比較して）
##高評価区分内では2km未満で50%を超えるが、2km越えの店舗でも高い割合で分布
##他の評価区分で2km以上の割合は高い→遠いからこそ人があまり行かないor評価する母数が少ない？

#港
cut_p<-data.frame(geo_r$店名,geo_r$`距離（港）`)
colnames(cut_p)<-c("店名","距離（港）")
max(cut_p$`距離（港）`)
cut_p$距離区分p <- fancycut(x = cut_p[,2],
                        "1km未満" = "[0,1)","1km~2km" = "[1,2)","2km~" = "[2,11.62]",
                        unmatched.bucket ='other')
#join
#港3.0~
geo0_P<-inner_join(geo0,cut_p,by="店名")
geo0_P<-distinct(geo0_P,geo0_P$店名,geo0_P$所在地,.keep_all = T)
geo0_P<-geo0_P[,1:15]
N0_p<-N+geom_point(data=geo0_P,aes(x=geo0_P$fX,y=geo0_P$fY,
                                      colour=geo0_P$距離区分p,
                                      group=geo0_P$距離区分p),size=2.5)+
  labs(title="評価3.0")+
  theme_bw(base_family = "HiraKakuProN-W3")
#港3.1~
geo1_P<-inner_join(geo1,cut_p,by="店名")
geo1_P<-distinct(geo1_P,geo1_P$店名,geo1_P$所在地,.keep_all = T)
geo1_P<-geo1_P[,1:15]
N1_p<-N+geom_point(data=geo1_P,aes(x=geo1_P$fX,y=geo1_P$fY,
                                      colour=geo1_P$距離区分p,
                                      group=geo1_P$距離区分p),size=2.5)+
  labs(title="評価3.1")+
  theme_bw(base_family = "HiraKakuProN-W3")

#港3.2~
geo2_P<-inner_join(geo2,cut_p,by="店名")
geo2_P<-distinct(geo2_P,geo2_P$店名,geo2_P$所在地,.keep_all = T)
geo2_P<-geo2_P[,1:15]
N2_p<-N+geom_point(data=geo2_P,aes(x=geo2_P$fX,y=geo2_P$fY,
                                      colour=geo2_P$距離区分p,
                                      group=geo2_P$距離区分p),size=2.5)+
  labs(title="評価3.2")+
  theme_bw(base_family = "HiraKakuProN-W3")

#港3.3~
geo3_P<-inner_join(geo3,cut_p,by="店名")
geo3_P<-distinct(geo3_P,geo3_P$店名,geo3_P$所在地,.keep_all = T)
geo3_P<-geo3_P[,1:15]
N3_p<-N3+geom_point(data=geo3_P,aes(x=geo3_P$fX,y=geo3_P$fY,
                                      colour=geo3_P$距離区分p,
                                      group=geo3_P$距離区分p),size=2.5)+
  labs(title="評価3.3")+
  theme_bw(base_family = "HiraKakuProN-W3")

grid.arrange(N0_p,N1_p,N2_p,N3_p)
港3.0<-summary(geo0_P$距離区分p)
港3.1<-summary(geo1_P$距離区分p)
港3.2<-summary(geo2_P$距離区分p)
港3.3<-summary(geo3_P$距離区分p)
港3.0<-港3.0/41*100
港3.1<-港3.1/13*100
港3.2<-港3.2/11*100
港3.3<-港3.3/22*100
list_p<-data.frame(港3.0,港3.1,港3.2,港3.3)
list_p
##どの評価区分についても、8割が2km以上離れた場所に立地している
##あまり港-ラーメンでのルートは考えられていないのではないか
##徒歩の場合、少々2kmは遠い
##船で徳島にきて、「さぁ、美味しい徳島のラーメンを楽しもうか」、とはならない立地
##その他は特に特徴などは見られなかった

#IC
cut_ic<-data.frame(geo_r$店名,geo_r$`距離（IC）`)
colnames(cut_ic)<-c("店名","距離（IC）")
max(cut_ic$`距離（IC）`)
cut_ic$距離区分ic <- fancycut(x = cut_ic[,2],
                        "1km未満" = "[0,1)","1km~2km" = "[1,2)","2km~" = "[2,11.31]",
                        unmatched.bucket ='other')
#join
#IC3.0~
geo0_ic<-inner_join(geo0,cut_ic,by="店名")
geo0_ic<-distinct(geo0_ic,geo0_ic$店名,geo0_ic$所在地,.keep_all = T)
geo0_ic<-geo0_ic[,1:15]
N0_ic<-N+geom_point(data=geo0_ic,aes(x=geo0_ic$fX,y=geo0_ic$fY,
                                      colour=geo0_ic$距離区分ic,
                                      group=geo0_ic$距離区分ic),size=2.5)+
  labs(title="評価3.0")+
  theme_bw(base_family = "HiraKakuProN-W3")

#IC3.1~
geo1_ic<-inner_join(geo1,cut_ic,by="店名")
geo1_ic<-distinct(geo1_ic,geo1_ic$店名,geo1_ic$所在地,.keep_all = T)
geo1_ic<-geo1_ic[,1:15]
N1_ic<-N+geom_point(data=geo1_ic,aes(x=geo1_ic$fX,y=geo1_ic$fY,
                                      colour=geo1_ic$距離区分ic,
                                      group=geo1_ic$距離区分ic),size=2.5)+
  labs(title="評価3.1")+
  theme_bw(base_family = "HiraKakuProN-W3")

#IC3.2~
geo2_ic<-inner_join(geo2,cut_ic,by="店名")
geo2_ic<-distinct(geo2_ic,geo2_ic$店名,geo2_ic$所在地,.keep_all = T)
geo2_ic<-geo2_ic[,1:15]
N2_ic<-N+geom_point(data=geo2_ic,aes(x=geo2_ic$fX,y=geo2_ic$fY,
                                      colour=geo2_ic$距離区分ic,
                                      group=geo2_ic$距離区分ic),size=2.5)+
  labs(title="評価3.2")+
  theme_bw(base_family = "HiraKakuProN-W3")

#IC3.3~
geo3_ic<-inner_join(geo3,cut_ic,by="店名")
geo3_ic<-distinct(geo3_ic,geo3_ic$店名,geo3_ic$所在地,.keep_all = T)
geo3_ic<-geo3_ic[,1:15]
N3_ic<-N+geom_point(data=geo3_ic,aes(x=geo3_ic$fX,y=geo3_ic$fY,
                                      colour=geo3_ic$距離区分ic,
                                      group=geo3_ic$距離区分ic),size=2.5)+
  labs(title="評価3.3")+
  theme_bw(base_family = "HiraKakuProN-W3")

IC3.0<-summary(geo0_ic$距離区分ic)
IC3.1<-summary(geo1_ic$距離区分ic)
IC3.2<-summary(geo2_ic$距離区分ic)
IC3.3<-summary(geo3_ic$距離区分ic)
IC3.0<-IC3.0/41*100
IC3.1<-IC3.1/13*100
IC3.2<-IC3.2/11*100
IC3.3<-IC3.3/22*100
list_ic<-data.frame(IC3.0,IC3.1,IC3.2,IC3.3)
list_ic
##現在の距離区分で見ると、港の場合とほとんど同じ
##相関係数で確認されるような近距離に高評価の店舗はあまりないように思える
##ただし、これまで駅や港は徒歩、自転車移動を前提に対象からの距離を計算していた
##車であれば2km以上の距離であっても抵抗感は小さい
##距離区分自体が間違っており、見直しの必要があるor単純に駅や港と比較不可能なのではないか


library(rmarkdown)
library(knitr)
render("kobe_180929.rmd")

