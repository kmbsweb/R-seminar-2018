#判別分析

#ライブラリーMASSの読み込み
library(MASS) 

#データの読み込
#setwd("C:/Users/mkeig/Desktop/R-seminar-2018/03_信用リスク評価とポートフォリオ")
X <- read.csv("取引データ.csv",row.names=1, header = T)　

#判別分析
res <- lda(格付~区分1+区分2+滞納額+滞納日数+滞納回数+売上+取引月+滞納割合,X) 

#Proportion of trace:から、LD1 に大きな判別力
res

#取引先ごとの判別結果
res2 <- predict(res) 
res2

#実分類と判別結果のクロス表
cls <- table(X$格付,res2$class) 

#対角線にどれだけ乗せられているか？
cls[row(cls)==col(cls)]
#全体
sum(cls) 
#判別正解率を算出
sum(cls[row(cls)==col(cls)])/sum(cls) 
#誤判別率を算出
sum(cls[row(cls)!=col(cls)])/sum(cls) 

#企業ごとの実格付と判別結果一覧
data.frame(row.names =row.names(X),実格付=X$格付,判別結果=res2$class) 


#回帰分析に基づき有効な指標（変数）の選択
lm(格付~区分1+区分2+滞納額+滞納日数+滞納回数+売上+取引月+滞納割合, data=X)

#変数増減法による変数選択
h <- step(lm(格付~区分1+区分2+滞納額+滞納日数+滞納回数+売上+取引月+滞納割合, data=X))
summary(h)

#選択された指標だけを用いた判別分析
res3 <- lda(格付~滞納日数+区分1+滞納割合+区分2+取引月+滞納額,X)

#取引先ごとの判別結果
res4 <- predict(res3)  
#実格付と判別結果のクロス表
cls2 <- table(X$格付,res4$class)
#判別正解率を算出
sum(cls2[row(cls2)==col(cls2)])/sum(cls2) 
#誤判別率を算出
sum(cls2[row(cls2)!=col(cls2)])/sum(cls2) 


##predict use
train <- X[490:498,]
testResult<-predict(res3,train)
data.frame(testResult$x[,1],train[,9])


##取引額を仮に設定する
XX <- data.frame(X,sample(c(1:5000), 498, replace=TRUE))

Result <- predict(res3,XX)
pre <- data.frame(XX, Result$class, Result$x[,1],rownames(XX))
names(pre)[10] <- "年取引額" 
names(pre)[11] <- "予測格付" 
names(pre)[12] <- "予測"
names(pre)[13] <- "name"

install.packages("rlang")
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(RColorBrewer)
library(plyr)


##range
cols <- c("4" = "dodgerblue4", "3" = "dodgerblue3", "2" = "lightsteelblue1", "1" = "lightslategrey")
##plot
ggplot(pre, aes(予測, 年取引額,color=予測格付)) +
  geom_vline(xintercept=0,colour = "black",alpha=0.3,linetype="dashed") + 
  geom_hline(yintercept=1000,colour = "black",alpha=0.3,linetype="dashed") + 
  geom_point(alpha=0.7) + 
  labs(title="") + 
  xlab("予測") +
  ylab("年取引額(千円)") + 
  scale_colour_manual(values = cols)+
  theme_classic(base_family="HiraKakuProN-W3") 

##plot2
ggplot(pre, aes(予測, 年取引額,color=予測格付)) +
  geom_vline(xintercept=0,colour = "black",alpha=0.3,linetype="dashed") + 
  geom_hline(yintercept=1000,colour = "black",alpha=0.3,linetype="dashed") + 
  geom_point(alpha=0.7) + 
  labs(title="") + 
  xlab("予測") +
  ylab("年取引額(千円)") + 
  scale_colour_manual(values = cols)+
  geom_text_repel(data=subset(pre, 年取引額 >= 3000 & 予測 >= 2),
                  aes(x=予測,y=年取引額, label = name),
                  col = "black", size = 2.5,segment.color = NA ,
                  family = "HiraKakuPro-W3")+
  annotate("rect",xmin=2, xmax=max(pre$予測), ymin=3000, ymax=max(pre$年取引額),
           alpha=0.2, fill="red") +
  theme_classic(base_family="HiraKakuProN-W3") 
