## 信用リスク評価とポートフォリオ

*目的*  
-　判別式を用いたリスク評価  
-　ポートフォリオを作成する  
-　クロス集計の自動化  

### 予測について
- 予測を外した地震学者[pdf](https://github.com/kmbsweb/R-seminar-2018/blob/master/03_%E4%BF%A1%E7%94%A8%E3%83%AA%E3%82%B9%E3%82%AF%E8%A9%95%E4%BE%A1%E3%81%A8%E3%83%9D%E3%83%BC%E3%83%88%E3%83%95%E3%82%A9%E3%83%AA%E3%82%AA/pic/2012102310126345.pdf) 
- 予測値と上手く付き合っていく

### 信用評価の手法
信用評価モデルは、金融機関だけでなく信用取引が発生するありとあらゆる業種・業態で必要となります。その信用評価モデルを構築するにあたっては決算書などの定量的な情報が必要となり、それら情報をもとにモデルを構築していくことになると思います。しかし、出資や多額の貸付を行っている場合を除けば、決算書はなかな手に入れるとは難しい上、零細企業を含めた取引先全ての財務データを収集するとなると多大なコストがかかります。    
では、限られた情報の中からどのようにして信用評価モデルを構築すれば良いのでしょうか？格付会社や銀行、商社などで独自の信用評価モデルが構築されていますが、予測手法として大別すると下記の3つの手法を用いてモデル構築を行っているものと思われます。  

【予測手法】  
- 判別式  
- ロジスティック回帰分析  
- SVM  

回帰分析：影響要因の有無、係数の大きさ
判別分析：3択以上の判別が可能

### 例題
Atman(1968)が判別分析を倒産企業判別に適用したことに倣い下記の例題を考えてみます。  
  
花きの卸売を行っているA社の売上規模は10億円程度、営業損益が100万円のプラスである。従業員数は役員を除いて20名程度で構成される。取引先は卸売業や小売業が中心であるが、個人事業主（町の花屋やレッスンを行う個人）も少なくない。最近は、大型量販店が台頭する中、売上が確保するのも容易ではなく、個人事業主の廃業などもあり、未回収の債権や焦げ付き債権も発生しており、与信管理が急務となっている。  
信用調査会社に問い合わせたものの、金額が高い上、個人事業主の情報は当然のごとく充実していないため、あまり役に立たたないのではないかと感じている。  
現状としては、社内に下記のような取引データは蓄積されている。  
  
---------------------------------------------------------------
*取引先区分 企業（ダミー）*  
*取引先区分 個人（ダミー）*  
*滞納額*  
*滞納日数*  
*滞納回数*  
*売上*  
*滞納割合（滞納平均額/売上合計値）*  
*取引月数（売上が発生した月数）*  
---------------------------------------------------------------
※3～8は標準化処理を施している。  


### 判別分析

```R
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
```
### 回帰分析の利用
step関数を用いて、有効な変数選択（絞り込み）を行う。

```R
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

```

### ポートフォリオ作成
ここでは、仮の取引額を設定しポートフォリオを作成する。  

```R
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

```

![](https://github.com/kmbsweb/R-seminar-2018/blob/master/03_%E4%BF%A1%E7%94%A8%E3%83%AA%E3%82%B9%E3%82%AF%E8%A9%95%E4%BE%A1%E3%81%A8%E3%83%9D%E3%83%BC%E3%83%88%E3%83%95%E3%82%A9%E3%83%AA%E3%82%AA/pic/cap1.JPG)

倒産予測値が高く、取引額が多い、いわゆる「要注意先」をマーキングしてみます。

```R
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
```
![](https://github.com/kmbsweb/R-seminar-2018/blob/master/03_%E4%BF%A1%E7%94%A8%E3%83%AA%E3%82%B9%E3%82%AF%E8%A9%95%E4%BE%A1%E3%81%A8%E3%83%9D%E3%83%BC%E3%83%88%E3%83%95%E3%82%A9%E3%83%AA%E3%82%AA/pic/cap2.JPG)


