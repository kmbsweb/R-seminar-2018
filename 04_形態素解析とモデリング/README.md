## 形態素解析とモデリング

`目的`  
-　Rmecabによる形態素解析  
-　wordcloudによる可視化と考察   
-　SVMを用いた業種分類モデルの構築  

### 形態素解析について
- 
- ダウンロード
  [こちら](http://taku910.github.io/mecab/#install-windows)よりRmecabをダウンロードする必要があります。  

### wordcloud


### 例題
Radの歌詞を可視化

  
- 歌詞  
`最大公約数`  [youtube](https://www.youtube.com/watch?v=cDpL4-UEs5s)  
`me me she`   [youtube](https://www.youtube.com/watch?v=wvvItrHEfRA)   
`前前前世`  [youtube](https://www.youtube.com/watch?v=PDSkFeMVNFs)  
`有心論`   [youtube](https://www.youtube.com/watch?v=c2y8Ba3WwPY)  
※原文そのままをテキスト化。  
  


### 形態素解析と可視化
ここでは、実際にRのコマンドを実行する。

```R
install.packages("RMeCab", repos = "http://rmecab.jp/R")
library(RMeCab)

# 解析対象となるデータの読み込み
res <- RMeCabFreq("最大公約数.txt")
res <- res[res[,2]=="名詞",]
res1 <- RMeCabFreq("me me she.txt")
res1 <- res1[res1[,2]=="名詞",]
res2 <- RMeCabFreq("前前前世.txt")
res2 <- res2[res2[,2]=="名詞",]
res3 <- RMeCabFreq("有心論.txt")
res3 <- res3[res3[,2]=="名詞",]

#ライブラリの読み込み
install.packages("wordcloud")
library("wordcloud")
#macで日本語文字化け防止は下記を実行
par(family = "HiraKakuProN-W3")

#wordcloudコマンド
#scale=c(3,1):c(文字の大きさ,文字間隔）を指定
#random.order＝FALSE：FALSEで頻度順に中心から描画
wordcloud(res1$Term, res1$Freq, colors = brewer.pal(8, "Dark2"),
          scale=c(3,1),min.freq=90,random.color=FALSE)
```

![](https://github.com/kmbsweb/R-seminar-2018/blob/master/04_%E5%BD%A2%E6%85%8B%E7%B4%A0%E8%A7%A3%E6%9E%90%E3%81%A8%E3%83%A2%E3%83%87%E3%83%AA%E3%83%B3%E3%82%B0/text/result.png)

###SVMを用いた業種分類モデル  

![](https://github.com/kmbsweb/R-seminar-2018/blob/master/04_%E5%BD%A2%E6%85%8B%E7%B4%A0%E8%A7%A3%E6%9E%90%E3%81%A8%E3%83%A2%E3%83%87%E3%83%AA%E3%83%B3%E3%82%B0/text/data_set.png)

```R
target <- read.csv("test1.csv", header=T, fileEncoding="Shift_JIS",,as.is =T)

#第1引数:ファイル名
#col:対象とする列
#type=0は意味を持たない文字数、type=1は意味を持つ塊りでみる
res <- docDF(target, col = 2, type=1, N=1,pos = c("名詞"), Genkei = 1, nDF = 1)
res$sum <- rowSums(res[,4:162])    
res2 <- subset(res,res$sum >= 10 )

FRM <- data.frame(t(res2))
FRM <- FRM[-1:-3,]
FRM <- FRM[-160,]
FRM <- cbind(target,FRM)

FRM <- FRM[,-1:-2]
res2$N1

#データを2つに分ける
train <- FRM[c(-5,-20,-30,-70,-120),]
test <- FRM[c(5,20,30,70,120),]


#SVMのパッケージ
install.packages("kernlab")
library(kernlab)

#SVM
#C-svc C classification
svm_1 <- ksvm(家賃保証~. , data=train, type="C-svc")
svmp_1 <- ksvm(家賃保証~. , data=train)

#でき上がったモデルを確認
svm_1
svmp_1

predict(svm_1, test)
predict(svmp_1, test)
result <- data.frame(target[c(5,20,30,70,120),],
                     predict(svm_1, test),
                     predict(svmp_1, test))
```

###過学習を回避する
モデルを修正していく。

```R
#モデルの修正
res2 <- subset(res,res$sum >= 10 )
text <- c("クレジット","リース", "不動産" ,"仲介" ,"住宅",
          "保証","借主","家賃","賃料","債権","賃貸" )
res3 <- subset(res2,N1 %in% text)

FRM <- data.frame(t(res3))
FRM <- FRM[-1:-3,]
FRM <- FRM[-160,]
FRM <- cbind(target,FRM)

FRM <- FRM[,-1:-2]

#データを2つに分ける
train <- FRM[c(-5,-20,-30,-70,-120),]
test <- FRM[c(5,20,30,70,120),]

#SVM
svm_2 <- ksvm(家賃保証~. , data=train,  type="C-svc")
svmp_2 <- ksvm(家賃保証~. , data=train)

#でき上がったモデルを確認
svm_2
svmp_2

predict(svm_2, test)
predict(svmp_2, test)
result <- data.frame(target[c(5,20,30,70,120),],
                     predict(svm_2, test),
                     predict(svmp_2, test))
```

### reference
Cathy O'Neil and Rachel Schutt(2013):Doing Data Science-Straight Talk.   
金明哲・董彦文(2015):経営と信用リスクのデータ科学,共立出版.  
