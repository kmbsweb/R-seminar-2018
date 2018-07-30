## 形態素解析とモデリング

`目的`  
-　Rmecabによる形態素解析  
-　wordcloudによる可視化と考察   
-　SVMを用いた業種分類モデルの構築  

### 形態素解析について
形態素解析（morphological analysis）は、自然言語処理の手法の一つで、ある文章やセンテンスを「意味を持つ最小限の単位（＝単語）」に分解することです。テキストマイニングを行う際によく利用されています。  
例えば、以下のような文があった場合、斜線部で区切られます。文章を一つ一つ品詞分解していくイメージです。  
> 明日/は/阪急/で/梅田/駅/に/行き/ます/。
  
今回は、Rmecabと呼ばれる形態素解析をサポートしているパッケージを用います。以下のサイトよりダウロードして、進めていきます。
- ダウンロード  
  [こちら](http://taku910.github.io/mecab/#install-windows)よりRmecabをダウンロードする必要があります。  

### wordcloud
形態素解析により分解された文章を可視化する手法として*word cloud*パッケージがよく利用されます。頻出度が高いキーワードを大きくしたり、色を変えたりすることも可能です。

### 例題
具体例として、Radwimpsの歌詞（4曲）に対して形態素解析をほどこし、可視化してみたいと思います。Radwimpsの曲は、言葉遊びも多く、歌詞が奥深いという意見も聞かれます。Radwimpsの歌詞が支持される理由やどういう世界観で歌詞が作られているのかを考察するためには、まず形態素解析を行い可視化してみるのが良いかと思います。
  
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

### SVMを用いた業種分類モデル  

<img src="https://github.com/kmbsweb/R-seminar-2018/blob/master/04_%E5%BD%A2%E6%85%8B%E7%B4%A0%E8%A7%A3%E6%9E%90%E3%81%A8%E3%83%A2%E3%83%87%E3%83%AA%E3%83%B3%E3%82%B0/text/data_set.png" width="500px">


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

### 過学習を回避する
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
少ないキーワードで業種分類をすることができるようになった。ただ、今回のケースではモデルが大幅に改善したというわけではない。サンプル数が少ない中では、変数をなるべく少なくしてモデル構築すること方が良いと思われる。  

### reference
Cathy O'Neil and Rachel Schutt(2013):Doing Data Science-Straight Talk.   
