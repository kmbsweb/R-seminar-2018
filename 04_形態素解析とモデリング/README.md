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



### reference
Cathy O'Neil and Rachel Schutt(2013):Doing Data Science-Straight Talk.   
金明哲・董彦文(2015):経営と信用リスクのデータ科学,共立出版.  
