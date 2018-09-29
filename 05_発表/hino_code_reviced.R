#packageの読み込み
library(ggplot2) 
library(reshape2)
library(Hmisc)
library(ca)

#データの読み込み
nisshin <- read.csv("https://raw.githubusercontent.com/kmbsweb/R-seminar-2018/master/05_%E7%99%BA%E8%A1%A8/gurume.csv",
                    header=T, fileEncoding="Shift_JIS",stringsAsFactors = FALSE)

#データフレームを加工
data <- melt(nisshin, id.vars=c("ＮＯ", "調理名", "メニュー", "会場"),
              variable.name="label", na.rm=TRUE)

#会場別、項目別のヒストグラム
ggplot(data, aes(x = value, fill = 会場)) +
  　geom_histogram(stat="count") +
  　facet_grid(会場 ~ label, scales = "free") +
  　theme_bw(base_family = "HiraKakuPro-W3")

#correlation matrixの出力
result <- rcorr(as.matrix(nisshin[,5:18]))
write.csv(result$r,"result.csv")

#nisshinデータの5〜18列をクロス集計
#その結果をlistに格納
x <-list()
for (i in 5:18) {             
  table <- table(nisshin$調理名,nisshin[,i])
  x <- c(x,list(table)) 
}
x
recommend <- x[[13]]

# caパッケージ、ca関数で分析
ca_recommend <- ca(recommend)
ca_recommend
plot(ca_recommend)
plot(ca_recommend, mass = TRUE, contrib = "absolute", map = "rowgreen", arrows = c(FALSE, TRUE)) # 非対称プロット

#クラスカル・ウォリス検定（対応の無い多群の差の検定）
result <- kruskal.test(nisshin$高齢者におすすめしたいか ~ nisshin$調理名)

#多重比較
TukeyHSD(aov(nisshin$高齢者におすすめしたいか ~ nisshin$調理名))