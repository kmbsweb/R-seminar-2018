#データの読み込み
nisshin <- read.csv("https://raw.githubusercontent.com/kmbsweb/R-seminar-2018/master/05_%E7%99%BA%E8%A1%A8/gurume.csv",
                    header=T, fileEncoding="Shift_JIS",stringsAsFactors = FALSE)

#ggplotのインストール
library(ggplot2) 

#ヒストグラム
cutsizeself <- ggplot(nisshin,aes(x=カットサイズ_ご自身)) + geom_histogram(bins=5) #bins=5で5本の棒にする
cutsizeold <- ggplot(nisshin,aes(x=カットサイズ_高齢者)) + geom_histogram(bins=5)
softself <- ggplot(nisshin,aes(x=やわらかさ_ご自身)) + geom_histogram(bins=5)
softold <- ggplot(nisshin,aes(x=やわらかさ_高齢者)) + geom_histogram(bins=5)
swallowself <- ggplot(nisshin,aes(x=飲み込みやすさ_ご自身)) + geom_histogram(bins=5)
swallowold <- ggplot(nisshin,aes(x=飲み込みやすさ_ご自身)) + geom_histogram(bins=5)
juicyself <- ggplot(nisshin,aes(x=ジューシーさ_ご自身)) + geom_histogram(bins=5)
juicyold <- ggplot(nisshin,aes(x=ジューシーさ_ご自身)) + geom_histogram(bins=5)
tasteself <- ggplot(nisshin,aes(x=旨み_ご自身)) + geom_histogram(bins=5)
tasteold <- ggplot(nisshin,aes(x=旨み_ご自身)) + geom_histogram(bins=5)
fattyself <- ggplot(nisshin,aes(x=脂っこさ_ご自身)) + geom_histogram(bins=5)
fattyold <- ggplot(nisshin,aes(x=脂っこさ_ご自身)) + geom_histogram(bins=5)
recommend <- ggplot(nisshin,aes(x=高齢者におすすめしたいか)) + geom_histogram(bins=5)
proper <- ggplot(nisshin,aes(x=高齢者福祉施設の入居者向け食事メニューとして適切か)) + geom_histogram(bins=5)

require(grid)
library(grid)
#カットサイズ
grid.newpage() #空の画面を作る
pushViewport(viewport(layout=grid.layout(2, 2))) #画面を区切る（2行1列の4分割）
print(cutsizeself, vp=viewport(layout.pos.row=1, layout.pos.col=1:2)) #1行目の1・2列
print(cutsizeold, vp=viewport(layout.pos.row=2, layout.pos.col=1:2)  ) #2行目の1・2列
#やわらかさ
grid.newpage() #空の画面を作る
pushViewport(viewport(layout=grid.layout(2, 2))) #画面を区切る（2行1列の4分割）
print(softself, vp=viewport(layout.pos.row=1, layout.pos.col=1:2)) #1行目の1・2列
print(softold, vp=viewport(layout.pos.row=2, layout.pos.col=1:2)  ) #2行目の1・2列
#飲み込みやすさ
grid.newpage() #空の画面を作る
pushViewport(viewport(layout=grid.layout(2, 2))) #画面を区切る（2行1列の4分割）
print(swallowself, vp=viewport(layout.pos.row=1, layout.pos.col=1:2)) #1行目の1・2列
print(swallowold, vp=viewport(layout.pos.row=2, layout.pos.col=1:2)  ) #2行目の1・2列
#ジューシーさ
grid.newpage() #空の画面を作る
pushViewport(viewport(layout=grid.layout(2, 2))) #画面を区切る（2行1列の4分割）
print(juicyself, vp=viewport(layout.pos.row=1, layout.pos.col=1:2)) #1行目の1・2列
print(juicyold, vp=viewport(layout.pos.row=2, layout.pos.col=1:2)  ) #2行目の1・2列
#旨み
grid.newpage() #空の画面を作る
pushViewport(viewport(layout=grid.layout(2, 2))) #画面を区切る（2行1列の4分割）
print(tasteself, vp=viewport(layout.pos.row=1, layout.pos.col=1:2)) #1行目の1・2列
print(tasteold, vp=viewport(layout.pos.row=2, layout.pos.col=1:2)  ) #2行目の1・2列
#脂っこさ
grid.newpage() #空の画面を作る
pushViewport(viewport(layout=grid.layout(2, 2))) #画面を区切る（2行1列の4分割）
print(fattyself, vp=viewport(layout.pos.row=1, layout.pos.col=1:2)) #1行目の1・2列
print(fattyold, vp=viewport(layout.pos.row=2, layout.pos.col=1:2)  ) #2行目の1・2列
#おすすめ・適切度
grid.newpage() #空の画面を作る
pushViewport(viewport(layout=grid.layout(2, 2))) #画面を区切る（2行1列の4分割）
print(recomend, vp=viewport(layout.pos.row=1, layout.pos.col=1:2)) #1行目の1・2列
print(proper, vp=viewport(layout.pos.row=2, layout.pos.col=1:2)  ) #2行目の1・2列

#記述統計量
summary(nisshin)

#クロス集計表
cutsizeself <- table(nisshin$調理名,nisshin$カットサイズ_ご自身)
cutsizeold <- table(nisshin$調理名,nisshin$カットサイズ_高齢者)
softself <- table(nisshin$調理名,nisshin$やわらかさ_ご自身)
softold	<- table(nisshin$調理名,nisshin$やわらかさ_高齢者)
swallowself <- table(nisshin$調理名,nisshin$飲み込みやすさ_ご自身)
swallowold <- table(nisshin$調理名,nisshin$飲み込みやすさ_高齢者)
juicyself <- table(nisshin$調理名,nisshin$ジューシーさ_ご自身)
juicyold <- table(nisshin$調理名,nisshin$ジューシーさ_高齢者)
tasteself <- table(nisshin$調理名,nisshin$旨み_ご自身)
tasteold <- table(nisshin$調理名,nisshin$旨み_高齢者)
fattyself <- table(nisshin$調理名,nisshin$脂っこさ_ご自身)
fattyold <- table(nisshin$調理名,nisshin$脂っこさ_高齢者)
recommend <- table(nisshin$調理名,nisshin$高齢者におすすめしたいか)
proper <- table(nisshin$調理名,nisshin$高齢者福祉施設の入居者向け食事メニューとして適切か)

#install package
install.packages("ca")
library(ca)

# caパッケージ、ca関数で分析
ca_recommend <- ca(recommend)
ca_recommend
plot(ca_recommend)
plot(ca_recommend, mass = TRUE, contrib = "absolute", map = "rowgreen", arrows = c(FALSE, TRUE)) # 非対称プロット

#クラスカル・ウォリス検定（対応の無い多群の差の検定）
result <- kruskal.test(nisshin$高齢者におすすめしたいか ~ nisshin$調理名)
result

#多重比較
TukeyHSD(aov(nisshin$高齢者におすすめしたいか ~ nisshin$調理名))
