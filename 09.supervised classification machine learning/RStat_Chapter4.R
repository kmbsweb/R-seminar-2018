

library(ISLR)
names(Smarket)
dim(Smarket)
head(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket)
cor(Smarket[,-9])
attach(Smarket)
plot(Volume)

# ロジスティック回帰(株式市場)
# family=binomialでロジスティック回帰指示
# step1 訓練データ = テストデータ
glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial)
summary(glm.fits)

coef(glm.fits)# 結果の一部抜き出し
summary(glm.fits)$coef
summary(glm.fits)$coef[,4]
glm.probs=predict(glm.fits,type="response")# Pr(Y = 1| X)の確率計算
glm.probs[1:10]# 最初の10個を表示
contrasts(Direction)# 0:1の定義確認
# 株式市場上昇の推定確率が0.5よりも大きいかでクラスの予測ベクトル作成
glm.pred=rep("Down",1250)
glm.pred[glm.probs>.5]="Up"　# glm.probsの値に沿って振り分け
table(glm.pred,Direction)　# 精度確認
(507+145)/1250 # 予測が正しかった割合(方法1)
mean(glm.pred==Direction) # 予測が正しかった割合(方法2)
#正しく評価するには訓練データとテストデータを分けて検証するのがbetter

# step2 訓練データ(2001 - 2004),  テストデータ(2005)
train=(Year<2005) #2005年以前のものをT,それ以外をF
Smarket.2005=Smarket[!train,] #デフォルトのtrainはT
dim(Smarket.2005)
Direction.2005=Direction[!train]

# subsetで2005年以前のデータにて再びロジスティック############
glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fits,Smarket.2005,type="response")# テストデータ当てはめ
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005)# 予測が正しくなかった割合(テスト誤分類率)
## 変数を減らして精度向上の有無を検証
glm.fits=glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fits,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
106/(106+76)
#特定の日の利益を予測する場合
predict(glm.fits,newdata=data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type="response")

# 線形判別分析

library(MASS)
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
lda.fit# 1段目：データの割合　2段目：クラスごとの各変数の平均値 三段目：線形判別関数の係数
plot(lda.fit)
lda.pred=predict(lda.fit, Smarket.2005)# モデルにテストデータ当てはめ
names(lda.pred)
lda.class=lda.pred$class
table(lda.class,Direction.2005)
mean(lda.class==Direction.2005)
# 事後確率50%(境界)においてのdownの数
sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)
# downの確率とそれに対応するクラスを確認
lda.pred$posterior[1:20,1]
lda.class[1:20]
# downの事後確率が50%以外となる境界を設定することも可能
sum(lda.pred$posterior[,1]>.9) #事後確率が90%以上の時のみ株式市場がdownする予測をしたい時


# 二次判別分析
# 計算方法はLDAと同様
qda.fit=qda(Direction~Lag1+Lag2,data=Smarket,subset=train)
qda.fit # 線形判別関数の係数は二次関数を用いているためない
qda.class=predict(qda.fit,Smarket.2005)$class
table(qda.class,Direction.2005)
mean(qda.class==Direction.2005) # 精度は線形手法と比較して向上している


# K最近傍方
# 準備
library(class)
train.X=cbind(Lag1,Lag2)[train,]# 予測変数
test.X=cbind(Lag1,Lag2)[!train,]# 応答変数
train.Direction=Direction[train]# クラス  
# 検証
set.seed(1) # 再現性確保
knn.pred=knn(train.X,test.X,train.Direction,k=1)# k = 1
table(knn.pred,Direction.2005)
(83+43)/252
knn.pred=knn(train.X,test.X,train.Direction,k=3)# k = 3
table(knn.pred,Direction.2005) 
mean(knn.pred==Direction.2005) # 精度としてはQDAが最も優れた方法となる

# 保険データへの適用

dim(Caravan)
head(Caravan)
attach(Caravan)
summary(Purchase)
348/5822
#変数を標準化
standardized.X=scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])
#分割
test=1:1000
train.X=standardized.X[-test,]
test.X=standardized.X[test,]
train.Y=Purchase[-test]
test.Y=Purchase[test]
# モデル作成
set.seed(1)
knn.pred=knn(train.X,test.X,train.Y,k=1)
mean(test.Y!=knn.pred)# テスト誤分類率
mean(test.Y!="No")

table(knn.pred,test.Y)
9/(68+9) # 予測のち11.7%の顧客が保険を購入している
knn.pred=knn(train.X,test.X,train.Y,k=3)
table(knn.pred,test.Y)
5/26 #精度向上
knn.pred=knn(train.X,test.X,train.Y,k=5)
table(knn.pred,test.Y)
4/15 #精度向上

#ロジスティック回帰で同じことを実践
glm.fits=glm(Purchase~.,data=Caravan,family=binomial,subset=-test)
glm.probs=predict(glm.fits,Caravan[test,],type="response")
glm.pred=rep("No",1000)
glm.pred[glm.probs>.5]="Yes"
table(glm.pred,test.Y) #境界が0.5では制度が悪い(結果が悪い)
glm.pred=rep("No",1000)
glm.pred[glm.probs>.25]="Yes"
table(glm.pred,test.Y) #予測された確率が25%以上で保険購入をすると予測する場合、精度向上
11/(22+11)