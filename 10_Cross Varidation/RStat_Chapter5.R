#  第5章　交差検証とブーとストラップ

## ホールドアウト検証
library(ISLR)
set.seed(1)
train=sample(392,196) #半分を無作為に抽出
lm.fit=lm(mpg~horsepower,data=Auto,subset=train)　#訓練のみ
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)　#テストMSE算出（テキストp27の公式参照）
#多項式にてテストMSE比較
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

## 一つ抜き交差検証
#glmとcv.glm関数を使用する（bootパッケージ）
glm.fit=glm(mpg~horsepower,data=Auto) #通常の回帰で当てはめ（glm = lm）
coef(glm.fit)
library(boot)
cv.err=cv.glm(Auto,glm.fit)
cv.err$delta　#deltaベクトル内に交差検証の結果が格納されている（テスト誤差推定値）

## K分割交差検証
#k = 10とし
#一次から十次までの多項式を当てはめた結果を表示
set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
cv.error.10 #一次よりも多項式の方がMSEは低い
#繰り返さない場合
cv.err=cv.glm(Auto,glm.fit,K= 10)
cv.err$delta　#最初の数値：交差検証のよる推定値、片方：バイアス調整後の推定値


## ブートストラップ
#Portfolioデータセットを用いた例
alpha.fn=function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
alpha.fn(Portfolio,1:100)
set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T)) #これを何回も繰り返してSEを計算してもよいが・・・
boot(Portfolio,alpha.fn,R=1000) #自動で計算を行うことが可能（ブートストラップ推定値）

# 線形回帰モデルの制度推定（ブートストラップを用いて）

boot.fn=function(data,index)
  return(coef(lm(mpg~horsepower,data=data,subset=index)))
boot.fn(Auto,1:392)#通常の回帰
set.seed(1)
boot.fn(Auto,sample(392,392,replace=T))

#ブートストラップにより係数の推定値とSEを求める
#比較
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower,data=Auto))$coef
#標準誤差が異なることから、ブートストラップの方が精度が悪いように見えるが・・・
#SE計算の際に用いるσは線形を過程していることから、非線形データへの当てはめでは過大評価となる
#ブートストラップの場合は上記のような過程はないことから高精度

#二次モデルと比較
boot.fn=function(data,index)
  coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index))
set.seed(1)
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef
#モデルがデータによく当てはまっている場合は、SEのブートストラップ推定値とよく一致している

s