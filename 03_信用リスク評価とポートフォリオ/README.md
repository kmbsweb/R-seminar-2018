# 信用リスク評価とポートフォリオ

*`目的`*
-　判別式を用いたリスク評価
-　ポートフォリオを作成する
-　クロス集計の自動化

## 予測について
- 予測を外した地震学者

## 信用評価の手法
信用評価モデルは、金融機関だけでなく信用取引が発生するありとあらゆる業種・業態で必要となります。
ただ、信用評価モデルを構築するには決算書が欠かせませんが、取引先から決算書をもらうことは一般的には簡単なことではないと思われます。出資や多額の貸付を行っている場合を除けば、決算書はなかな手に入れられません。
【予測手法】
- 判別式
- ロジスティック回帰分析
- SVM



.) from google API. Also ArcGIS(ESRI) provide some function to create service area. However, it is very expensive for student to use such charged software. So this package provide some function to get transit data from [yahoo transit service](https://transit.yahoo.co.jp/ "yahoo transit service").
The aim of this package is creating service area.

![](https://github.com/kmbsweb/Jtransit/blob/master/pic/fare%20vs%20duration.PNG?raw=true)


* [Install](#install)
* [Documentation](#documentation)
* [Features](#features)
* [Examples](#examples)

**Updated the package to better suit `rvest`**

## Install

```R
# dev version only
devtools::install_github("kmbsweb/Jtransit") 
```

## Documentation 

* [Examples](https://kmbsweb.wordpress.com/)

## Features

*`function`*

- `transit` - get duration and fare from origin to destianation.
- `covert_m` - converting unit(from hour to minute).
- `dest_loc` - facility geocoding.
- `transit_map_fare` - create map of fare.
- `transit_map_dura` - create map of duration.

*`data`*

- `all_sta` - station geometry data all over Japan.
- `hyo_sta` - station geometry data all over Hyogo prefecture.


See `NEWS.md` for changes.


## Examples

```R
library(Jtransit)
library(dplyr)

# one origin, one destination
# origin:神戸大学, destination:夙川駅
# departure:14:15
transit("神戸大学","夙川駅",14,1,5)

# result
# origin destination time_h time_m1 time_m2 duration  fare   transit
# 神戸大学      夙川駅     14       1       5     29分 220円 乗換：0回
# alternative
#      6
```

```R
# more than 2 origins, more than 2 destinations
# make the example data.frame
# remove the blank

df <- read.csv(textConnection(
"origin,destination,origin2
神戸大学,夙川駅,
神戸大学,阪急六甲駅,
神戸大学,王子公園駅,
神戸大学,阪急岡本駅,
神戸大学,阪急花隈駅,
神戸大学,阪急御影駅,
神戸大学,神戸三宮駅,
神戸大学,西宮北口駅,"
))

or <- as.character(df$origin)
des <- as.character(df$destination)

# prepare for the blank data.frame
# ErrorPage <- NULL
Data <- data.frame()

# repetition processing
for (i in seq(or)){
  print(paste0("...", i, "行目を処理しています。"))
  exdata <- transit(or[i],des[i],14,1,5)
    #row bind
  Data <- rbind(Data, exdata)
  }

```
