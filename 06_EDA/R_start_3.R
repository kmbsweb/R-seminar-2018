install.packages("nycflights13")
install.packages("tidyverse")
library(nycflights13)
library(tidyverse)

flights
###filter(データ名, 抜き出し対象1, 対象2, 3...)

filter(flights, month == 1, day == 1)
jan1 <- filter(flights, month == 1, day == 1)
#出力と格納同時
(dec25 <- filter(flights, month == 12, day == 25))
##比較(エラーについて)
filter(flights, month = 1)#=では==
sqrt(2) ^ 2 == 2
1 / 49 * 49 == 1
near(sqrt(2) ^ 2,  2)
near(1 / 49 * 49, 1)
##論理演算子
#&は論理積、つまり「A且つB」
#|は論理和、つまり「AまたはB」
filter(flights, month == 11 | month == 12)#11月または12月に出発した全フライト抽出

nov_dec <- filter(flights, month %in% c(11, 12))#「または」の省略方法
#以下はどちらでも意味は同じ
filter(flights, !(arr_delay > 120 | dep_delay > 120))
filter(flights, arr_delay <= 120, dep_delay <= 120)
##欠損値処理
NA > 5
10 == NA
NA + 10
NA / 2

NA == NA
#  Maryの年齢をxとする
x <- NA
# Johnの年齢をyとする
y <- NA
# John and Maryは同じ歳？
x == y# わかりません
#値が欠損値かどうか調べる方法
is.na(x)#T=欠損値
#そもそもfilter関数は欠損値を除外した上で抽出してくれるので、残したい場合以下の処理を
df <- tibble(x = c(1, NA, 3))
filter(df, x > 1)
filter(df, is.na(x) | x > 1)#NAが除外されない

###練習問題###
##1. 次のようなフライトを探す
#a. 到着が2時間以上遅れた
a1<-filter(flights, arr_delay >= 120)
#b. ヒューストン(IAH または HOU)へのフライト
b1<-filter(flights, dest == "IAH" | dest == "HOU")
#c. United, American,またはDeltaによるフライト
c1<-filter(flights, carrier == "UA" | carrier == "DL")
#d. 夏季(7~9月)のフライト
d1<-filter(flights, month %in% c(7,8,9))
#e. 到着が2時間を超えて遅れたが,出発が遅れなかったフライト
e1<-filter(flights, dep_delay == 0 & arr_delay > 120)
#f. 遅延は少なくとも1時間を超えたが,運航では30分以上取り返したフライト
#・・・・・・

#g. 深夜0時から午前6時まで(深夜0時,午前6時も含む)のフライト
g1<-filter(flights, hour %in% c(0,1,2,3,4,5,6))

##2. dplyrでフィルタ処理に役立つもう一つのヘルパー関数がbetween().
##これは何をするか. 問題1の中でこれを使って簡略化できるか？
d1_1<-filter(flights,between(month,7,9))
g1_1<-filter(flights, between(hour,0,6))#区間指定が可能,区間には指定した範囲の値も含まれる

##3. dep_timeが欠損値の便がいくつあるか？他に欠損している変数は何か？それらの行は何を表す？
summary(flights)#summary結果を参照のこと

##4. NA^0はなぜ欠損値にならないか. NA | TRUE, FALSE & NAはなぜ欠損値にならないのか.
##一般規則を導けるか(NA*0はややこしい反例になる)
#→どの値であっても0乗であれば1となるため、欠損値にはならない



###arrange()
###行を並び替える

arrange(flights, year, month, day)#デフォルトでは指定変数を昇順に並び替え
arrange(flights, desc(dep_delay))#desc:降順に並び替え

df <- tibble(x = c(5, 2, NA))
arrange(df, x)
arrange(df, desc(x))#昇順・降順いずれの処理にしても欠損値は一番最後の行へ

###練習問題###
##1. 欠損値を頭に整列させる為にarrange()をどのように使えば良いか？
##(ヒント：is.naを用いる)
arrange(df, desc(is.na(x)))#NAとしてではなくT or Fとして処理されるから並び替え可能に...？

##2. flightsを整列して、遅延が最も大きかった便を探す。最も朝早く出発したフライトを探すには？
arrange(flights,desc(dep_delay))
arrange(flights,hour)

##3. flightsを整列して、最速のフライトを探す。
##(ヒント：距離を飛行時間で割れば平均速度が求められる)
arrange(flights,distance/air_time)

##3. どのフライトが最長の距離を飛んだか。最短距離のフライトはどれか？
arrange(flights,distance)#EWR-LGA
arrange(flights,desc(distance))#JFK-HNL


###select()
###名前で変数を選ぶ→変数が何十,何百とあるデータで有効

# 列名を抽出→必要な列のみ抽出可能
select(flights, year, month, day)
# 指定列を含む間の列を全て抽出
select(flights, year:day)
# 指定列以外を抽出する
select(flights, -(year:day))

rename(flights, tail_num = tailnum)#?
#指定列を先頭列へ持ってきたい場合はeverything()
select(flights, time_hour, air_time, everything())

###練習問題###
##1. flightsからdep_time,dep_delay,air_time,arr_delayを選ぶ方法をできるだけたくさん見つけて
select(flights,dep_time,dep_delay,air_time,arr_delay)
select(flights,dep_time,dep_delay,air_time,arr_delay,everything())
select(flights,dep_time:arr_delay,-sched_dep_time,-sched_arr_time)

##2. selectにおいて、変数名を複数回繰り返すとどうなるか？
select(flights,dep_time,dep_time,dep_time,dep_time)#一列のみ抽出可能

##3. one_of関数は何をするか。次のベクトルと一緒に使うとなぜ役立つのか？
vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights,one_of(vars))#文字ベクトルにて指定の列を持ってくる為に活用

##4. 次のコードの実行結果には驚かされる。大文字小文字をselectのヘルパー関数はデフォルトでどう扱う？デフォルトをどう変換できる？
select(flights, contains("TIME"))
#デフォルトでは大文字小文字を区別しない
#ヘルパー関数中に"ignore.case = FALSE"を加えることで、指定した大文字or小文字のもののみ抽出
#contains():列名に指定単語が含まれれば抽出
select(flights, contains("TIME", ignore.case = FALSE))


###mutate()
###既存の変数の関数で新たな変数を作る

flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time
)
#新たな列を既存の列より作成し、データ内に追加
#mutate("データ名",新たな列名"=計算式)
mutate(flights_sml,
       gain = dep_delay - arr_delay,
       speed = distance / air_time * 60
)

mutate(flights_sml,
       gain = dep_delay - arr_delay,
       hours = air_time / 60,
       gain_per_hour = gain / hours
)
#新規作成列のみ参照したい場合
transmute(flights,
          gain = dep_delay - arr_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours
)

#便利な関数{＃mutate-funs}
#モジュラー演算：整数分解
#%/%で指定列を指定した数で除し、整数部分のみ抽出
#%%で指定列を指定した数で除し、余った部分のみ抽出
transmute(flights,
          dep_time,
          hour = dep_time %/% 100,
          minute = dep_time %% 100
)


(x <- 1:10)
#階差の計算の際に便利なもの
lag(x)#列の内容を一行下げる（行数は指定可能）
lead(x)#列の内容を一行上げる（行数は指定可能）

x
cumsum(x)#積み上げ算
cummean(x)#積み上げ算しての各地点での平均値

#ランク付け
y <- c(1, 2, 2, NA, 3, 4)
min_rank(y)#NAを含めて割り当ててくれる
#また、同じ値は同順位となり、次点の順位の数字をとばしてに順位付け
min_rank(desc(y))

row_number(y)#同じ値の場合は先に来た値を優先して順位付け
dense_rank(y)#同じ値は同順位となるが、次点の順位の数字をとばさずに順位付け
percent_rank(y)
cume_dist(y)

###練習問題###
##1. dep_timeとsched_dep_timeとは見やすいが、連続的な数になっていないので、計算が面倒。
##より便利な深夜0時からの分単位の数に変換しなさい
transmute(flights,
          hour = dep_time %/% 100,
          minute = dep_time %% 100)
transmute(flights,
          hour = sched_dep_time %/% 100,
          minute = sched_dep_time %% 100)

##2. air_timeをair_time - dep_timeと比較しなさい。何がわかると期待するか？
##何がわかったか？不具合を解決するにはどうするか？
sele<-select(flights,air_time,arr_time,dep_time)
sele<-mutate(sele,
             new_time = arr_time - dep_time)
sele#予定の飛行時間をどれだけ超過したのかがわかる
#・・・・・・

##3. dep_timeとsched_dep_time,dep_delayを比較しなさい。この三つにはどんな関係があると期待する？
select(flights,dep_time,sched_dep_time,dep_delay)
#・・・・・・

##4. 遅延が最も大きかった10便をランク付け関数で求めなさい。同一の値はどう処理したいか？
flights%>%
  select(tailnum,dep_delay)%>%
  arrange(desc(dep_delay))%>%
  mutate(rank = min_rank(desc(dep_delay)))%>%
  filter(rank<=10)
  

##5. 1:3 + 1:10 は何を返すか？それはなぜか？
x<-1:3
y<-1:10
x + y #列数が少ない方が繰り返しで多い方に足し合わされる

##6. Rにはどんな三角関数が用意されているか？
#・・・・・

###summarise()
###多数の値から単一の要約量を作る
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))
#グループ分けをして、日毎の平均値を
by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))
#今回のように、group_byとsummariseはセットで使用すると便利

#複数の操作をパイプと組み合わせる
#例：目的地ごとに距離と平均遅延時間の関係を調べる
by_dest <- group_by(flights, dest)
delay <- summarise(by_dest,
                   count = n(),#n()で総数をカウント
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)
)
delay <- filter(delay, count > 20, dest != "HNL")
#可視化
ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)

#別の方法
delays <- flights %>% 
  group_by(dest) %>% 
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  filter(count > 20, dest != "HNL")

#欠損値の処理について（na.rm）
flights %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))#データ内に欠損値があるため、通常通り平均値が計算不可

flights %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay, na.rm = TRUE))
#欠損値処理の別方法（事前に欠損値を処理しておく方法）
not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))

#グループごとの要約量カウントn()
delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay)
  )
ggplot(data = delays, mapping = aes(x = delay)) + 
  geom_freqpoly(binwidth = 10)

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )
ggplot(data = delays, mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)
#ggplotも%>%のパイプでつなぎ、データ前処理後に続けて作業可能
delays %>% 
  filter(n > 25) %>% 
  ggplot(mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)

##便利な要約関数{＃summarize-funs}
#中心傾向の代表値
not_cancelled %>% 
group_by(year, month, day) %>%
  summarise(
    avg_delay1 = mean(arr_delay),
    avg_delay2 = mean(arr_delay[arr_delay > 0])) #抽出（16.2.5にて説明あり）
    
#散らばりの代表値
#なぜ目的地によって距離の変動が大きくなるのか？
not_cancelled %>% 
  group_by(dest) %>% 
  summarise(distance_sd = sd(distance)) %>% 
  arrange(desc(distance_sd))

#ランクの代表値
#各日の始発と出発日はいつ？
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first = min(dep_time),
    last = max(dep_time))

#位置の代表値
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first_dep = first(dep_time), 
    last_dep = last(dep_time))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  mutate(r = min_rank(desc(dep_time))) %>% 
  filter(r %in% range(r))##・・・・・

#カウント
#航空会社が最も多いのはどの目的地？
#n_distinct:重複を無視して考え、カウントする
not_cancelled %>% 
  group_by(dest) %>% 
  summarise(carriers = n_distinct(carrier)) %>% 
  arrange(desc(carriers))

not_cancelled %>% 
  count(dest)#単純に目的地ごとに集計

not_cancelled %>% 
  count(tailnum, wt = distance)#単純に便ごとに集計し、それらを距離で重み付け

##論理値の数と比率
#sumでグループごとの条件をクリアしている値の数を算出
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(n_early = sum(dep_time < 500))#グループごとに500未満の数を集計

#meanでグループごとのある割合を算出できる
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(hour_perc = mean(arr_delay > 60))#グループごとに60を超える数の割合算出

##複数の変数のグループ化 例えば年・年月など分けてみる
daily <- group_by(flights, year, month, day)
(per_day   <- summarise(daily, flights = n()))
(per_month <- summarise(per_day, flights = sum(flights)))
(per_year  <- summarise(per_month, flights = sum(flights)))

##グループ解除
daily %>% 
  ungroup() %>%             # これで解除
  summarise(flights = n())  # 解除されているかを試しにカウントで試してみる

###練習問題###
##1. フライトのグループの典型的な遅延性を査定するための
##少なくとも五つの異なる方法をプレインストーミングしなさい。
##　次のシナリオを考えよう！
##・50%は15分早く、50%は15分遅い
##・常に10分遅れる
##・50%は30分早く、50%は30分遅い
##・99%が定時で、1%が2時間遅れとなる。
##　到着遅延と出発遅延とどちらが重要か？

#・・・・・

##2. (count()を使わずに)not_cancelled %>% count(dest)とnot_cancelled %>% count(tailnum,wt = distance)
## と同じ出力を与える他の方法を試しなさい。
not_cancelled %>% 
  group_by(dest) %>%
  summarize(n())
  
##・・・・・

##3. キャンセル便の定義(is.na(dep_delay) | is.na(arr_delay))は完璧とは言えない。なぜか。また、どれが最も重要な列か？

#キャンセル便＝出発していないという意味
#つまりis.na(dep_delay)がキャンセル便でis.na(arr_delay)は前者と共にNAになっているもののみ見るべきでは？
#キャンセル便の定義(is.na(dep_delay) & is.na(arr_delay))


##4.日毎のキャンセル便数を求めなさい。何かパターンがあるか？キャンセル便の割合は平均遅延時間と関係するか？
flights %>%
  group_by(time_hour) %>%
  summarise(cancel_count = sum(is.na(dep_delay & arr_delay)),
            cancel_per = mean(is.na(dep_delay & arr_delay)*100),
            delay = mean(arr_delay,na.rm = T)) %>%
  ggplot(mapping = aes(x = time_hour, y = cancel_count))+
  geom_line()#二月のキャンセルが多い
            
flights %>%
  group_by(year, month, day) %>%
  summarise(cancel_count = sum(is.na(dep_delay & arr_delay)),
            cancel_per = mean(is.na(dep_delay & arr_delay)*100),
            delay = mean(arr_delay,na.rm = T)) %>%
  ggplot(mapping = aes(x = cancel_per, y = delay))+
  geom_point()+
  geom_smooth(se = F)#キャンセル率20%あたりまでは、遅延時間も長くなる

##5. 最悪遅延はどの航空会社か？課題：悪い空港の影響と悪い航空会社の影響とを別々に分けることができるか？
## なぜできるorできないのか？(ヒント：flights %>% group_by(carrier,dest) %>% summarize(n())を考える)

#航空会社・目的地
flights %>% 
  group_by(carrier,dest) %>% 
  summarize(n(),
            delay = mean(arr_delay,na.rm = T)) %>%
  arrange(desc(delay))#UAが一番

#航空会社のみ
flights %>% 
  group_by(carrier) %>% 
  summarize(n(),
            delay = mean(arr_delay,na.rm = T)) %>%
  arrange(desc(delay))#F9が一番

#3.7を参照
flights %>% 
  group_by(carrier) %>% 
filter(min_rank(desc(arr_delay)) == 1 ) %>%
arrange(desc(arr_delay))#HAが一番

##・・・・・


##6. 飛行機ごとに、最初に1時間以上遅延する前の飛行回数を数えなさい 
not_cancelled %>%
  filter(arr_delay < 1) %>%
  group_by(tailnum) %>%
##・・・・・

##7. sortの引数はcount()に何をするか？いつ使えば良いのだろうか？

##・・・・・


##グループ化されたものの操作
#各グループの最悪のメンバーを見つける
flights_sml %>% 
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10)
#指定の値よりも大きな値を見つける
popular_dests <- flights %>% 
  group_by(dest) %>% 
  filter(n() > 365)
popular_dests

#グループごとの指標を標準化する
popular_dests %>% 
  filter(arr_delay > 0) %>% 
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>% 
  select(year:day, dest, arr_delay, prop_delay)


###練習問題###
##1. 有効なmutateとfilterの機能の表を再度整理しなさい。グループ処理と組み合わせると
## どのように演算が変わるかを述べなさい

##2. どの飛行機(tailnum)が定時着陸記録に関して最悪か？
flights %>% 
  group_by(tailnum) %>% 
  filter(min_rank(desc(arr_delay)) == 1 ) %>%
  arrange(desc(arr_delay))#N384HAが最悪

##3. 遅延をできるだけ避けたいとすれば、どの時間で飛行すると良いか？
not_cancelled %>%
  mutate(dep_hour = dep_time %/% 100) %>%
  select(dep_hour,everything()) %>%
  group_by(dep_hour) %>%
  summarize(delay = mean(arr_delay)) %>%
  ggplot(mapping = aes(x = dep_hour, y = delay))+
           geom_line()##4時~16時前後

##4. 目的地ごとに総遅延時間を分で計算する。各便ごとに目的地への総遅延時間の割合を計算する
not_cancelled %>%
  group_by(dest) %>%
  summarize(delay = sum(arr_delay))

not_cancelled %>%
  group_by(tailnum,dest) %>%
  summarize(
    delay = sum(arr_delay),
    delay_per = mean(arr_delay > 0)*100) 

##5. 遅延は通常、時系列に関連する。最初の遅延の問題がおさまったとしても、次の便は
##前の便の飛行機が出発するまで待たされる。lag()を使って、ある便の遅延が直前の便の遅延と
##どのように関係するか調べなさい
not_cancelled %>%
  select(dep_delay) %>%
  mutate(lag = lag(dep_delay)) %>%
  ggplot(mapping = aes(x = lag, y= dep_delay))+
  geom_point()+
  geom_smooth()#遅延時間500分以内であれば、次の便も遅延する
#500分以降は次の便の遅延時間も減少する

##6. 各目的地を調べなさい。速いのが怪しい便を見つけられるか（すなわちデータ入力が間違っていた
##可能性のある便）。その目的地への最短便に対する滞空時間を計算しなさい。どのフライトが飛行中に最も多く遅延したか？
##・・・・・

##7. 少なくとも2つの航空会社が運行している目的地をすべて見つけなさい。
##この情報を使って航空会社をランク付けしなさい
flights %>%
  group_by(dest) %>%
  summarise(n = sum(carrier > 2)) %>%
  mutate(rank = min_rank(desc(n))) %>%
  arrange(rank)
