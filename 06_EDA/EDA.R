install.packages("tidyverse")
library(tidyverse)

#棒グラフ
#xにカテゴリカルな列を指定
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = clarity))

#観測数を調べる
diamonds %>%
  count(cut)

#ヒストグラム
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

#bin widthの設定を検討
diamonds %>%
  count(cut_width(carat, 0.5))

#cratの絞り込み
smaller <- diamonds %>%
  filter(carat < 3)

#binwidth = 0.5でプロット
ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.1)

#カテゴリと連続数を表現
ggplot(data = smaller, mapping = aes(x = carat, color = cut)) +
  geom_freqpoly(binwidth = 0.1)

#質問を考える(1)
ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

#質問を考える(1)間欠泉
#eruptions(噴出)3分程度がなぜこんなに少ないの？
ggplot(data = faithful, mapping = aes(x = eruptions)) +
  geom_histogram(binwidth = 0.1)


#異常値発見
ggplot(diamonds) +
  geom_histogram(mapping = aes(x = y), binwidth = 0.5)

#拡大
ggplot(diamonds) +
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim =c(0, 50))

#arrange(x)はxについて昇順で並び替え
unusual <- diamonds %>%
  filter(y < 3 | y > 20) %>%
  select(price, x, y, z) %>%
  arrange(x)

##########################
#p81_practice
##########################
#1ヒストグラム
ggplot(data = diamonds) + 
  geom_histogram(mapping = aes(x = z), binwidth = 0.1)

#2販売されていない価格帯1500がある
ggplot(data = diamonds) + 
  geom_histogram(mapping = aes(x = price), binwidth = 100) +
　coord_cartesian(xlim = c(0, 2000))

#3carat=0.99 23, carat=1 1558
#1carat区切りの良い数で取引されるため？機械の自動化？
set <- diamonds %>%
  filter(carat == 1) %>%
  select(price, carat, x, y, z) %>%
  arrange(price)

#4
#xlim, ylim
ggplot(data = diamonds) + 
  geom_histogram(mapping = aes(x = price), binwidth = 100) +
  xlim(0,2000)+ylim(0,2500)
#binwidth設定なし
#better valueで設定される
ggplot(data = diamonds) + 
  geom_histogram(mapping = aes(x = price)) +
  xlim(0,2000)+ylim(0,2500)
#coord_cartesianを使うと拡大されだけで数字はカットされない
ggplot(data = diamonds) + 
  geom_histogram(mapping = aes(x = price), binwidth = 100) +
  coord_cartesian(xlim = c(0,2000), ylim = c(0,2500))

#異常値を削除
diamonds2 <- diamonds %>%
  filter(between(y, 3, 20))
#異常値を欠損値処理
diamonds2 <- diamonds %>%
  mutate(y, ifelse(y < 3 | y > 20, NA, y))

ggplot(data = diamonds2, mapping = aes(x = x, y = y)) +
  geom_point()

##共変動
#カテゴリと連続変数
ggplot(data = diamonds, mapping = aes(x = price, color = cut)) +
  geom_freqpoly(binwidth = 500)

#密度分布
ggplot(data = diamonds, mapping = aes(x = price, y = ..density.., color = cut)) +
  geom_freqpoly(binwidth = 500)

#box plot
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot()

#box plot 並び替え
ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
  coord_flip()

##########################
#p87_practice
##########################
#1cancel flight
#y = ..density..を追加する→密度分布を作る
flights <- nycflights13::flights 
 flights%>% 
 mutate(cancelled = is.na(dep_time),
         sched_hour = sched_dep_time %/% 100,
         sched_min = sched_dep_time %% 100,
         sched_dep_time = sched_hour + sched_min / 60) %>%
  ggplot(mapping = aes(x = sched_dep_time, y = ..density..)) +
   geom_freqpoly(mapping = aes(color = cancelled),
                binwidth = 1/4)

#3 ggstance 第1四分位〜第3四分位の距離
install.packages("ggstance")
library(ggstance)
ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
  coord_flip()
ggplot(data = mpg) +
  geom_boxploth(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
  coord_flip()

#4
install.packages("lvplot")
library(lvplot)
ggplot(diamonds, aes(cut, price)) + 
  geom_lv() 

#5
#geom_violin()はgeom_boxplotが表現できない頻度を表現できる
ggplot(mtcars, aes(factor(cyl), mpg)) +
  geom_violin()

#6geom_jitter
#適当に点をずらしてプロットしてくれる
ggplot(mpg, aes(cyl, hwy)) + 
  geom_point() +
  geom_jitter()

#2つのカテゴリ変数
ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color))

#2つのカテゴリ変数
#geom_tile
diamonds %>% 
  count(color, cut) %>%  
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n))

##########################
#p89_practice
##########################
#1,3 geom_tile
diamonds %>% 
  filter(color == "D")  %>%  
  count(color, cut) %>%  
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n))

#2 dest and month  delay
#目的地が小さくなりすぎる
flights %>% 
  filter(dep_delay > 1)  %>%  
  count(dest, month) %>%  
  ggplot(mapping = aes(x = as.factor(month), y = dest)) +
  geom_tile(mapping = aes(fill = n))


#2つの連続変数 
ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price))

#alpha
ggplot(data = diamonds) + 
  geom_point(mapping = aes(x = carat, y = price), alpha = 1 / 100)

#geom_bin2d
ggplot(data = smaller) +
  geom_bin2d(mapping = aes(x = carat, y = price))

install.packages("hexbin")
library(hexbin)
# hexbin
ggplot(data = smaller) +
  geom_hex(mapping = aes(x = carat, y = price))

#geom_boxplot
ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))

ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_number(carat, 20)))




