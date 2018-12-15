library(gapminder)
library(broom)
#1つずつfilterでデータセットを作るのは大変
by_country <- gapminder %>%
  group_by(country, continent) %>%
  nest()
#modeling each country
country_model <- function(df){
  lm(lifeExp ~ year, data=df)
}
#各要素にデータを当てはめる
models <- map(by_country$data, country_model)
by_country <- by_country %>%
  mutate(models <- map(by_country$data, country_model))
  
#modelの評価
by_country %>%
  mutate(glace = map(models,broom::glance)) 
