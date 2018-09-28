install.packages("choroplethr")
install.packages("choroplethrAdmin1")

library("choroplethr")
library("choroplethrAdmin1")
admin1_map("switzerland")　#白地図を作成

# df_japan_censusデータ
data(df_japan_census)
head(df_japan_census)

df_japan_census
PlotData <- data.frame(region = df_japan_census[, 1], value = df_japan_census[, 2])
#プロット
admin1_choropleth(country.name = "japan",
                  df           = PlotData,
                  title        = "2010 Japan Population Estimates",
                  legend       = "Population",
                  num_colors   = 3)
