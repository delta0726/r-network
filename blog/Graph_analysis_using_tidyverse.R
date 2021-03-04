# ***********************************************************************************************
# Title     : Graph analysis using the tidyverse
# Objective : TODO
# Created by: Owner
# Created on: 2021/02/22
# URL       : https://rviews.rstudio.com/2019/03/06/intro-to-graph-analysis/
# ***********************************************************************************************


# ＜ポイント＞
# 1 グラフテーブルの作成とネットワーク描画を体験する
# 2 ネットワーク分析を最短経路問題に当てはめる


# ＜目次＞
# 0 準備
# 1 グラフテーブルの作成
# 2 グラフテーブルの加工
# 3 ネットワーク描画
# 4 最短経路の探索
# 5 最短経路の探索 (プロセス集約)


# 0 準備 ------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidygraph)
library(ggraph)


# データロード
# --- 電車区間ごとの消費時間
url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-26/small_trains.csv"
small_trains <- url %>% read_csv()

# データ確認
small_trains %>% head()
small_trains %>% glimpse()

# プロット設定
thm <- theme_minimal() +
  theme(
    legend.position = "none",
     axis.title = element_blank(),
     axis.text = element_blank(),
     panel.grid = element_blank(),
     panel.grid.major = element_blank(),
  )

# テーマ設定
theme_set(thm)



# 1 グラフテーブルの作成 --------------------------------------------------------------------

# ＜ポイント＞
# - A列とB列がネットワーク関係にあるデータセットを作成する


# データ加工
# --- 出発駅/到着駅ごとの所要時間
routes <-
  small_trains %>%
    group_by(departure_station, arrival_station) %>%
    summarise(journey_time = mean(journey_time_avg), .groups = "drop") %>%
    ungroup() %>%
    mutate(from = departure_station,
           to = arrival_station) %>%
    select(from, to, journey_time)

# 確認
routes %>% print()

# グラフテーブルに変換
graph_routes <- routes %>% as_tbl_graph()

# 確認
# --- NodeとEdegeの情報を持つ
graph_routes %>% print()
graph_routes %>% attributes()



# 2 グラフテーブルの加工 --------------------------------------------------------------------

# ノードの加工
# --- titleとlabelを追加
graph_routes <-
  graph_routes %>%
  activate(nodes) %>%
  mutate(title = str_to_title(name),
         label = str_replace_all(title, " ", "\n"))

# ノードからタイトルを抽出
stations <-
  graph_routes %>%
    activate(nodes) %>%
    pull(title)

# 確認
stations %>% print()



# 3 ネットワーク描画 --------------------------------------------------------------------

# ネットワーク描画
graph_routes %>%
  ggraph(layout = "kk") +
    geom_node_point() +
    geom_edge_diagonal()

# ネットワーク描画/装飾
# --- ノードに名前を付ける
# --- エッジを薄く表示
graph_routes %>%
  ggraph(layout = "kk") +
    geom_node_text(aes(label = label, color = name), size = 3) +
    geom_edge_diagonal(color = "gray", alpha = 0.4)



# 4 最短経路の探索 --------------------------------------------------------------------

# アイテムカウント
from <- which(stations == "Arras")
to <-  which(stations == "Nancy")

# モーフ化
# --- 最短経路を調べる
# --- tidygraph::to_shortest_path()
shortest <-
  graph_routes %>%
    morph(to_shortest_path, from, to, weights = journey_time)

# 確認
shortest %>% print()
shortest %>% attributes()


# グラフテーブルに列追加
shortest <-
  shortest %>%
    mutate(selected_node = TRUE) %>%
    activate(edges) %>%
    mutate(selected_edge = TRUE) %>%
    unmorph()

# 確認
shortest %>% print()


# グラフテーブルに列追加
shortest <-
  shortest %>%
    activate(nodes) %>%
    mutate(selected_node = ifelse(is.na(selected_node), 1, 2)) %>%
    activate(edges) %>%
    mutate(selected_edge = ifelse(is.na(selected_edge), 1, 2)) %>%
    arrange(selected_edge)

# 確認
shortest %>% print()

# ネットワーク描画
# --- アルファを調整して最短経路をフォーカス
shortest %>%
  ggraph(layout = "kk") +
    geom_edge_diagonal(aes(alpha = selected_edge), color = "gray") +
    geom_node_text(aes(label = label, color =name, alpha = selected_node ), size = 3)

# 時間集計
shortest %>%
  activate(edges) %>%
  filter(selected_edge == 2) %>%
  as_tibble() %>%
  summarise(total_stops = n() - 1,
            total_time = round(sum(journey_time) / 60))


# 5 最短経路の探索 (プロセス集約) ----------------------------------------------------------

# アイテムカウント
from <- which(stations == "Montpellier")
to <-  which(stations == "Laval")

# 最短経路の作成
shortest <-
  graph_routes %>%
    morph(to_shortest_path, from, to, weights = journey_time) %>%
    mutate(selected_node = TRUE) %>%
    activate(edges) %>%
    mutate(selected_edge = TRUE) %>%
    unmorph() %>%
    activate(nodes) %>%
    mutate(selected_node = ifelse(is.na(selected_node), 1, 2)) %>%
    activate(edges) %>%
    mutate(selected_edge = ifelse(is.na(selected_edge), 1, 2)) %>%
    arrange(selected_edge)

# ネットワーク描画
shortest %>%
  ggraph(layout = "kk") +
    geom_edge_diagonal(aes(alpha = selected_edge), color = "gray") +
    geom_node_text(aes(label = label, color =name, alpha = selected_node ), size = 3)

# 時間集計
shortest %>%
  activate(edges) %>%
  filter(selected_edge == 2) %>%
  as_tibble() %>%
  summarise(total_stops = n() - 1,
            total_time = round(sum(journey_time) / 60))
