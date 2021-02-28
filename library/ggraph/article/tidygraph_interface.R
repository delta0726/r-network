# ***********************************************************************************************
# Title     : tidygraph and ggraph
# Objective : TODO
# Created by: Owner
# Created on: 2021/02/28
# URL       : https://ggraph.data-imaginist.com/articles/tidygraph.html
# ***********************************************************************************************


# ＜概要＞
# - ggraphはtidygraphのデータをプロットするための中心的なパッケージ
# - 現在のVersion2ではシンプルな記述のためのアップデートがなされて、コード複雑性を大幅軽減させている


# ＜目次＞
# 0 準備
# 1 ネットワーク作成
# 2 tidygraphのアルゴリズムを直接利用


# 0 準備 ----------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidygraph)
library(ggraph)


# データ作成
graph_raw <-
  tibble(from = sample(5, 20, TRUE),
         to = sample(5, 20, TRUE),
         weight = runif(20))

# グラフテーブルに変換
graph <- graph_raw %>% as_tbl_graph()

# データ確認
graph_raw %>% print()
graph


# 1 ネットワーク作成 -------------------------------------------------------------------------

# 基本的なネットワーク
graph %>%
  ggraph(layout = 'fr', weights = weight) +
    geom_edge_link() +
    geom_node_point()

# ウエイトを対数表示
graph %>%
  ggraph(layout = 'fr', weights = log(weight)) +
    geom_edge_link() +
    geom_node_point()



# 2 tidygraphのアルゴリズムを直接利用 -----------------------------------------------------------

# データロード
graph2 <- create_notable('zachary')


# ネットワーク作成
# --- ノードサイズを調整（ページランク）
# --- tidygrapht::centrality_pagerank()をプロット作成プロセスで直接使用
graph2 %>%
  ggraph(layout = 'fr') +
    geom_edge_link() +
    geom_node_point(aes(size = centrality_pagerank())) +
    theme(legend.position = 'bottom')


# ネットワーク作成
# --- ノードサイズを調整
# --- tidygrapht::centrality_edge_betweenness()をプロット作成プロセスで直接使用
graph2 %>%
  ggraph( 'matrix', sort.by = node_rank_leafsort()) +
    geom_edge_point(aes(colour = centrality_edge_betweenness()), mirror = TRUE) +
    theme(legend.position = 'bottom')


# ネットワーク作成
# --- ファセットの利用
graph2 %>%
  ggraph('fr') +
    geom_edge_link() +
    geom_node_point() +
    facet_nodes(~ group_infomap())
