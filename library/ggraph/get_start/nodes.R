# ***********************************************************************************************
# Title     : Nodes
# Objective : TODO
# Created by: Owner
# Created on: 2021/02/28
# URL       : https://ggraph.data-imaginist.com/articles/Nodes.html
# ***********************************************************************************************


# ＜概要＞
# - ノードは、接続されているエンティティ(頂点)のことを指す。


# ＜目次＞
# 0 準備
# 1 散布図によるグラフ表現


# 0 準備 ----------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidygraph)
library(ggraph)


# プロットスタイルの設定
set_graph_style(plot_margin = margin(1,1,1,1))


# highschoolデータ *********************************************************

# データ確認
highschool %>% as_tibble()
highschool %>% glimpse()

# グラフテーブルに変換
gr <- highschool %>% as_tbl_graph()


# flareデータ **************************************************************

# データ確認
flare %>% attributes()

# グラフテーブルに変換
gr <- flare %$% tbl_graph(vertices, edges)


# 1 散布図によるグラフ表現 ------------------------------------------------------------------------

# ＜ポイント＞
# - グラフ内のノードはエンティティの抽象的な概念でレイアウトはそれらの物理的な配置
#   --- 概念的には、散布図の観点から簡単に考えることができる
# - レイアウトはx座標とy座標を提供し、これらを使用してプロットエリアにポイントを作成する


# 元データ
gr %>% print()

# データ変換
gr %>% create_layout(layout = 'kk') %>% head()

# 散布図の作成
gr %>%
  ggraph(layout = 'kk') +
    geom_point(aes(x = x, y = y))

# 散布図の作成
# --- ネットワークの際はgeom_node_point()を用いる
# --- 関数名の直観性 / x,yの指定が不要
gr %>%
  ggraph(layout = 'kk') +
    geom_node_point()


# 2 散布図によるグラフ表現 ------------------------------------------------------------------------


gr %>%
  ggraph( layout = 'partition') +
    geom_node_tile(aes(y = -y, fill = depth))

gr %>%
  ggraph(layout = 'dendrogram', circular = TRUE) +
    geom_edge_diagonal() +
    geom_node_point(aes(filter = leaf)) +
    coord_fixed()


# 3 The different node geoms ---------------------------------------------------------------

graph <-
  create_notable('meredith') %>%
    mutate(group = sample(c('A', 'B'), n(), TRUE))

graph %>%
  ggraph('stress') +
    geom_node_voronoi(aes(fill = group), max.radius = 1) +
    geom_node_point() +
    geom_edge_link() +
    coord_fixed()

gr %>%
  ggraph(layout = 'treemap', weight = size) +
    geom_node_tile(aes(fill = depth))


gr %>%
  ggraph(layout = 'partition', circular = TRUE) +
    geom_node_arc_bar(aes(fill = depth)) +
    coord_fixed()


gr %>%
  ggraph(layout = 'partition', circular = TRUE) +
  geom_edge_diagonal() +
  geom_node_point(aes(colour = depth)) +
  coord_fixed()

