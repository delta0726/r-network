# ***********************************************************************************************
# Title     : Network Analysis and Manipulation using R
# Objective : TODO
# Created by: Owner
# Created on: 2021/02/22
# URL       : http://www.sthda.com/english/articles/33-social-network-analysis/136-network-analysis-and-manipulation-using-r/
# ***********************************************************************************************


# ＜概要＞
# - tidygraphパッケージを使用してRのネットワークグラフを操作および分析する方法を確認する
# - tidygraphフレームワークでは、ネットワークデータは2つの整頓されたデータテーブルと見なされる
#   --- ｢ノードデータ｣と｢エッジデータ｣


# ＜目次＞
# 0 準備
# 1 相関ネットワーク
# 2 ネットワークグラフの操作
# 3 ネットワーク中心性


# 0 準備 ------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidygraph)
library(ggraph)
library(navdata)
library(corrr)


# データロード
data("phone.call2")

# データ確認
# --- tidygraphのデータ（ノードとエッジのデータを持つ）
phone.call2 %>% print()
phone.call2 %>% class()
phone.call2 %>% glimpse()

# データ分割
nodes <- phone.call2$nodes
edges <- phone.call2$edges

# オブジェクトの作成
# --- tidygraph用のデータ
phone.net <-
  tbl_graph(nodes = nodes, edges = edges, directed = TRUE)


# プロット作成
# --- ネットワーク
phone.net %>%
  ggraph(layout = "graphopt") +
    geom_edge_link(width = 1, colour = "lightgray") +
    geom_node_point(size = 4, colour = "#00AFBB") +
    geom_node_text(aes(label = label), repel = TRUE)+
    theme_graph()


# 1 相関ネットワーク -----------------------------------------------------

# 相関係数フレーム
# --- 相関係数行列の三角行列をロング型に変換
res.cor <-
  mtcars[, c(1, 3:6)] %>%
    t() %>%
    correlate() %>%
    shave(upper = TRUE) %>%
    stretch(na.rm = TRUE) %>%
    filter(r >= 0.998)

# 確認
# --- 2つの関係性を示すデータ（ネットワークデータと同じ性質）
res.cor %>% print()

# tidygraphオブジェクトに変換
set.seed(1)
cor.graph <- res.cor %>% as_tbl_graph(directed = FALSE)

# 相関ネットワークの作成
cor.graph %>%
  ggraph() +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), size = 3, repel = TRUE) +
  theme_graph()


cor.graph %>%
  activate(edges) %>%
  arrange(desc(r))


# 2 ネットワークグラフの操作 --------------------------------------------

# データ作成
cars.group <-
  tibble(name = rownames(mtcars),
         cyl = as.factor(mtcars$cyl))


# Modify the nodes data
cor.graph <-
  cor.graph %>%
    activate(nodes) %>%
    left_join(cars.group, by = "name") %>%
    rename(label = name)


cor.graph <-
  cor.graph %>%
    activate(edges) %>%
    rename(weight = r)

cor.graph

# プロット作成
set.seed(1)
cor.graph %>%
  ggraph() +
  geom_edge_link(aes(width = weight), alpha = 0.2) +
  scale_edge_width(range = c(0.2, 1)) +
  geom_node_point(aes(color = cyl), size = 2) +
  geom_node_text(aes(label = label), size = 3, repel = TRUE) +
  theme_graph()


# 3 ネットワーク中心性 ------------------------------------------------------------

# ＜ポイント＞
# - 中心性はネットワークグラフを分析する際の重要な概念
#   --- ノード/エッジの中心性は、ネットワーク内のノードまたはエッジの重要性を測定します。
#-  --- 多くのエンティティと関係がある場合、エンティティは重要であるとみなす
#   --- 中心性はノードに接続されているエッジの数を表す

# ＜中心性スコア＞
# - centrality_authority()
# - centrality_betweenness()
# - centrality_closeness()
# - centrality_hub()
# - centrality_pagerank()
# - centrality_eigen()
# - centrality_edge_betweenness()


# プロット作成
# --- centrality_authorityに応じてノードを色付け
set.seed(123)
phone.net %>%
  activate(nodes) %>%
  mutate(centrality = centrality_authority()) %>%
  ggraph(layout = "graphopt") +
  geom_edge_link(width = 1, colour = "lightgray") +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  geom_node_text(aes(label = label), repel = TRUE)+
  scale_color_gradient(low = "yellow", high = "red")+
  theme_graph()


# プロット作成
# --- group_infomap(に応じてネットワークを分類
set.seed(123)
cor.graph %>%
  activate(nodes) %>%
  mutate(community = as.factor(group_infomap())) %>%
  ggraph(layout = "graphopt") +
  geom_edge_link(width = 1, colour = "lightgray") +
  geom_node_point(aes(colour = community), size = 4) +
  geom_node_text(aes(label = label), repel = TRUE)+
  theme_graph()


