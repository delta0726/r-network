# ***********************************************************************************************
# Title     : Introduction to Network Analysis with R
# Objective : TODO
# Created by: Owner
# Created on: 2021/03/05
# URL       : https://www.jessesadler.com/post/network-analysis-with-r/
# ***********************************************************************************************


# ＜概要＞
# - ノードとエッジの概念を理解する
# - tidygraphパッケージを使用してRのネットワークグラフを操作および分析する方法を確認する
# - インタラクティブなネットワーク描画を学ぶ


# ＜目次＞
# 0 準備
# 1 ノードとエッジ
# 2 ノードの作成
# 3 エッジの作成
# 4 {network}によるネットワーク作成
# 5 {igraph}によるネットワーク作成
# 6 {tidygraph}と{ggraph}によるネットワーク作成
# 7 インタラクティブなネットワーク描画


# 0 準備 ---------------------------------------------------------------------

library(tidyverse)
library(tidygraph)
library(ggraph)
library(igraph)
library(network)
library(visNetwork)
library(networkD3)


# データロード
letters <- read_csv("blog/data/correspondence-data-1585.csv")

# 確認
letters %>% print()
letters %>% glimpse()


# 1 ノードとエッジ ----------------------------------------------------------------

# ＜ポイント＞
# - エッジとは2つの変数の関係性を示すデータ（1列目と2列目に同じカテゴリ値を持つ）


# データ作成
edge_list <- tibble(from = c(1, 2, 2, 3, 4), to = c(2, 3, 4, 2, 1))
node_list <- tibble(id = 1:4)

# エッジのイメージ
edge_list %>% print()

# ノードのイメージ
node_list %>% print()


# 2 ノードの作成 -------------------------------------------------------------

# From
sources <-
  letters %>%
    distinct(source) %>%
    rename(label = source)

# To
destinations <-
  letters %>%
    distinct(destination) %>%
    rename(label = destination)

# 確認
sources %>% print()
destinations %>% print()

# 全パターン
nodes <-
  sources %>%
    full_join(destinations, by = "label") %>%
    rowid_to_column("id")

# 確認
nodes %>% print()


# 3 エッジの作成 -------------------------------------------------------------

# カテゴリ集計
# --- レコード数をウエイトとする
per_route <-
  letters %>%
    group_by(source, destination) %>%
    summarise(weight = n()) %>%
    ungroup()

# 確認
per_route %>% print()

# エッジの作成
# --- sourceとdestinationをnoに置換
edges <-
  per_route %>%
    left_join(nodes, by = c("source" = "label")) %>%
    rename(from = id) %>%
    left_join(nodes, by = c("destination" = "label")) %>%
    rename(to = id) %>%
    select(from, to, weight)

# 確認
edges %>% print()


# 4 {network}によるネットワーク作成 ------------------------------------------------------

# オブジェクト作成
routes_network <-
  edges %>%
    network(vertex.attr = nodes, matrix.type = "edgelist", ignore.eval = FALSE)

# 確認
routes_network %>% print()
routes_network %>% attributes()

# ネットワーク描画
routes_network %>% plot(vertex.cex = 3)
routes_network %>% plot(vertex.cex = 3, mode = "circle")


# 5 {igraph}によるネットワーク作成 ------------------------------------------------------

# オブジェクト作成
routes_igraph <-
  edges %>%
    graph_from_data_frame(vertices = nodes, directed = TRUE)

# 確認
routes_igraph %>% print()
routes_igraph %>% attributes()


# ネットワーク描画
routes_igraph %>% plot(edge.arrow.size = 0.2)
routes_igraph %>% plot(layout = layout_with_graphopt, edge.arrow.size = 0.2)


# 6 {tidygraph}と{ggraph}によるネットワーク作成 -------------------------------------------

# グラフテーブルの作成
# --- データフレームのnodeとedgeから作成
routes_tidy <-
  nodes %>%
    tbl_graph(edges = edges, directed = TRUE)

# グラフテーブルの作成
# --- igraphオブジェクトを変換
routes_igraph_tidy <-
  routes_igraph %>%
    as_tbl_graph()

# クラス比較
routes_tidy %>% class()
routes_igraph_tidy %>% class()
routes_igraph %>% class()

# 確認
# --- igraphと出力は大きく異なる
# --- tibble形式の出力
routes_tidy %>% print()
routes_igraph %>% print()

# エッジのアクティブ化
# --- アクティブで選択した要素のtibbleは操作可能となる
routes_tidy %>%
  activate(edges) %>%
  arrange(desc(weight))

# ネットワークの描画
# --- 装飾なし（形状のみ把握可能）
routes_tidy %>%
  ggraph() +
    geom_edge_link() +
    geom_node_point() +
    theme_graph()

# ネットワークの描画
# --- 装飾あり
routes_tidy %>%
  ggraph(layout = "graphopt") +
    geom_node_point() +
    geom_edge_link(aes(width = weight), alpha = 0.8) +
    scale_edge_width(range = c(0.2, 2)) +
    geom_node_text(aes(label = label), repel = TRUE) +
    labs(edge_width = "Letters") +
    theme_graph()

# ネットワークの描画
# --- ggraphは独自のレイアウトも実装
# --- サークル形式のネットワーク
routes_igraph %>%
  ggraph(layout = "linear") +
    geom_edge_arc(aes(width = weight), alpha = 0.8) +
    scale_edge_width(range = c(0.2, 2)) +
    geom_node_text(aes(label = label)) +
    labs(edge_width = "Letters") +
    theme_graph()



# 7 インタラクティブなネットワーク描画 -------------------------------------------

# ＜ポイント＞
# - htmlwidgetsの中にはインタラクティブなネットワーク描画が可能なライブラリがある


# ネットワーク描画
# --- {visNetwork}
nodes %>% visNetwork(edges)


# エッジにウエイトを追加
edges <- edges %>% mutate(width = weight/5 + 1)

# ネットワーク描画
# --- {visNetwork}
# --- ウエイト追加/有方向グラフ
visNetwork(nodes, edges) %>%
  visIgraphLayout(layout = "layout_with_fr") %>%
  visEdges(arrows = "middle")


# ノードとエッジの加工
nodes_d3 <- nodes %>% mutate(id = id - 1)
edges_d3 <- edges %>% mutate(from = from - 1, to = to - 1)

# ネットワーク描画
edges_d3 %>%
  forceNetwork(Nodes = nodes_d3, Source = "from", Target = "to",
               NodeID = "label", Group = "id", Value = "weight",
               opacity = 1, fontSize = 16, zoom = TRUE)

# サンキープロット
edges_d3 %>%
  sankeyNetwork(Nodes = nodes_d3, Source = "from", Target = "to",
                NodeID = "label", Value = "weight", fontSize = 16, unit = "Letter(s)")