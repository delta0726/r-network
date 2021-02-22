# ***********************************************************************************************
# Title     : Network Visualization Essentials in R
# Objective : TODO
# Created by: Owner
# Created on: 2021/02/22
# URL       : http://www.sthda.com/english/articles/33-social-network-analysis/135-network-visualization-essentials-in-r/
# ***********************************************************************************************


# ＜概要＞
# - ネットワークのデータ加工とプロット作成のフローを確認する


# ＜活用チャネル＞
# - ネットワーク分析は、エンティティ(個人/物)の相互関係を調査および視覚化するために使用される
#   --- ソーシャルメディアネットワーク
#   --- 友情ネットワーク
#   --- コラボレーションネットワーク
#   --- 病気の伝染


# ＜目次＞
# 0 準備
# 1 igraphによるネットワーク作成
# 2 tidygraphによるネットワーク作成
# 3 ツリーマップ
# 4 デンドログラム


# 0 準備 ----------------------------------------------------------------------------------

library(tidyverse)
library(navdata)
library(igraph)
library(tidygraph)
library(ggraph)
library(ggpubr)


# データロード
data("phone.call")

# データ加工
# --- n.callは電話回数でエッジのウエイトとして使用される
phone.call <- phone.call %>% rename(weight = n.call)

# データ確認
# --- 電話の送信元と送信先をノードとして扱う
phone.call %>% as_tibble()
phone.call %>% glimpse()


# 1 igraphによるネットワーク作成 -----------------------------------------------------------

# ＜作成手順＞
# 1. "source"と"direction"の両方から異なる国を取得する
# 2. 列名をラベルに変更する
# 3. 2つの列の情報を結合する


# アイテム取得
# --- source列
sources <-
  phone.call %>%
    distinct(source) %>%
    rename(label = source)

# アイテム取得
# --- destination列
destinations <-
  phone.call %>%
    distinct(destination) %>%
    rename(label = destination)

# ノードの作成
# --- ネットワーク上のエンティティ
# --- アイテム和集合の作成
# --- ノードにユニークidを付与
nodes <-
  sources %>%
    full_join(destinations, by = "label") %>%
    mutate(id = 1:nrow(.)) %>%
    select(id, everything())

# エッジの作成
# --- sourceとdestinationにユニークidを付与（文字をidに置き換える）
edges <-
  phone.call %>%
    left_join(nodes, by = c("source" = "label")) %>%
    rename(from = id) %>%
    left_join(nodes, by = c("destination" = "label")) %>%
    rename(to = id) %>%
    select(from, to, weight)

# グラフの作成
# --- vertices： ノードリスト
# --- directed: データが有向/無向
net.igraph <-
  edges %>%
    graph_from_data_frame(vertices = nodes,
                          directed = TRUE)

# 確認
net.igraph %>% print()
net.igraph %>% glimpse()

# プロット作成
set.seed(123)
net.igraph %>%
  plot(edge.arrow.size = 0.2,
       layout = layout_with_graphopt)



# 2 tidygraphとggraphによるネットワーク作成 -------------------------------------------

# ＜参考＞
# ggraphの作図パターン集
# https://www.data-imaginist.com/2017/ggraph-introduction-layouts/


# ネットワークの作成
net.tidy <-
  nodes %>%
    tbl_graph(edges = edges, directed = TRUE)

# プロット作成
# ---ネットワーク
net.tidy %>%
  ggraph(layout = "graphopt") +
    geom_node_point() +
    geom_edge_link(aes(width = weight), alpha = 0.8) +
    scale_edge_width(range = c(0.2, 2)) +
    geom_node_text(aes(label = label), repel = TRUE) +
    labs(edge_width = "phone.call") +
    theme_graph()

# プロット作成
# ---線形ネットワーク
net.tidy %>%
  ggraph(layout = "linear") +
    geom_edge_arc(aes(width = weight), alpha = 0.8) +
    scale_edge_width(range = c(0.2, 2)) +
    geom_node_text(aes(label = label), repel = TRUE) +
    labs(edge_width = "Number of calls") +
    theme_graph()+
    theme(legend.position = "top")

# プロット作成
# ---線形の円形ネットワーク
net.tidy %>%
  ggraph(layout = "linear", circular = TRUE) +
    geom_edge_arc(aes(width = weight), alpha = 0.8) +
    scale_edge_width(range = c(0.2, 2)) +
    geom_node_text(aes(label = label), repel = TRUE) +
    labs(edge_width = "Number of calls") +
    theme_graph()+
    theme(legend.position = "top")



# 3 ツリーマップ ------------------------------------------------------------------

# ＜ポイント＞
# - ツリーマップはネストされた長方形を使用して樹形図のブランチを表す階層データを表示するための視覚的な方法
#   --- 各長方形は、それが表すデータの量に比例する面積を持つ


# データロード
data("france.trade")

# データ確認
# --- SourceはFanceのみ
france.trade %>% print()
france.trade %>% glimpse()
france.trade %>% map(table)

# ノードリスト
# --- ユニーク化した国一覧
countries <-
  c(france.trade$source,
    france.trade$destination) %>%
  unique()

# ウエイト
# --- Source(France)に対するウエイト
weight <-
  france.trade %>%
    select(destination, trade.percentage)

# ノード作成
# --- ノードリストにユニークidを付与する
nodes <-
  tibble(id = 1:length(countries),
         label = countries) %>%
    left_join(weight, by = c("label" = "destination")) %>%
    mutate(trade.percentage = ifelse(is.na(trade.percentage), 0, trade.percentage))

# エッジ作成
# --- sourceとdestinationにユニークidを付与（文字をidに置き換える）
edges <-
  france.trade %>%
    select(source, destination) %>%
    left_join(nodes, by = c("source" = "label")) %>%
    rename(from = id) %>%
    left_join(nodes, by = c("destination" = "label")) %>%
    rename(to = id) %>%
    select( from, to)

# 確認
edges %>% print()

# ネットワークの作成
trade.graph <-
  nodes %>%
    tbl_graph(edges = edges, directed = TRUE)

# ツリーマップの作成
set.seed(123)
trade.graph %>%
  ggraph( 'treemap', weight = trade.percentage) +
      geom_node_tile(aes(fill = label), size = 0.25, color = "white")+
    geom_node_text(
      aes(label = paste(label, trade.percentage, sep = "\n"),
          size = trade.percentage), color = "white"
      )+
    scale_fill_manual(values = get_palette("Dark2", nrow(france.trade) + 1))+
    scale_size(range = c(0, 6) )+
    theme_void()+
    theme(legend.position = "none")


# 4 デンドログラム ---------------------------------------------------------------

# ＜ポイント＞
# - デンドログラムはggraphでもプロット可能

# クラスタリングデータの作成
res.hclust <-
  USArrests %>%
    scale() %>%
    dist() %>%
    hclust()

# データ変換
# --- デンドログラム用に変換
res.tree <-
  res.hclust %>%
    as.dendrogram()

# デンドログラムの作成
# --- ネットワーク型
res.tree %>%
  ggraph(layout = "dendrogram") +
    geom_edge_diagonal() +
    geom_node_text(aes(label = label), angle = 90, hjust = 1, size = 3)+
    ylim(-1.5, NA) +
    theme_minimal()

# デンドログラムの作成
# --- エルボー型
res.tree %>%
  ggraph(layout = "dendrogram") +
    geom_edge_elbow() +
    geom_node_text(aes(label = label), angle = 90, hjust = 1, size = 3)+
    ylim(-1.5, NA) +
    theme_minimal()

