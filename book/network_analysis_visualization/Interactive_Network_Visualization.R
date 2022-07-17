# ***********************************************************************************************
# Title     : Interactive Network Visualization using R
# Objective : TODO
# Created by: Owner
# Created on: 2021/02/22
# URL       : http://www.sthda.com/english/articles/33-social-network-analysis/137-interactive-network-visualization-using-r/
# ***********************************************************************************************


# ＜概要＞
# - ネットワークはインタラクティブな可視化が理解を助けることがある
#   --- インタラクティブ系のライブラリを確認


# ＜目次＞
# 0 準備
# 1 networkD3
# 2 visNetwork


# 0 準備 --------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(networkD3)
library(visNetwork)
library(rpart)
library(navdata)


# データロード
data("phone.call2")

# データ確認
# --- リストにノードとエッジのデータを格納している
# --- tidygraphと同じ形式だが、こちらはリストに格納している点に注意
phone.call2 %>% print()
phone.call2 %>% class()
phone.call2 %>% glimpse()

# データ分割
nodes <- phone.call2$nodes
edges <- phone.call2$edges

# 階層クラスタリングデータ
set.seed(123)
hc <-
  USArrests %>%
    sample_n(15) %>%
    scale() %>%
    dist() %>%
    hclust(method = "complete")

# 決定木のデータ
res <- rpart(Species~., data = iris)


# 1 networkD3 ------------------------------------------------------------------------

# データ加工
nodes_d3 <- nodes %>% mutate(id = id - 1)
edges_d3 <- edges %>% mutate(from = from - 1, to = to - 1)

# プロット表示
# --- ネットワーク
edges_d3 %>%
  forceNetwork(Nodes = nodes_d3, Source = "from", Target = "to", NodeID = "label",
               Group = "id", Value = "weight", opacity = 1, fontSize = 16, zoom = TRUE)

# プロット表示
# --- サンキーダイアグラム
edges_d3 %>%
  sankeyNetwork(Nodes = nodes_d3, Source = "from", Target = "to", NodeID = "label",
                Value = "weight", fontSize = 16, unit = "Letter(s)")

# デンドログラム
# --- あまり有用ではなさそう（見た目通り）
hc %>% dendroNetwork(fontSize = 15)

# 円形ネットワーク
# --- 距離の概念は見えにくくなる（ネットワーク重視の見方）
hc %>%
  as.radialNetwork() %>%
  radialNetwork(fontSize = 15)

# 階層ネットワーク
hc %>%
  as.radialNetwork() %>%
  diagonalNetwork(fontSize = 15)


# 2 visNetwork ---------------------------------------------------------------

# ＜ポイント＞
# - インタラクティブなネットワークグラフを作成
# - 必要に応じてノードとエッジをカスタマイズ
# - igraphパッケージで生成されたネットワークをインタラクティブに直接視覚化するために使用
# - rpartパッケージで生成された再帰的パーティショニングおよび回帰ツリーを視覚化するために使用
# - ノード形状に画像やアイコンを使用できます。
# - igraphレイアウトをサポート


# ネットワーク
nodes %>%
  visNetwork(edges) %>%
  visLayout(randomSeed = 12)

# 幅の情報を追加
edges <- edges %>% mutate(width = 1 + weight/5)

# 有方向グラフ
nodes %>%
  visNetwork(edges) %>%
    visIgraphLayout(layout = "layout_with_fr") %>%
    visEdges(arrows = "middle") %>%
    visLayout(randomSeed = 1234)

# Visualize
res %>%
  visTree(main = "Iris classification Tree", width = "80%",  height = "400px")


