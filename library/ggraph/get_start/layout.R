# ***********************************************************************************************
# Title     : Layout
# Objective : TODO
# Created by: Owner
# Created on: 2021/02/28
# URL       : https://ggraph.data-imaginist.com/articles/Layouts.html
# ***********************************************************************************************


# ＜概要＞
# - レイアウトとは、特定のグラフ構造をプロットするときのノードの垂直方向と水平方向の配置すること
# - {ggraph}はtbl_graphオブジェクトを起点としてネットワーク可視化を行う
#   --- tbl_graphオブジェクトは{tidygraph}により作成され、Rの多くのネットワークオブジェクトのラッパーとなる
# - {ggraph}では多くのレイアウトによってネットワークの可視化と理解を促進する
#   --- ネットワークは複雑な概念なので、プロットにこだわらないと何もインプリケーションを得られない


# ＜レイアウト＞
# - {graphlayouts}および{igraph}のすべてのレイアウトが利用可能
# - {ggraph}自体もより特殊なレイアウトのいくつかを提供
# - 20をはるかに超えるさまざまなレイアウトから選択可能
#   --- 盲目的にデフォルトフォーマットを使うことは、ネットワーク分析で最も多い間違い
#   --- 目的に応じた(インプリケーションを得れる)レイアウトを追求すべきである


# ＜目次＞
# 0 準備
# 1 最初のネットワーク可視化
# 2 カスタムレイアウトの作成
# 3 サークル系のレイアウト
# 4 深さを表現したレイアウト
# 5 さまざまなノードエッジダイアグラム
# 6 その他のダイアグラム
# 7 階層的レイアウト
# 8 マトリックスレイアウト


# 0 準備 ----------------------------------------------------------------------------------

# ライブラリ
library(magrittr)
library(tidyverse)
library(tidygraph)
library(ggraph)


# プロットスタイルの設定
set_graph_style(plot_margin = margin(1,1,1,1))


# highschoolデータ **********************************************************

# データ確認
highschool %>% as_tibble()
highschool %>% glimpse()

# グラフテーブルに変換
graph <- highschool %>% as_tbl_graph()
graph %>% print()


# flareデータ **************************************************************

# データ確認
flare %>% attributes()

# グラフテーブルに変換
graph2 <- flare %$% tbl_graph(vertices, edges)
graph2 %>% print()


# highschoolデータ **************************************************************

# データ確認
highschool %>% as_tibble()
highschool %>% glimpse()


# 1 最初のネットワーク可視化 ------------------------------------------------------------------

# ネットワーク作成
# --- レイアウト指定：なし
# --- すばやく起動して実行することのみを目的としている
graph %>%
  ggraph() +
    geom_edge_link(aes(colour = factor(year))) +
    geom_node_point()

# ネットワーク作成
# --- レイアウト指定：kk
graph %>%
  ggraph(layout = 'kk') +
    geom_edge_link(aes(colour = factor(year))) +
    geom_node_point()

# ネットワーク作成
# --- レイアウト指定：kk
# --- 追加アルゴリズム：イテレーション指定
graph %>%
  ggraph(layout = 'kk', maxiter = 100) +
    geom_edge_link(aes(colour = factor(year))) +
    geom_node_point()


# 2 カスタムレイアウトの作成 ------------------------------------------------------------------

# ネットワーク作成
# --- レイアウトを作成してggraph()に渡す
graph %>%
  create_layout(layout = 'eigen') %>%
  ggraph() +
    geom_edge_link(aes(colour = factor(year))) +
    geom_node_point()


# レイアウトのみ作成
layout <- graph %>% create_layout(layout = 'eigen')

# 確認
layout %>% class()
layout %>% head()
layout %>% attributes()


# 3 サークル系のレイアウト ------------------------------------------------------------------

# arc diagram
# --- ノードを軸に揃えて、エッジをサークルで表現
graph %>%
  ggraph(layout = 'linear') +
    geom_edge_arc(aes(colour = factor(year)))


# coord diagram
# ---  ノードをサークルに揃えて、エッジはサークル内で表現
graph %>%
  ggraph(layout = 'linear', circular = TRUE) +
    geom_edge_arc(aes(colour = factor(year))) +
    coord_fixed()


# 4 深さを表現したレイアウト ------------------------------------------------------------------

# An icicle plot
# ---
graph2 %>%
  ggraph( 'partition') +
    geom_node_tile(aes(fill = depth), size = 0.25)


# A sunburst plot
graph2 %>%
  ggraph( 'partition', circular = TRUE) +
    geom_node_arc_bar(aes(fill = depth), size = 0.25) +
    coord_fixed()



# 5 さまざまなノードエッジ・ダイアグラム --------------------------------------------------

# グラフテーブルに変換
# --- 媒介中心性を追加
graph <-
  highschool %>%
    as_tbl_graph() %>%
    mutate(degree = centrality_degree())

# 確認
graph %>% print()
graph %>% attributes()

# ノードエッジ・ダイアグラムの作成
# --- 4パターン
c('stress', 'fr', 'lgl', 'graphopt') %>%
  lapply(function(layout) {
    ggraph(graph, layout = layout) +
      geom_edge_link(aes(colour = factor(year)), show.legend = FALSE) +
      geom_node_point() +
      labs(caption = paste0('Layout: ', layout))
  })


# 6 その他のダイアグラム ------------------------------------------------------------------

# ＜ポイント＞
# - 技術的にはノードエッジ図だが、ノードに関連する情報を使用して線を書いている
#   --- 程度解釈しやすく、グラフ構造の小さな変更に対する脆弱性が少ない
#   --- あまり一般的ではないため、使用には追加の説明が必要になること多い


# グラフテーブルに変換
# --- 媒介中心性を追加
graph <-
  highschool %>%
    as_tbl_graph() %>%
    mutate(degree = centrality_degree(),
           friends = ifelse(centrality_degree(mode = 'in') < 5, 'few',
                     ifelse(centrality_degree(mode = 'in') >= 15, 'many', 'medium')))

# HIVEプロットの作成
graph %>%
  ggraph( 'hive', axis = friends, sort.by = degree) +
    geom_edge_hive(aes(colour = factor(year))) +
    geom_axis_hive(aes(colour = friends), size = 2, label = FALSE) +
    coord_fixed()

# FOCUSプロット
graph %>%
  ggraph('focus', focus = node_is_center()) +
    ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = r), data.frame(r = 1:5), colour = 'grey') +
    geom_edge_link() +
    geom_node_point() +
    coord_fixed()


# 7 階層的レイアウト ------------------------------------------------------------------

# グラフテーブルに変換
graph <-
  flare %$%
    tbl_graph(vertices, edges)


# サークルパック *******************************************************

# サークル表示
set.seed(1)
graph %>%
  ggraph( 'circlepack', weight = size) +
    geom_node_circle(aes(fill = depth), size = 0.25, n = 50) +
    coord_fixed()

# ポイント表示
set.seed(1)
graph %>%
  ggraph( 'circlepack', weight = size) +
    geom_edge_link() +
    geom_node_point(aes(colour = depth)) +
    coord_fixed()



# ツリーマップ *******************************************************

# 基本的なマップ
graph %>%
  ggraph( 'treemap', weight = size) +
    geom_node_tile(aes(fill = depth), size = 0.25)


# ネットワーク表示
graph %>%
  ggraph( 'treemap', weight = size) +
    geom_edge_link() +
    geom_node_point(aes(colour = depth))

# 樹形図
graph %>%
  ggraph( 'tree') +
    geom_edge_diagonal()


# ＜参考＞
# 階層クラスタリング
dendrogram <- iris[, 1:4] %>% dist() %>% hclust()

# デンドログラム
dendrogram %>%
  ggraph( 'dendrogram', height = height) +
    geom_edge_elbow()

# サークル表示
dendrogram %>%
  ggraph( 'dendrogram', circular = TRUE) +
    geom_edge_elbow() +
    coord_fixed()


# ツリーマップ *******************************************************

# ツリー作成
tree <-
  create_tree(100, 2, directed = FALSE) %>%
    activate(edges) %>%
    mutate(length = runif(n()))

tree %>%
  ggraph( 'unrooted', length = length) +
    geom_edge_link()


# 8 マトリックスレイアウト ----------------------------------------------------------------

# ＜課題＞
# - 多くのノードエッジダイアグラムのレイアウトはスケーラビリティが低いという問題がある
#   --- エッジが重なり始めてプロットが理解できなくなる
#   --- 対応策1：より大きなプロットのサブセットのみをプロットする
#   --- 対応策2：エッジが完全に重ならないようにレイアウトを選択すること


# ＜マトリックスレイアウト＞
# - 各ノードを対角線上に配置し、ターミナルノードの垂直位置と水平位置の交点に点またはタイルを描画することでエッジを描画
# - マトリックスレイアウトを効率的に使用するには、さまざまなネットワークトポロジが引き起こす特定のパターンを認識し始める必要があります。
# - ノードの順序がマトリックスレイアウトの外観に与える大きな影響を認識する


# データ作成
graph <- create_notable('zachary')

# 確認
graph %>% print()
graph %>% attributes()

# マトリックスレイアウト
graph %>%
  ggraph( 'matrix', sort.by = node_rank_leafsort()) +
    geom_edge_point(mirror = TRUE) +
    coord_fixed()


# 9 ファブリックレイアウト ----------------------------------------------------------------

# データ作成
graph <- create_notable('zachary')

# 確認
graph %>% print()
graph %>% attributes()

# ファブリックレイアウト
graph %>%
  ggraph( 'fabric', sort.by = node_rank_fabric()) +
    geom_node_range(colour = 'grey') +
    geom_edge_span(end_shape = 'square') +
    coord_fixed()

graph %>%
  ggraph( 'fabric', sort.by = node_rank_fabric(), shadow.edges =TRUE) +
    geom_node_range(colour = 'grey') +
    geom_edge_span(aes(filter = shadow_edge), colour ='lightblue' , end_shape = 'square') +
    geom_edge_span(aes(filter = !shadow_edge), end_shape = 'square') +
    coord_fixed()
