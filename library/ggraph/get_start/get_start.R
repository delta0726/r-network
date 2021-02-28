# ***********************************************************************************************
# Title     : A grammar of graphics for relational data
# Objective : TODO
# Created by: Owner
# Created on: 2021/02/28
# URL       : https://ggraph.data-imaginist.com/
# ***********************************************************************************************


# ＜概要＞
# - ggraphはリレーショナルデータ構造をサポートすることを目的としたggplot2の拡張
#   --- ネットワーク/グラフ/ツリーなどのデータを扱う


# ＜活用チャネル＞
# - ネットワーク分析は、エンティティ(個人/物)の相互関係を調査および視覚化するために使用される
#   --- ソーシャルメディアネットワーク
#   --- 友情ネットワーク
#   --- コラボレーションネットワーク
#   --- 病気の伝染


# ＜目次＞
# 0 準備
# 1 ネットワーク作成


# 0 準備 ----------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidygraph)
library(ggraph)

# データ確認
highschool %>% as_tibble()
highschool %>% glimpse()



# 1 ネットワーク作成 -------------------------------------------------------------------------

# データ加工
# --- グラフテーブルに変換
# --- 次数中心性を追加
graph <-
  highschool %>%
    as_tbl_graph() %>%
    mutate(Popularity = centrality_degree(mode = 'in'))

# データ構造
graph %>% glimpse()

# ネットワーク作成
graph %>%
  ggraph(layout = 'kk') +
    geom_edge_fan(aes(alpha = stat(index)), show.legend = FALSE) +
    geom_node_point(aes(size = Popularity)) +
    facet_edges(~year) +
    theme_graph(foreground = 'steelblue', fg_text_colour = 'white')
