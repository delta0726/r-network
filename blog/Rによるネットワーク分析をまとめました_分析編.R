# ***********************************************************************************************
# Title   : Rによるネットワーク分析をまとめました（ネットワークの指標編）
# Create  : 2021/02/22
# Update  : 2022/07/16
# URL     : https://qiita.com/saltcooky/items/baefce3b6faceac2bf5c
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



#ネットワークスタックの作成
gstack <- array(dim = c(2,7,7))
gstack[1,,] <- A1
gstack[2,,] <- A2
gscor(gstack)[1,2]




qap <- qaptest(gstack, gcor, g1 = 1, g2 = 2, reps = 1000)
summary(qap)


qap_df <- data.frame(dist = qap$dist)
gp <- ggplot(qap_df, aes(x = dist)) +
  geom_histogram(binwidth = 0.1) +
  geom_vline(xintercept = gscor(gstack)[1,2], colour = "red")
gp


gcor(ADVICE, FRIEND)


gstack <- array(dim = c(2,21,21))
gstack[1,,] <- ADVICE
gstack[2,,] <- FRIEND

cug_test <- cugtest(gstack, gcor)
summary(cug_test)


# 推移性の検定(サイズと密度を固定)
gtrans(FRIEND)
cug_gtrans <- cug.test(FRIEND, gtrans, cmode = "edges")
print(cug_gtrans)



# 密度の検定(辺が張る確率が0.5に固定)
gden(FRIEND)

cug.gden <- cug.test(FRIEND, gden, cmode="size")
print(cug.gden)



# 推移性の検定(サイズを固定)
cug.gtrans2 <- cug.test(FRIEND, gtrans, cmode="size")
print(cug.gtrans2)



library(statnet)

data(florentine)

flomarriage_tbl <- as_tbl_graph(flomarriage)

# 富を重みにしたグラフ化
flomarriage %>%
  as_tbl_graph() %>%
  ggraph(layout = "kk") +
  geom_edge_link(alpha=0.8, colour = "lightgray") +
  scale_edge_width(range = c(0.1,1)) +
  geom_node_point(aes(size = wealth)) +
  geom_node_label(aes(label = name),repel = TRUE)


# エッジのみを加味したモデル
flomarriage.model.0 <- ergm(flomarriage ~ edges)
summary(flomarriage.model.0)



# エッジと三角形と加味したモデル
flomarriage.model.1 <- ergm(flomarriage ~ edges + triangle)
summary(flomarriage.model.1)



# 属性の追加
ADVICE <- as.network(ADVICE)

age <- c(33,42,40,33,32,59,55,34,62,37,46,
         34,48,43,40,27,30,33,32,38,36)
tenure <- c(9.333,19.583,12.75,7.5,3.333,
            28,30,11.333,5.417,9.25,27,8.917,0.25,10.417,
            8.417,4.667,12.417,9.083,4.833,11.667,12.5)
dpt <- c(4,4,2,4,2,1,0,1,2,3,3,1,2,2,2,4,1,3,2,2,1)
level <- c(3,2,3,3,3,3,1,3,3,3,3,3,3,2,3,3,3,2,3,3,2)
ADVICE %v% "age" <- age # 年齢
ADVICE %v% "tenure" <- tenure # 勤続年数
ADVICE %v% "dpt" <- dpt # 部署
ADVICE %v% "level" <- level # 階級


# 二者間の差分のデータを要因データとする
diff.age <- abs(sweep(matrix(age, nrow = 21, ncol = 21), 2, age))
diff.tenure <-
  sweep(matrix(tenure, nrow = 21, ncol = 21, byrow = TRUE), 1, tenure)
diff.level <- sweep(matrix(level, nrow = 21, ncol = 21), 2, level)

ADVICE_model <- ergm(ADVICE ~ edges + edgecov(diff.age) +
+                          edgecov(diff.tenure) + edgecov(diff.level) +  nodefactor("dpt") +
+                          nodematch("dpt") + receiver(base = 13) + mutual)
summary(advice.model.2)



library(RSiena)

data(s501)

#データの確認
plot_tbl_directed_data <- function(data, title){
  data %>%
    as_tbl_graph(directed = TRUE,mode = "out") %>%
    ggraph(layout = "kk") +
    geom_edge_link(alpha=0.8, colour = "#424242",arrow = arrow(length = unit(2, 'mm')), end_cap = circle(2, 'mm')) +
    scale_edge_width(range = c(0.1,1)) +
    geom_node_point() +
    ggtitle(title) +
    coord_fixed()
}

plot_tbl_directed_data(s501, title = "s501")
plot_tbl_directed_data(s502, title = "s502")
plot_tbl_directed_data(s503, title = "s503")


friendship <- array(c(s501,s502,s503), dim = c(50,50,3))
friendship <- sienaDependent(friendship)

alcohol <- varCovar(s50a) #varCovar：時間変化する変数オブジェクトを生成
smoke <- coCovar(s50s[,1]) #coCovar：時間変化しない変数オブジェクトを生成



mydata <- sienaDataCreate(friendship, alcohol, smoke)
mydata

myeff <- getEffects(mydata)
effectsDocumentation(myeff)

myproject <- sienaAlgorithmCreate(projname = "s50")
resalt <- siena07(myproject, data=mydata, effects=myeff)



summary(resalt)
siena.table(resalt, type="html", sig = T)

# モデルの当てはまりの診断
resalt <- siena07(myproject, data=mydata, effects=myeffect, returnDeps = TRUE)
gofi <- sienaGOF(resalt, IndegreeDistribution, varName = "friendship", cumulative = FALSE, join=TRUE)
plot(gofi)