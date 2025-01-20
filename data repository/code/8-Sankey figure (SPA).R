install.packages("networkD3")
install.packages("ggalluvial")
library(networkD3)

# 数据准备
nodes <- data.frame(
  name = c("Japan-Transportation",             # 第一列
           "Japan-Petroleum & Coke", "Japan-Business services", "Japan-Warehousing","Japan-Transportation","Japan-Trade", "China-Transportation","China-Petroleum & Coke","China-Textile","Other",# 第二列
           "Japan-Petroleum & Coke","China-Transportation", "China-Petroleum & Coke", "Japan-Business services","China-Other Products", "China-Textile", "Japan-Business services", "China-Other","China-Petroleum & Coke","Japan-Business services","Japan-Business services","Japan-Transportation",
           "China-Petroleum & Coke","China-Transportation","China-Business services","China-Petroleum & Coke","China-Textile") # 第三列
)

links <- data.frame(
  source = c(0, 0, 0, 0 ,0, 0, 0, 0,0,                # 日本交通到第二列
             1, 1, 1,                    # 日本制造业到第三列
             2, 2, 2,                   # 日本服务业到第三列
             3, 3,
             4, 4,
             5, 5,
             6 ,6, 6,
             7,
             8
             ),                   # 日本农业到第三列
  target = c(1, 2, 3, 4, 5, 6, 7, 8,9,                # 第二列
             10, 11, 12,                    # 第三列（日本制造业的传递）
             13, 14, 15,                  # 第三列（日本服务业的传递）
             16, 17,
             18, 19,
             20, 21,
             22, 23, 24,
             25,
             26
             ),                   # 第三列（日本农业的传递）
  value = c(150118.28, 114908.90, 49213.46, 48238.21,32831.96 , 1006.13, 905.78, 782.03,213383,            # 第一列到第二列
            28598.89, 283.99, 170.40,                   # 第二列到第三列（日本制造业）
            12993.99, 581.99, 109.01,                   # 第二列到第三列（日本服务业）
            5659.09, 134.53,
            6367.97, 4874.40,
            2228.86, 1628.87,
            122.65, 97.42, 96.20,
            229.28,
            351.81)                   # 第二列到第三列（日本农业）
)

# 绘制桑吉图
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              units = "Tons", fontSize = 11, nodeWidth = 30)


# 数据准备
nodes <- data.frame(
  name = c("Japan-Transportation",             # 第一列
           "China-Other Products", "China-Textile", "China-Petroleum & Coke","China-Transportation","China-Business services","China-Other sectors")
)
links <- data.frame(
  source = c(0, 0, 0,0,0,0)  ,           
  target = c(1, 2, 3,                 # 第二列
             4, 5, 6),                   # 第三列（日本农业的传递）
  value = c(9118.19 , 
            3692.92 ,
            3072.57 ,
            2921.91 ,
            2881.12 ,
            3758.69 )                   # 第二列到第三列（日本农业）
)

# 绘制桑吉图
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "source", Target = "target",
                   Value = "value", NodeID = "name",
                   units = "Tons", fontSize = 12, nodeWidth = 30)

# 在节点上显示数值（通过 JavaScript 自定义）
p$x$options$nodeLabel <- nodes$value  # 将数值添加为节点的标签
htmlwidgets::onRender(
  p,
  "
  function(el, x) {
    d3.select(el).selectAll('.node text')
      .text(function(d) { 
        return d.name + ' (' + Math.round(d.value) + ')';  // 取整并显示
      });
  }
  "
)