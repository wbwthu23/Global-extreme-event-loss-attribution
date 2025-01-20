library("tidyr")
library("readxl")
library("dplyr")
library("ggplot2")
library(RColorBrewer)
library(patchwork)
library(scales)
library(gridExtra) 
library(ggrepel)
library(openxlsx) 
library(maps)
library(countrycode)
library(forcats)
library(ggalluvial)
library(ggrepel)

load("/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/loss.RData")
load("/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/region_match.RData")
load("/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/direct_loss_far.RData")
#cap_stock_drought_loss_full cap_stock_flood_loss_full cap_stock_storm_loss_full
load("/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/direct_loss.RData")
#direct_drought_loss_full direct_flood_loss_full direct_storm_loss_full

align_rgn <- read_excel("/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/wwh/align_rgn.xlsx")
align_sec <- read_excel("/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/wwh/align_sec.xlsx")

# color setting
colors <- c("Africa" = "#ef476f", "Americas" = "#ffd166", "Asia" = "#06d6a0", "Europe" = "#118ab2", "Oceania" = "#073b4c")
color_disaster <- c("drought" = "#e63946", "flood"="#06d6a0","storm"="#0081a7")
color_pie <- c("direct losses" = "#e63946", "indirect losses by disasters in China"="#06d6a0","indirect losses by disasters outside China"="#0081a7")
color_losses <- c("direct" = "#9a8c98", "indirect"="#c9ada7")
# sector setting 
#direct affected sectors
manu <- c("TWL", "EEQ", "ROIL", "CHM", "CRP", "NMM",
          "I_S", "NFM", "TEQ")
tran <- c("TRAN", "WHS")
elec <- c("ELEC")
hydr <- c("WTR")
mine <- c("COAL", "OIL", "GAS", "MINE")
arch <- c("CNS")
#indirect affected sectors
serv <- c("DWE","HHT","EDU","ROS","OSG","AFS","CMN")
fina <- c("OFI","INS","RSA","OBS")
food <- c('FOOD')
trade <- c('TRD')
other <- c('OTHR')
agri <- c("AGR")

#--------------------------1   global        -----------------------------------

#--------------------------1.1 global total-------------------------------------

data1 <- data.frame(
  Category = c("drought", "drought", "drought", "drought", "flood", "flood","flood", "flood", "storm", "storm", "storm", "storm"),
  Type = c("disaster losses", "disaster losses", "climate change attributed losses", "climate change attributed losses", "disaster losses", "disaster losses", "climate change attributed losses", "climate change attributed losses","disaster losses", "disaster losses", "climate change attributed losses", "climate change attributed losses"),
  damages = c("direct", "indirect", "direct", "indirect", "direct", "indirect", "direct", "indirect","direct", "indirect", "direct", "indirect"),
  Value = c(10.79, 9.87, 5.11, 4.62, 45.50, 63.44, 9.26, 12.41, 85.50, 104.56, 52.51, 65.73),
  multiplier = c(1.90, 1.90, 1.90, 1.90, 2.34, 2.34, 2.34, 2.34, 2.25, 2.25, 2.25, 2.25),
  attributer = c(0.47, 0.47, 0.47, 0.47 , 0.20, 0.20, 0.20, 0.20, 0.62, 0.62,0.62, 0.62)
)

# a Economic Costs of Extreme Weather Events
data1$damages <- factor(data1$damages, levels = c("indirect", "direct"))
data1$Type <- factor(data1$Type, levels = c("climate change attributed losses", "disaster losses"))

ggplot(data1, aes(x = Type, y = Value, fill = damages)) +
  geom_col(width = 0.8) + 
  facet_wrap(~Category) +  # 按灾害类型分面显示
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(title = "a) Economic Losses of Extreme Weather Events",
       x = "",
       y = "billion dollars",
       fill = "Type of losses") +
  scale_fill_manual(values = color_losses)+
  scale_x_discrete(labels = function(x) gsub("attr", "\nattr", x)) +  theme_minimal() +
  theme(axis.text.x = element_text(size=10),
        plot.title = element_text(size = 12),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.text = element_text(size = 10))

# b Percentage of attributed losses

data2 <- data.frame(
  Category = c("drought", "flood", "storm"),
  multiplier = c(1.90, 2.34, 2.25),
  attributable = c(0.47, 0.20, 0.62)
)

data2$Category <- factor(data2$Category, levels = c("drought", "flood", "storm"))

ggplot(data2, aes(x = Category, y = attributable, fill = Category)) + 
  geom_col() + 
  geom_text(aes(label = scales::label_percent(accuracy = 1)(attributable)), vjust = 1.5, color = "black") + 
  labs(x = "", y = "", title = "b) Percentage of Attributable Losses") +
  theme_minimal() + 
  guides(fill = FALSE) +
  scale_fill_manual(values = color_disaster) +
  theme(axis.text.x = element_text(size=10),
        plot.title = element_text(size = 12),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.text = element_text(size = 10))

# c Multiplier Effect 

ggplot(data2, aes(x = Category, y = multiplier, fill = Category)) + 
  geom_col() +  
  geom_text(aes(label=multiplier), vjust = 1.5, color = "black") + 
  labs(x = "", y = "", title = "c) Multiplier Effect of Attributable Losses") +
  scale_fill_manual(values = color_disaster) +
  theme_minimal() +  # 使用简洁主题
  theme(axis.text.x = element_text(size=10),
        plot.title = element_text(size = 12),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.text = element_text(size = 10))


#--------------------------1.2 global by sector----------------------------

#far-direct

direct_loss_far_sec <- direct_loss_far

direct_loss_far_sec <- direct_loss_far_sec %>% 
  mutate(i = case_when(
    i %in% manu ~ "MANU",  
    TRUE ~ i                            
  ))

direct_loss_far_sec <- direct_loss_far_sec %>% 
  mutate(i = case_when(
    i %in% tran ~ "TRAN",  
    TRUE ~ i                            
  ))

direct_loss_far_sec <- direct_loss_far_sec %>% 
  mutate(i = case_when(
    i %in% mine ~ "MINE",  
    TRUE ~ i                            
  ))

direct_loss_far_sec <- direct_loss_far_sec %>% 
  mutate(i = case_when(
    i %in% serv ~ "SERV",  
    TRUE ~ i                            
  ))

direct_loss_far_sec <- direct_loss_far_sec %>% 
  mutate(i = case_when(
    i %in% fina ~ "FINA",  
    TRUE ~ i                            
  ))

direct_loss_far_sec <- direct_loss_far_sec %>%
  group_by(i) %>%
  summarize(across(c(drought,flood,storm), sum, .names = "{.col}"))

direct_loss_far_sec <- direct_loss_far_sec %>%
  filter(flood != 0)

#far-x

x_loss_far_sec <- x_loss_far
colnames(x_loss_far_sec)[3:5] <- c("drought", "flood","storm")

x_loss_far_sec <- x_loss_far_sec %>% 
  mutate(i = case_when(
    i %in% manu ~ "MANU",  
    TRUE ~ i                            
  ))

x_loss_far_sec <- x_loss_far_sec %>% 
  mutate(i = case_when(
    i %in% tran ~ "TRAN",  
    TRUE ~ i                            
  ))

x_loss_far_sec <- x_loss_far_sec %>% 
  mutate(i = case_when(
    i %in% mine ~ "MINE",  
    TRUE ~ i                            
  ))

x_loss_far_sec <- x_loss_far_sec %>% 
  mutate(i = case_when(
    i %in% serv ~ "SERV",  
    TRUE ~ i                            
  ))

x_loss_far_sec <- x_loss_far_sec %>% 
  mutate(i = case_when(
    i %in% fina ~ "FINA",  
    TRUE ~ i                            
  ))

x_loss_far_sec <- x_loss_far_sec %>%
  group_by(i) %>%
  summarize(across(c(drought,flood,storm), sum, .names = "{.col}"))

#改变数据类型以便做图
x_loss_far_sec_long <- pivot_longer(
  x_loss_far_sec,
  cols = c("drought", "flood", "storm"), 
  names_to = "disaster",         
  values_to = "loss"           
)
x_loss_far_sec_long$loss <- x_loss_far_sec_long$loss/1000000 #change to billion dollars

x_loss_far_sec_long <- x_loss_far_sec_long %>%
  group_by(i) %>%
  mutate(total_loss = sum(loss)) %>%
  ungroup()


direct_loss_far_sec_long <- pivot_longer(
  direct_loss_far_sec,
  cols = c("drought", "flood", "storm"), 
  names_to = "disaster",         
  values_to = "loss"           
)
direct_loss_far_sec_long$loss <- direct_loss_far_sec_long$loss/1000000 #change to billion dollars

direct_loss_far_sec_long <- direct_loss_far_sec_long %>%
  group_by(i) %>%
  mutate(total_loss = sum(loss)) %>%
  ungroup()

#把部门缩写变为全程
direct_loss_far_sec_long$i <- gsub("MANU", "Manufacture", direct_loss_far_sec_long$i)
direct_loss_far_sec_long$i <- gsub("AGR", "Agriculture", direct_loss_far_sec_long$i)
direct_loss_far_sec_long$i <- gsub("TRAN", "Transportation", direct_loss_far_sec_long$i)
direct_loss_far_sec_long$i <- gsub("FINA", "Finance & Real Estate", direct_loss_far_sec_long$i)
direct_loss_far_sec_long$i <- gsub("CNS", "Construction", direct_loss_far_sec_long$i)
direct_loss_far_sec_long$i <- gsub("MINE", "Mining Industry", direct_loss_far_sec_long$i)
direct_loss_far_sec_long$i <- gsub("ELEC", "Electricity", direct_loss_far_sec_long$i)
direct_loss_far_sec_long$i <- gsub("OTHR", "Other Industries", direct_loss_far_sec_long$i)
direct_loss_far_sec_long$i <- gsub("TRD", "Trade", direct_loss_far_sec_long$i)
direct_loss_far_sec_long$i <- gsub("SERV", "Services", direct_loss_far_sec_long$i)
direct_loss_far_sec_long$i <- gsub("WTR", "Water Supply", direct_loss_far_sec_long$i)
direct_loss_far_sec_long$i <- gsub("FOOD", "Food Industry", direct_loss_far_sec_long$i)

# a) 可归因直接损失的部门堆叠柱状图

ggplot(direct_loss_far_sec_long, aes(x = fct_reorder(i, total_loss), y = loss, fill = disaster)) +
  geom_col() + 
  labs(x = "", y = "billion dollars", fill = "disaster",title = "a) Direct Attributable Losses") +
  theme_minimal()+
  scale_fill_manual(values = color_disaster) +
  coord_flip() +
  guides(fill = FALSE) +
  theme(axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        plot.title = element_text(size = 12),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.text = element_text(size = 10))


# b) 可归因直接损失的部门比例堆叠柱状图

direct_loss_far_sec_long_pro <- direct_loss_far_sec_long %>%
  group_by(i) %>%
  mutate(total = sum(loss),      # 计算每个category的总和
         proportion = loss / total)  # 计算每个部分的比例

ggplot(direct_loss_far_sec_long_pro, aes(x = fct_reorder(i, total_loss), y = proportion, fill = disaster)) +
  geom_col() +
  labs(x = "", y = "", fill = "disaster",title = "b) proportion of direct attributed losses") +
  scale_y_continuous(labels = scales::percent) +  # 将y轴标签格式化为百分比
  theme_minimal() +
  scale_fill_manual(values = color_disaster) +
  coord_flip() +
  guides(fill = FALSE) +
  theme(axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        plot.title = element_text(size = 12),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.text = element_text(size = 10))


# c) 可归因总损失的部门堆叠柱状图
#把部门缩写变为全程
x_loss_far_sec_long$i <- gsub("MANU", "Manufacture", x_loss_far_sec_long$i)
x_loss_far_sec_long$i <- gsub("AGR", "Agriculture", x_loss_far_sec_long$i)
x_loss_far_sec_long$i <- gsub("TRAN", "Transportation", x_loss_far_sec_long$i)
x_loss_far_sec_long$i <- gsub("FINA", "Finance & Real Estate", x_loss_far_sec_long$i)
x_loss_far_sec_long$i <- gsub("CNS", "Construction", x_loss_far_sec_long$i)
x_loss_far_sec_long$i <- gsub("MINE", "Mining Industry", x_loss_far_sec_long$i)
x_loss_far_sec_long$i <- gsub("ELEC", "Electricity", x_loss_far_sec_long$i)
x_loss_far_sec_long$i <- gsub("OTHR", "Other Industries", x_loss_far_sec_long$i)
x_loss_far_sec_long$i <- gsub("TRD", "Trade", x_loss_far_sec_long$i)
x_loss_far_sec_long$i <- gsub("SERV", "Services", x_loss_far_sec_long$i)
x_loss_far_sec_long$i <- gsub("WTR", "Water Supply", x_loss_far_sec_long$i)
x_loss_far_sec_long$i <- gsub("FOOD", "Food Industry", x_loss_far_sec_long$i)

ggplot(x_loss_far_sec_long, aes(x = fct_reorder(i, total_loss), y = loss, fill = disaster)) +
  geom_col() +
  labs(x = "", y = "billion dollars", fill = "disaster",title = "b) Total Attributable Losses") +
  theme_minimal()+
  scale_fill_manual(values = color_disaster) +
  coord_flip() +
  guides(fill = FALSE) +
  theme(axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        plot.title = element_text(size = 12),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.text = element_text(size = 10))


# d) 可归因总损失的部门比例堆叠柱状图

x_loss_far_sec_long_pro <- x_loss_far_sec_long %>%
  group_by(i) %>%
  mutate(total = sum(loss),      # 计算每个category的总和
         proportion = loss / total)  # 计算每个部分的比例

ggplot(x_loss_far_sec_long_pro, aes(x = fct_reorder(i, total_loss), y = proportion, fill = disaster)) +
  geom_col() +
  labs(x = "", y = "", fill = "Disaster",title = "d) proportion of total attributed losses") +
  scale_y_continuous(labels = scales::percent) +  # 将y轴标签格式化为百分比
  theme_minimal() +
  scale_fill_manual(values = color_disaster) +
  coord_flip() +
  theme(axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        plot.title = element_text(size = 12),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.text = element_text(size = 10))




#--------------------------2   regional-----------------------------------

#--------------------------2.1 countries' losses (Quadrant)---------------------
GDP_from_GTAP <- read_excel("/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/wwh/GDP_from_GTAP.xlsx")

t_GDP_from_GTAP <- data.frame(t(GDP_from_GTAP))
t_GDP_from_GTAP$RowNames <- rownames(t_GDP_from_GTAP)
t_GDP_from_GTAP <- t_GDP_from_GTAP[, c(ncol(t_GDP_from_GTAP), 1:(ncol(t_GDP_from_GTAP)-1))]
colnames(t_GDP_from_GTAP) <- c('ISO','GDP')

# change the type of data
for (i in 2:ncol(cap_stock_drought_loss_full)) {
  cap_stock_drought_loss_full[,i] <- as.numeric(as.matrix(cap_stock_drought_loss_full[,i]))
  direct_drought_loss_full[,i] <- as.numeric(as.matrix(direct_drought_loss_full[,i]))
  cap_stock_flood_loss_full[,i] <- as.numeric(as.matrix(cap_stock_flood_loss_full[,i]))
  direct_flood_loss_full[,i] <- as.numeric(as.matrix(direct_flood_loss_full[,i]))
  cap_stock_storm_loss_full[,i] <- as.numeric(as.matrix(cap_stock_storm_loss_full[,i]))
  direct_storm_loss_full[,i] <- as.numeric(as.matrix(direct_storm_loss_full[,i]))
}
cap_stock_drought_loss_full[is.na(cap_stock_drought_loss_full)] <- 0
direct_drought_loss_full[is.na(direct_drought_loss_full)] <- 0
cap_stock_flood_loss_full[is.na(cap_stock_flood_loss_full)] <- 0
direct_flood_loss_full[is.na(direct_flood_loss_full)] <- 0
cap_stock_storm_loss_full[is.na(cap_stock_storm_loss_full)] <- 0
direct_storm_loss_full[is.na(direct_storm_loss_full)] <- 0


GDP <- read.csv("/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/wwh/data_gdp_2014.csv")
GDP <- na.omit(GDP)
POP <- read.csv("/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/wwh/data_population_2014.csv")
POP <- na.omit(POP)
colnames(GDP) <- c("Region", "ISO", "Value", "ISO_new")
colnames(POP) <- c("Region", "ISO", "Value", "ISO_new")

# calculate GDP shares for different types of disasters and different types of costs
drought_loss_GDP_attr <- colSums(cap_stock_drought_loss_full[,-1]) / (1e6 * GDP_from_GTAP[1,]) #可归因直接损失
drought_loss_GDP_total <- colSums(direct_drought_loss_full[,-1]) / (1e6 * GDP_from_GTAP[1,]) #直接损失
flood_loss_GDP_attr <- colSums(cap_stock_flood_loss_full[,-1]) / (1e6 * GDP_from_GTAP[1,])
flood_loss_GDP_total <- colSums(direct_flood_loss_full[,-1]) / (1e6 * GDP_from_GTAP[1,])
storm_loss_GDP_attr <- colSums(cap_stock_storm_loss_full[,-1]) / (1e6 * GDP_from_GTAP[1,])
storm_loss_GDP_total <- colSums(direct_storm_loss_full[,-1]) / (1e6 * GDP_from_GTAP[1,])

GDP <- GDP[,-c(1,2)] %>%
  group_by(ISO_new) %>%
  summarize(total_number = sum(Value))


POP <- POP[,-c(1,2)] %>%
  group_by(ISO_new) %>%
  summarize(total_number = sum(Value))

GDP_per_capita <- POP
GDP_per_capita[,3] <- GDP[,2] / POP[,2]
colnames(GDP_per_capita) <- c("ISO", "POP", "GDP_per_capita")

drought_loss_GDP_attr_vertical <- cbind(as.data.frame(t(drought_loss_GDP_attr)),
                                        GTAP_rgn)
colnames(drought_loss_GDP_attr_vertical) <- c("costs", "ISO")

drought_loss_GDP_total_vertical <- cbind(as.data.frame(t(drought_loss_GDP_total)),
                                         GTAP_rgn)
colnames(drought_loss_GDP_total_vertical) <- c("costs", "ISO")

flood_loss_GDP_attr_vertical <- cbind(as.data.frame(t(flood_loss_GDP_attr)),
                                      GTAP_rgn)
colnames(flood_loss_GDP_attr_vertical) <- c("costs", "ISO")

flood_loss_GDP_total_vertical <- cbind(as.data.frame(t(flood_loss_GDP_total)),
                                       GTAP_rgn)
colnames(flood_loss_GDP_total_vertical) <- c("costs", "ISO")

storm_loss_GDP_attr_vertical <- cbind(as.data.frame(t(storm_loss_GDP_attr)),
                                      GTAP_rgn)
colnames(storm_loss_GDP_attr_vertical) <- c("costs", "ISO")

storm_loss_GDP_total_vertical <- cbind(as.data.frame(t(storm_loss_GDP_total)),
                                       GTAP_rgn)
colnames(storm_loss_GDP_total_vertical) <- c("costs", "ISO")

#figure 1: Climate attributed loss of direct damages
colnames(x_loss_far)[1] <- c("ISO")
colnames(ISO_region) <- c('Region','ISO')
x_loss_far <- merge(x_loss_far, ISO_region , by = "ISO", all.x = TRUE)
colnames(x_loss_far)[3:5] <- c("drought", "flood","storm")

sum_loss_GDP_attr_vertical <- drought_loss_GDP_attr_vertical
sum_loss_GDP_attr_vertical[,1] <- sum_loss_GDP_attr_vertical[,1] + 
  flood_loss_GDP_attr_vertical[,1] + 
  storm_loss_GDP_attr_vertical[,1]

data_bubble_AC_sum <- left_join(GDP_per_capita, sum_loss_GDP_attr_vertical, by = "ISO")
colnames(data_bubble_AC_sum)[1] <- "ISO"
data_bubble_AC_sum <- left_join(data_bubble_AC_sum, ISO_region, by = "ISO")
data_bubble_AC_sum <- data_bubble_AC_sum %>%
  filter(costs != 0)
world_loss_GDP <- c(sum(cap_stock_drought_loss_full[,-1]) / (1e6 * sum(GDP_from_GTAP)),
                    sum(direct_drought_loss_full[,-1]) / (1e6 * sum(GDP_from_GTAP)),
                    sum(cap_stock_flood_loss_full[,-1]) / (1e6 * sum(GDP_from_GTAP)),
                    sum(direct_flood_loss_full[,-1]) / (1e6 * sum(GDP_from_GTAP)),
                    sum(cap_stock_storm_loss_full[,-1]) / (1e6 * sum(GDP_from_GTAP)),
                    sum(direct_storm_loss_full[,-1]) / (1e6 * sum(GDP_from_GTAP)))
world_loss_GDP <- as.data.frame(world_loss_GDP)
world_loss_GDP <- cbind(world_loss_GDP, c("Drought", 
                                          "Flood",
                                          "Storm",
                                          "Drought", 
                                          "Flood",
                                          "Storm"))
world_loss_GDP <- cbind(world_loss_GDP, c("AC", 
                                          "AC",
                                          "AC",
                                          "TC", 
                                          "TC",
                                          "TC"))
colnames(world_loss_GDP) <- c("Loss", "Type of Disaster", "Type of Costs")

glob_avg_GDP <- sum(GDP[,2]) / sum(POP[,2])
glob_avg_sum_total <- sum(world_loss_GDP[4:6,1])
glob_avg_sum_attr <- sum(world_loss_GDP[1:3,1])

plot_bubble_AC_sum <- ggplot(data_bubble_AC_sum, aes(x = log10(GDP_per_capita), y = costs, size = POP, color = Region)) +
  geom_point(alpha = 0.7) +              
  scale_size_area(max_size = 20) +        
  labs(
    title = "a) Attributable Direct Losses",
    x = "GDP per capita ($)",
    y = "Loss share of GDP",
    size = "Bubble Size") +
  guides(size = "none") + 
  theme_classic() + 
  scale_y_continuous(labels = label_percent(accuracy = 0.1))+
  coord_cartesian(ylim = c(0, 0.008)) +
  scale_color_manual(values = colors) +
  geom_label_repel(aes(label = ISO), nudge_x = 0.001, nudge_y = 0.0001,size = 3) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = function(x) format(round(10^x), big.mark = ",", scientific = FALSE)) + 
  geom_vline(xintercept = log10(glob_avg_GDP), linetype = "dashed", color = "black", linewidth = 0.5) + 
  geom_hline(yintercept = glob_avg_sum_attr, linetype = "dashed", color = "black", linewidth = 0.5) +
  theme(axis.text.x = element_text(size=10),
      axis.text.y = element_text(size=10),
      plot.title = element_text(size = 12),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      strip.text = element_text(size = 10))


plot_bubble_AC_sum

#figure 2: Climate attributed loss of direct and indirect damages

x_loss_far_GDP_attr  <- x_loss_far %>% 
  group_by(ISO,Region) %>% 
  summarize(across(c(drought,flood,storm), sum, .names = "{.col}"))

x_loss_far_GDP_attr <- merge(x_loss_far_GDP_attr, t_GDP_from_GTAP , by = "ISO", all.x = TRUE)

x_loss_far_GDP_attr$drought_ratio <- x_loss_far_GDP_attr$drought / (1e6 * x_loss_far_GDP_attr$GDP)
x_loss_far_GDP_attr$storm_ratio <- x_loss_far_GDP_attr$storm / (1e6 * x_loss_far_GDP_attr$GDP)
x_loss_far_GDP_attr$flood_ratio <- x_loss_far_GDP_attr$flood / (1e6 * x_loss_far_GDP_attr$GDP)

data_bubble_x_far <- left_join(GDP_per_capita, x_loss_far_GDP_attr, by = "ISO")
colnames(data_bubble_x_far)[1] <- "r"
data_bubble_x_far$sum_ratio <- data_bubble_x_far$drought_ratio + data_bubble_x_far$storm_ratio + data_bubble_x_far$flood_ratio
data_bubble_x_far$sum <- data_bubble_x_far$drought + data_bubble_x_far$storm + data_bubble_x_far$flood
glob_avg_sum_attr_x <- sum(data_bubble_x_far$sum) / (1e6 * sum(GDP_from_GTAP))

ggplot(data_bubble_x_far, aes(x = log10(GDP_per_capita), y = sum_ratio, size = POP, color = Region)) +
  geom_point(alpha = 0.7) +              
  scale_size_area(max_size = 20) +        
  labs(
    title = "b) Attributable Direct and Indirect Losses",
    x = "GDP per capita ($)",
    y = "Loss share of GDP",
    size = "Bubble Size") +
  guides(size = "none") + 
  theme_classic() + 
  scale_y_continuous(labels = label_percent(accuracy = 0.1))+
  coord_cartesian(ylim = c(0, 0.012)) +
  scale_color_manual(values = colors) +
  geom_label_repel(aes(label = r), nudge_x = 0.001, nudge_y = 0.0001,size = 3) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = function(x) format(round(10^x), big.mark = ",", scientific = FALSE)) + 
  geom_vline(xintercept = log10(glob_avg_GDP), linetype = "dashed", color = "black", linewidth = 0.5) + 
  geom_hline(yintercept = glob_avg_sum_attr_x, linetype = "dashed", color = "black", linewidth = 0.5) +    
  theme(axis.text.x = element_text(size=10),
      axis.text.y = element_text(size=10),
      plot.title = element_text(size = 12),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      strip.text = element_text(size = 10))



#--------------------------2.2 countries' losses (Stacked)----------------------
#direct-AC
data_bubble_AC_sum <- left_join(GDP_per_capita, sum_loss_GDP_attr_vertical, by = "ISO")
colnames(data_bubble_AC_sum)[1] <- "ISO"
data_bubble_AC_sum <- left_join(data_bubble_AC_sum, ISO_region, by = "ISO")
data_bubble_AC_sum <- data_bubble_AC_sum %>%
  filter(costs != 0)

world_loss_attr <- cbind(
  as.data.frame(colnames(cap_stock_drought_loss_full[,-1])),
  as.data.frame(colSums(cap_stock_drought_loss_full[,-1])),
  as.data.frame(colSums(cap_stock_flood_loss_full[,-1])),
  as.data.frame(colSums(cap_stock_storm_loss_full[,-1]))
)

world_loss_attr <- cbind(world_loss_attr[,1], rowSums(world_loss_attr[,2:4]))
colnames(world_loss_attr) <- c("ISO_new", "costs")
world_loss_attr <- as.data.frame(world_loss_attr)
world_loss_attr[,2] <- as.numeric(world_loss_attr[,2])
colors_type1 <- colorRampPalette(c("#9a8c98", "#22223b"))(100)
colors_type2 <- colorRampPalette(c("#c9ada7", "#4a4e69"))(100)

assign_colors <- function(loss_type, POP_normalized) {
  if (loss_type == "Direct") {
    return(colors_type1[ceiling(POP_normalized * 100)])
  } else if (loss_type == "Indirect") {
    return(colors_type2[ceiling(POP_normalized * 100)])
  } else {
    return(NA) 
  }
}

x_loss_attr <- x_loss_far %>%
  group_by(ISO) %>%
  summarize(drought = sum(drought),
            flood = sum(flood),
            storm = sum(storm))

sum_x_loss_attr <- cbind(x_loss_attr[,1], c(rowSums(x_loss_attr[,2:4])))
colnames(sum_x_loss_attr) <- c("ISO", "costs")

glob_avg_sum_attr_x <- sum(sum_x_loss_attr[,2]) / (1e6 * sum(GDP_from_GTAP))

data_bubble_AC_sum_x <- left_join(GDP_per_capita, sum_x_loss_attr, by = "ISO")
colnames(data_bubble_AC_sum_x)[1] <- "ISO_new"
data_bubble_AC_sum_x <- left_join(data_bubble_AC_sum_x, GDP, by = "ISO_new")
colnames(data_bubble_AC_sum_x)[5] <- "GDP_from_GTAP"
colnames(data_bubble_AC_sum_x)[1] <- "ISO"
data_bubble_AC_sum_x <- left_join(data_bubble_AC_sum_x, ISO_region, by = "ISO")
data_bubble_AC_sum_x <- data_bubble_AC_sum_x %>%
  filter(costs != 0)


data_bubble_AC_sum_x$GDP_from_GTAP <- sapply(data_bubble_AC_sum_x$ISO, 
                                             function(x) GDP_from_GTAP[1, as.character(x)])
data_bubble_AC_sum_x$GDP_from_GTAP <- as.numeric(data_bubble_AC_sum_x$GDP_from_GTAP)

data_bubble_AC_sum_x[,4] <- data_bubble_AC_sum_x[,4] / (1e6 * data_bubble_AC_sum_x[,5])
data_bubble_AC_sum_x <- na.omit(data_bubble_AC_sum_x)

data_accu_attr <- left_join(GDP, data_bubble_AC_sum[,c(1,4)], by = c("ISO_new" = "ISO"))
data_accu_attr <- left_join(data_accu_attr, data_bubble_AC_sum_x[,c(1,4)], by = c("ISO_new" = "ISO"))
data_accu_attr[is.na(data_accu_attr)] <- 0
data_accu_attr[,5] <- data_accu_attr[,4] - data_accu_attr[,3]
colnames(data_accu_attr) <- c("ISO_new", "GDP", "Direct", "Amplified", "Indirect")

data_accu_attr <- left_join(GDP, data_bubble_AC_sum[,c(1,4)], by = c("ISO_new" = "ISO"))
data_accu_attr <- left_join(data_accu_attr, data_bubble_AC_sum_x[,c(1,4)], by = c("ISO_new" = "ISO"))
data_accu_attr[is.na(data_accu_attr)] <- 0
data_accu_attr[,5] <- data_accu_attr[,4] - data_accu_attr[,3]
colnames(data_accu_attr) <- c("ISO_new", "GDP", "Direct", "Amplified", "Indirect")

data_accu_attr <- data_accu_attr %>%
  arrange(desc(Amplified)) %>% 
  mutate(cumulative_GDP = cumsum(GDP),
         midpoint_GDP = cumulative_GDP - GDP / 2)

data_accu_attr_long <- data_accu_attr %>%
  pivot_longer(cols = c(Direct, Indirect),
               names_to = "loss_type",
               values_to = "loss")

data_accu_attr_long <- left_join(data_accu_attr_long, POP, by = "ISO_new")
colnames(data_accu_attr_long)[8] <- "POP"
min_pop <- min(data_accu_attr_long$POP)
max_pop <- max(data_accu_attr_long$POP)
data_accu_attr_long$POP_normalized <- (data_accu_attr_long$POP - min_pop) / (max_pop - min_pop)
data_accu_attr_long$color <- mapply(assign_colors, data_accu_attr_long$loss_type, data_accu_attr_long$POP_normalized)

ggplot(data_accu_attr_long, aes(x = midpoint_GDP / 1e9, y = 100 * loss, fill = color)) +
  geom_bar(stat = "identity", position = "stack", width = data_accu_attr_long$GDP / 1e9) +
  labs(x = "Cumulative GDP (billion $)", y = "Loss share of GDP (%)", title = "Climate attributed, direct and indirect losses") +
  theme_classic() + 
  coord_cartesian(ylim = c(0.000, 1.5)) +
  scale_fill_identity()

write.xlsx(data_accu_attr, file = "/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/data_accu_attr.xlsx", sheetName = "Sheet1", rowNames = FALSE)

# only include developing countries

data_accu_attr <- read_excel("/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/data_accu_attr_developing.xlsx")

data_accu_attr_long <- data_accu_attr %>%
  pivot_longer(cols = c(Direct, Indirect),
               names_to = "loss_type",
               values_to = "loss")

data_accu_attr_long <- left_join(data_accu_attr_long, POP, by = "ISO_new")
colnames(data_accu_attr_long)[8] <- "POP"
min_pop <- min(data_accu_attr_long$POP)
max_pop <- max(data_accu_attr_long$POP)
data_accu_attr_long$POP_normalized <- (data_accu_attr_long$POP - min_pop) / (max_pop - min_pop)
data_accu_attr_long$color <- mapply(assign_colors, data_accu_attr_long$loss_type, data_accu_attr_long$POP_normalized)

ggplot(data_accu_attr_long, aes(x = midpoint_GDP / 1e9, y = 100 * loss, fill = color)) +
  geom_bar(stat = "identity", position = "stack", width = data_accu_attr_long$GDP / 1e9) +
  labs(x = "Cumulative GDP (billion $)", y = "Attributable losses share of GDP (%)") +
  theme_classic() + 
  coord_cartesian(ylim = c(0.000, 1.5)) +
  scale_fill_identity()

#--------------------------3   China-----------------------------------

#--------------------------3.1 China loss composition ------------------
data_pie <- data.frame(
  group = c("Sum", "Sum","Sum","Drought", "Drought","Drought",
            "Flood","Flood","Flood", "Storm","Storm","Storm"),
  subgroup = c("direct losses", "indirect losses by disasters in China", "indirect losses by disasters outside China",
               "direct losses", "indirect losses by disasters in China", "indirect losses by disasters outside China",
               "direct losses", "indirect losses by disasters in China", "indirect losses by disasters outside China",
               "direct losses", "indirect losses by disasters in China", "indirect losses by disasters outside China"),
  value = c(36, 49, 15, 50, 38, 12,
            39, 52,  9, 33, 50, 17)
)
ggplot(data_pie, aes(x = "", y = value, fill = subgroup)) +
  geom_bar(stat = "identity", width = 1) +
  facet_wrap(~ group, scales = "free", ncol = 4) +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_manual(values = color_pie) +
  geom_text(aes(label = paste0(round(value), "%")), position = position_stack(vjust = 0.5)) +
  labs(fill = "") +
  theme(legend.position = "top")

#--------------------------3.2 China loss sources (Sankey) ---------------------
chn_loss_far_row <- read_excel("/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/chn_loss_far_row.xlsx")
load("/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/x_loss_far_toCHN_by_region.RData")
#chn_loss_far_oceania,chn_loss_far_asia, chn_loss_far_africa,chn_loss_far_europe,chn_loss_far_northamerica,chn_loss_far_southamerica,
colnames(chn_loss_far_northamerica)[2:4] <- c("drought_na","flood_na","storm_na")
colnames(chn_loss_far_asia)[2:4] <- c("drought_asia","flood_asia","storm_asia")
colnames(x_loss_far)[1] <- c("r")

x_loss_far_CHN <- subset(x_loss_far, r %in% c("CHN", "HKG", "TWN"))
colnames(x_loss_far_CHN) <- c("r","i","drought","flood","storm","region")
x_loss_far_CHN <- x_loss_far_CHN %>%
  group_by(i) %>%
  summarize(across(c(drought,flood,storm), sum, .names = "{.col}"))

x_loss_far_CHN <- left_join(x_loss_far_CHN, chn_loss_far_row, by = "i")
colnames(x_loss_far_CHN)[5:7] <- c("drought_row","flood_row","storm_row")

x_loss_far_CHN$drought_chn <- x_loss_far_CHN$drought - x_loss_far_CHN$drought_row
x_loss_far_CHN$flood_chn <- x_loss_far_CHN$flood - x_loss_far_CHN$flood_row
x_loss_far_CHN$storm_chn <- x_loss_far_CHN$storm - x_loss_far_CHN$storm_row



#combine sectors
x_loss_far_CHN <- x_loss_far_CHN %>% 
  mutate(i = case_when(
    i %in% manu ~ "MANU",  
    TRUE ~ i                            
  ))

x_loss_far_CHN <- x_loss_far_CHN %>% 
  mutate(i = case_when(
    i %in% tran ~ "TRAN",  
    TRUE ~ i                            
  ))

x_loss_far_CHN <- x_loss_far_CHN %>% 
  mutate(i = case_when(
    i %in% mine ~ "MINE",  
    TRUE ~ i                            
  ))

x_loss_far_CHN <- x_loss_far_CHN %>% 
  mutate(i = case_when(
    i %in% serv ~ "SERV",  
    TRUE ~ i                            
  ))

x_loss_far_CHN <- x_loss_far_CHN %>% 
  mutate(i = case_when(
    i %in% fina ~ "FINA",  
    TRUE ~ i                            
  ))

x_loss_far_CHN <- x_loss_far_CHN %>%
  group_by(i) %>%
  summarize(across(c(drought_row,flood_row,storm_row,drought_chn,flood_chn,storm_chn), sum, .names = "{.col}"))

#CHNtoCHN
x_loss_far_CHNtoCHN <- subset(x_loss_far_CHN, select = -c(drought_row,flood_row,storm_row))
colnames(x_loss_far_CHNtoCHN) <- c("i","drought","flood","storm")
x_loss_far_CHNtoCHN_long <- x_loss_far_CHNtoCHN %>%
  pivot_longer(cols = c("drought","flood","storm"), 
               names_to = "type", 
               values_to = "loss")
x_loss_far_CHNtoCHN_long$loss <- x_loss_far_CHNtoCHN_long$loss/1000 #units: million dollars

#把部门缩写变为全称
x_loss_far_CHN$i <- gsub("MANU", "Manufacture", x_loss_far_CHN$i)
x_loss_far_CHN$i <- gsub("AGR", "Agriculture", x_loss_far_CHN$i)
x_loss_far_CHN$i <- gsub("TRAN", "Transportation", x_loss_far_CHN$i)
x_loss_far_CHN$i <- gsub("FINA", "Finance & Real Estate", x_loss_far_CHN$i)
x_loss_far_CHN$i <- gsub("CNS", "Construction", x_loss_far_CHN$i)
x_loss_far_CHN$i <- gsub("MINE", "Mining Industry", x_loss_far_CHN$i)
x_loss_far_CHN$i <- gsub("ELEC", "Electricity", x_loss_far_CHN$i)
x_loss_far_CHN$i <- gsub("OTHR", "Other Industries", x_loss_far_CHN$i)
x_loss_far_CHN$i <- gsub("TRD", "Trade", x_loss_far_CHN$i)
x_loss_far_CHN$i <- gsub("SERV", "Services", x_loss_far_CHN$i)
x_loss_far_CHN$i <- gsub("WTR", "Water Supply", x_loss_far_CHN$i)
x_loss_far_CHN$i <- gsub("FOOD", "Food Industry", x_loss_far_CHN$i)

#把部门缩写变为全称
x_loss_far_CHNtoCHN_long$i <- gsub("MANU", "Manufacture", x_loss_far_CHNtoCHN_long$i)
x_loss_far_CHNtoCHN_long$i <- gsub("AGR", "Agriculture", x_loss_far_CHNtoCHN_long$i)
x_loss_far_CHNtoCHN_long$i <- gsub("TRAN", "Transportation", x_loss_far_CHNtoCHN_long$i)
x_loss_far_CHNtoCHN_long$i <- gsub("FINA", "Finance & \nReal Estate", x_loss_far_CHNtoCHN_long$i)
x_loss_far_CHNtoCHN_long$i <- gsub("CNS", "Construction", x_loss_far_CHNtoCHN_long$i)
x_loss_far_CHNtoCHN_long$i <- gsub("MINE", "Mining Industry", x_loss_far_CHNtoCHN_long$i)
x_loss_far_CHNtoCHN_long$i <- gsub("ELEC", "Electricity", x_loss_far_CHNtoCHN_long$i)
x_loss_far_CHNtoCHN_long$i <- gsub("OTHR", "Other Industries", x_loss_far_CHNtoCHN_long$i)
x_loss_far_CHNtoCHN_long$i <- gsub("TRD", "Trade", x_loss_far_CHNtoCHN_long$i)
x_loss_far_CHNtoCHN_long$i <- gsub("SERV", "Services", x_loss_far_CHNtoCHN_long$i)
x_loss_far_CHNtoCHN_long$i <- gsub("WTR", "Water Supply", x_loss_far_CHNtoCHN_long$i)
x_loss_far_CHNtoCHN_long$i <- gsub("FOOD", "Food Industry", x_loss_far_CHNtoCHN_long$i)
x_loss_far_CHNtoCHN_long$type <- gsub("drought", "Drought", x_loss_far_CHNtoCHN_long$type)
x_loss_far_CHNtoCHN_long$type <- gsub("flood", "Flood", x_loss_far_CHNtoCHN_long$type)
x_loss_far_CHNtoCHN_long$type <- gsub("storm", "Storm", x_loss_far_CHNtoCHN_long$type)

flow_plt1 <- ggplot(x_loss_far_CHNtoCHN_long,
       aes(axis1 = type, axis2 = i, y = loss)) +
  geom_alluvium(aes(fill = type)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3.5) +
  scale_x_discrete(limits = c("disaster", "sector")) +
  labs(title = "a) Attributable losses caused by extreme events happened in China", y = "climate change attributed economic loss (million dollars)", x = "") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 12)) +
  guides(fill = FALSE)
flow_plt1

#ROWtoCHN
chn_loss_far_key <- left_join(chn_loss_far_northamerica, chn_loss_far_asia, by = "i")
chn_loss_far_key <- chn_loss_far_key %>%
  select(-drought_na, -drought_asia, -flood_na)
chn_loss_far_key <- left_join(chn_loss_far_row,chn_loss_far_key, by="i")
colnames(chn_loss_far_key)[2:4] <- c("drought_row","flood_row","storm_row")
chn_loss_far_key$flood_other <- chn_loss_far_key$flood_row - chn_loss_far_key$flood_asia
chn_loss_far_key$storm_other <- chn_loss_far_key$storm_row - chn_loss_far_key$storm_na - chn_loss_far_key$storm_asia
chn_loss_far_key <- chn_loss_far_key %>%
  select(-flood_row, -storm_row)

#combine sectors
chn_loss_far_key <- chn_loss_far_key %>% 
  mutate(i = case_when(
    i %in% manu ~ "MANU",  
    TRUE ~ i                            
  ))

chn_loss_far_key <- chn_loss_far_key %>% 
  mutate(i = case_when(
    i %in% tran ~ "TRAN",  
    TRUE ~ i                            
  ))

chn_loss_far_key <- chn_loss_far_key %>% 
  mutate(i = case_when(
    i %in% mine ~ "MINE",  
    TRUE ~ i                            
  ))

chn_loss_far_key <- chn_loss_far_key %>% 
  mutate(i = case_when(
    i %in% serv ~ "SERV",  
    TRUE ~ i                            
  ))

chn_loss_far_key <- chn_loss_far_key %>% 
  mutate(i = case_when(
    i %in% fina ~ "FINA",  
    TRUE ~ i                            
  ))

chn_loss_far_key <- chn_loss_far_key %>%
  group_by(i) %>%
  summarize(across(c(drought_row,flood_asia, flood_other,storm_na,storm_asia,storm_other), sum, .names = "{.col}"))


x_loss_far_ROWtoCHN <- subset(chn_loss_far_key)
colnames(x_loss_far_ROWtoCHN)[2:7] <- c("drought_ROW", "flood_Asia","flood_Other","storm_Asia","storm_NorthAmerica","storm_Other")

x_loss_far_ROWtoCHN_long <- x_loss_far_ROWtoCHN %>%
  pivot_longer(cols = c("drought_ROW", "flood_Asia","flood_Other","storm_Asia","storm_NorthAmerica","storm_Other"), 
               names_to = "type", 
               values_to = "loss")
x_loss_far_ROWtoCHN_long$loss <- x_loss_far_ROWtoCHN_long$loss/1000 #units: million dollars
x_loss_far_ROWtoCHN_long <- x_loss_far_ROWtoCHN_long[!(x_loss_far_ROWtoCHN_long$i == "AGR" & x_loss_far_ROWtoCHN_long$type == "flood_Asia"), ]
x_loss_far_ROWtoCHN_long <- x_loss_far_ROWtoCHN_long[!(x_loss_far_ROWtoCHN_long$i == "AGR" & x_loss_far_ROWtoCHN_long$type == "flood_Other"), ]
x_loss_far_ROWtoCHN_long <- x_loss_far_ROWtoCHN_long[!(x_loss_far_ROWtoCHN_long$i == "AGR" & x_loss_far_ROWtoCHN_long$type == "storm_Asia"), ]
x_loss_far_ROWtoCHN_long <- x_loss_far_ROWtoCHN_long[!(x_loss_far_ROWtoCHN_long$i == "AGR" & x_loss_far_ROWtoCHN_long$type == "storm_NorthAmerica"), ]
x_loss_far_ROWtoCHN_long <- x_loss_far_ROWtoCHN_long[!(x_loss_far_ROWtoCHN_long$i == "AGR" & x_loss_far_ROWtoCHN_long$type == "drought_ROW"), ]
x_loss_far_ROWtoCHN_long <- x_loss_far_ROWtoCHN_long[!(x_loss_far_ROWtoCHN_long$i == "AGR" & x_loss_far_ROWtoCHN_long$type == "storm_Other"), ]

#把部门缩写变为全称
x_loss_far_ROWtoCHN_long$i <- gsub("MANU", "Manufacture", x_loss_far_ROWtoCHN_long$i)
x_loss_far_ROWtoCHN_long$i <- gsub("AGR", "Agriculture", x_loss_far_ROWtoCHN_long$i)
x_loss_far_ROWtoCHN_long$i <- gsub("TRAN", "Transportation", x_loss_far_ROWtoCHN_long$i)
x_loss_far_ROWtoCHN_long$i <- gsub("FINA", "Finance & \nReal Estate", x_loss_far_ROWtoCHN_long$i)
x_loss_far_ROWtoCHN_long$i <- gsub("CNS", "-", x_loss_far_ROWtoCHN_long$i) #Construction后加
x_loss_far_ROWtoCHN_long$i <- gsub("MINE", "Mining Industry", x_loss_far_ROWtoCHN_long$i)
x_loss_far_ROWtoCHN_long$i <- gsub("ELEC", "Electricity", x_loss_far_ROWtoCHN_long$i)
x_loss_far_ROWtoCHN_long$i <- gsub("OTHR", "Other Industries", x_loss_far_ROWtoCHN_long$i)
x_loss_far_ROWtoCHN_long$i <- gsub("TRD", "Trade", x_loss_far_ROWtoCHN_long$i)
x_loss_far_ROWtoCHN_long$i <- gsub("SERV", "Services", x_loss_far_ROWtoCHN_long$i)
x_loss_far_ROWtoCHN_long$i <- gsub("WTR", "", x_loss_far_ROWtoCHN_long$i)#Water supply后加
x_loss_far_ROWtoCHN_long$i <- gsub("FOOD", "Food Industry", x_loss_far_ROWtoCHN_long$i)


x_loss_far_ROWtoCHN_long$type <- gsub("drought_ROW", "Drought\n(All Regions)", x_loss_far_ROWtoCHN_long$type)
x_loss_far_ROWtoCHN_long$type <- gsub("flood_Asia", "Flood\n(Asia)", x_loss_far_ROWtoCHN_long$type)
x_loss_far_ROWtoCHN_long$type <- gsub("flood_Other", "Flood\n(Other Regions)", x_loss_far_ROWtoCHN_long$type)
x_loss_far_ROWtoCHN_long$type <- gsub("storm_Asia", "Storm\n(Asia)", x_loss_far_ROWtoCHN_long$type)
x_loss_far_ROWtoCHN_long$type <- gsub("storm_NorthAmerica", "Storm\n(North America)", x_loss_far_ROWtoCHN_long$type)
x_loss_far_ROWtoCHN_long$type <- gsub("storm_Other", "Storm\n(Other Regions)", x_loss_far_ROWtoCHN_long$type)

x_loss_far_ROWtoCHN_long$i <- factor(x_loss_far_ROWtoCHN_long$i, levels = c("-", "Electricity", "Finance & \nReal Estate","Food Industry","Manufacture",
                                                                            "Mining Industry","Other Industries","Services",
                                                                            "Trade","Transportation",""))


ggplot(x_loss_far_ROWtoCHN_long,
       aes(axis1 = type, axis2 = i, y = loss)) +
  geom_alluvium(aes(fill = type)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3.5) +
  scale_x_discrete(limits = c("disaster", "sector")) +
  labs(title = "b) Attributable losses caused by extreme events happened in other regions", y = "climate change attributed economic loss (million dollars)", x = "") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 12)) +
  guides(fill = FALSE) +
  annotate("text", x = 1.87, y = 4355, label = "Construction", size = 3.5, hjust = 0)  +
  #annotate("text", x = 2.18, y = 4300, label = "Water Supply", size = 3.5, hjust = 0) 
  annotate("text", x = 1.87, y = -30, label = "Water Supply", size = 3.5, hjust = 0) 

