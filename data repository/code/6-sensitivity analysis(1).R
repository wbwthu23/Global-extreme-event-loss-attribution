library("tidyr")
library("readxl")
library("dplyr")
library("ggplot2")

# read EMDAT database

EMDAT <- read_excel("/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/EM_DAT_处理.xlsx")

# filter different disasters

drought <- EMDAT %>% filter(`Disaster Type` == "Drought")
flood <- EMDAT %>% filter(`Disaster Type` == "Flood")
storm <- EMDAT %>% filter(`Disaster Type` == "Storm")
#coldwave <- EMDAT %>% filter(`Disaster Type` == "Cold wave")
#wildfire <-  EMDAT %>% filter(`Disaster Type` == "Wildfire")
#heatwave <-  EMDAT %>% filter(`Disaster Type` == "Heat wave")

#drop disasters with year beyond 2009-2019

#drought_10 <- drought %>% filter(!(`Start Year` == "2000"|`Start Year` == "2001"|`Start Year` == "2002"|
#                                     `Start Year` == "2020"|`Start Year` == "2021"|`Start Year` == "2022"))

# calculate disaster losses by year and country

# drought

drought_loss <- drought %>%
  group_by(`Start Year`, ISO) %>%
  summarise(Sum_D = sum(`Total Damage, Adjusted ('000 US$)`), .groups = "drop")

drought_loss_wide <- pivot_wider(
  drought_loss,
  names_from = `Start Year`,
  values_from = Sum_D,
  values_fill = list(Sum_D = 0)  
)

# flood

flood_loss <- flood %>%
  group_by(`Start Year`, ISO) %>%
  summarise(Sum_D = sum(`Total Damage, Adjusted ('000 US$)`), .groups = "drop")

flood_loss_wide <- pivot_wider(
  flood_loss,
  names_from = `Start Year`,
  values_from = Sum_D,
  values_fill = list(Sum_D = 0)  
)

# storm

storm_loss <- storm %>%
  group_by(`Start Year`, ISO) %>%
  summarise(Sum_D = sum(`Total Damage, Adjusted ('000 US$)`), .groups = "drop")

storm_loss_wide <- pivot_wider(
  storm_loss,
  names_from = `Start Year`,
  values_from = Sum_D,
  values_fill = list(Sum_D = 0) 
)


# calculate annual average direct loss in 2009-2019

drought_loss_wide$mean <- rowMeans(drought_loss_wide[, c("2009", "2010", "2011","2012", "2013", "2014",
                                                         "2015", "2016", "2017","2018", "2019")])
flood_loss_wide$mean <- rowMeans(flood_loss_wide[, c("2009", "2010", "2011","2012", "2013", "2014",
                                                     "2015", "2016", "2017","2018", "2019")])
storm_loss_wide$mean <- rowMeans(storm_loss_wide[, c("2009", "2010", "2011","2012", "2013", "2014",
                                                     "2015", "2016", "2017","2018", "2019")])
#coldwave_loss_wide$mean <- rowMeans(coldwave_loss_wide[, c("2009", "2011", "2012", "2014", "2015", "2016")])
#wildfire_loss_wide$mean <- rowMeans(wildfire_loss_wide[, c("2009", "2010", "2011","2012", "2013", "2014",
#                                                       "2015", "2016", "2017","2018", "2019")])
#heatwave_loss_wide$mean <- rowMeans(heatwave_loss_wide[, c("2010", "2014")])


# sumup in disaster_loss dataframe

drought_loss_wide_selected <- drought_loss_wide[, c("ISO", "mean")]

flood_loss_wide_selected <- flood_loss_wide[, c("ISO", "mean")]

storm_loss_wide_selected <- storm_loss_wide[, c("ISO", "mean")]

disaster_loss<- merge(drought_loss_wide_selected, flood_loss_wide_selected, by = "ISO", all = TRUE)

disaster_loss<- merge(disaster_loss, storm_loss_wide_selected, by = "ISO", all = TRUE)

colnames(disaster_loss) <- c("ISO","drought", "flood","storm")

disaster_loss[is.na(disaster_loss)] <- 0
colSums(disaster_loss[,2:4])
EMDAT_selected <- EMDAT[, c("ISO", "Region")]

EMDAT_selected <- unique(EMDAT_selected)

disaster_loss <- merge(disaster_loss, EMDAT_selected , by = "ISO", all.x = TRUE)

#-----FAR (fraction of attributed risk) Source: Newman 2023

FAR <- read_excel("/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/FAR.xlsx")

colnames(FAR) <- c("Region","d_far","f_far", "s_far","h_far","w_far","c_far")

loss_FAR <- merge(disaster_loss, FAR , by = "Region", all.x = TRUE)

#------calculate attributed direct economic cost 

loss_FAR$drought_far <- loss_FAR$drought * loss_FAR$d_far

loss_FAR$flood_far <- loss_FAR$flood * loss_FAR$f_far

loss_FAR$storm_far <- loss_FAR$storm * loss_FAR$s_far

far_loss <- loss_FAR[, c("Region","ISO","drought_far","flood_far","storm_far")]


#----- Unify EMDAT regions as GTAP regions

EMDAT_rgn <- EMDAT[, c("ISO", "Region")]
EMDAT_rgn <- unique(EMDAT_rgn)

# Use VLOOKUP for EMDAT (with more regions) and manually adjust until each rgn
# (region) in EMDAT has a respective GTAP region.

# however, there are regions only existing in GTAP (e.g.: Brunei, Finland), and 
# their disaster data are set as 0 in the following data processing.

# this could also be indicated by the number of rows (126, < 141)in far_loss_aggr.

align_rgn <- read_excel("/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/wwh/align_rgn.xlsx")

ISO_region <- merge(EMDAT_rgn, align_rgn , by = "ISO", all.x = TRUE)
ISO_region <- ISO_region %>% select(-c("ISO"))
ISO_region <- unique(ISO_region)
colnames(ISO_region) <- c("Region","r")

new_rows = data.frame(
  'Region'= c('Asia','Africa','Asia','Asia','Africa','Europe','Africa','Asia','Asia','Europe','Asia','Africa','Europe','ROW','Africa'),
  'r'= c('ARE','BEN','BHR','BRN','CIV','FIN','GIN','JOR','KWT','MLT','SGP','TGO','XEF','XTW','ZMB')
)
ISO_region <- rbind(ISO_region, new_rows)
ISO_region <- unique(ISO_region)
ISO_region <- ISO_region[!(ISO_region$Region == 'Americas' & ISO_region$r == "FRA"), ]
ISO_region <- ISO_region[!(ISO_region$Region == 'Africa' & ISO_region$r == "ESP"), ]
ISO_region <- ISO_region[!(ISO_region$Region == 'Africa' & ISO_region$r == "FRA"), ]
ISO_region <- ISO_region[!(ISO_region$Region == 'Americas' & ISO_region$r == "NLD"), ]
# 141 gtap regions and region 
colnames(ISO_region) <- c("Region","ISO_new")
# matching process of gtap-region-emdat
ISO_region_match <- merge(ISO_region, align_rgn , by = "ISO_new", all.x = TRUE)

far_loss_unified <- left_join(far_loss, align_rgn, by = "ISO")
far_loss_aggr <- far_loss_unified[,3:6] %>%  
  group_by(ISO_new) %>%  
  summarise(drought_far = sum(drought_far), flood_far = sum(flood_far), 
            storm_far = sum(storm_far))


#----- Distribute the losses into affected sectors

loss_agriculture <- far_loss_aggr
loss_agriculture$drought_far <- loss_agriculture$drought_far * 0.84
loss_agriculture$flood_far <- loss_agriculture$flood_far * 0.15
loss_agriculture$storm_far <- loss_agriculture$storm_far * 0.18
colnames(loss_agriculture)[2:4] <- c("drought_value", "flood_value", "storm_value")

loss_non_agriculture <- far_loss_aggr
loss_non_agriculture$drought_far <- loss_non_agriculture$drought_far * 0.16
loss_non_agriculture$flood_far <- loss_non_agriculture$flood_far * 0.85
loss_non_agriculture$storm_far <- loss_non_agriculture$storm_far * 0.82
colnames(loss_non_agriculture)[2:4] <- c("drought_value", "flood_value", "storm_value")

# 干旱:农业、制造业、交通运输、电力、水利设施
# 洪涝:农业、采矿业、制造业、建筑业、交通运输、电力、水利设施
# 风暴:农业、制造业、建筑业、交通运输、电力、水利设施
# drought: agriculture, manufacturing, transportation, electricity, hydraulic
#   flood: agriculture, manufacturing, transportation, electricity, hydraulic, mining, architecture
#   storm: agriculture, manufacturing, transportation, electricity, hydraulic, architecture
# align GTAP sectors to the sectors described above

manu <- c("TWL", "EEQ", "ROIL", "CHM", "CRP", "NMM",
          "I_S", "NFM", "TEQ")
tran <- c("TRAN", "WHS")
elec <- c("ELEC")
hydr <- c("WTR")
mine <- c("COAL", "OIL", "GAS", "MINE")
arch <- c("CNS")

sectors_drought_nonagr <- c(manu, tran, elec, hydr)
sectors_flood_nonagr <- c(manu, tran, elec, hydr, mine, arch)
sectors_storm_nonagr <- c(manu, tran, elec, hydr, arch)

cap_stock <- read_excel("/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/wwh/SAvalue_added.xlsx")

# filter the sectors AFFECTED and establish a table of AFFECTED capital stock
# for each type of events, and calculate sectoral proportions.

GTAP_rgn <- names(cap_stock[,2:142]) 

# drought

cap_stock_drought <- cap_stock

# filter affected non-agricultural sectors
cap_stock_drought <- cap_stock_drought %>%  
  filter(sectors %in% sectors_drought_nonagr)

# sum up the total affected capitals of these sectors and calculate their 
# proportional values
cap_sum_drought <- as.numeric(colSums(cap_stock_drought[,2:142]))

cap_stock_drought_prop <- cap_stock_drought
for (i in 1:141) {
  cap_stock_drought_prop[,i + 1] <- cap_stock_drought_prop[,i + 1] / cap_sum_drought[i]
}

# multiply the proportions by total non-agricultural loss given above for
# countries that both exists in GTAP countries and EMDAT countries

cap_stock_drought_loss <- cap_stock_drought_prop
for (i in 1:141) {  
  match_pos <- match(GTAP_rgn[i], loss_non_agriculture$ISO_new)  
  
  # make sure a value can be found in loss_non_agriculture to avoid 
  # halfway termination
  
  if (!is.na(match_pos)) {  
    cap_stock_drought_loss[,i + 1] <- cap_stock_drought_loss[,i + 1] *   
      loss_non_agriculture$drought_value[match_pos]  
  } else {  
    cap_stock_drought_loss[,i + 1] <- cap_stock_drought_loss[,i + 1] * 0  
  }  
}

# flood

cap_stock_flood <- cap_stock

cap_stock_flood <- cap_stock_flood %>%  
  filter(sectors %in% sectors_flood_nonagr)

cap_sum_flood <- as.numeric(colSums(cap_stock_flood[,2:142]))

cap_stock_flood_prop <- cap_stock_flood
for (i in 1:141) {
  cap_stock_flood_prop[,i + 1] <- cap_stock_flood_prop[,i + 1] / cap_sum_flood[i]
}

cap_stock_flood_loss <- cap_stock_flood_prop
for (i in 1:141) {  
  match_pos <- match(GTAP_rgn[i], loss_non_agriculture$ISO_new)  
  
  if (!is.na(match_pos)) {  
    cap_stock_flood_loss[,i + 1] <- cap_stock_flood_loss[,i + 1] *   
      loss_non_agriculture$flood_value[match_pos]  
  } else {  
    cap_stock_flood_loss[,i + 1] <- cap_stock_flood_loss[,i + 1] * 0  
  }  
}

# storm

cap_stock_storm <- cap_stock

cap_stock_storm <- cap_stock_storm %>%  
  filter(sectors %in% sectors_storm_nonagr)

cap_sum_storm <- as.numeric(colSums(cap_stock_storm[,2:142]))

cap_stock_storm_prop <- cap_stock_storm
for (i in 1:141) {
  cap_stock_storm_prop[,i + 1] <- cap_stock_storm_prop[,i + 1] / cap_sum_storm[i]
}

cap_stock_storm_loss <- cap_stock_storm_prop
for (i in 1:141) {  
  match_pos <- match(GTAP_rgn[i], loss_non_agriculture$ISO_new)  
  
  if (!is.na(match_pos)) {  
    cap_stock_storm_loss[,i + 1] <- cap_stock_storm_loss[,i + 1] *   
      loss_non_agriculture$storm_value[match_pos]  
  } else {  
    cap_stock_storm_loss[,i + 1] <- cap_stock_storm_loss[,i + 1] * 0  
  }  
}

# add agriculture loss back
GTAP_rgn <- as.data.frame(colnames(cap_stock_drought_loss)[2:142])
colnames(GTAP_rgn) <- "ISO_new"

# create an individual row and add it at the bottom of non-agri loss table
loss_agr_drought <- left_join(GTAP_rgn, loss_agriculture[,c(1,2)], by = "ISO_new")
row_agr_drought <- c("AGR", t(loss_agr_drought[,2]))
cap_stock_drought_loss_full <- rbind(cap_stock_drought_loss, row_agr_drought)

# verification
as.numeric(cap_stock_drought_loss_full[nrow(cap_stock_drought_loss_full), 2]) / 
  sum(as.numeric(as.matrix(cap_stock_drought_loss_full[,2])))

# apply the same for flood and storm
loss_agr_flood <- left_join(GTAP_rgn, loss_agriculture[,c(1,3)], by = "ISO_new")
row_agr_flood <- c("AGR", t(loss_agr_flood[,2]))
cap_stock_flood_loss_full <- rbind(cap_stock_flood_loss, row_agr_flood)

loss_agr_storm <- left_join(GTAP_rgn, loss_agriculture[,c(1,4)], by = "ISO_new")
row_agr_storm <- c("AGR", t(loss_agr_storm[,2]))
cap_stock_storm_loss_full <- rbind(cap_stock_storm_loss, row_agr_storm)

# verification
as.numeric(cap_stock_flood_loss_full[nrow(cap_stock_flood_loss_full), 2]) / 
  sum(as.numeric(as.matrix(cap_stock_flood_loss_full[,2])))

as.numeric(cap_stock_storm_loss_full[nrow(cap_stock_storm_loss_full), 2]) / 
  sum(as.numeric(as.matrix(cap_stock_storm_loss_full[,2])))

save(cap_stock_drought_loss_full,cap_stock_flood_loss_full, cap_stock_storm_loss_full, file = "/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/direct_loss_SA.RData")
save(GTAP_rgn,ISO_region,ISO_region_match, file = "/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/region_match_SA.RData")

# calculating total cost

direct_loss_unified <- left_join(align_rgn, disaster_loss, by = "ISO")
direct_loss_aggr <- direct_loss_unified[,2:5] %>%  
  group_by(ISO_new) %>%  
  summarise(drought = sum(drought), flood = sum(flood), 
            storm = sum(storm))


#----- Distribute the losses into affected sectors

loss_agriculture <- direct_loss_aggr
loss_agriculture$drought <- loss_agriculture$drought * 0.84
loss_agriculture$flood <- loss_agriculture$flood * 0.15
loss_agriculture$storm <- loss_agriculture$storm * 0.18
colnames(loss_agriculture)[2:4] <- c("drought_value", "flood_value", "storm_value")

loss_non_agriculture <- direct_loss_aggr
loss_non_agriculture$drought <- loss_non_agriculture$drought * 0.16
loss_non_agriculture$flood <- loss_non_agriculture$flood * 0.85
loss_non_agriculture$storm <- loss_non_agriculture$storm * 0.82
colnames(loss_non_agriculture)[2:4] <- c("drought_value", "flood_value", "storm_value")

# drought

# multiply the proportions by total non-agricultural loss given above for
# countries that both exists in GTAP countries and EMDAT countries
GTAP_rgn <- names(cap_stock[,2:142]) 

direct_drought_loss <- cap_stock_drought_prop
for (i in 1:141) {  
  match_pos <- match(GTAP_rgn[i], loss_non_agriculture$ISO_new)  
  
  # make sure a value can be found in loss_non_agriculture to avoid 
  # halfway termination
  
  if (!is.na(match_pos)) {  
    direct_drought_loss[,i + 1] <- direct_drought_loss[,i + 1] *   
      loss_non_agriculture$drought_value[match_pos]  
  } else {  
    direct_drought_loss[,i + 1] <- direct_drought_loss[,i + 1] * 0  
  }  
}

# flood

direct_flood_loss <- cap_stock_flood_prop
for (i in 1:141) {  
  match_pos <- match(GTAP_rgn[i], loss_non_agriculture$ISO_new)  
  
  if (!is.na(match_pos)) {  
    direct_flood_loss[,i + 1] <- direct_flood_loss[,i + 1] *   
      loss_non_agriculture$flood_value[match_pos]  
  } else {  
    direct_flood_loss[,i + 1] <- direct_flood_loss[,i + 1] * 0  
  }  
}

# storm

direct_storm_loss <- cap_stock_storm_prop
for (i in 1:141) {  
  match_pos <- match(GTAP_rgn[i], loss_non_agriculture$ISO_new)  
  
  if (!is.na(match_pos)) {  
    direct_storm_loss[,i + 1] <- direct_storm_loss[,i + 1] *   
      loss_non_agriculture$storm_value[match_pos]  
  } else {  
    direct_storm_loss[,i + 1] <- direct_storm_loss[,i + 1] * 0  
  }  
}

# add agriculture loss back
GTAP_rgn <- as.data.frame(colnames(cap_stock_drought_loss)[2:142])
colnames(GTAP_rgn) <- "ISO_new"

# create an individual row and add it at the bottom of non-agri loss table
direct_loss_agr_drought <- left_join(GTAP_rgn, loss_agriculture[,c(1,2)], by = "ISO_new")
direct_loss_agr_drought <- c("AGR", t(direct_loss_agr_drought[,2]))
direct_drought_loss_full <- rbind(direct_drought_loss, direct_loss_agr_drought)

# apply the same for flood and storm
direct_loss_agr_flood <- left_join(GTAP_rgn, loss_agriculture[,c(1,3)], by = "ISO_new")
direct_loss_agr_flood <- c("AGR", t(direct_loss_agr_flood[,2]))
direct_flood_loss_full <- rbind(direct_flood_loss, direct_loss_agr_flood)

direct_loss_agr_storm <- left_join(GTAP_rgn, loss_agriculture[,c(1,4)], by = "ISO_new")
direct_loss_agr_storm <- c("AGR", t(direct_loss_agr_storm[,2]))
direct_storm_loss_full <- rbind(direct_storm_loss, direct_loss_agr_storm)


# verification
as.numeric(direct_drought_loss_full[nrow(direct_drought_loss_full), 2]) / 
  sum(as.numeric(as.matrix(direct_drought_loss_full[,2])))

as.numeric(direct_flood_loss_full[nrow(direct_flood_loss_full), 2]) / 
  sum(as.numeric(as.matrix(direct_flood_loss_full[,2])))

as.numeric(direct_storm_loss_full[nrow(direct_storm_loss_full), 2]) / 
  sum(as.numeric(as.matrix(direct_storm_loss_full[,2])))

save(cap_stock_drought_loss_full,cap_stock_flood_loss_full, cap_stock_storm_loss_full, file = "/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/direct_loss_far_SA.RData")
save(direct_drought_loss_full,direct_flood_loss_full, direct_storm_loss_full, file = "/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/direct_loss_SA.RData")
save(GTAP_rgn,ISO_region,ISO_region_match, file = "/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/region_match_SA.RData")


load("/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/IO_result.RData")

library(Matrix)
library(dplyr)

#中间流量
A0 <- as.matrix(Z_final)

#总产出向量
X0 <- Xr_filtered$Xr

#直接消耗系数矩阵A
A <- A0 / rep(X0, each = nrow(A0))

#单位矩阵
I <- diag(4653)

#I-A的逆 
L <- solve(I-A)

#最终需求汇总到一列
Y0 <- rowSums(Y_final)

# check0 = A %*% X0 + Y0 - X0
# check0 = L %*% Y0 - X0


L_df <- as.data.frame(L)
label <- as.data.frame(label)
L_df <- cbind(label$r,label$i, L_df)
names(L_df)[1] <- "r"
names(L_df)[2] <- "i"

L_agr <- L_df[L_df$i == "AGR", ]


# attributed indirect loss

drought_direct_loss_far <- pivot_longer(cap_stock_drought_loss_full, 
                                        cols = -sectors,
                                        names_to = "r", 
                                        values_to = "drought")
names(drought_direct_loss_far)[1] <- "i"

flood_direct_loss_far <- pivot_longer(cap_stock_flood_loss_full, 
                                      cols = -sectors,
                                      names_to = "r", 
                                      values_to = "flood")
names(flood_direct_loss_far)[1] <- "i"

storm_direct_loss_far <- pivot_longer(cap_stock_storm_loss_full, 
                                      cols = -sectors,
                                      names_to = "r", 
                                      values_to = "storm")
names(storm_direct_loss_far)[1] <- "i"

# sumup and join -- direct loss

direct_loss_far <- label

direct_loss_far <- left_join(direct_loss_far, drought_direct_loss_far, by = c("r", "i"))
direct_loss_far <- left_join(direct_loss_far, flood_direct_loss_far, by = c("r", "i"))
direct_loss_far <- left_join(direct_loss_far, storm_direct_loss_far, by = c("r", "i"))
direct_loss_far[is.na(direct_loss_far)] <- 0

direct_loss_far$drought <- as.numeric(as.character(direct_loss_far$drought))
direct_loss_far$flood <- as.numeric(as.character(direct_loss_far$flood))
direct_loss_far$storm <- as.numeric(as.character(direct_loss_far$storm))

# divide direct loss: agriculture loss: x and other sectors' loss: y

x_loss_agr_far <- direct_loss_far[direct_loss_far$i == "AGR", ]
y_loss_oth_far <- direct_loss_far[direct_loss_far$i != "AGR", ]

L_agr_v <- L_agr %>% select(-c("i","r"))
L_agr_v_t = as.data.frame(t(L_agr_v))
L_agr_t <- cbind(label, L_agr_v_t)

L_agr_t_oth <- L_agr_t[L_agr_t$i != "AGR", ]
L_agr_t_oth_v <- L_agr_t_oth %>% select(-c("i","r"))
L_agr_t_oth_v_t = t(L_agr_t_oth_v)

L_agr_t_agr <- L_agr_t[L_agr_t$i == "AGR", ]
L_agr_t_agr_v <- L_agr_t_agr %>% select(-c("i","r"))
L_agr_t_agr_v_t = t(L_agr_t_agr_v)
agragr_inverse <- solve(L_agr_t_agr_v_t)

# drought

result <- L_agr_t_oth_v_t %*% y_loss_oth_far$drought
delta_x <- x_loss_agr_far$drought - result
y_loss_agr_drought_far <- as.data.frame(agragr_inverse %*% delta_x)

# flood

result <- L_agr_t_oth_v_t %*% y_loss_oth_far$flood
delta_x <- x_loss_agr_far$flood - result
y_loss_agr_flood_far <- as.data.frame(agragr_inverse %*% delta_x)

# storm

result <- L_agr_t_oth_v_t %*% y_loss_oth_far$storm
delta_x <- x_loss_agr_far$storm - result
y_loss_agr_storm_far <- as.data.frame(agragr_inverse %*% delta_x)

y_loss_agr_far <-cbind(x_loss_agr_far$r,x_loss_agr_far$i,y_loss_agr_drought_far,y_loss_agr_flood_far,y_loss_agr_storm_far)
colnames(y_loss_agr_far) <- c("r","i","drought","flood","storm")

y_loss_all_far <- rbind(y_loss_agr_far,y_loss_oth_far)

y_loss_far <- left_join(label, y_loss_all_far, by = c("r", "i"))

x_loss_drought_far <- L %*% y_loss_far$drought
x_loss_flood_far <- L %*% y_loss_far$flood
x_loss_storm_far <- L %*% y_loss_far$storm

x_loss_far <- cbind(label, x_loss_drought_far, x_loss_flood_far, x_loss_storm_far)


###### total indirect loss

drought_direct_loss <- pivot_longer(direct_drought_loss_full, 
                                    cols = -sectors,
                                    names_to = "r", 
                                    values_to = "drought")
names(drought_direct_loss)[1] <- "i"

flood_direct_loss <- pivot_longer(direct_flood_loss_full, 
                                  cols = -sectors,
                                  names_to = "r", 
                                  values_to = "flood")
names(flood_direct_loss)[1] <- "i"

storm_direct_loss <- pivot_longer(direct_storm_loss_full, 
                                  cols = -sectors,
                                  names_to = "r", 
                                  values_to = "storm")
names(storm_direct_loss)[1] <- "i"

# sumup and join -- direct loss

direct_loss <- label

direct_loss <- left_join(direct_loss, drought_direct_loss, by = c("r", "i"))
direct_loss <- left_join(direct_loss, flood_direct_loss, by = c("r", "i"))
direct_loss <- left_join(direct_loss, storm_direct_loss, by = c("r", "i"))
direct_loss[is.na(direct_loss)] <- 0

direct_loss$drought <- as.numeric(as.character(direct_loss$drought))
direct_loss$flood <- as.numeric(as.character(direct_loss$flood))
direct_loss$storm <- as.numeric(as.character(direct_loss$storm))

# divide direct loss: agriculture loss: x and other sectors' loss: y

x_loss_agr <- direct_loss[direct_loss$i == "AGR", ]
y_loss_oth <- direct_loss[direct_loss$i != "AGR", ]

# drought

result <- L_agr_t_oth_v_t %*% y_loss_oth$drought
delta_x <- x_loss_agr$drought - result
y_loss_agr_drought <- as.data.frame(agragr_inverse %*% delta_x)

# flood

result <- L_agr_t_oth_v_t %*% y_loss_oth$flood
delta_x <- x_loss_agr$flood - result
y_loss_agr_flood <- as.data.frame(agragr_inverse %*% delta_x)

# storm

result <- L_agr_t_oth_v_t %*% y_loss_oth$storm
delta_x <- x_loss_agr$storm - result
y_loss_agr_storm <- as.data.frame(agragr_inverse %*% delta_x)

y_loss_agr <-cbind(x_loss_agr$r,x_loss_agr$i,y_loss_agr_drought,y_loss_agr_flood,y_loss_agr_storm)
colnames(y_loss_agr) <- c("r","i","drought","flood","storm")

y_loss_all <- rbind(y_loss_agr,y_loss_oth)

y_loss <- left_join(label, y_loss_all, by = c("r", "i"))

x_loss_drought <- L %*% y_loss$drought
x_loss_flood <- L %*% y_loss$flood
x_loss_storm <- L %*% y_loss$storm

x_loss <- cbind(label, x_loss_drought, x_loss_flood, x_loss_storm)
save(direct_loss,direct_loss_far, x_loss,x_loss_far, file = "/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/loss_SA.RData")


