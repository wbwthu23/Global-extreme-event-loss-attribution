library("tidyr")
library("readxl")
library("dplyr")
library("ggplot2")

# read EMDAT database

EMDAT <- read_excel("/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/EM_DAT_clean.xlsx")

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

cap_stock <- read_excel("/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/wwh/cap_stock.xlsx")

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

save(cap_stock_drought_loss_full,cap_stock_flood_loss_full, cap_stock_storm_loss_full, file = "/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/direct_loss.RData")
save(GTAP_rgn,ISO_region,ISO_region_match, file = "/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/region_match.RData")

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

save(cap_stock_drought_loss_full,cap_stock_flood_loss_full, cap_stock_storm_loss_full, file = "/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/direct_loss_far.RData")
save(direct_drought_loss_full,direct_flood_loss_full, direct_storm_loss_full, file = "/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/direct_loss.RData")
save(GTAP_rgn,ISO_region,ISO_region_match, file = "/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/region_match.RData")

