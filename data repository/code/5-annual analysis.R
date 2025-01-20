library("tidyr")
library("readxl")
library("dplyr")
library("ggplot2")

EMDAT <- read_excel("/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/EM_DAT_处理.xlsx")

# filter different disasters

drought <- EMDAT %>% filter(`Disaster Type` == "Drought")
flood <- EMDAT %>% filter(`Disaster Type` == "Flood")
storm <- EMDAT %>% filter(`Disaster Type` == "Storm")

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


merged_loss <- full_join(drought_loss, flood_loss, by = c("ISO", "Start Year")) %>%
  full_join(., storm_loss, by = c("ISO", "Start Year"))
colnames(merged_loss) <- c("Year","ISO","drought", "flood","storm")
merged_loss <- merged_loss %>%
  filter(Year >= 2009 & Year <= 2019)
merged_loss[is.na(merged_loss)] <- 0
EMDAT_selected <- EMDAT[, c("ISO", "Region")]
EMDAT_selected <- unique(EMDAT_selected)
merged_loss <- merge(merged_loss, EMDAT_selected , by = "ISO", all.x = TRUE)



#-----FAR (fraction of attributed risk) Source: Newman 2023

FAR <- read_excel("/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/FAR.xlsx")
colnames(FAR) <- c("Region","d_far","f_far", "s_far","h_far","w_far","c_far")
loss_FAR <- merge(merged_loss, FAR , by = "Region", all.x = TRUE)

#------calculate attributed direct economic cost 

loss_FAR$drought_far <- loss_FAR$drought * loss_FAR$d_far
loss_FAR$flood_far <- loss_FAR$flood * loss_FAR$f_far
loss_FAR$storm_far <- loss_FAR$storm * loss_FAR$s_far
far_loss <- loss_FAR[, c("Region","ISO","Year","drought_far","flood_far","storm_far")]

#----- Unify EMDAT regions as GTAP regions

EMDAT_rgn <- EMDAT[, c("ISO", "Region")]
EMDAT_rgn <- unique(EMDAT_rgn)

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
far_loss_aggr <- far_loss_unified %>%  
  group_by(ISO_new, Year) %>%  
  summarise(
    drought_far = sum(drought_far, na.rm = TRUE), 
    flood_far = sum(flood_far, na.rm = TRUE), 
    storm_far = sum(storm_far, na.rm = TRUE),
    .groups = "drop"
  )

#----- Distribute the losses into affected sectors

loss_agriculture <- far_loss_aggr
loss_agriculture$drought_far <- loss_agriculture$drought_far * 0.84
loss_agriculture$flood_far <- loss_agriculture$flood_far * 0.15
loss_agriculture$storm_far <- loss_agriculture$storm_far * 0.18
colnames(loss_agriculture)[3:5] <- c("drought_value", "flood_value", "storm_value")

loss_non_agriculture <- far_loss_aggr
loss_non_agriculture$drought_far <- loss_non_agriculture$drought_far * 0.16
loss_non_agriculture$flood_far <- loss_non_agriculture$flood_far * 0.85
loss_non_agriculture$storm_far <- loss_non_agriculture$storm_far * 0.82
colnames(loss_non_agriculture)[3:5] <- c("drought_value", "flood_value", "storm_value")

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

GTAP_rgn <- names(cap_stock[,2:142]) 


#拆分到每年
yearly_tables <- split(far_loss_aggr, far_loss_aggr$Year)
yearly_tables <- lapply(yearly_tables, function(df) {
  df %>%
    select(ISO_new, drought_far, flood_far, storm_far)
})
for (year in names(yearly_tables)) {
  assign(paste0("Loss_", year), yearly_tables[[year]])
}



# drought

cap_stock_drought <- cap_stock
cap_stock_drought <- cap_stock_drought %>%  
  filter(sectors %in% sectors_drought_nonagr)
cap_sum_drought <- as.numeric(colSums(cap_stock_drought[,2:142]))

cap_stock_drought_prop <- cap_stock_drought
for (i in 1:141) {
  cap_stock_drought_prop[,i + 1] <- cap_stock_drought_prop[,i + 1] / cap_sum_drought[i]
}
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

