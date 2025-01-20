library("readxl")
library("tidyr")
library("dplyr")
library(stringr)
library(purrr)
library(openxlsx)
#---------1. read IO elements from gtap10-remap results

# vdfm: intermediates - firms' domestic purchases at market prices

vdfm <- read_excel("/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/IO-elements.xlsx", sheet = "vdfm")
vdfm <- vdfm %>% fill("...1", .direction = "down")
vdfm[is.na(vdfm)] <- 0

# vifm: intermediates - firms' imports at market prices

vifm <- read_excel("/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/IO-elements.xlsx", sheet = "vifm")
vifm <- vifm %>% fill("...1", .direction = "down")
vifm[is.na(vifm)] <- 0

# vom: aggretate output

vom <- read_excel("/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/IO-elements.xlsx", sheet = "vom")
vom[is.na(vom)] <- 0

# vst: trade - exports for international transportation

vst <- read_excel("/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/IO-elements.xlsx", sheet = "vst")
vst <- vst %>% fill("...1", .direction = "down")
vst[is.na(vst)] <- 0

# vtwr: trade - exports for international transportation

vtwr <- read_excel("/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/IO-elements.xlsx", sheet = "vtwr")
vtwr <- vtwr %>% fill("...1", .direction = "down")
vtwr <- vtwr %>% fill("...2", .direction = "down")
vtwr[is.na(vtwr)] <- 0

# vxmd: trade - bilateral exports at market prices

vxmd <- read_excel("/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/IO-elements.xlsx", sheet = "vxmd")
vxmd <- vxmd %>% fill("...1", .direction = "down")
vxmd[is.na(vxmd)] <- 0

# r: list of regions

r <- read_excel("/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/IO-elements.xlsx", sheet = "r")

# i: list of sectors

i <- read_excel("/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/IO-elements.xlsx", sheet = "i")


#---------2. processing: domestic

# X total output

Xr <- pivot_longer(vom,cols = -c("...1"),names_to = "r",values_to = "Xr")
# reorder with given region order
Xr$r <- factor(Xr$r, levels = r$r)
Xr <- Xr[order(Xr$r), ]

colnames(Xr) <- c("i","r","Xr")

column_order <- names(Xr)
index_B <- which(column_order == "i")
index_C <- which(column_order == "r")
column_order[c(index_B, index_C)] <- column_order[c(index_C, index_B)]
Xr <- Xr[, column_order]


Xr_filtered <- Xr[Xr$i != "c", ]
Xr_filtered <- Xr_filtered[Xr_filtered$i != "g", ]
Xr_filtered <- Xr_filtered[Xr_filtered$i != "i", ]

# Zrr and yrr without internal trade adjustment

Zrr_1 <- vdfm %>% select(-c("c", "g","i"))
names(Zrr_1)[1] <- "r"
names(Zrr_1)[2] <- "i"

yrr_1 <- vdfm %>% select(c("...1","...2","c", "g","i"))
names(yrr_1)[1] <- "r"
names(yrr_1)[2] <- "i"

# Zm vifm Import firm purchase of i by j in region s

Zm <- vifm %>% select(-c("c", "g","i"))
names(Zm)[1] <- "s"
names(Zm)[2] <- "i"

# Ym vifm Import firm purchase of i by j in region s

Ym <- vifm %>% select(c("...1","...2","c", "g","i"))
names(Ym)[1] <- "s"
names(Ym)[2] <- "i"
names(Ym)[5] <- "inv"

# ers vxmd Export of i from region r to region s (valued in market prices of r)

ers <- vxmd 
names(ers)[1] <- "r"
names(ers)[2] <- "i"

# vim total import of i into region s

vifm_v <- vifm %>% select(-c("...1","...2"))

vim <- rowSums(vifm_v)

vim <- cbind(vifm[, 1:2], vim = vim)
names(vim)[1] <- "s"
names(vim)[2] <- "i"

# vtw Total global supply and use of transport margin k

vtw <- sum(vst$value)

names(vtwr)[1] <- "tran"
names(vtwr)[2] <- "s"
names(vtwr)[3] <- "r"

# vtwr_s vtwr sum r

vtwr_s <- vtwr %>%
  group_by_at("s") %>%
  summarise(across(.cols = -c(1,2), .fns = sum, na.rm = TRUE))


# first calculate the IO parts with transportation sector exogenous as Y part

# Zrs

# Zm_stacked

Zm_wide <- pivot_wider(Zm, 
                       names_from = s, 
                       values_from = -c(1,2),
                       names_sep = "_")

group_ids <- unique(stringr::str_extract(names(Zm_wide)[-1], "(?<=_)\\w+$"))
ordered_names <- unlist(map(group_ids, function(group) {
  cols <- names(Zm_wide)[stringr::str_detect(names(Zm_wide), paste0("_", group, "$"))]
}))
Zm_wide_ordered <- Zm_wide %>%
  select(i, all_of(ordered_names))

Zm_stacked <- do.call(rbind, replicate(141, Zm_wide_ordered, simplify = FALSE))

# vim_stacked

vim_wide <- pivot_wider(vim, 
                       names_from = s, 
                       values_from = vim)
vim_stacked <- do.call(rbind, replicate(141, vim_wide, simplify = FALSE))

# Zrs

vim_stacked_v <- vim_stacked %>% select(-c("i"))
vim_stacked_v_m <- as.matrix(vim_stacked_v)

ers_v <- ers %>% select(-c("r","i"))
ers_v_m <- as.matrix(ers_v)

weight <- ers_v_m / vim_stacked_v_m
weight_df <- as.data.frame(weight) 

Zm_stacked_v <- Zm_stacked %>% select(-c("i"))

num_parts <- ncol(weight_df)
cols_per_part <- ncol(Zm_stacked_v) / num_parts

results <- lapply(1:num_parts, function(i) {
  part_A <- Zm_stacked_v[, ((i - 1) * cols_per_part + 1):(i * cols_per_part)]
  part_A * weight_df[[i]]
})

results_list <- setNames(results, paste("Result", 1:num_parts, sep = "_"))
Zrs <- bind_cols(results_list)


# Yrs

# Ym_stacked

Ym_wide <- pivot_wider(Ym, 
                       names_from = s, 
                       values_from = -c(1,2),
                       names_sep = "_")

group_ids <- unique(stringr::str_extract(names(Ym_wide)[-1], "(?<=_)\\w+$"))
ordered_names <- unlist(map(group_ids, function(group) {
  cols <- names(Ym_wide)[stringr::str_detect(names(Ym_wide), paste0("_", group, "$"))]
}))
Ym_wide_ordered <- Ym_wide %>%
  select(i, all_of(ordered_names))

Ym_stacked <- do.call(rbind, replicate(141, Ym_wide_ordered, simplify = FALSE))

Ym_stacked_v <- Ym_stacked %>% select(-c("i"))

# Yrs
num_parts <- ncol(weight_df)
cols_per_part <- ncol(Ym_stacked_v) / num_parts

results <- lapply(1:num_parts, function(i) {
  part_A <- Ym_stacked_v[, ((i - 1) * cols_per_part + 1):(i * cols_per_part)]
  part_A * weight_df[[i]]
})

results_list <- setNames(results, paste("Result", 1:num_parts, sep = "_"))
Yrs <- bind_cols(results_list)

# calculate the final Z and Y

# first, change Zrr_1 to a big matrix

Zrr_1_v <- Zrr_1 %>% select(-c("r","i"))

Zrr_1_v_m <- as.matrix(Zrr_1_v)

n <- 33
m <- nrow(Zrr_1_v_m) / n

result_Zrr <- matrix(0, nrow = n * m, ncol = n * m)

for (i in 1:m) {
  start_index <- (i - 1) * n + 1
  end_index <- i * n
  result_Zrr[(start_index:end_index), (start_index:end_index)] <- Zrr_1_v_m[(start_index:end_index), ]
}

# calculate final Z matrix

Zrs_m <- as.matrix(Zrs)

Z_m <- Zrs_m + result_Zrr

Z <- as.data.frame(Z_m) 


# second, change yrr_1 to a big matrix

yrr_1_v <- yrr_1 %>% select(-c("r","i"))

yrr_1_v_m <- as.matrix(yrr_1_v)

a <- 33
b <- 3
c <- nrow(yrr_1_v_m) / a

result_Yrr <- matrix(0, nrow = a * c, ncol = b * c)

for (i in 1:c) {
  row_start <- (i - 1) * a + 1
  row_end <- i * a
  col_start <- (i - 1) * b + 1
  col_end <- i * b
  result_Yrr[(row_start:row_end), (col_start:col_end)] <- yrr_1_v_m[(row_start:row_end), ]
}

# calculate final Y matrix

Yrs_m <- as.matrix(Yrs)

Y_m <- Yrs_m + result_Yrr

Y <- as.data.frame(Y_m) 


check <- as.data.frame(rowSums(Y)+rowSums(Z))
check['Xr'] <- Xr_filtered$Xr
check['check'] <- check['Xr'] - check[1]


# 2- calculate the IO parts with transportation sector endogenous

# tran-Z

vtwr_s$s <- factor(vtwr_s$s, levels = group_ids)

vtwr_s <- vtwr_s[order(vtwr_s$s), ]

vtwr_s_t <- as.data.frame(t(vtwr_s))

colnames(vtwr_s_t) <- as.character(vtwr_s_t[1, ])

vtwr_s_t <- vtwr_s_t[-1, ]

vtwr_new <- data.frame(RowNames = rownames(vtwr_s_t), vtwr_s_t, row.names = NULL)

vtwr_all <- data.frame(vim_wide$i)

names(vtwr_all)[1] <- "i"
names(vtwr_new)[1] <- "i"
  
vtwr_all <- left_join(vtwr_all, vtwr_new, by = "i")
vtwr_all[is.na(vtwr_all)] <- 0

vtwr_all_v <- vtwr_all %>% select(-c("i"))

vtwr_all_v[] <- lapply(vtwr_all_v, as.numeric)


vtwr_all_v_m <- as.matrix(vtwr_all_v)

vim_wide_v <- vim_wide %>% select(-c("i"))

vim_wide_v_m <- as.matrix(vim_wide_v)

weight_tran <- vtwr_all_v_m / vim_wide_v_m

weight_tran_df <- as.data.frame(weight_tran) 

Zm_wide_ordered_v <- Zm_wide_ordered %>% select(-c("i"))

num_parts <- ncol(weight_tran_df)
cols_per_part <- ncol(Zm_wide_ordered_v) / num_parts

results <- lapply(1:num_parts, function(i) {
  part_A <- Zm_wide_ordered_v[, ((i - 1) * cols_per_part + 1):(i * cols_per_part)]
  part_A * weight_tran_df[[i]]
})

results_list <- setNames(results, paste("Result", 1:num_parts, sep = "_"))
Us <- bind_cols(results_list)

column_sums <- colSums(Us)

Us <- rbind(Us, column_sums)

rownames(Us)[nrow(Us)] <- "Sum"

Us_sum <- Us[34, , drop = FALSE]

Us_sum_stacked <- do.call(rbind, replicate(141, Us_sum, simplify = FALSE))

weight_pool <- vst$value/vtw

Trs <- Us_sum_stacked * weight_pool

#check: colSums(Trs)

# tran-Y

Ym_wide_ordered_v <- Ym_wide_ordered %>% select(-c("i"))

num_parts <- ncol(weight_tran_df)
cols_per_part <- ncol(Ym_wide_ordered_v) / num_parts

results <- lapply(1:num_parts, function(i) {
  part_A <- Ym_wide_ordered_v[, ((i - 1) * cols_per_part + 1):(i * cols_per_part)]
  part_A * weight_tran_df[[i]]
})

results_list <- setNames(results, paste("Result", 1:num_parts, sep = "_"))

Us_y <- bind_cols(results_list)

Us_y <- rbind(Us_y, colSums(Us_y))

rownames(Us_y)[nrow(Us_y)] <- "Sum"

Us_y_sum <- Us_y[34, , drop = FALSE]

Us_y_sum_stacked <- do.call(rbind, replicate(141, Us_y_sum, simplify = FALSE))

Trs_y <- Us_y_sum_stacked * weight_pool

# final IO

# Z with trans in

Z_label <- cbind(vdfm[, 1:2], Z)

new_Z <- Z_label[Z_label$...2 == "TRAN", ]

new_Z_v <- new_Z %>% select(-c("...1","...2"))

new_Z_tran_v <- new_Z_v + Trs

new_Z_tran_v <- cbind("TRAN", new_Z_tran_v)

new_Z_tran_v <- cbind(new_Z[, 1], new_Z_tran_v)

Z_label[Z_label$...2 == "TRAN", ] <- new_Z_tran_v

# Y with trans in

Y_label <- cbind(vdfm[, 1:2], Y)

new_Y <- Y_label[Y_label$...2 == "TRAN", ]

new_Y_v <- new_Y %>% select(-c("...1","...2"))

new_Y_tran_v <- new_Y_v + Trs_y

new_Y_tran_v <- cbind("TRAN", new_Y_tran_v)

new_Y_tran_v <- cbind(new_Y[, 1], new_Y_tran_v)

Y_label[Y_label$...2 == "TRAN", ] <- new_Y_tran_v

Z_final <- Z_label %>% select(-c("...1","...2"))
Y_final <- Y_label %>% select(-c("...1","...2"))

# Z_final and Y_final are final IO elements
label <- cbind(vifm$...1,vifm$...2)
colnames(label) <- c("r","i")
#check <- as.data.frame(rowSums(Y_final)+rowSums(Z_final))
#check['Xr'] <- Xr_filtered$Xr
#check['check'] <- check['Xr'] - check[1]
save(Z_final, Y_final,Xr_filtered,label, file = "/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/IO_result.RData")
