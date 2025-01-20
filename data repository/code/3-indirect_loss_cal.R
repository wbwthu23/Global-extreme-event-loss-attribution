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

# read direct loss
load("/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/direct_loss_far.RData")
load("/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/direct_loss.RData")
load("/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/region_match.RData")

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

save(direct_loss,direct_loss_far, x_loss,x_loss_far, file = "/Users/bowen/Documents/博-科研/极端天气和气候损失/global/数据处理/loss.RData")
