#########################################################################################################################
### Project  : FP Performance Evaluation
### Script   : DBFP.2120 on GIT.R
### Contents : FP Performance Evaluation Model - Two stages
#########################################################################################################################

#########################################################################################################################
### Setting up environment
#########################################################################################################################

# Load library
pkgs <- c("DJL", "abind")
sapply(pkgs, require, character.only = T)

# Load data & parameters
load("DBFP.2011.2109.Rdata")

# Switch
e.type <- "fp" # fp(fp) vs branch(br)

# Naming for coding convenience
df.raw <- df.raw.2011.2109
nm.m   <- unique(df.raw[,1])


#########################################################################################################################
### Data cleansing
#########################################################################################################################

# IDs for exclusion
id.out.level  <- which( is.na(match(df.raw[,3], c(100, 101, 102, 109, 112))))
id.out.branch <- which(!is.na(match(df.raw[,5], c(8262, 8312, 8335, 8354))))
id.out.bunit  <- which(!is.na(match(df.raw[,4], c(74, NA))))
id.out.month  <- if(e.type == "fp") which(df.raw[,9] %in% c(0, 1, 2, 3)) else NULL

# Effective data
id.out.all <- unique(c(id.out.level, id.out.branch, id.out.bunit, id.out.month))
df.eff.mw  <- df.raw[-id.out.all,][order(df.raw[-id.out.all,][,1], df.raw[-id.out.all,][,2]),]

# Derivative variables (Coverage analysis, Withdrawal + Return)
df.eff.mw[,ncol(df.eff.mw) + 1] <- df.eff.mw[,91] + df.eff.mw[,92]
df.eff.mw[,ncol(df.eff.mw) + 1] <- df.eff.mw[,33] + df.eff.mw[,35]
names(df.eff.mw)[93:94] <- c("보장분석건수(최적+일반)", "청철+반송")


#########################################################################################################################
### Descriptive Statistics
#########################################################################################################################

# Who and When
m      <- 202108
id.fp  <- 775190
id.me  <- which(df.eff.mw[,1] == m & df.eff.mw[,2] == id.fp)
id.six <- df.eff.mw[,1] %in% tail(nm.m[nm.m <= m], 6)

# R1. Sales Outcome
id.so.kpi     <- c(50, 45, 30, 32, 68, 69, 70:89, 25, 16, 22)
df.so.crm.kpi <- df.eff.mw[df.eff.mw[,1] == m & df.eff.mw[,2] == id.fp, id.so.kpi]
df.so.six.kpi <- df.eff.mw[id.six & df.eff.mw[,2] == id.fp, id.so.kpi]
res.so.crm    <- list(Product.ratio = df.so.crm.kpi[seq(8, 26, 2)]/sum(df.so.crm.kpi[seq(8, 26, 2)]),
                      Product.premium = df.so.crm.kpi[seq(8, 26, 2)],
                      Product.count = df.so.crm.kpi[seq(7, 26, 2)],
                      KPI = data.frame(premium.p.total = df.so.crm.kpi[1]/df.so.crm.kpi[2],
                                       total = df.so.crm.kpi[2],
                                       contract.p.design = df.so.crm.kpi[3],
                                       age.contractor = df.so.crm.kpi[5],
                                       age.insurant = df.so.crm.kpi[6],
                                       contract.p.design = df.so.crm.kpi[4]))

res.so.six    <- list(Product.ratio = colSums(df.so.six.kpi[,seq(8, 26, 2)])/sum(df.so.six.kpi[,seq(8, 26, 2)]),
                      Product.premium = colSums(df.so.six.kpi[,seq(8, 26, 2)]),
                      Product.count = colSums(df.so.six.kpi[,seq(7, 26, 2)]),
                      KPI = data.frame(premium.p.total = sum(df.so.six.kpi[,1])/sum(df.so.six.kpi[,2]),
                                       total = sum(df.so.six.kpi[,2]),
                                       contract.p.design = sum(df.so.six.kpi[,27])/sum(df.so.six.kpi[,28]) * 100,
                                       age.contractor = mean(df.so.six.kpi[,5]),
                                       age.insurant = mean(df.so.six.kpi[,6]),
                                       contract.p.design = sum(df.so.six.kpi[,27])/sum(df.so.six.kpi[,29]) * 100))
                           
# R2. Sales Activity
id.same.g <- df.eff.mw[,8] == df.eff.mw[id.me, 8] & df.eff.mw[,10] == df.eff.mw[id.me, 10]
id.stdd.g <- id.same.g & df.eff.mw[,13] == 1
id.best.g <- id.same.g & df.eff.mw[,14] == 1
id.sa.kpi <- c(1, 15, 16, 22, 25, 26, 27, 30, 32, 33, 35, 37, 45, 50, 54, 57, 58, 61, 62, 63, 67, 68, 69, 90, 93)
res.sa.me <- df.eff.mw[id.six & df.eff.mw[,2] == id.fp, id.sa.kpi]
res.sa.sg <- aggregate(df.eff.mw[id.six & id.same.g, id.sa.kpi][,-1], list(df.eff.mw[id.six & id.same.g, 1]), "mean")
res.sa.st <- aggregate(df.eff.mw[id.six & id.stdd.g, id.sa.kpi][,-1], list(df.eff.mw[id.six & id.stdd.g, 1]), "mean")
res.sa.bg <- aggregate(df.eff.mw[id.six & id.best.g, id.sa.kpi][,-1], list(df.eff.mw[id.six & id.best.g, 1]), "mean")

# R3. Benchmark
stat.me.product.ratio <- res.so.crm$Product.ratio
stat.sg.product.ratio <- colSums(df.eff.mw[df.eff.mw[,1] == m & id.same.g, seq(71, 89, 2)]) / sum(df.eff.mw[df.eff.mw[,1] == m & id.same.g, seq(71, 89, 2)])
stat.st.product.ratio <- colSums(df.eff.mw[df.eff.mw[,1] == m & id.stdd.g, seq(71, 89, 2)]) / sum(df.eff.mw[df.eff.mw[,1] == m & id.stdd.g, seq(71, 89, 2)])
stat.bg.product.ratio <- colSums(df.eff.mw[df.eff.mw[,1] == m & id.best.g, seq(71, 89, 2)]) / sum(df.eff.mw[df.eff.mw[,1] == m & id.best.g, seq(71, 89, 2)])

res.bm <- list(Main = unlist(c(stat.me.product.ratio[which(stat.me.product.ratio == max(stat.me.product.ratio))], 
                               stat.sg.product.ratio[which(stat.sg.product.ratio == max(stat.sg.product.ratio))], 
                               stat.st.product.ratio[which(stat.st.product.ratio == max(stat.st.product.ratio))], 
                               stat.bg.product.ratio[which(stat.bg.product.ratio == max(stat.bg.product.ratio))])),
               Stat = data.frame(FP.own = unlist(tail(res.sa.me, 1)),
                                 Same.G = round(unlist(tail(res.sa.sg, 1)), 1),
                                 Stdd.G = round(unlist(tail(res.sa.st, 1)), 1),
                                 Best.G = round(unlist(tail(res.sa.bg, 1)), 1))[-1,])


#########################################################################################################################
### FP Evaluation
#########################################################################################################################

# Plates
res.all <- data.frame()

# Loop for all periods
for(m in 202011:202108){
  
  # Data of interest
  df.fp.mw <- df.eff.mw[df.eff.mw[,1] == m,]

  # Sub-grouping
  id.m.00.06.mw <- which(df.fp.mw[,9] < 07)
  id.m.07.12.mw <- which(df.fp.mw[,9] > 06 & df.fp.mw[,9] < 13)
  id.m.13.24.mw <- which(df.fp.mw[,9] > 12 & df.fp.mw[,9] < 25)
  id.m.25.60.mw <- which(df.fp.mw[,9] > 24 & df.fp.mw[,9] < 61)
  id.m.61.00.mw <- which(df.fp.mw[,9] > 60)
  
  # Plates
  res.all.m <- data.frame()
  
  # Productivity analysis
  for(i in 1:2){
    for(j in 1:5){
      
      # Data of interest
      id.type.mw <- if(i == 1) which(df.fp.mw[,8] == 1) else which(df.fp.mw[,8] == 0)
      id.m.mw    <- if(j == 1) id.m.00.06.mw else if(j == 2) id.m.07.12.mw else if(j == 3) id.m.13.24.mw else if(j == 4) id.m.25.60.mw else id.m.61.00.mw
      df.temp.mw <- df.fp.mw[intersect(id.type.mw, id.m.mw),]
      
      # IDs for evaluation
      id.calc.s1 <- which(df.temp.mw[,id.y.s1[1]] > 0)
      id.excd.s1 <- which(df.temp.mw[,id.y.s1[1]] == 0)
      id.calc.s2 <- which(apply(df.temp.mw[,id.y.s2], 1, function(x) sum(x > 0) == 2))
      id.excd.s2 <- which(apply(df.temp.mw[,id.y.s2], 1, function(x) sum(x > 0) <  2))
      id.calc.s3 <- which(df.temp.mw[,id.y.s3[1]] >  0)
      id.excd.s3 <- which(df.temp.mw[,id.y.s3[1]] <= 0)
      
      # Plates
      res.score.s1   <- res.score.s2 <- res.score.s3 <- res.rank.s1 <- res.rank.s2 <- res.rank.s3 <- res.group.s1 <- res.group.s2 <- res.wsum.s1 <- res.wsum.s2 <- res.group.s3 <- res.wsum.s3 <- rep(NA, nrow(df.temp.mw))
      rank.calc.s1   <- rep(NA, length(id.calc.s1)); rank.calc.s2 <- rep(NA, length(id.calc.s2)); rank.calc.s3 <- rep(NA, length(id.calc.s3))
      res.tar.sbm.s1 <- array(NA, c(nrow(df.temp.mw), length(id.y.s1))); res.tar.sup.s1 <- array(NA, c(nrow(df.temp.mw), 1 + length(id.x.s1) + length(id.y.s1)))
      res.tar.sbm.s2 <- array(NA, c(nrow(df.temp.mw), length(id.y.s2))); res.tar.sup.s2 <- array(NA, c(nrow(df.temp.mw), 1 + length(id.x.s2) + length(id.y.s2)))
      res.tar.sbm.s3 <- array(NA, c(nrow(df.temp.mw), length(id.y.s3))); res.tar.sup.s3 <- array(NA, c(nrow(df.temp.mw), 1 + length(id.x.s3) + length(id.y.s3)))
      
      
      ###############
      # Scoring
      ###############
      # Stage 1
      res.s1    <- dm.sbm(df.temp.mw[id.calc.s1, id.x.s1], 
                          cbind(df.temp.mw[id.calc.s1, id.y.s1[1]], 
                                max(df.temp.mw[id.calc.s1, id.y.s1[2]]) + 1 - df.temp.mw[id.calc.s1, id.y.s1[2]]), 
                          "drs", "o")
      res.s1.se <- dm.sbm(df.temp.mw[id.calc.s1, id.x.s1], 
                          cbind(df.temp.mw[id.calc.s1, id.y.s1[1]], 
                                max(df.temp.mw[id.calc.s1, id.y.s1[2]]) + 1 - df.temp.mw[id.calc.s1, id.y.s1[2]]), 
                          "drs", "o", se = T)
      
      # Stage 2
      res.s2    <- dm.sbm(df.temp.mw[id.calc.s2, id.x.s2], 
                          df.temp.mw[id.calc.s2, id.y.s2],
                          "drs", "o")
      res.s2.se <- dm.sbm(df.temp.mw[id.calc.s2, id.x.s2], 
                          df.temp.mw[id.calc.s2, id.y.s2],
                          "drs", "o", se = T)
      
      # Stage 3
      res.s3    <- dm.sbm(df.temp.mw[id.calc.s3, id.x.s3], 
                          cbind(df.temp.mw[id.calc.s3, id.y.s3[1]], 
                                max(df.temp.mw[id.calc.s3, id.y.s3[2]]) + 1 - df.temp.mw[id.calc.s3, id.y.s3[2]],
                                max(df.temp.mw[id.calc.s3, id.y.s3[3]]) + 1 - df.temp.mw[id.calc.s3, id.y.s3[3]]),
                          "drs", "o")
      res.s3.se <- dm.sbm(df.temp.mw[id.calc.s3, id.x.s3], 
                          cbind(df.temp.mw[id.calc.s3, id.y.s3[1]], 
                                max(df.temp.mw[id.calc.s3, id.y.s3[2]]) + 1 - df.temp.mw[id.calc.s3, id.y.s3[2]],
                                max(df.temp.mw[id.calc.s3, id.y.s3[3]]) + 1 - df.temp.mw[id.calc.s3, id.y.s3[3]]),
                          "drs", "o", se = T)
      
      # Temporal treatment for -Inf
      res.s1.se$eff[res.s1.se$eff == -Inf] <- 1
      res.s2.se$eff[res.s2.se$eff == -Inf] <- 1
      res.s3.se$eff[res.s3.se$eff == -Inf] <- 1
      
      # Score
      res.score.s1[id.calc.s1] <- res.s1$eff * res.s1.se$eff^0.1
      res.score.s2[id.calc.s2] <- res.s2$eff * res.s2.se$eff^0.1
      res.score.s3[id.calc.s3] <- res.s3$eff * res.s3.se$eff^0.1
      
      # Zero score
      res.score.s1[id.excd.s1] <- 0
      res.score.s2[id.excd.s2] <- 0
      res.score.s3[id.excd.s3] <- 0
      
      
      #################
      # Ranking 
      #################
      # Stage 1
      # XS(<Q1) vs S(<Q2) vs M(<Q3) vs L
      w.s1.ef.ws <- -c(0.5, 0.5) * c(100, 10); w.s1.y1.y2 <- c(0.7, 0.3)
      res.rank.s1[id.excd.s1] <- nrow(df.temp.mw)
      ts.s1.xs <- summary(df.temp.mw[id.calc.s1, id.y.s1[1]])[2]
      ts.s1.s  <- summary(df.temp.mw[id.calc.s1, id.y.s1[1]])[3]
      ts.s1.m  <- summary(df.temp.mw[id.calc.s1, id.y.s1[1]])[5]
      id.s1.xs <- which(df.temp.mw[id.calc.s1, id.y.s1[1]] <= ts.s1.xs)
      id.s1.s  <- which(df.temp.mw[id.calc.s1, id.y.s1[1]] >  ts.s1.xs & df.temp.mw[id.calc.s1, id.y.s1[1]] <= ts.s1.s)
      id.s1.m  <- which(df.temp.mw[id.calc.s1, id.y.s1[1]] >  ts.s1.s  & df.temp.mw[id.calc.s1, id.y.s1[1]] <= ts.s1.m)
      id.s1.l  <- which(df.temp.mw[id.calc.s1, id.y.s1[1]] >  ts.s1.m)
      if(length(id.s1.xs) > 0) rank.calc.s1[id.s1.xs] <- round(rank(res.score.s1[id.calc.s1][id.s1.xs] * w.s1.ef.ws[1] + as.matrix(df.temp.mw[id.calc.s1,][id.s1.xs, 28:29]) %*% w.s1.y1.y2 * w.s1.ef.ws[2])) + length(id.s1.l) + length(id.s1.m) + length(id.s1.s)
      if(length(id.s1.s ) > 0) rank.calc.s1[id.s1.s ] <- round(rank(res.score.s1[id.calc.s1][id.s1.s ] * w.s1.ef.ws[1] + as.matrix(df.temp.mw[id.calc.s1,][id.s1.s , 28:29]) %*% w.s1.y1.y2 * w.s1.ef.ws[2])) + length(id.s1.l) + length(id.s1.m)
      if(length(id.s1.m ) > 0) rank.calc.s1[id.s1.m ] <- round(rank(res.score.s1[id.calc.s1][id.s1.m ] * w.s1.ef.ws[1] + as.matrix(df.temp.mw[id.calc.s1,][id.s1.m , 28:29]) %*% w.s1.y1.y2 * w.s1.ef.ws[2])) + length(id.s1.l)
      if(length(id.s1.l ) > 0) rank.calc.s1[id.s1.l ] <- round(rank(res.score.s1[id.calc.s1][id.s1.l ] * w.s1.ef.ws[1] + as.matrix(df.temp.mw[id.calc.s1,][id.s1.l , 28:29]) %*% w.s1.y1.y2 * w.s1.ef.ws[2]))
      res.group.s1[id.calc.s1][id.s1.xs] <- "Q1"; res.group.s1[id.calc.s1][id.s1.s] <- "Q2"; res.group.s1[id.calc.s1][id.s1.m] <- "Q3"; res.group.s1[id.calc.s1][id.s1.l] <- "Q4"
      if(length(id.s1.xs) > 0) res.wsum.s1[id.calc.s1][id.s1.xs] <- as.matrix(df.temp.mw[id.calc.s1,][id.s1.xs, 28:29]) %*% w.s1.y1.y2
      if(length(id.s1.s ) > 0) res.wsum.s1[id.calc.s1][id.s1.s ] <- as.matrix(df.temp.mw[id.calc.s1,][id.s1.s , 28:29]) %*% w.s1.y1.y2
      if(length(id.s1.m ) > 0) res.wsum.s1[id.calc.s1][id.s1.m ] <- as.matrix(df.temp.mw[id.calc.s1,][id.s1.m , 28:29]) %*% w.s1.y1.y2
      if(length(id.s1.l ) > 0) res.wsum.s1[id.calc.s1][id.s1.l ] <- as.matrix(df.temp.mw[id.calc.s1,][id.s1.l , 28:29]) %*% w.s1.y1.y2
      # View(cbind(df.temp.mw[id.calc.s1, c(id.x.s1, 23:24, id.y.s1)], res.score.s1[id.calc.s1], rank.calc.s1))
      
      # Stage 2
      # XS(<Q1) vs S(<Q2) vs M(<Q3) vs L
      w.s2.ef.ws <- -c(0.5, 0.5) * c(100, 10); w.s2.y1.y2 <- c(0.3, 0.7)
      res.rank.s2[id.excd.s2] <- nrow(df.temp.mw)
      ny.s2    <- data.frame(apply(df.temp.mw[id.calc.s2, id.y.s2], 2, function(x) nor(x)), 
                             nor(df.temp.mw[id.calc.s2, id.y.s2[1]]) * w.s2.y1.y2[1] + nor(df.temp.mw[id.calc.s2, id.y.s2[2]]) * w.s2.y1.y2[2])
      ts.s2.xs <- summary(ny.s2[,3])[2]
      ts.s2.s  <- summary(ny.s2[,3])[3]
      ts.s2.m  <- summary(ny.s2[,3])[5]
      id.s2.xs <- which(ny.s2[,3] <= ts.s2.xs)
      id.s2.s  <- which(ny.s2[,3] > ts.s2.xs & ny.s2[,3] <= ts.s2.s)
      id.s2.m  <- which(ny.s2[,3] > ts.s2.s  & ny.s2[,3] <= ts.s2.m)
      id.s2.l  <- which(ny.s2[,3] > ts.s2.m)
      if(length(id.s2.xs) > 0) rank.calc.s2[id.s2.xs] <- round(rank(res.score.s2[id.calc.s2][id.s2.xs] * w.s2.ef.ws[1] + ny.s2[id.s2.xs, 3] * w.s2.ef.ws[2])) + length(id.s2.l) + length(id.s2.m) + length(id.s2.s)
      if(length(id.s2.s ) > 0) rank.calc.s2[id.s2.s ] <- round(rank(res.score.s2[id.calc.s2][id.s2.s ] * w.s2.ef.ws[1] + ny.s2[id.s2.s , 3] * w.s2.ef.ws[2])) + length(id.s2.l) + length(id.s2.m)
      if(length(id.s2.m ) > 0) rank.calc.s2[id.s2.m ] <- round(rank(res.score.s2[id.calc.s2][id.s2.m ] * w.s2.ef.ws[1] + ny.s2[id.s2.m , 3] * w.s2.ef.ws[2])) + length(id.s2.l)
      if(length(id.s2.l ) > 0) rank.calc.s2[id.s2.l ] <- round(rank(res.score.s2[id.calc.s2][id.s2.l ] * w.s2.ef.ws[1] + ny.s2[id.s2.l , 3] * w.s2.ef.ws[2]))
      res.group.s2[id.calc.s2][id.s2.xs] <- "Q1"; res.group.s2[id.calc.s2][id.s2.s] <- "Q2"; res.group.s2[id.calc.s2][id.s2.m] <- "Q3"; res.group.s2[id.calc.s2][id.s2.l] <- "Q4"
      if(length(id.s2.xs) > 0) res.wsum.s2[id.calc.s2][id.s2.xs] <- ny.s2[id.s2.xs, 3]
      if(length(id.s2.s ) > 0) res.wsum.s2[id.calc.s2][id.s2.s ] <- ny.s2[id.s2.s , 3]
      if(length(id.s2.m ) > 0) res.wsum.s2[id.calc.s2][id.s2.m ] <- ny.s2[id.s2.m , 3]
      if(length(id.s2.l ) > 0) res.wsum.s2[id.calc.s2][id.s2.l ] <- ny.s2[id.s2.l , 3]
      # View(cbind(df.temp.mw[id.calc.s2, c(id.x.s2, id.y.s2)], res.score.s2[id.calc.s2], rank.calc.s2))
      
      # Stage 3
      # XS(<Q1) vs S(<Q2) vs M(<Q3) vs L
      w.s3.ef.ws <- -c(0.5, 0.5) * c(100, 10); w.s3.y1.y2 <- c(0.3, 0.7)
      res.rank.s3[id.excd.s3] <- nrow(df.temp.mw)
      ny.s3    <- data.frame(nor(df.temp.mw[id.calc.s3, id.y.s3[1]]), 
                             nor(df.temp.mw[id.calc.s3, id.y.s3[1]]/df.temp.mw[id.calc.s3, id.x.s3]),
                             nor(df.temp.mw[id.calc.s3, id.y.s3[1]]) * w.s3.y1.y2[1] + nor(df.temp.mw[id.calc.s3, id.y.s3[1]]/df.temp.mw[id.calc.s3, id.x.s3]) * w.s2.y1.y2[2])
      ts.s3.xs <- summary(ny.s3[,1])[2]
      ts.s3.s  <- summary(ny.s3[,1])[3]
      ts.s3.m  <- summary(ny.s3[,1])[5]
      id.s3.xs <- which(ny.s3[,1] <= ts.s3.xs)
      id.s3.s  <- which(ny.s3[,1] >  ts.s3.xs & ny.s3[,1] <= ts.s3.s)
      id.s3.m  <- which(ny.s3[,1] >  ts.s3.s  & ny.s3[,1] <= ts.s3.m)
      id.s3.l  <- which(ny.s3[,1] >  ts.s3.m)
      if(length(id.s3.xs) > 0) rank.calc.s3[id.s3.xs] <- round(rank(res.score.s3[id.calc.s3][id.s3.xs] * w.s3.ef.ws[1] + ny.s3[id.s3.xs,3] * w.s3.ef.ws[2])) + length(id.s3.l) + length(id.s3.m) + length(id.s3.s)
      if(length(id.s3.s ) > 0) rank.calc.s3[id.s3.s ] <- round(rank(res.score.s3[id.calc.s3][id.s3.s ] * w.s3.ef.ws[1] + ny.s3[id.s3.s, 3] * w.s3.ef.ws[2])) + length(id.s3.l) + length(id.s3.m)
      if(length(id.s3.m ) > 0) rank.calc.s3[id.s3.m ] <- round(rank(res.score.s3[id.calc.s3][id.s3.m ] * w.s3.ef.ws[1] + ny.s3[id.s3.m, 3] * w.s3.ef.ws[2])) + length(id.s3.l)
      if(length(id.s3.l ) > 0) rank.calc.s3[id.s3.l ] <- round(rank(res.score.s3[id.calc.s3][id.s3.l ] * w.s3.ef.ws[1] + ny.s3[id.s3.l, 3] * w.s3.ef.ws[2]))
      res.group.s3[id.calc.s3][id.s3.xs] <- "Q1"; res.group.s3[id.calc.s3][id.s3.s] <- "Q2"; res.group.s3[id.calc.s3][id.s3.m] <- "Q3"; res.group.s3[id.calc.s3][id.s3.l] <- "Q4"
      if(length(id.s3.xs) > 0) res.wsum.s3[id.calc.s3][id.s3.xs] <- ny.s3[id.s3.xs, 3]
      if(length(id.s3.s ) > 0) res.wsum.s3[id.calc.s3][id.s3.s ] <- ny.s3[id.s3.s,  3]
      if(length(id.s3.m ) > 0) res.wsum.s3[id.calc.s3][id.s3.m ] <- ny.s3[id.s3.m,  3]
      if(length(id.s3.l ) > 0) res.wsum.s3[id.calc.s3][id.s3.l ] <- ny.s3[id.s3.l,  3]
      # View(cbind(df.temp.mw[id.calc.s3, c(id.x.s3, id.y.s3)], res.wsum.s3[id.calc.s3], res.score.s3[id.calc.s3], rank.calc.s3))
      
      # Rank
      res.rank.s1[id.calc.s1] <- rank.calc.s1
      res.rank.s2[id.calc.s2] <- rank.calc.s2
      res.rank.s3[id.calc.s3] <- rank.calc.s3
      
      
      #################
      # Targeting
      #################
      # Stage 1
      # SBM target
      tar.calc.sbm.s1 <- res.s1$lambda %*% as.matrix(cbind(df.temp.mw[id.calc.s1, id.y.s1[1]], 
                                                           max(df.temp.mw[id.calc.s1, id.y.s1[2]]) + 1 - df.temp.mw[id.calc.s1, id.y.s1[2]]))
      res.tar.sbm.s1[id.calc.s1,] <- as.matrix(data.frame(ceiling(tar.calc.sbm.s1[,1]), floor(max(df.temp.mw[id.calc.s1, id.y.s1[2]]) + 1 - round(tar.calc.sbm.s1[,2], 4))))
      res.tar.sbm.s1[which(res.rank.s1 == 1),] <- as.matrix(df.temp.mw[id.calc.s1, id.y.s1][which(res.rank.s1[id.calc.s1] == 1),])
      
      # Scaled-up target
      # id.tar.sup.s1 <- target.scale.up(df.temp.mw[id.calc.s1, id.n.l.t], df.temp.mw[id.calc.s1, id.n.g.t], df.temp.mw[id.calc.s1, id.flex])$res.target
      # res.tar.sup.s1[id.calc.s1,] <- as.matrix(df.temp.mw[id.calc.s1, c(2, id.x.s1, id.y.s1)][id.tar.sup.s1,])
      # 
      # View(cbind(df.temp.12[id.calc.s1, c(id.x.s1, id.y.s1)], res.score.s1[id.calc.s1], res.rank.s1[id.calc.s1], res.tar.sbm.s1[id.calc.s1,], res.tar.sup.s1[id.calc.s1,]))
      
      # Stage 2
      # SBM target
      tar.calc.sbm.s2 <- res.s2$lambda %*% as.matrix(df.temp.mw[id.calc.s2, id.y.s2])
      res.tar.sbm.s2[id.calc.s2,] <- as.matrix(tar.calc.sbm.s2)
      res.tar.sbm.s2[which(res.rank.s2 == 1),] <- as.matrix(df.temp.mw[id.calc.s2, id.y.s2][which(res.rank.s2[id.calc.s2] == 1),])
      
      # Scaled-up target
      # id.tar.sup.s2 <- target.scale.up(df.temp.mw[id.calc.s2, id.x.s2], df.temp.mw[id.calc.s2, id.y.s2], res.score.s2[id.calc.s2]*100, res.rank.s2[id.calc.s2], c(10^10, 10^6, 1, 1, 10^-2), efc = T)$res.target
      # res.tar.sup.s2[id.calc.s2,] <- as.matrix(df.temp.mw[id.calc.s2, c(2, id.x.s2, id.y.s2)][id.tar.sup.s2,])
      # 
      # View(cbind(df.temp.12[id.calc.s2, c(id.x.s2, id.y.s2)], res.score.s2[id.calc.s2], res.rank.s2[id.calc.s2], res.tar.sbm.s2[id.calc.s2,], res.tar.sup.s2[id.calc.s2,]))
      
      # Stage 3
      # SBM target
      tar.calc.sbm.s3 <- res.s3$lambda %*% as.matrix(cbind(df.temp.mw[id.calc.s3, id.y.s3[1]], 
                                                           max(df.temp.mw[id.calc.s3, id.y.s3[2]]) + 1 - df.temp.mw[id.calc.s3, id.y.s3[2]],
                                                           max(df.temp.mw[id.calc.s3, id.y.s3[3]]) + 1 - df.temp.mw[id.calc.s3, id.y.s3[3]]))
      res.tar.sbm.s3[id.calc.s3,] <- as.matrix(data.frame(tar.calc.sbm.s3[,1], 
                                                          round(max(df.temp.mw[id.calc.s3, id.y.s3[2]]) + 1 - tar.calc.sbm.s3[,2]),
                                                          round(max(df.temp.mw[id.calc.s3, id.y.s3[3]]) + 1 - tar.calc.sbm.s3[,3])))
      res.tar.sbm.s3[which(res.rank.s3 == 1),] <- as.matrix(df.temp.mw[id.calc.s3, id.y.s3][which(res.rank.s3[id.calc.s3] == 1),])
      
      # Scaled-up target
      # id.tar.sup.s3 <- target.scale.up(df.temp.mw[id.calc.s3, id.x.s3], df.temp.mw[id.calc.s3, id.y.s3], res.score.s3[id.calc.s3]*100, res.rank.s3[id.calc.s3], 10, c(2, 3), nd = T)$res.target
      # res.tar.sup.s3[id.calc.s3,] <- as.matrix(df.temp.mw[id.calc.s3, c(2, id.x.s3, id.y.s3)][id.tar.sup.s3,])
      # 
      # View(cbind(df.temp.mw[id.calc.s3, c(id.x.s3, id.y.s3)], res.score.s3[id.calc.s3], res.rank.s3[id.calc.s3], res.tar.sbm.s3[id.calc.s3,], res.tar.sup.s3[id.calc.s3,]))
      
      # Scaled-up target at once
      id.tar.sup  <- target.scale.up(df.temp.mw[,id.n.l.t], df.temp.mw[,id.n.g.t], df.temp.mw[,id.flex])$res.target
      res.tar.sup <- df.temp.mw[id.tar.sup, 2]
      
      # Results
      res.all.m <- rbind(res.all, data.frame(Closed.m   = m,
                                             FP.id      = df.temp.mw[,2],
                                             Target.sup = res.tar.sup))
    }
  }
  write.csv(res.all.m[order(res.all.m[,2]),], file = "benchmark.csv")
  res.all <- rbind(res.all, res.all.m)
}

write.csv(res.all.m.s1.s2, file = "res.s1.s2.csv")
write.csv(res.all.m.s3, file = "res.s3.csv")


# Extract thresholds
res.all.q <- data.frame()
for(m in 202006:202012){
  
  # Data
  df.fp.mw <- df.eff.mw[df.eff.mw[,1] == m,]
  df.fp.sa <- df.eff.sa[df.eff.sa[,1] == m,]
  
  # Insert churns aggregated 
  df.fp.mw$agg.churn <- rowSums(df.fp.mw[,42:44])
  
  # Sub-grouping
  id.m.02.06.sa <- which(df.fp.sa[,10] < 07)
  id.m.07.12.sa <- which(df.fp.sa[,10] > 06 & df.fp.sa[,10] < 13)
  id.m.13.24.sa <- which(df.fp.sa[,10] > 12 & df.fp.sa[,10] < 25)
  id.m.25.60.sa <- which(df.fp.sa[,10] > 24 & df.fp.sa[,10] < 61)
  id.m.61.00.sa <- which(df.fp.sa[,10] > 60)
  id.m.02.06.mw <- which(df.fp.mw[,10] < 07)
  id.m.07.12.mw <- which(df.fp.mw[,10] > 06 & df.fp.mw[,10] < 13)
  id.m.13.24.mw <- which(df.fp.mw[,10] > 12 & df.fp.mw[,10] < 25)
  id.m.25.60.mw <- which(df.fp.mw[,10] > 24 & df.fp.mw[,10] < 61)
  id.m.61.00.mw <- which(df.fp.mw[,10] > 60)
  
  # Productivity analysis
  res.s1.all.q <- res.s2.all.q <- res.s3.all.q <- data.frame()
  for(i in 1:2){
    for(j in 1:5){
      
      # Sub-grouping
      id.type.sa <- if(i == 1) which(df.fp.sa[,9] == 1) else which(df.fp.sa[,9] == 0)
      id.type.mw <- if(i == 1) which(df.fp.mw[,9] == 1) else which(df.fp.mw[,9] == 0)
      id.m.sa    <- if(j == 1) id.m.02.06.sa else if(j == 2) id.m.07.12.sa else if(j == 3) id.m.13.24.sa else if(j == 4) id.m.25.60.sa else id.m.61.00.sa
      id.m.mw    <- if(j == 1) id.m.02.06.mw else if(j == 2) id.m.07.12.mw else if(j == 3) id.m.13.24.mw else if(j == 4) id.m.25.60.mw else id.m.61.00.mw
      df.temp.sa <- df.fp.sa[intersect(id.type.sa, id.m.sa),]
      df.temp.mw <- df.fp.mw[intersect(id.type.mw, id.m.mw),]
      
      # IDs for evaluation
      df.temp.12 <- if(m.type == "mw") df.temp.mw else df.temp.sa
      id.calc.s1 <- which(apply(df.temp.12[,id.y.s1[1:2]], 1, function(x) sum(x) >  0))
      id.excd.s1 <- which(apply(df.temp.12[,id.y.s1[1:2]], 1, function(x) sum(x) == 0))
      id.calc.s2 <- which(apply(df.temp.12[,id.y.s2], 1, function(x) sum(x > 0) == 2))
      id.excd.s2 <- which(apply(df.temp.12[,id.y.s2], 1, function(x) sum(x > 0) <  2))
      id.calc.s3 <- which(df.temp.mw[,id.y.s3[1]] >  0)
      id.excd.s3 <- which(df.temp.mw[,id.y.s3[1]] <= 0)
      
      
      #################
      # Ranking 
      #################
      # Stage 1
      s1.ts.ys.xs <- summary(rowSums(df.temp.12[id.calc.s1, id.y.s1[1:2]]))[2]
      s1.ts.ys.s  <- summary(rowSums(df.temp.12[id.calc.s1, id.y.s1[1:2]]))[3]
      s1.ts.ys.m  <- summary(rowSums(df.temp.12[id.calc.s1, id.y.s1[1:2]]))[5]
      
      # Stage 2
      # s2.ts.all.y1 <- s2.ts.all.y2 <- rep(NA, 11)
      # for(s in 1:11){
      #   id.group        <- which(res.s2.sa$eff < s/10 & round(res.s2.sa$eff, 8) >= (s - 1)/10)
      #   s2.ts.all.y1[s] <- summary(df.temp.sa[id.calc.s2,][id.group, id.y.s2[1]])[2]
      #   s2.ts.all.y2[s] <- summary(df.temp.sa[id.calc.s2,][id.group, id.y.s2[2]])[2]
      # }
      w.s2.y1.y2  <- c(0.3, 0.7)
      ny.sum      <- nor(df.temp.12[id.calc.s2, id.y.s2[1]]) * w.s2.y1.y2[1] + nor(df.temp.12[id.calc.s2, id.y.s2[2]]) * w.s2.y1.y2[2]
      s2.ts.ny.xs <- summary(ny.sum)[2]
      s2.ts.ny.s  <- summary(ny.sum)[3]
      s2.ts.ny.m  <- summary(ny.sum)[5]
      
      # Stage 3
      # s3.ts.all.y1 <- rep(NA, 11)
      # for(s in 1:11){
      #   id.group        <- which(res.s3.mw$eff < s/10 & round(res.s3.mw$eff, 8) >= (s - 1)/10)
      #   s3.ts.all.y1[s] <- summary(df.temp.mw[id.calc.s3,][id.group, id.y.s3[1]])[2]
      # }
      w.s3.y1.y2 <- c(0.3, 0.7)
      y          <- df.temp.mw[id.calc.s3, id.y.s3[1]]
      s3.ts.y.xs <- summary(y)[2]
      s3.ts.y.s  <- summary(y)[3]
      s3.ts.y.m  <- summary(y)[5]
      
      # Summary
      res.s1.all.q <- rbind(res.s1.all.q, matrix(c(m, i, j, s1.ts.ys.xs, s1.ts.ys.s, s1.ts.ys.m), nrow = 1))
      # res.s2.all.q <- rbind(res.s2.all.q, matrix(c(s2.ts.all.y1, s2.ts.all.y2), nrow = 1))
      res.s2.all.q <- rbind(res.s2.all.q, matrix(c(s2.ts.ny.xs, s2.ts.ny.s, s2.ts.ny.m), nrow = 1))
      # res.s3.all.q <- rbind(res.s3.all.q, matrix(c(s3.ts.all.y1), nrow = 1))
      res.s3.all.q <- rbind(res.s3.all.q, matrix(c(s3.ts.y.xs, s3.ts.y.s, s3.ts.y.m), nrow = 1))
      
    }
  }
  res.all.q <- rbind(res.all.q, data.frame(res.s1.all.q, res.s2.all.q, res.s3.all.q))
}

write.csv(res.all.q, file = "res.all.q.csv")


#########################################################################################################################
### Branch Evaluation
#########################################################################################################################

# Loop for all periods
res.all.br <- data.frame()
id.v.all   <- c(id.x.s1, id.y.s1, id.y.s2, id.x.s3, id.y.s3)
for(m in 202003:202012){
  
  # Data of evaluation
  df.fp.mw <- df.eff.mw[df.eff.mw[,1] == m,]
  
  # Productivity analysis
  res.all.m.br <- data.frame()
  for(i in 1:3){ 
    
    # Data of interest - 1:Incu, 2:Nor.Small, 3:Nor.Large
    if(i == 1){
      id.temp <- which(df.fp.mw[,6] == 1)
      df.temp <- aggregate(df.fp.mw[id.temp, id.v.all], list(df.fp.mw[id.temp, 5]), sum)
    }else if(i == 2){
      id.temp <- which(df.fp.mw[,6] == 0 & df.fp.mw[,8] == 1)
      df.temp <- aggregate(df.fp.mw[id.temp, id.v.all], list(df.fp.mw[id.temp, 5]), sum)
    }else{
      id.temp <- which(df.fp.mw[,8] == 2)
      df.temp <- aggregate(df.fp.mw[id.temp, id.v.all], list(df.fp.mw[id.temp, 5]), sum)
    }
    
    # Average for comprehensive profit margin
    df.temp[,9] <- aggregate(df.fp.mw[id.temp, id.y.s2[1]], list(df.fp.mw[id.temp, 5]), mean)[,2]
    
    # IDs for evaluation
    id.calc.s1 <- which(apply(df.temp[,6:7],  1, function(x) sum(x) >  0))
    id.excd.s1 <- which(apply(df.temp[,6:7],  1, function(x) sum(x) == 0))
    id.calc.s2 <- which(apply(df.temp[,9:10], 1, function(x) sum(x > 0) == 2))
    id.excd.s2 <- which(apply(df.temp[,9:10], 1, function(x) sum(x > 0) <  2))
    id.calc.s3 <- which(df.temp[,12] >  0)
    id.excd.s3 <- which(df.temp[,12] <= 0)
    
    # Empty box for results
    res.score.s1 <- res.score.s2 <- res.score.s3 <- rep(NA, nrow(df.temp))
    res.rank.s1  <- res.rank.s2  <- res.rank.s3  <- rep(NA, nrow(df.temp))
    res.group.s1 <- res.group.s2 <- res.group.s3 <- rep(NA, nrow(df.temp))
    res.wsum.s1  <- res.wsum.s2  <- res.wsum.s3  <- rep(NA, nrow(df.temp))
    rank.calc.s1 <- rep(NA, length(id.calc.s1)); rank.calc.s2 <- rep(NA, length(id.calc.s2)); rank.calc.s3 <- rep(NA, length(id.calc.s3))
    
    ###############
    # Scoring
    ###############
    # Stage 1
    res.s1    <- dm.sbm(df.temp[id.calc.s1, id.x.s1 - 11], 
                        cbind(df.temp[id.calc.s1, 6:7] + 1, 
                              max(df.temp[id.calc.s1, id.y.s1[3] - 11]) + 1 - df.temp[id.calc.s1, id.y.s1[3] - 11]), 
                        "drs", "o")
    res.s1.se <- dm.sbm(df.temp[id.calc.s1, id.x.s1 - 11], 
                        cbind(df.temp[id.calc.s1, 6:7] + 1, 
                              max(df.temp[id.calc.s1, id.y.s1[3] - 11]) + 1 - df.temp[id.calc.s1, id.y.s1[3] - 11]), 
                        "drs", "o", se = T)
    
    # Stage 2
    res.s2    <- dm.sbm(df.temp[id.calc.s2, id.x.s2 - 11], 
                        df.temp[id.calc.s2, id.y.s2 - 11],
                        "drs", "o")
    res.s2.se <- dm.sbm(df.temp[id.calc.s2, id.x.s2 - 11], 
                        df.temp[id.calc.s2, id.y.s2 - 11],
                        "drs", "o", se = T)
    
    # Stage 3
    res.s3    <- dm.sbm(df.temp[id.calc.s3, id.x.s3 - 11], 
                        cbind(df.temp[id.calc.s3, 12], 
                              max(df.temp[id.calc.s3, 13]) + 1 - df.temp[id.calc.s3, 13],
                              max(df.temp[id.calc.s3, 14]) + 1 - df.temp[id.calc.s3, 14]),
                        "drs", "o")
    res.s3.se <- dm.sbm(df.temp[id.calc.s3, id.x.s3 - 11], 
                        cbind(df.temp[id.calc.s3, 12], 
                              max(df.temp[id.calc.s3, 13]) + 1 - df.temp[id.calc.s3, 13],
                              max(df.temp[id.calc.s3, 14]) + 1 - df.temp[id.calc.s3, 14]),
                        "drs", "o", se = T)
    
    # temporal treatment for -Inf
    res.s1.se$eff[res.s1.se$eff == -Inf] <- 1
    res.s2.se$eff[res.s2.se$eff == -Inf] <- 1
    res.s3.se$eff[res.s3.se$eff == -Inf] <- 1
    
    # Score
    res.score.s1[id.calc.s1] <- res.s1$eff * res.s1.se$eff^0.1
    res.score.s2[id.calc.s2] <- res.s2$eff * res.s2.se$eff^0.1
    res.score.s3[id.calc.s3] <- res.s3$eff * res.s3.se$eff^0.1
    
    # Zero score
    res.score.s1[id.excd.s1] <- 0
    res.score.s2[id.excd.s2] <- 0
    res.score.s3[id.excd.s3] <- 0
    
    
    #################
    # Ranking 
    #################
    # Stage 1
    # XS(<Q1) vs S(<Q2) vs M(<Q3) vs L
    w.s1.ef.ws <- -c(0.5, 0.5) * c(100, 10); w.s1.y1.y2 <- c(0.7, 0.3)
    ts.ys.xs <- summary(rowSums(df.temp[id.calc.s1, 6:7]))[2]
    ts.ys.s  <- summary(rowSums(df.temp[id.calc.s1, 6:7]))[3]
    ts.ys.m  <- summary(rowSums(df.temp[id.calc.s1, 6:7]))[5]
    id.ys.xs <- which(rowSums(df.temp[id.calc.s1, 6:7]) <  ts.ys.xs)
    id.ys.s  <- which(rowSums(df.temp[id.calc.s1, 6:7]) >= ts.ys.xs & rowSums(df.temp[id.calc.s1, 6:7]) < ts.ys.s)
    id.ys.m  <- which(rowSums(df.temp[id.calc.s1, 6:7]) >= ts.ys.s  & rowSums(df.temp[id.calc.s1, 6:7]) < ts.ys.m)
    id.xs <- id.ys.xs; id.s <- id.ys.s; id.m <- id.ys.m; id.l <- setdiff(c(1:length(id.calc.s1)), c(id.xs, id.s, id.m))
    if(length(id.xs) > 0) rank.calc.s1[id.xs] <- round(rank(res.score.s1[id.calc.s1][id.xs] * w.s1.ef.ws[1] + apply(df.temp[id.calc.s1,][id.xs, 6:7], 2, function(x) if(length(id.xs) == 1) x else nor(x)) %*% w.s1.y1.y2 * w.s1.ef.ws[2])) + length(id.l) + length(id.m) + length(id.s)
    if(length(id.s ) > 0) rank.calc.s1[id.s ] <- round(rank(res.score.s1[id.calc.s1][id.s ] * w.s1.ef.ws[1] + apply(df.temp[id.calc.s1,][id.s , 6:7], 2, function(x) if(length(id.s ) == 1) x else nor(x)) %*% w.s1.y1.y2 * w.s1.ef.ws[2])) + length(id.l) + length(id.m)
    if(length(id.m ) > 0) rank.calc.s1[id.m ] <- round(rank(res.score.s1[id.calc.s1][id.m ] * w.s1.ef.ws[1] + apply(df.temp[id.calc.s1,][id.m , 6:7], 2, function(x) if(length(id.m ) == 1) x else nor(x)) %*% w.s1.y1.y2 * w.s1.ef.ws[2])) + length(id.l)
    if(length(id.l ) > 0) rank.calc.s1[id.l ] <- round(rank(res.score.s1[id.calc.s1][id.l ] * w.s1.ef.ws[1] + apply(df.temp[id.calc.s1,][id.l , 6:7], 2, function(x) if(length(id.l ) == 1) x else nor(x)) %*% w.s1.y1.y2 * w.s1.ef.ws[2]))
    res.group.s1[id.calc.s1][id.xs] <- "Q1"; res.group.s1[id.calc.s1][id.s]  <- "Q2"; res.group.s1[id.calc.s1][id.m]  <- "Q3"; res.group.s1[id.calc.s1][id.l]  <- "Q4"
    if(length(id.xs) > 0) res.wsum.s1[id.calc.s1][id.xs] <- apply(df.temp[id.calc.s1,][id.xs, 6:7], 2, function(x) if(length(id.xs) == 1) x else nor(x)) %*% w.s1.y1.y2
    if(length(id.s ) > 0) res.wsum.s1[id.calc.s1][id.s ] <- apply(df.temp[id.calc.s1,][id.s , 6:7], 2, function(x) if(length(id.s ) == 1) x else nor(x)) %*% w.s1.y1.y2
    if(length(id.m ) > 0) res.wsum.s1[id.calc.s1][id.m ] <- apply(df.temp[id.calc.s1,][id.m , 6:7], 2, function(x) if(length(id.m ) == 1) x else nor(x)) %*% w.s1.y1.y2
    if(length(id.l ) > 0) res.wsum.s1[id.calc.s1][id.l ] <- apply(df.temp[id.calc.s1,][id.l , 6:7], 2, function(x) if(length(id.l ) == 1) x else nor(x)) %*% w.s1.y1.y2
    
    # Stage 2
    # XS(<Q1) vs S(<Q2) vs M(<Q3) vs L
    w.s2.ef.ws <- -c(0.5, 0.5) * c(100, 10); w.s2.y1.y2 <- c(0.3, 0.7)
    ny.sum   <- data.frame(apply(df.temp[id.calc.s2, 9:10], 2, function(x) nor(x)), nor(df.temp[id.calc.s2, 9]) * w.s2.y1.y2[1] + nor(df.temp[id.calc.s2, 10]) * w.s2.y1.y2[2])
    ts.ny.xs <- summary(ny.sum[,3])[2]
    ts.ny.s  <- summary(ny.sum[,3])[3]
    ts.ny.m  <- summary(ny.sum[,3])[5]
    id.ny.xs <- which(ny.sum[,3] < ts.ny.xs)
    id.ny.s  <- which(ny.sum[,3] >= ts.ny.xs & ny.sum[,3] < ts.ny.s)
    id.ny.m  <- which(ny.sum[,3] >= ts.ny.s  & ny.sum[,3] < ts.ny.m)
    id.ny.l  <- which(ny.sum[,3] >= ts.ny.m)
    if(length(id.ny.xs) > 0) rank.calc.s2[id.ny.xs] <- round(rank(res.score.s2[id.calc.s2][id.ny.xs] * w.s2.ef.ws[1] + as.matrix(ny.sum[id.ny.xs, 1:2]) %*% w.s2.y1.y2 * w.s2.ef.ws[2])) + length(id.ny.l) + length(id.ny.m) + length(id.ny.s)
    if(length(id.ny.s ) > 0) rank.calc.s2[id.ny.s ] <- round(rank(res.score.s2[id.calc.s2][id.ny.s ] * w.s2.ef.ws[1] + as.matrix(ny.sum[id.ny.s , 1:2]) %*% w.s2.y1.y2 * w.s2.ef.ws[2])) + length(id.ny.l) + length(id.ny.m)
    if(length(id.ny.m ) > 0) rank.calc.s2[id.ny.m ] <- round(rank(res.score.s2[id.calc.s2][id.ny.m ] * w.s2.ef.ws[1] + as.matrix(ny.sum[id.ny.m , 1:2]) %*% w.s2.y1.y2 * w.s2.ef.ws[2])) + length(id.ny.l)
    if(length(id.ny.l ) > 0) rank.calc.s2[id.ny.l ] <- round(rank(res.score.s2[id.calc.s2][id.ny.l ] * w.s2.ef.ws[1] + as.matrix(ny.sum[id.ny.l , 1:2]) %*% w.s2.y1.y2 * w.s2.ef.ws[2]))
    res.group.s2[id.calc.s2][id.ny.xs] <- "Q1"; res.group.s2[id.calc.s2][id.ny.s] <- "Q2"; res.group.s2[id.calc.s2][id.ny.m] <- "Q3"; res.group.s2[id.calc.s2][id.ny.l] <- "Q4"
    if(length(id.ny.xs) > 0) res.wsum.s2[id.calc.s2][id.ny.xs] <- as.matrix(ny.sum[id.ny.xs, 1:2]) %*% w.s2.y1.y2
    if(length(id.ny.s ) > 0) res.wsum.s2[id.calc.s2][id.ny.s ] <- as.matrix(ny.sum[id.ny.s , 1:2]) %*% w.s2.y1.y2
    if(length(id.ny.m ) > 0) res.wsum.s2[id.calc.s2][id.ny.m ] <- as.matrix(ny.sum[id.ny.m , 1:2]) %*% w.s2.y1.y2
    if(length(id.ny.l ) > 0) res.wsum.s2[id.calc.s2][id.ny.l ] <- as.matrix(ny.sum[id.ny.l , 1:2]) %*% w.s2.y1.y2
    
    # Stage 3
    # XS(<Q1) vs S(<Q2) vs M(<Q3) vs L
    w.s3.ef.ws <- -c(0.5, 0.5) * c(100, 10); w.s3.y1.y2 <- c(0.3, 0.7)
    ny       <- data.frame(nor(df.temp[id.calc.s3, 12]), nor(df.temp[id.calc.s3, 12]/df.temp[id.calc.s3, 11]))
    ts.ny.xs <- summary(ny[,1])[2]
    ts.ny.s  <- summary(ny[,1])[3]
    ts.ny.m  <- summary(ny[,1])[5]
    id.ny.xs <- which(ny[,1] < ts.ny.xs)
    id.ny.s  <- which(ny[,1] >= ts.ny.xs & ny[,1] < ts.ny.s)
    id.ny.m  <- which(ny[,1] >= ts.ny.s  & ny[,1] < ts.ny.m)
    id.ny.l  <- which(ny[,1] >= ts.ny.m)
    if(length(id.ny.xs) > 0) rank.calc.s3[id.ny.xs] <- round(rank(res.score.s3[id.calc.s3][id.ny.xs] * w.s3.ef.ws[1] + as.matrix(ny[id.ny.xs,]) %*% w.s3.y1.y2 * w.s3.ef.ws[2])) + length(id.ny.l) + length(id.ny.m) + length(id.ny.s)
    if(length(id.ny.s ) > 0) rank.calc.s3[id.ny.s ] <- round(rank(res.score.s3[id.calc.s3][id.ny.s ] * w.s3.ef.ws[1] + as.matrix(ny[id.ny.s, ]) %*% w.s3.y1.y2 * w.s3.ef.ws[2])) + length(id.ny.l) + length(id.ny.m)
    if(length(id.ny.m ) > 0) rank.calc.s3[id.ny.m ] <- round(rank(res.score.s3[id.calc.s3][id.ny.m ] * w.s3.ef.ws[1] + as.matrix(ny[id.ny.m, ]) %*% w.s3.y1.y2 * w.s3.ef.ws[2])) + length(id.ny.l)
    if(length(id.ny.l ) > 0) rank.calc.s3[id.ny.l ] <- round(rank(res.score.s3[id.calc.s3][id.ny.l ] * w.s3.ef.ws[1] + as.matrix(ny[id.ny.l, ]) %*% w.s3.y1.y2 * w.s3.ef.ws[2]))
    res.group.s3[id.calc.s3][id.ny.xs] <- "Q1"; res.group.s3[id.calc.s3][id.ny.s] <- "Q2"; res.group.s3[id.calc.s3][id.ny.m] <- "Q3"; res.group.s3[id.calc.s3][id.ny.l] <- "Q4"
    if(length(id.ny.xs) > 0) res.wsum.s3[id.calc.s3][id.ny.xs] <- as.matrix(ny[id.ny.xs,]) %*% w.s3.y1.y2
    if(length(id.ny.s ) > 0) res.wsum.s3[id.calc.s3][id.ny.s ] <- as.matrix(ny[id.ny.s, ]) %*% w.s3.y1.y2
    if(length(id.ny.m ) > 0) res.wsum.s3[id.calc.s3][id.ny.m ] <- as.matrix(ny[id.ny.m, ]) %*% w.s3.y1.y2
    if(length(id.ny.l ) > 0) res.wsum.s3[id.calc.s3][id.ny.l ] <- as.matrix(ny[id.ny.l, ]) %*% w.s3.y1.y2
    
    # NaN to 50
    res.wsum.s1[is.nan(res.wsum.s1)] <- res.wsum.s2[is.nan(res.wsum.s2)] <- res.wsum.s3[is.nan(res.wsum.s3)] <- 50
    
    # Rank
    res.rank.s1[id.excd.s1] <- res.rank.s2[id.excd.s2] <- res.rank.s3[id.excd.s3] <- nrow(df.temp)
    res.rank.s1[id.calc.s1] <- rank.calc.s1
    res.rank.s2[id.calc.s2] <- rank.calc.s2
    res.rank.s3[id.calc.s3] <- rank.calc.s3
    
    # Summary
    res.all.m.br <- rbind(res.all.m.br, data.frame(Closed.m   = m,
                                                   B.unit     = sapply(df.temp[,1], function(x) df.fp.mw[which(df.fp.mw[,5] == x)[1], 4]),
                                                   Br.id      = df.temp[,1],
                                                   Br.type    = i,
                                                   df.temp[,-1],
                                                   Group.s1   = res.group.s1,
                                                   E.score.s1 = res.score.s1,
                                                   W.sum.s1   = res.wsum.s1,
                                                   Rank.s1    = res.rank.s1,
                                                   Group.s2   = res.group.s2,
                                                   E.score.s2 = res.score.s2,
                                                   W.sum.s2   = res.wsum.s2,
                                                   Rank.s2    = res.rank.s2,
                                                   Group.s3   = res.group.s3,
                                                   E.score.s3 = res.score.s3,
                                                   W.sum.s3   = res.wsum.s3,
                                                   Rank.s3    = res.rank.s3))
  }
  res.all.br <- rbind(res.all.br, res.all.m.br)
}

write.csv(res.all.br, file = "res.br.csv")


#########################################################################################################################
### Malmquist Analysis - Branch
#########################################################################################################################

# Branches lasted for 7 months
id.7       <- which(apply(table(df.eff.sa[,1], df.eff.sa[,5]) > 0, 2, function(x) sum(x) == 7) == T)
df.7       <- subset(df.eff.sa, df.eff.sa[,5] %in% names(id.7))
df.br.7.2d <- aggregate(df.7[,c(id.x.s1, id.y.s1, id.y.s2)], list(df.7[,1], df.7[,5]), sum)
df.tot.mg  <- aggregate(df.7[,20], list(df.7[,1], df.7[,5]), mean); df.br.7.2d[,10] <- df.tot.mg$x
df.br.7.3d <- simplify2array(by(df.br.7.2d, df.br.7.2d[,1], as.matrix))

# Branch type: 1(incu), 2(nor.sm), 3(nor.lg)
br.type <- c()
for(i in 1:length(id.7)){
  if(df.7[which(df.7[,5] == names(id.7)[i]), 6][1] == 1){
    br.type <- c(br.type, 1)
  }else if(df.7[which(df.7[,5] == names(id.7)[i]), 8][1] == 1){
    br.type <- c(br.type, 2)
  }else{
    br.type <- c(br.type, 3)
  }
}
names(br.type) <- names(id.7)

# Parameters
id.x.s1.m   <- id.x.s1 - 10
id.y.s1.m   <- id.y.s1 - 10
id.x.s2.m   <- id.y.s1.m[1:2]
id.y.s2.m   <- id.y.s2 - 10
tm          <- c(paste0("2020.0", 6:9), paste0("2020.", 10:12))
dm.s1       <- "sf"
dm.s2       <- "dea"
rts.s1      <- "vrs" 
rts.s2      <- "crs" # "drs"
orientation <- "o"
g.m         <- df.br.7.3d[,c(id.x.s1.m, id.y.s1.m),]; g.m[,id.x.s1.m - 2,] <- 0
wd          <- c(0, 0, 1)

# Special treatment for zero input in stage 2
df.br.7.3d[6, id.x.s2.m, 2][1] <- df.br.7.3d[6, id.x.s2.m, 1][1]

# Run Malmquist
# Incu: 5
# res.mq.s1.inc <- roc.malmquist(df.br.7.3d[br.type == 1, id.x.s1.m,], df.br.7.3d[br.type == 1, id.y.s1.m,], tm, dm.s1, rts.s1, g = g.m[br.type == 1,,], wd = wd)
# res.mq.s2.inc <- roc.malmquist(df.br.7.3d[br.type == 1, id.x.s2.m,], df.br.7.3d[br.type == 1, id.y.s2.m,], tm, dm.s2, rts.s2, orientation)

# Nor.sm: 29
# res.mq.s1.nsm <- roc.malmquist(df.br.7.3d[br.type == 2, id.x.s1.m,], df.br.7.3d[br.type == 2, id.y.s1.m,], tm, dm.s1, rts.s1, g = g.m[br.type == 2,,], wd = wd)
# res.mq.s2.nsm <- roc.malmquist(df.br.7.3d[br.type == 2, id.x.s2.m,], df.br.7.3d[br.type == 2, id.y.s2.m,], tm, dm.s2, rts.s2, orientation)

# Alternate model via SBM
ceiling       <- max(df.br.7.3d[br.type == 2, 9, ]) + 2
df.y.temp     <- abind(df.br.7.3d[br.type == 2, id.y.s1.m,], array(ceiling - df.br.7.3d[br.type == 2, 9,], c(29, 1, 7)), along = 2)
res.mq.s1.nsm <- roc.malmquist(df.br.7.3d[br.type == 2, id.x.s1.m,], df.y.temp[,-3,], tm, "sbm", "vrs")
res.mq.s2.nsm <- roc.malmquist(df.br.7.3d[br.type == 2, id.x.s2.m,], df.br.7.3d[br.type == 2, id.y.s2.m,], tm, "sbm", "vrs")

# Nor.lg: 6
# res.mq.s1.nlg <- roc.malmquist(df.br.7.3d[br.type == 3, id.x.s1.m,], df.br.7.3d[br.type == 3, id.y.s1.m,], tm, dm.s1, rts.s1, g = g.m[br.type == 3,,], wd = wd)
# res.mq.s2.nlg <- roc.malmquist(df.br.7.3d[br.type == 3, id.x.s2.m,], df.br.7.3d[br.type == 3, id.y.s2.m,], tm, dm.s2, rts.s2, orientation)

# Visualize the results
res.mq.s1.nsm.avg <- data.frame(Period = unique(res.mq.s1.nsm$cu$Period),
                                CU     = aggregate(res.mq.s1.nsm$cu$CU, list(res.mq.s1.nsm$cu$Period), mean)$x,
                                FS     = aggregate(res.mq.s1.nsm$fs$FS, list(res.mq.s1.nsm$fs$Period), mean)$x,
                                MI     = aggregate(res.mq.s1.nsm$mi$MI, list(res.mq.s1.nsm$mi$Period), mean)$x)

res.mq.s2.nsm.avg <- data.frame(Period = unique(res.mq.s2.nsm$cu$Period),
                                CU     = aggregate(res.mq.s2.nsm$cu$CU, list(res.mq.s2.nsm$cu$Period), mean)$x,
                                FS     = aggregate(res.mq.s2.nsm$fs$FS, list(res.mq.s2.nsm$fs$Period), mean)$x,
                                MI     = aggregate(res.mq.s2.nsm$mi$MI, list(res.mq.s2.nsm$mi$Period), mean)$x)

# DMU of interest
data.frame(DMU   = 1:length(names(id.7)[which(br.type == 2)]),
           Br.No = names(id.7)[which(br.type == 2)])
id.doi.s1 <- 7:11
df.s1.fs  <- data.frame(subset(res.mq.s1.nsm$fs, DMU %in% id.doi.s1),
                        Name = names(id.7)[which(br.type == 2)][id.doi.s1])
df.s1.mi  <- data.frame(subset(res.mq.s1.nsm$mi, DMU %in% id.doi.s1),
                        Name = names(id.7)[which(br.type == 2)][id.doi.s1])
id.doi.s2 <- c(21)
df.s2.fs  <- data.frame(subset(res.mq.s2.nsm$fs, DMU %in% id.doi.s2),
                        Name = names(id.7)[which(br.type == 2)][id.doi.s2])
df.s2.mi  <- data.frame(subset(res.mq.s2.nsm$mi, DMU %in% id.doi.s2),
                        Name = names(id.7)[which(br.type == 2)][id.doi.s2])


############################################## Stage 1 ##############################################

# FS
ggplot(data = res.mq.s1.nsm$fs, aes(x = Period, y = FS)) + 
  geom_point(alpha = 0.2, size = 1.2) + theme_bw() + geom_hline(yintercept = 1.0, color = "red", size = 0.5) +
  geom_line(data = res.mq.s1.nsm.avg, aes(x = Period, y = FS, group = 1), size = 1) +
  geom_line(data = df.s1.fs, aes(x = Period, y = FS, group = Name, color = Name), size = 1.5) + 
  scale_colour_manual(values = c("blueviolet")) +
  #scale_colour_manual(values = c("orange")) +
  #scale_colour_manual(values = c("limegreen")) +
  #scale_colour_manual(values = c("royalblue")) +
  scale_y_continuous(name = "Frontier Shift (Stage 1)") +
  #scale_y_continuous(name = "Malmquist Index (MI)", limits = c(0.85, 1.4), breaks = seq(0.85, 1.4, 0.2)) +
  theme(axis.title.x         = element_text(size = 14, colour = "gray35"),
        axis.title.y         = element_text(size = 14, colour = "gray35"),
        legend.title         = element_blank(),
        legend.background    = element_rect(fill = "transparent", colour = "transparent"), 
        legend.direction     = "horizontal", 
        legend.justification = c(1, 1), legend.position = c(0.95, 0.993))
# MI
ggplot(data = res.mq.s1.nsm$mi, aes(x = Period, y = MI)) + 
  geom_point(alpha = 0.2, size = 1.2) + theme_bw() + geom_hline(yintercept = 1.0, color = "red", size = 0.5) +
  geom_line(data = res.mq.s1.nsm.avg, aes(x = Period, y = MI, group = 1), size = 1) +
  geom_line(data = df.s1.mi, aes(x = Period, y = MI, group = Name, color = Name), size = 1.5) + 
  scale_colour_manual(values = c("blueviolet")) +
  #scale_colour_manual(values = c("orange")) +
  #scale_colour_manual(values = c("limegreen")) +
  #scale_colour_manual(values = c("royalblue")) +
  scale_y_continuous(name = "Malmquist Index (Stage 1)") +
  #scale_y_continuous(name = "Malmquist Index (MI)", limits = c(0.85, 1.4), breaks = seq(0.85, 1.4, 0.2)) +
  theme(axis.title.x         = element_text(size = 14, colour = "gray35"),
        axis.title.y         = element_text(size = 14, colour = "gray35"),
        legend.title         = element_blank(),
        legend.background    = element_rect(fill = "transparent", colour = "transparent"), 
        legend.direction     = "horizontal", 
        legend.justification = c(1, 1), legend.position = c(0.95, 0.993))


############################################## Stage 2 ##############################################
# FS
ggplot(data = res.mq.s2.nsm$fs, aes(x = Period, y = FS)) + 
  geom_point(alpha = 0.2, size = 1.2) + theme_bw() + geom_hline(yintercept = 1.0, color = "red", size = 0.5) +
  geom_line(data = res.mq.s2.nsm.avg, aes(x = Period, y = FS, group = 1), size = 1) +
  geom_line(data = df.s2.fs, aes(x = Period, y = FS, group = Name, color = Name), size = 1.5) + 
  scale_colour_manual(values = c("mediumslateblue")) +
  #scale_colour_manual(values = c("tomato")) +
  #scale_colour_manual(values = c("darkturquoise")) +
  scale_y_continuous(name = "Frontier Shift (Stage 1)") +
  #scale_y_continuous(name = "Malmquist Index (MI)", limits = c(0.85, 1.4), breaks = seq(0.85, 1.4, 0.2)) +
  theme(axis.title.x         = element_text(size = 14, colour = "gray35"),
        axis.title.y         = element_text(size = 14, colour = "gray35"),
        legend.title         = element_blank(),
        legend.background    = element_rect(fill = "transparent", colour = "transparent"), 
        legend.direction     = "horizontal", 
        legend.justification = c(1, 1), legend.position = c(0.95, 0.993))

# MI
ggplot(data = res.mq.s2.nsm$mi, aes(x = Period, y = MI)) + 
  geom_point(alpha = 0.2, size = 1.2) + theme_bw() + geom_hline(yintercept = 1.0, color = "red", size = 0.5) +
  geom_line(data = res.mq.s2.nsm.avg, aes(x = Period, y = MI, group = 1), size = 1) + 
  geom_line(data = df.s2.mi, aes(x = Period, y = MI, group = Name, color = Name), size = 1.5) + 
  scale_colour_manual(values = c("mediumslateblue")) +
  #scale_colour_manual(values = c("tomato")) +
  #scale_colour_manual(values = c("darkturquoise")) +
  scale_y_continuous(name = "Malmquist Index (Stage 2)") +
  #scale_y_continuous(name = "Malmquist Index (MI)", limits = c(0.85, 1.4), breaks = seq(0.85, 1.4, 0.2)) +
  theme(axis.title.x         = element_text(size = 14, colour = "gray35"),
        axis.title.y         = element_text(size = 14, colour = "gray35"),
        legend.title         = element_blank(),
        legend.background    = element_rect(fill = "transparent", colour = "transparent"), 
        legend.direction     = "horizontal", 
        legend.justification = c(1, 1), legend.position = c(0.95, 0.993))


# FS
# ggplot(data = res.mq.s2.nsm$fs, aes(x = Period, y = 1/FS)) + 
#   geom_point(alpha = 0.2, size = 1.2) + theme_bw() + geom_hline(yintercept = 1.0, color = "red", size = 0.5) +
#   geom_line(data = res.mq.s2.nsm.avg, aes(x = Period, y = 1/FS, group = 1), size = 1) +
#   geom_line(data = df.s2.fs, aes(x = Period, y = 1/FS, group = Name, color = Name), size = 1.2) + 
#   scale_y_continuous(name = "Frontier Shift (Stage 1)") +
#   #scale_y_continuous(name = "Malmquist Index (MI)", limits = c(0.85, 1.4), breaks = seq(0.85, 1.4, 0.2)) +
#   theme(axis.title.x         = element_text(size = 14, colour = "gray35"),
#         axis.title.y         = element_text(size = 14, colour = "gray35"),
#         legend.title         = element_blank(),
#         legend.background    = element_rect(fill = "transparent", colour = "transparent"), 
#         legend.direction     = "horizontal", 
#         legend.justification = c(1, 1), legend.position = c(0.95, 0.993))
# 
# MI
# ggplot(data = res.mq.s2.nsm$mi, aes(x = Period, y = 1/MI)) + 
#   geom_point(alpha = 0.2, size = 1.2) + theme_bw() + geom_hline(yintercept = 1.0, color = "red", size = 0.5) +
#   geom_line(data = res.mq.s2.nsm.avg, aes(x = Period, y = 1/MI, group = 1), size = 1) + 
#   geom_line(data = df.s2.mi, aes(x = Period, y = 1/MI, group = Name, color = Name), size = 1.2) + 
#   scale_y_continuous(name = "Malmquist Index (Stage 2)") +
#   #scale_y_continuous(name = "Malmquist Index (MI)", limits = c(0.85, 1.4), breaks = seq(0.85, 1.4, 0.2)) +
#   theme(axis.title.x         = element_text(size = 14, colour = "gray35"),
#         axis.title.y         = element_text(size = 14, colour = "gray35"),
#         legend.title         = element_blank(),
#         legend.background    = element_rect(fill = "transparent", colour = "transparent"), 
#         legend.direction     = "horizontal", 
#         legend.justification = c(1, 1), legend.position = c(0.95, 0.993))


# Investigation with SF/DEA
# from <- 202010
# to   <- from + 1
# name <- 6481
# df.from <- subset(df.br.7.2d, Group.1 == from)[br.type == 2,]
# df.to   <- subset(df.br.7.2d, Group.1 == to  )[br.type == 2,]
# 
# df.br.7.3d[which(df.br.7.3d[,2,1] == name),,(from - 202005):(to - 202005)]
# 100/(dm.sf(df.from[,id.x.s1.m], df.from[,id.y.s1.m], rts.s1, g.m[br.type == 2,,from - 202005], wd)$eff[which(df.from$Group.2 == name)] + 1)
# 100/(dm.sf(df.to  [,id.x.s1.m], df.to  [,id.y.s1.m], rts.s1, g.m[br.type == 2,,to   - 202005], wd)$eff[which(df.to  $Group.2 == name)] + 1)
# 
# 100/(dm.dea(df.from[,id.x.s2.m], df.from[,id.y.s2.m], rts.s2, orientation)$eff[which(df.from$Group.2 == name)])
# 100/(dm.dea(df.to  [,id.x.s2.m], df.to  [,id.y.s2.m], rts.s2, orientation)$eff[which(df.to  $Group.2 == name)])
# 
# Investigation with SBM
# Stage 1
cbind(res.mq.s1.nsm$cu, FS = res.mq.s1.nsm$fs$FS, MI = res.mq.s1.nsm$mi$MI, BR.no = names(id.7)[which(br.type == 2)])
name <- 4281; from <- 202006; to <- from + 1; id <- which(names(id.7)[which(br.type == 2)] == name)
name <- 6403; from <- 202007; to <- from + 1; id <- which(names(id.7)[which(br.type == 2)] == name)
name <- 7015; from <- 202009; to <- from + 1; id <- which(names(id.7)[which(br.type == 2)] == name)

df.from <- cbind(df.br.7.3d[br.type == 2, id.x.s1.m, from - 202005], df.y.temp[,-3, from - 202005])
df.to   <- cbind(df.br.7.3d[br.type == 2, id.x.s1.m, to   - 202005], df.y.temp[,-3, to   - 202005])

dm.sbm(df.from[,1:4], df.from[,5:7], "vrs")$eff[id] * 100
dm.sbm(df.to  [,1:4], df.to  [,5:7], "vrs")$eff[id] * 100
round(df.br.7.3d[which(df.br.7.3d[,2,1] == name),,(from - 202005):(to - 202005)])
cbind(FS = df.s1.fs[from - 202005, 3], MI = df.s1.mi[from - 202005, 3])

# Stage 2
cbind(res.mq.s2.nsm$cu, FS = res.mq.s2.nsm$fs$FS, MI = res.mq.s2.nsm$mi$MI, BR.no = names(id.7)[which(br.type == 2)])
name <- 1708; from <- 202010; to <- from + 1; id <- which(names(id.7)[which(br.type == 2)] == name)
name <- 2305; from <- 202007; to <- from + 1; id <- which(names(id.7)[which(br.type == 2)] == name)

name <- 7052; from <- 202010; to <- from + 1; id <- which(names(id.7)[which(br.type == 2)] == name)


df.from <- df.br.7.3d[br.type == 2, c(id.x.s2.m, id.y.s2.m), from - 202005]
df.to   <- df.br.7.3d[br.type == 2, c(id.x.s2.m, id.y.s2.m), to   - 202005]

dm.sbm(df.from[,1:2], df.from[,3:4], "vrs")$eff[id] * 100
dm.sbm(df.to  [,1:2], df.to  [,3:4], "vrs")$eff[id] * 100
round(df.br.7.3d[which(df.br.7.3d[,2,1] == name), c(id.x.s2.m, id.y.s2.m),(from - 202005):(to - 202005)])
cbind(FS = df.s2.fs[from - 202005, 3], MI = df.s2.mi[from - 202005, 3])
