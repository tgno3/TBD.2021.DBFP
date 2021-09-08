#########################################################################################################################
### Project  : FP Performance Evaluation
### Script   : DBFP on GIT.R
### Contents : FP Performance Evaluation Model - Two stages
#########################################################################################################################

#########################################################################################################################
### Setting up environment
#########################################################################################################################

# Load library
pkgs <- c("DJL", "ggplot2", "abind")
sapply(pkgs, require, character.only = T)

# Load data & parameters
load("DBFP.2003.2012.Rdata")

# Switch for MW vs SA
e.type <- "mw"


#########################################################################################################################
### Data cleansing
#########################################################################################################################

# IDs for exclusion
id.out.level   <- which( is.na(match(df.raw[,3], c(100, 101, 102, 109, 112))))
id.out.branch  <- which(!is.na(match(df.raw[,5], c(8262, 8312, 8335, 8354))))
id.out.bunit   <- which(!is.na(match(df.raw[,4], c(74, NA))))
id.out.month   <- which(df.raw[,10] %in% c(1, 2, 3))
#id.out.zero.s1 <- which(rowSums(df.raw[,c(17, 18)]) == 0)

# Effective data
id.out.all <- unique(c(id.out.level, id.out.branch, id.out.bunit, id.out.month))
df.eff.mw  <- df.raw[-id.out.all,][order(df.raw[-id.out.all,][,1], df.raw[-id.out.all,][,2]),]

# DF for sliding average
df.eff.sa <- data.frame()
for(i in 202006:202012){
  id.eff <- c()
  for(j in df.eff.mw[which(df.eff.mw[,1] == i), 2]){
    if(sum(df.eff.mw[which(df.eff.mw[,1] %in% c(i - 2, i - 1)), 2] == j) == 2){
      id.eff <- c(id.eff, j)
    }else{
      next
    }
  }
  df.temp.cr <- subset(df.eff.mw, df.eff.mw[,1] == i       & df.eff.mw[,2] %in% id.eff)
  df.temp.p1 <- subset(df.eff.mw, df.eff.mw[,1] == (i - 1) & df.eff.mw[,2] %in% id.eff)
  df.temp.p2 <- subset(df.eff.mw, df.eff.mw[,1] == (i - 2) & df.eff.mw[,2] %in% id.eff)
  df.temp.cr <- df.temp.cr[order(df.temp.cr[,2]),]
  df.temp.p1 <- df.temp.p1[order(df.temp.p1[,2]),]
  df.temp.p2 <- df.temp.p2[order(df.temp.p2[,2]),]
  df.temp    <- df.temp.cr
  df.temp[,13:21] <- df.temp.cr[,13:21] + df.temp.p1[,13:21] + df.temp.p2[,13:21]
  df.eff.sa  <- rbind(df.eff.sa, df.temp)
}

# Average for comprehensive profit margin
df.eff.sa[,20] <- df.eff.sa[,20]/3

# Only consider the same FP for mw
#df.fp.mw <- df.fp.mw[df.fp.mw[,2] %in% df.fp.sa[,2],]

# IDs
id.x.s1 <- c(13:16)
id.y.s1 <- c(17:19)
id.x.s2 <- c(17:18)
id.y.s2 <- c(20:21)
id.x.s3 <- c(22)
id.y.s3 <- c(25, 37, 45)


#########################################################################################################################
### FP Evaluation
#########################################################################################################################

# Loop for all periods
res.all.m.s1.s2 <- res.all.m.s3 <- data.frame()
for(m in 202006:202012){
  
  # Data of evaluation
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
  res.all.s1.s2 <- res.all.s3 <- data.frame(); res.s1.l.mw <- res.s2.l.mw <- res.s1.l.sa <- res.s2.l.sa <- res.s2.l.sa.se <- list()
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
      df.temp.12 <- if(e.type == "mw") df.temp.mw else df.temp.sa
      id.calc.s1 <- which(apply(df.temp.12[,id.y.s1[1:2]], 1, function(x) sum(x) >  0))
      id.excd.s1 <- which(apply(df.temp.12[,id.y.s1[1:2]], 1, function(x) sum(x) == 0))
      id.calc.s2 <- which(apply(df.temp.12[,id.y.s2], 1, function(x) sum(x > 0) == 2))
      id.excd.s2 <- which(apply(df.temp.12[,id.y.s2], 1, function(x) sum(x > 0) <  2))
      id.calc.s3 <- which(df.temp.mw[,id.y.s3[1]] >  0)
      id.excd.s3 <- which(df.temp.mw[,id.y.s3[1]] <= 0)
  
      # Empty box for results
      res.score.s1   <- res.score.s2 <- res.rank.s1 <- res.rank.s2 <- res.group.s1 <- res.group.s2 <- res.wsum.s1 <- res.wsum.s2 <- rep(NA, nrow(df.temp.12))
      res.score.s3   <- res.rank.s3 <- res.group.s3 <- res.wsum.s3 <- rep(NA, nrow(df.temp.mw))
      rank.calc.s1   <- rep(NA, length(id.calc.s1))
      rank.calc.s2   <- rep(NA, length(id.calc.s2))
      rank.calc.s3   <- rep(NA, length(id.calc.s3))
      res.tar.sbm.s1 <- array(NA, c(nrow(df.temp.12), length(id.y.s1))); res.tar.sup.s1 <- array(NA, c(nrow(df.temp.12), 1 + length(id.x.s1) + length(id.y.s1)))
      res.tar.sbm.s2 <- array(NA, c(nrow(df.temp.12), length(id.y.s2))); res.tar.sup.s2 <- array(NA, c(nrow(df.temp.12), 1 + length(id.x.s2) + length(id.y.s2)))
      res.tar.sbm.s3 <- array(NA, c(nrow(df.temp.mw), length(id.y.s3))); res.tar.sup.s3 <- array(NA, c(nrow(df.temp.mw), 1 + length(id.x.s3) + length(id.y.s3)))
      
      
      ###############
      # Scoring
      ###############
      # Stage 1
      res.s1    <- dm.sbm(df.temp.12[id.calc.s1, id.x.s1], 
                          cbind(df.temp.12[id.calc.s1, id.y.s1[1:2]] + 1, 
                                max(df.temp.12[id.calc.s1, id.y.s1[3]]) + 1 - df.temp.12[id.calc.s1, id.y.s1[3]]), 
                          "drs", "o")
      res.s1.se <- dm.sbm(df.temp.12[id.calc.s1, id.x.s1], 
                          cbind(df.temp.12[id.calc.s1, id.y.s1[1:2]] + 1, 
                                max(df.temp.12[id.calc.s1, id.y.s1[3]]) + 1 - df.temp.12[id.calc.s1, id.y.s1[3]]), 
                          "drs", "o", se = T)
      
      # Stage 2
      res.s2    <- dm.sbm(df.temp.12[id.calc.s2, id.x.s2], 
                          df.temp.12[id.calc.s2, id.y.s2],
                          "drs", "o")
      res.s2.se <- dm.sbm(df.temp.12[id.calc.s2, id.x.s2], 
                          df.temp.12[id.calc.s2, id.y.s2],
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
      
      # temporal treatment for -Inf
      res.s1.se$eff[res.s1.se$eff == -Inf] <- 1
      res.s2.se$eff[res.s2.se$eff == -Inf] <- 1
      res.s3.se$eff[res.s3.se$eff == -Inf] <- 1
      
      # Score
      res.score.s1[id.calc.s1] <- res.s1$eff * res.s1.se$eff^0.1
      res.score.s2[id.calc.s2] <- res.s2$eff * res.s2.se$eff^0.1
      res.score.s3[id.calc.s3] <- res.s3$eff * res.s3.se$eff^0.1
  
      
      #################
      # Ranking 
      #################
      # Stage 1
      # XS(<Q1) vs S(<Q2) vs M(<Q3) vs L
      w.s1.ef.ws <- -c(0.5, 0.5) * c(100, 10); w.s1.y1.y2 <- c(0.7, 0.3)
      res.rank.s1[id.excd.s1] <- nrow(df.temp.12)
      ts.ys.xs <- summary(rowSums(df.temp.12[id.calc.s1, id.y.s1[1:2]]))[2]
      ts.ys.s  <- summary(rowSums(df.temp.12[id.calc.s1, id.y.s1[1:2]]))[3]
      ts.ys.m  <- summary(rowSums(df.temp.12[id.calc.s1, id.y.s1[1:2]]))[5]
      id.ys.xs <- which(rowSums(df.temp.12[id.calc.s1, id.y.s1[1:2]]) <  ts.ys.xs)
      id.ys.s  <- which(rowSums(df.temp.12[id.calc.s1, id.y.s1[1:2]]) >= ts.ys.xs & rowSums(df.temp.12[id.calc.s1, id.y.s1[1:2]]) < ts.ys.s)
      id.ys.m  <- which(rowSums(df.temp.12[id.calc.s1, id.y.s1[1:2]]) >= ts.ys.s  & rowSums(df.temp.12[id.calc.s1, id.y.s1[1:2]]) < ts.ys.m)
      id.xs <- id.ys.xs; id.s <- id.ys.s; id.m <- id.ys.m; id.l <- setdiff(c(1:length(id.calc.s1)), c(id.xs, id.s, id.m))
      if(length(id.xs) > 0) rank.calc.s1[id.xs] <- round(rank(res.score.s1[id.calc.s1][id.xs] * w.s1.ef.ws[1] + apply(df.temp.12[id.calc.s1,][id.xs, id.y.s1[1:2]], 2, function(x) if(length(id.xs) == 1) x else nor(x)) %*% w.s1.y1.y2 * w.s1.ef.ws[2])) + length(id.l) + length(id.m) + length(id.s)
      if(length(id.s ) > 0) rank.calc.s1[id.s ] <- round(rank(res.score.s1[id.calc.s1][id.s ] * w.s1.ef.ws[1] + apply(df.temp.12[id.calc.s1,][id.s , id.y.s1[1:2]], 2, function(x) if(length(id.s ) == 1) x else nor(x)) %*% w.s1.y1.y2 * w.s1.ef.ws[2])) + length(id.l) + length(id.m)
      if(length(id.m ) > 0) rank.calc.s1[id.m ] <- round(rank(res.score.s1[id.calc.s1][id.m ] * w.s1.ef.ws[1] + apply(df.temp.12[id.calc.s1,][id.m , id.y.s1[1:2]], 2, function(x) if(length(id.m ) == 1) x else nor(x)) %*% w.s1.y1.y2 * w.s1.ef.ws[2])) + length(id.l)
      if(length(id.l ) > 0) rank.calc.s1[id.l ] <- round(rank(res.score.s1[id.calc.s1][id.l ] * w.s1.ef.ws[1] + apply(df.temp.12[id.calc.s1,][id.l , id.y.s1[1:2]], 2, function(x) if(length(id.l ) == 1) x else nor(x)) %*% w.s1.y1.y2 * w.s1.ef.ws[2]))
      res.group.s1[id.calc.s1][id.xs] <- "Q1"; res.group.s1[id.calc.s1][id.s]  <- "Q2"; res.group.s1[id.calc.s1][id.m]  <- "Q3"; res.group.s1[id.calc.s1][id.l]  <- "Q4"
      if(length(id.xs) > 0) res.wsum.s1[id.calc.s1][id.xs] <- apply(df.temp.12[id.calc.s1,][id.xs, id.y.s1[1:2]], 2, function(x) if(length(id.xs) == 1) x else nor(x)) %*% w.s1.y1.y2
      if(length(id.s ) > 0) res.wsum.s1[id.calc.s1][id.s ] <- apply(df.temp.12[id.calc.s1,][id.s , id.y.s1[1:2]], 2, function(x) if(length(id.s ) == 1) x else nor(x)) %*% w.s1.y1.y2
      if(length(id.m ) > 0) res.wsum.s1[id.calc.s1][id.m ] <- apply(df.temp.12[id.calc.s1,][id.m , id.y.s1[1:2]], 2, function(x) if(length(id.m ) == 1) x else nor(x)) %*% w.s1.y1.y2
      if(length(id.l ) > 0) res.wsum.s1[id.calc.s1][id.l ] <- apply(df.temp.12[id.calc.s1,][id.l , id.y.s1[1:2]], 2, function(x) if(length(id.l ) == 1) x else nor(x)) %*% w.s1.y1.y2
      
      # Stage 2
      # XS(<Q1) vs S(<Q2) vs M(<Q3) vs L
      w.s2.ef.ws <- -c(0.5, 0.5) * c(100, 10); w.s2.y1.y2 <- c(0.3, 0.7)
      res.rank.s2[id.excd.s2] <- nrow(df.temp.12)
      ny.sum   <- data.frame(apply(df.temp.12[id.calc.s2, id.y.s2], 2, function(x) nor(x)), nor(df.temp.12[id.calc.s2, id.y.s2[1]]) * w.s2.y1.y2[1] + nor(df.temp.12[id.calc.s2, id.y.s2[2]]) * w.s2.y1.y2[2])
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
      res.rank.s3[id.excd.s3] <- nrow(df.temp.mw)
      ny       <- data.frame(nor(df.temp.mw[id.calc.s3, id.y.s3[1]]), nor(df.temp.mw[id.calc.s3, id.y.s3[1]]/df.temp.mw[id.calc.s3, id.x.s3]))
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
      
      # Rank
      res.rank.s1[id.calc.s1] <- rank.calc.s1
      res.rank.s2[id.calc.s2] <- rank.calc.s2
      res.rank.s3[id.calc.s3] <- rank.calc.s3
      
      # Zero score
      res.score.s1[id.excd.s1] <- 0
      res.score.s2[id.excd.s2] <- 0
      res.score.s3[id.excd.s3] <- 0
  
      
      #################
      # Targeting
      #################
      # Stage 1
      # SBM target
      tar.calc.sbm.s1 <- res.s1$lambda %*% as.matrix(cbind(df.temp.12[id.calc.s1, id.y.s1[1:2]] + 1, 
                                                     max(df.temp.12[id.calc.s1, id.y.s1[3]]) + 1 - df.temp.12[id.calc.s1, id.y.s1[3]]))
      res.tar.sbm.s1[id.calc.s1,] <- as.matrix(data.frame(ceiling(tar.calc.sbm.s1[,1:2]), floor(max(df.temp.12[id.calc.s1, id.y.s1[3]]) + 1 - round(tar.calc.sbm.s1[,3], 4))))
      res.tar.sbm.s1[which(res.rank.s1 == 1),] <- as.matrix(df.temp.12[id.calc.s1, id.y.s1][which(res.rank.s1[id.calc.s1] == 1),])
      
      # Scaled-up target
      id.tar.sup.s1 <- target.scale.up(df.temp.12[id.calc.s1, id.x.s1], df.temp.12[id.calc.s1, id.y.s1], res.score.s1[id.calc.s1]*100, res.rank.s1[id.calc.s1], 10, 3)$res.target
      res.tar.sup.s1[id.calc.s1,] <- as.matrix(df.temp.12[id.calc.s1, c(2, id.x.s1, id.y.s1)][id.tar.sup.s1,])
  
      # Stage 2
      # SBM target
      tar.calc.sbm.s2 <- res.s2$lambda %*% as.matrix(df.temp.12[id.calc.s2, id.y.s2])
      res.tar.sbm.s2[id.calc.s2,] <- as.matrix(tar.calc.sbm.s2)
      res.tar.sbm.s2[which(res.rank.s2 == 1),] <- as.matrix(df.temp.12[id.calc.s2, id.y.s2][which(res.rank.s2[id.calc.s2] == 1),])
      
      # Scaled-up target
      id.tar.sup.s2 <- target.scale.up(df.temp.12[id.calc.s2, id.x.s2], df.temp.12[id.calc.s2, id.y.s2], res.score.s2[id.calc.s2]*100, res.rank.s2[id.calc.s2], c(10^10, 10^6, 1, 1, 10^-2), efc = T)$res.target
      res.tar.sup.s2[id.calc.s2,] <- as.matrix(df.temp.12[id.calc.s2, c(2, id.x.s2, id.y.s2)][id.tar.sup.s2,])
  
      # View(cbind(df.temp.12[id.calc.s2, c(id.x.s2, id.y.s2)], res.score.s2[id.calc.s2], res.rank.s2[id.calc.s2], res.tar.sbm.s2[id.calc.s2,], res.tar.sup.s2[id.calc.s2,]))
      
      
      # Summary
      res.all.s1.s2 <- rbind(res.all.s1.s2, data.frame(Closed.m   = m,
                                                       B.unit     = df.temp.12[,4],
                                                       B.branch   = df.temp.12[,5],
                                                       FP.e.type  = i,
                                                       FP.m.type  = j,
                                                       FP.month   = df.temp.12[,10],
                                                       FP.id      = df.temp.12[,2],
                                                       df.temp.12[,c(id.x.s1, id.y.s1, id.y.s2)],
                                                       Group.s1   = res.group.s1,
                                                       E.score.s1 = res.score.s1,
                                                       W.sum.s1   = res.wsum.s1,
                                                       Rank.s1    = res.rank.s1,
                                                       Target.sbm = res.tar.sbm.s1,
                                                       Target.sup = res.tar.sup.s1,
                                                       Group.s2   = res.group.s2,
                                                       E.score.s2 = res.score.s2,
                                                       W.sum.s2   = res.wsum.s2,
                                                       Rank.s2    = res.rank.s2,
                                                       Target.sbm = res.tar.sbm.s2,
                                                       Target.sup = res.tar.sup.s2))
  
      res.all.s3 <- rbind(res.all.s3, data.frame(Closed.m   = m,
                                                 B.unit     = df.temp.mw[,4],
                                                 B.branch   = df.temp.mw[,5],
                                                 FP.type    = i,
                                                 FP.month   = j,
                                                 FP.month   = df.temp.mw[,10],
                                                 FP.id      = df.temp.mw[,2],
                                                 df.temp.mw[,c(id.x.s3, id.y.s3)],
                                                 Group.s3   = res.group.s3,
                                                 E.score.s3 = res.score.s3,
                                                 W.sum.s3   = res.wsum.s3,
                                                 Rank.s3    = res.rank.s3))
    }
  }
  res.all.m.s1.s2 <- rbind(res.all.m.s1.s2, res.all.s1.s2)
  res.all.m.s3    <- rbind(res.all.m.s3, res.all.s3)
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
      df.temp.12 <- if(e.type == "mw") df.temp.mw else df.temp.sa
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

# Data of evaluation
m        <- 202012
df.fp.mw <- df.eff.mw[df.eff.mw[,1] == m,]

# Data aggregation
id.v.all     <- c(id.x.s1, id.y.s1, id.y.s2)
id.inc       <- which(df.fp.mw[,6] == 1)
id.nor.sm    <- which(df.fp.mw[,6] == 0 & df.fp.mw[,8] == 1)
id.nor.lg    <- which(df.fp.mw[,8] == 2)
df.br.inc    <- aggregate(df.fp.mw[id.inc,    id.v.all], list(df.fp.mw[id.inc,    5]), sum); nrow(df.br.inc)
df.br.nor.sm <- aggregate(df.fp.mw[id.nor.sm, id.v.all], list(df.fp.mw[id.nor.sm, 5]), sum); nrow(df.br.nor.sm)
df.br.nor.lg <- aggregate(df.fp.mw[id.nor.lg, id.v.all], list(df.fp.mw[id.nor.lg, 5]), sum); nrow(df.br.nor.lg)

View(df.fp.mw[df.fp.mw[,4] == 73, ])

# # Descriptive stats: Incu vs Normal, 20>= vs 20<
# table(df.fp.sa[,6], df.fp.sa[,8])
# 
# # Data aggregation
# id.v.all     <- c(id.x.s1, id.y.s1, id.y.s2)
# id.inc       <- which(df.fp.sa[,6] == 1)
# id.nor.sm    <- which(df.fp.sa[,6] == 0 & df.fp.sa[,8] == 1)
# id.nor.lg    <- which(df.fp.sa[,8] == 2)
# df.br.inc    <- aggregate(df.fp.sa[id.inc,    id.v.all], list(df.fp.sa[id.inc,    5]), sum)
# df.br.nor.sm <- aggregate(df.fp.sa[id.nor.sm, id.v.all], list(df.fp.sa[id.nor.sm, 5]), sum)
# df.br.nor.lg <- aggregate(df.fp.sa[id.nor.lg, id.v.all], list(df.fp.sa[id.nor.lg, 5]), sum)


# Productivity analysis
type           <- 2 # 1:incu, 2:normal.small, 3:normal.large
df.temp        <- if(type == 1) df.br.inc else if(type == 2) df.br.nor.sm else df.br.nor.lg
adj.min        <- apply(df.temp[,9:10], 2, function(x) if(min(x) < 0) -min(x) else 0)
df.temp[,9:10] <- t(t(df.temp[,9:10]) + adj.min)
g              <- cbind(matrix(rep(0, nrow(df.temp) * length(id.x.s1)), ncol = length(id.x.s1)), df.temp[,6:8])
wd             <- c(0, 0, 1)
res.s1         <- dm.sf (df.temp[,2:5], df.temp[,6:8], "vrs", g, wd)
res.s1.se      <- dm.sf (df.temp[,2:5], df.temp[,6:8], "crs", g, wd, se = T)
res.s2         <- dm.dea(df.temp[,6:7], df.temp[,9:10], "vrs", "o")
res.s2.se      <- dm.sbm(df.temp[,6:7], df.temp[,9:10], "vrs", se = T)

# Super-eff for ranking efficient branches
id.eff.s1.sa   <- which(round(res.s1$eff + 1, 8) == 1)
res.comp.s1.sa <- res.s1$eff + 1
res.rank.s1.sa <- rep(NA, length(res.s1$eff))
res.rank.s1.sa[-id.eff.s1.sa] <- rank(res.comp.s1.sa[-id.eff.s1.sa,]) + length(id.eff.s1.sa)
res.comp.s1.sa[ id.eff.s1.sa] <- res.s1.se$eff[id.eff.s1.sa] + 1
res.comp.s1.sa[which(res.comp.s1.sa < 10^-10 | res.comp.s1.sa > 10^10),] <- 1

# Some adjustments to consider sales scale
adj.comp.s1.sa <- apply(res.comp.s1.sa - 1, 1, function(x) if(x != 0) x/2 else 0)
res.comp.s1.sa <- res.comp.s1.sa - adj.comp.s1.sa
adj.w          <- c(10, 1, -10)
adj.score      <- apply(df.temp[id.eff.s1.sa, 6:8], 1, function(x) sum(x * adj.w))^0.5
adj.r.rate     <- 0.7
res.rank.s1.sa[id.eff.s1.sa] <- rank(res.comp.s1.sa[id.eff.s1.sa] - adj.score / max(adj.score) * adj.r.rate)

id.eff.s2.sa   <- which(round(res.s2$eff, 8) == 1)
res.comp.s2.sa <- res.s2$eff
res.comp.s2.sa[id.eff.s2.sa] <- 1/res.s2.se$eff[id.eff.s2.sa]
res.comp.s2.sa[which(res.comp.s2.sa < 10^-10 | res.comp.s2.sa > 10^10),] <- 1
res.rank.s2.sa <- rank(res.comp.s2.sa)

# Summary of the results
res.eff.br.sa  <- data.frame(Brc.id  = df.temp[,1],
                             Eff.s1  = res.s1$eff + 1,
                             Rank.s1 = res.rank.s1.sa,
                             Eff.s2  = res.s2$eff,
                             Rank.s2 = res.rank.s2.sa)

df.temp[order(res.eff.br.sa$Rank.s1),]


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


