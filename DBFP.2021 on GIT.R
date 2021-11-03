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
df.raw.mw <- df.raw.2011.2109
df.raw.dw <- FP_daily_20211029
nm.m   <- unique(df.raw.mw[,1])


#########################################################################################################################
### Data cleansing
#########################################################################################################################

# IDs for exclusion
id.out.level.mw  <- which( is.na(match(df.raw.mw[,3], c(100, 101, 102, 109, 112))))
id.out.branch.mw <- which(!is.na(match(df.raw.mw[,5], c(8262, 8312, 8335, 8354))))
id.out.bunit.mw  <- which(!is.na(match(df.raw.mw[,4], c(74, NA))))
id.out.month.mw  <- if(e.type == "fp") which(df.raw.mw[,9] %in% c(0, 1, 2, 3)) else NULL
id.out.level.dw  <- which( is.na(match(df.raw.dw[,7], c(100, 101, 102, 109, 112))))
id.out.branch.dw <- which(!is.na(match(df.raw.dw[,5], c(8262, 8312, 8335, 8354))))
id.out.bunit.dw  <- which(!is.na(match(df.raw.dw[,3], c(74, NA))))
id.out.month.dw  <- if(e.type == "fp") which(df.raw.dw[,8] %in% c(0, 1, 2, 3)) else NULL

# Effective data
id.out.all.mw <- unique(c(id.out.level.mw, id.out.branch.mw, id.out.bunit.mw, id.out.month.mw))
id.out.all.dw <- unique(c(id.out.level.dw, id.out.branch.dw, id.out.bunit.dw, id.out.month.dw))
df.eff.mw     <- df.raw.mw[-id.out.all.mw,][order(df.raw.mw[-id.out.all.mw,][,1], df.raw.mw[-id.out.all.mw,][,2]),]
df.eff.dw     <- df.raw.dw[-id.out.all.dw,][order(df.raw.dw[-id.out.all.dw,][,1], df.raw.dw[-id.out.all.dw,][,2]),]

# Derivative variables (Coverage analysis, Withdrawal + Return)
n.col.mw <- ncol(df.eff.mw)
n.col.dw <- ncol(df.eff.dw)
df.eff.mw[,n.col.mw + 1]        <- df.eff.mw[,91] + df.eff.mw[,92]
df.eff.mw[,n.col.mw + 2]        <- df.eff.mw[,33] + df.eff.mw[,35]
df.eff.dw[,n.col.dw + 1]        <- df.eff.dw[,20] + df.eff.dw[,21]
names(df.eff.mw)[-(1:n.col.mw)] <- c("보장분석건수(최적+일반)", "청철+반송")
names(df.eff.dw)[1 + n.col.dw]  <- "보장분석건수(최적+일반)"


#########################################################################################################################
### FP Evaluation
#########################################################################################################################

# Plates
res.all.fp <- data.frame()

# Loop for all periods
for(m in nm.m){
  
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
      
      # Grade
      res.grade.s1 <- ifelse(res.rank.s1 < length(res.rank.s1)/3, "A", ifelse(res.rank.s1 < length(res.rank.s1)*2/3, "B", "C"))
      res.grade.s2 <- ifelse(res.rank.s2 < length(res.rank.s2)/3, "A", ifelse(res.rank.s2 < length(res.rank.s2)*2/3, "B", "C"))
      res.grade.s3 <- ifelse(res.rank.s3 < length(res.rank.s3)/3, "A", ifelse(res.rank.s3 < length(res.rank.s3)*2/3, "B", "C"))
      
      
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
      # nl.data <- df.temp.mw[,id.n.l.t];ng.data <- df.temp.mw[,id.n.g.t]; fx.data <- df.temp.mw[,id.flex]; w <- c(1, 1, 1, 1.1, 1.1, 1.1); max.slack <- NULL; time.out <- 10
      res.who     <- target.scale.up(df.temp.mw[,id.n.l.t], df.temp.mw[,id.n.g.t], df.temp.mw[,id.flex], w = c(1, 1, 1, 1.1, 1.1, 1.1), time.out = 10)$res.target
      res.tar.sup <- df.temp.mw[res.who, 2]
      
      # Results
      res.all.m <- rbind(res.all.m, data.frame(Closed.m   = m,
                                               B.unit.id  = df.temp.mw[,4],
                                               Branch.id  = df.temp.mw[,5],
                                               FP.id      = df.temp.mw[,2],
                                               Rank.s1    = res.rank.s1,
                                               n.s1       = length(res.rank.s1),
                                               Grade.s1   = res.grade.s1,
                                               Rank.s2    = res.rank.s2,
                                               n.s2       = length(res.rank.s2),
                                               Grade.s2   = res.grade.s2,
                                               Rank.s3    = res.rank.s3,
                                               n.s3       = length(res.rank.s3),
                                               Grade.s3   = res.grade.s3,
                                               Target.sup = res.tar.sup))
    }
  }
  # write.csv(res.all.m[order(res.all.m[,2]),], file = "benchmark.csv")
  res.all.fp <- rbind(res.all.fp, res.all.m[order(res.all.m[,2]),])
}
# write.csv(res.all.fp, file = "res.all.fp.csv")


#########################################################################################################################
### FP_UI: FP evaluation
#########################################################################################################################

# Parameters
crm.m     <- 202109
m.pre     <- df.eff.mw[,1] == (crm.m - 1)
l.six     <- df.eff.mw[,1] %in% tail(nm.m[nm.m < crm.m], 6)
df.six.mw <- aggregate(df.eff.mw[l.six, c(16, 22, 45, 50, seq(70, 88, 2), seq(71, 89, 2))], list(df.eff.mw[l.six, 2]), "sum")
df.six.mw <- cbind(df.six.mw, aggregate(df.eff.mw[l.six, c(68, 69)] * df.eff.mw[l.six, 45], list(df.eff.mw[l.six, 2]), "sum"))
id.sa.kpi <- c(15, 16, 22, 25, 26, 27, 30, 32, 33, 35, 37, 45, 50, 54, 57, 58, 61, 62, 63, 67, 68, 69, 90, 93)

# P1. Sales Outcome
# res.so.now.d <- list(Product.ratio   = cbind(df.eff.mw[m.pre, 1:2], df.eff.mw[m.pre, seq(71, 89, 2)]/rowSums(df.eff.mw[m.pre, seq(71, 89, 2)])),
#                      Product.premium = cbind(df.eff.mw[m.pre, 1:2], df.eff.mw[m.pre, seq(71, 89, 2)]),
#                      Product.count   = cbind(df.eff.mw[m.pre, 1:2], df.eff.mw[m.pre, seq(70, 88, 2)]),
#                      KPI             = cbind(df.eff.mw[m.pre, 1:2], data.frame(Premium.total = df.eff.mw[m.pre, 50],
#                                                                                Premium.p.total     = df.eff.mw[m.pre, 50]/df.eff.mw[m.pre, 45],
#                                                                                Count               = df.eff.mw[m.pre, 45],
#                                                                                Contract.p.design   = df.eff.mw[m.pre, 30],
#                                                                                Age.contractor      = df.eff.mw[m.pre, 68],
#                                                                                Age.insurant        = df.eff.mw[m.pre, 69],
#                                                                                Contract.p.design   = df.eff.mw[m.pre, 32])))


res.so.pre.m <- list(Product.ratio   = cbind(df.eff.mw[m.pre, 1:2], df.eff.mw[m.pre, seq(71, 89, 2)]/rowSums(df.eff.mw[m.pre, seq(71, 89, 2)])),
                     Product.premium = cbind(df.eff.mw[m.pre, 1:2], df.eff.mw[m.pre, seq(71, 89, 2)]),
                     Product.count   = cbind(df.eff.mw[m.pre, 1:2], df.eff.mw[m.pre, seq(70, 88, 2)]),
                     KPI             = cbind(df.eff.mw[m.pre, 1:2], data.frame(Premium.total = df.eff.mw[m.pre, 50],
                                                                               Premium.p.total     = df.eff.mw[m.pre, 50]/df.eff.mw[m.pre, 45],
                                                                               Count               = df.eff.mw[m.pre, 45],
                                                                               Contract.p.design   = df.eff.mw[m.pre, 30],
                                                                               Age.contractor      = df.eff.mw[m.pre, 68],
                                                                               Age.insurant        = df.eff.mw[m.pre, 69],
                                                                               Contract.p.design   = df.eff.mw[m.pre, 32])))

res.so.six.m <- list(Product.ratio   = cbind(df.six.mw[1], df.six.mw[16:25]/rowSums(df.six.mw[16:25])),
                     Product.premium = cbind(df.six.mw[1], df.six.mw[16:25]),
                     Product.count   = cbind(df.six.mw[1], df.six.mw[ 6:15]),
                     KPI             = cbind(df.six.mw[1], data.frame(Premium.total     = df.six.mw[5],
                                                                      Premium.p.total   = df.six.mw[5]/df.six.mw[4],
                                                                      Count             = df.six.mw[4],
                                                                      Contract.p.design = df.six.mw[4]/df.six.mw[2],
                                                                      Age.contractor    = df.six.mw[27]/df.six.mw[4],
                                                                      Age.insurant      = df.six.mw[28]/df.six.mw[4],
                                                                      Contract.p.design = df.six.mw[4]/df.six.mw[3])))

# P2. Sales Activity - Monthly
# res.sa.d <- list(Each.FP    = df.eff.mw[l.six, id.sa.kpi],
#                  Peer.Group = aggregate(df.eff.mw[,id.sa.kpi], list(df.eff.mw[,1], df.eff.mw[,8], df.eff.mw[,10]), "mean"),
#                  Stdd.Group = aggregate(df.eff.mw[,id.sa.kpi], list(df.eff.mw[,1], df.eff.mw[,8], df.eff.mw[,10], df.eff.mw[,13]), "mean"),
#                  Crck.Group = aggregate(df.eff.mw[,id.sa.kpi], list(df.eff.mw[,1], df.eff.mw[,8], df.eff.mw[,10], df.eff.mw[,14]), "mean"))

res.sa.m <- list(Each.FP    = df.eff.mw[l.six, id.sa.kpi],
                 Peer.Group = aggregate(df.eff.mw[,id.sa.kpi], list(df.eff.mw[,1], df.eff.mw[,8], df.eff.mw[,10]), "mean"),
                 Stdd.Group = aggregate(df.eff.mw[,id.sa.kpi], list(df.eff.mw[,1], df.eff.mw[,8], df.eff.mw[,10], df.eff.mw[,13]), "mean"),
                 Crck.Group = aggregate(df.eff.mw[,id.sa.kpi], list(df.eff.mw[,1], df.eff.mw[,8], df.eff.mw[,10], df.eff.mw[,14]), "mean"))


# R3. Benchmark
id.scale.up.target    <- which(df.eff.mw[,1] == (m - 1) & df.eff.mw[,2] == res.all.fp[which(res.all.fp[,1] == (m - 1) & res.all.fp[,2] == fp.id), 12])
stat.me.product.ratio <- res.so.crm$Product.ratio
stat.sg.product.ratio <- colSums(df.eff.mw[df.eff.mw[,1] == (m - 1) & id.same.g, seq(71, 89, 2)]) / sum(df.eff.mw[df.eff.mw[,1] == (m - 1) & id.same.g, seq(71, 89, 2)])
stat.st.product.ratio <- colSums(df.eff.mw[df.eff.mw[,1] == (m - 1) & id.stdd.g, seq(71, 89, 2)]) / sum(df.eff.mw[df.eff.mw[,1] == (m - 1) & id.stdd.g, seq(71, 89, 2)])
stat.bg.product.ratio <- colSums(df.eff.mw[df.eff.mw[,1] == (m - 1) & id.best.g, seq(71, 89, 2)]) / sum(df.eff.mw[df.eff.mw[,1] == (m - 1) & id.best.g, seq(71, 89, 2)])
stat.bm.product.ratio <- df.eff.mw[id.scale.up.target, seq(71, 89, 2)] / sum(df.eff.mw[id.scale.up.target, seq(71, 89, 2)])

res.bm <- list(Info = data.frame(Month.Closing    = df.eff.mw[id.fp, 1],
                                 Business.Unit.ID = df.eff.mw[id.fp, 4],
                                 Branch.ID        = df.eff.mw[id.fp, 5],
                                 FP.ID            = df.eff.mw[id.fp, 2],
                                 FP.Month         = df.eff.mw[id.fp, 9],
                                 Benchmark.FP.ID  = df.eff.mw[id.scale.up.target, 2]),
               Main = unlist(c(stat.me.product.ratio[which(stat.me.product.ratio == max(stat.me.product.ratio))], 
                               stat.sg.product.ratio[which(stat.sg.product.ratio == max(stat.sg.product.ratio))], 
                               stat.st.product.ratio[which(stat.st.product.ratio == max(stat.st.product.ratio))], 
                               stat.bg.product.ratio[which(stat.bg.product.ratio == max(stat.bg.product.ratio))],
                               stat.bm.product.ratio[which(stat.bm.product.ratio == max(stat.bm.product.ratio))])),
               Stat = data.frame(FP.own     = unlist(df.eff.mw[id.fp, id.sa.kpi]),
                                 Same.Group = round(unlist(tail(res.sa.sg, 1)), 1),
                                 Stdd.Group = round(unlist(tail(res.sa.st, 1)), 1),
                                 Best.Group = round(unlist(tail(res.sa.bg, 1)), 1),
                                 Benchmark  = unlist(df.eff.mw[id.scale.up.target, id.sa.kpi]))[-1,])


#########################################################################################################################
### Branch_UI: Branch evaluation
#########################################################################################################################

# Who and When
m      <- 202109
br.id  <- 1545
id.crm <- df.eff.mw[,1] == (m - 1) & df.eff.mw[,5] == br.id
id.six <- df.eff.mw[,1] %in% tail(nm.m[nm.m < m], 6)

# R1. Sales Outcome
id.so.kpi     <- c(50, 45, 30, 32, 68, 69, 70:89, 25, 16, 22)
df.so.crm.kpi <- df.eff.mw[id.crm, id.so.kpi]
df.so.six.kpi <- df.eff.mw[id.six & df.eff.mw[,5] == br.id, id.so.kpi]
res.so.crm    <- list(Product.ratio   = colSums(df.so.crm.kpi[seq(8, 26, 2)])/sum(df.so.crm.kpi[seq(8, 26, 2)]),
                      Product.premium = colSums(df.so.crm.kpi[seq(8, 26, 2)]),
                      Product.count   = colSums(df.so.crm.kpi[seq(7, 26, 2)]),
                      KPI             = data.frame(premium.total     = sum(df.so.crm.kpi[1]),
                                                   premium.p.total   = sum(df.so.crm.kpi[1])/sum(df.so.crm.kpi[2]),
                                                   total             = sum(df.so.crm.kpi[2]),
                                                   contract.p.design = sum(df.so.six.kpi[2])/sum(df.so.six.kpi[28]) * 100,
                                                   age.contractor    = sum(df.so.crm.kpi[5]*df.so.crm.kpi[2])/sum(df.so.crm.kpi[2]),
                                                   age.insurant      = sum(df.so.crm.kpi[6]*df.so.crm.kpi[2])/sum(df.so.crm.kpi[2]),
                                                   contract.p.design = sum(df.so.six.kpi[2])/sum(df.so.six.kpi[29]) * 100))

res.so.six    <- list(Product.ratio   = colSums(df.so.six.kpi[seq(8, 26, 2)])/sum(df.so.six.kpi[seq(8, 26, 2)]),
                      Product.premium = colSums(df.so.six.kpi[seq(8, 26, 2)]),
                      Product.count   = colSums(df.so.six.kpi[seq(7, 26, 2)]),
                      KPI             = data.frame(premium.total     = sum(df.so.six.kpi[1]),
                                                   premium.p.total   = sum(df.so.six.kpi[1])/sum(df.so.six.kpi[2]),
                                                   total             = sum(df.so.six.kpi[2]),
                                                   contract.p.design = sum(df.so.six.kpi[2])/sum(df.so.six.kpi[28]) * 100,
                                                   age.contractor    = sum(df.so.six.kpi[5]*df.so.six.kpi[2])/sum(df.so.six.kpi[2]),
                                                   age.insurant      = sum(df.so.six.kpi[6]*df.so.six.kpi[2])/sum(df.so.six.kpi[2]),
                                                   contract.p.design = sum(df.so.six.kpi[2])/sum(df.so.six.kpi[29]) * 100))

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

# R2. Sales Activity
id.sa.kpi.add           <- c(15, 16, 22, 25, 26, 27, 33, 35, 37, 45, 50, 54, 57, 58, 62, 63, 67, 90, 93)
id.sa.kpi.avg           <- c(30, 32, 61, 68, 69)
id.same.g               <- df.eff.mw[,6] == df.eff.mw[id.crm, 6][1] & df.eff.mw[,7] == df.eff.mw[id.crm, 7][1]
res.sa.kpi.add          <- aggregate(df.eff.mw[df.eff.mw[,5] == br.id, id.sa.kpi.add], list(df.eff.mw[df.eff.mw[,5] == br.id, 1]), "sum")
res.sa.kpi.avg.1        <- aggregate(df.eff.mw[df.eff.mw[,5] == br.id, c(45, 16, 22)], list(df.eff.mw[df.eff.mw[,5] == br.id, 1]), "sum")
res.sa.kpi.avg.1$CPD.1  <- res.sa.kpi.avg.1[,2]/res.sa.kpi.avg.1[,3]*100; res.sa.kpi.avg.1$CPD.2 <- res.sa.kpi.avg.1[,2]/res.sa.kpi.avg.1[,4]*100
res.sa.kpi.avg.2        <- aggregate(df.eff.mw[df.eff.mw[,5] == br.id, c(61)], list(df.eff.mw[df.eff.mw[,5] == br.id, 1]), function(x) mean(x[x != 0]))
df.temp                 <- df.eff.mw[df.eff.mw[,5] == br.id, c(45, 68, 69)]; 
res.sa.kpi.avg.3        <- aggregate(data.frame(df.temp[,1], df.temp[,1]*df.temp[,2], df.temp[,1]*df.temp[,3]), list(df.eff.mw[df.eff.mw[,5] == br.id, 1]), "sum")
res.sa.kpi.avg.3$AoC    <- res.sa.kpi.avg.3[,3]/res.sa.kpi.avg.3[,2]; res.sa.kpi.avg.3$AoI <- res.sa.kpi.avg.3[,4]/res.sa.kpi.avg.3[,2]
res.sa.me               <- cbind(res.sa.kpi.add, res.sa.kpi.avg.1[,5:6], res.sa.kpi.avg.2[,2], res.sa.kpi.avg.3[,5:6])
names(res.sa.me)[21:25] <- names(df.eff.mw[,id.sa.kpi.avg])

res.sa.me <- df.eff.mw[id.six & df.eff.mw[,2] == fp.id, id.sa.kpi]
res.sa.sg <- aggregate(df.eff.mw[id.six & id.same.g, id.sa.kpi][,-1], list(df.eff.mw[id.six & id.same.g, 1]), "mean")
res.sa.st <- aggregate(df.eff.mw[id.six & id.stdd.g, id.sa.kpi][,-1], list(df.eff.mw[id.six & id.stdd.g, 1]), "mean")


# R3. FP comparison
fp.id.all     <- unique(res.all.fp[res.all.fp[,3] == br.id, 4])
res.grade.all <- data.frame(FP.id = rep(fp.id.all, each = 3),
                            Stage = rep(1:3, length(fp.id.all)),
                            Process = rep(c("New.contract.closing", "Profitability", "Contract.mgt"), length(fp.id.all)))
for(i in unique(res.all.fp[,1])){
  grade   <- matrix(NA, nrow = length(fp.id.all) * 3)
  df.temp <- res.all.fp[res.all.fp[,1] == i & res.all.fp[,3] == br.id,]
  for(k in fp.id.all){
    if(k %in% df.temp[,4]){
      grade[((which(k == fp.id.all) - 1)*3 + 1):((which(k == fp.id.all) - 1)*3 + 3),] <- unlist(df.temp[df.temp[,4] == k, c(7, 10, 13)])
    }else{
      next
    }
  }
  res.grade.all <- cbind(res.grade.all, grade)
}
names(res.grade.all)[4:ncol(res.grade.all)] <- unique(res.all.fp[,1])
res.grade.m <- cbind(res.grade.all[,1:3], res.grade.all[,match(tail(nm.m[nm.m <= m], 6), names(res.grade.all))])
res.grade.m <- res.grade.m[apply(res.grade.m[,-c(1:3)], 1, function(x) sum(is.na(x)) != ncol(res.grade.m[,-c(1:3)])),]
res.grade.m[order(res.grade.m[,1]),]


#########################################################################################################################
### Branch Evaluation
#########################################################################################################################

id.v.all   <- c(id.x.s1, id.y.s1, id.y.s2, id.x.s3, id.y.s3)

# Plate
res.all.br <- data.frame()

# Loop for all periods
for(m in nm.m){
  
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
    
    # Plates
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
