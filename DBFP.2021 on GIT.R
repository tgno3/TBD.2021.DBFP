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
load("DBFP.2011.2110.Rdata")

# Naming and sorting for coding convenience
df.raw.mw <- df.raw.2011.2110
df.raw.mw <- df.raw.mw[order(df.raw.mw[,1], df.raw.mw[,2]),]
df.raw.dw <- df.raw.211001.211108[df.raw.211001.211108[,1] %in% 20211001:20211108,]
df.raw.dw <- df.raw.dw[order(df.raw.dw[1], df.raw.dw[2]),]
nm.m      <- unique(df.raw.mw[,1])


#########################################################################################################################
### Data cleansing
#########################################################################################################################

# IDs for exclusion
id.out.level.mw  <- which( is.na(match(df.raw.mw[,3], c(100, 101, 102, 109, 112))))
id.out.branch.mw <- which(!is.na(match(df.raw.mw[,5], c(8262, 8312, 8335, 8354))))
id.out.bunit.mw  <- which(!is.na(match(df.raw.mw[,4], c(74, NA))))
id.out.month.mw  <- which(df.raw.mw[,9] %in% c(0, 1, 2, 3))
id.out.level.dw  <- which( is.na(match(df.raw.dw[,7], c(100, 101, 102, 109, 112))))
id.out.branch.dw <- which(!is.na(match(df.raw.dw[,5], c(8262, 8312, 8335, 8354))))
id.out.bunit.dw  <- which(!is.na(match(df.raw.dw[,3], c(74, NA))))
id.out.month.dw  <- which(df.raw.dw[,8] %in% c(0, 1, 2, 3))

# Effective data
id.out.all.mw <- unique(c(id.out.level.mw, id.out.branch.mw, id.out.bunit.mw, id.out.month.mw))
id.out.all.dw <- unique(c(id.out.level.dw, id.out.branch.dw, id.out.bunit.dw, id.out.month.dw))
df.eff.mw     <- df.raw.mw[-id.out.all.mw,]
df.eff.dw     <- df.raw.dw[-id.out.all.dw,]

# Derivative variables
df.eff.mw <- cbind(df.eff.mw, data.frame('보장분석건수(최적+일반)' = df.eff.mw[,91] + df.eff.mw[,92],
                                         '청철+반송'               = df.eff.mw[,33] + df.eff.mw[,35]))
df.eff.dw <- cbind(df.eff.dw, data.frame('보장분석건수(최적+일반' = df.eff.dw[,20] + df.eff.dw[,21]))

# Parameters
m.now       <- 202111
m.pre       <- df.eff.mw[,1] == (m.now - 1)
id.so.kpi.m <- c(16, 22, 30, 32, 45, 50, 67, 68, 69, 70:89)
id.so.kpi.d <- c(15, 18:23, 25:35)
id.sa.kpi.d <- c(19, 22, 23, 25, 29, 33, 15, 20, 21)
id.sa.kpi.m <- c(15, 16, 22, 25, 26, 27, 37, 91, 92, 67, 93, 94)


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
  res.all.fp <- rbind(res.all.fp, res.all.m[order(res.all.m[,2], res.all.m[,3], res.all.m[,4]),])
}
# write.csv(res.all.fp[complete.cases(res.all.fp),], file = "res.all.fp.csv")


#########################################################################################################################
### FP_UI: FP evaluation
#########################################################################################################################

# P1. Sales Outcome
res.so.fp.d <- list(Product.ratio   = cbind(df.eff.dw[1:2], df.eff.dw[seq(37, 55, 2)]/rowSums(df.eff.dw[seq(37, 55, 2)])),
                    Product.premium = cbind(df.eff.dw[1:2], df.eff.dw[seq(37, 55, 2)]),
                    Product.count   = cbind(df.eff.dw[1:2], df.eff.dw[seq(36, 54, 2)]),
                    KPI             = cbind(df.eff.dw[1:2], data.frame(Premium.total       = df.eff.dw[32],
                                                                       Premium.p.total     = df.eff.dw[32]/df.eff.dw[31],
                                                                       Premium.cvt.total   = df.eff.dw[35],
                                                                       Count               = df.eff.dw[31],
                                                                       Contract.p.design   = df.eff.dw[31]/df.eff.dw[22]*100,
                                                                       Contract.v.online   = df.eff.dw[33],
                                                                       Cancel.return       = df.eff.dw[30],
                                                                       # Age.contractor    = df.fp.agg.dw[28]/df.fp.agg.dw[4],
                                                                       # Age.insurant      = df.fp.agg.dw[29]/df.fp.agg.dw[4],
                                                                       Contract.p.design   = df.eff.dw[31]/df.eff.dw[23]*100,
                                                                       Premium.cvt.p.total = df.eff.dw[35]/df.eff.dw[31])))

# write.csv(cbind(res.so.fp.d$Product.ratio, res.so.fp.d$Product.premium[,-c(1:2)], res.so.fp.d$Product.count[,-c(1:2)], res.so.fp.d$KPI[,-c(1:2)]), file = "res.so.fp.d.csv")

res.so.fp.m <- list(Product.ratio   = cbind(df.eff.mw[1:2], df.eff.mw[seq(71, 89, 2)]/rowSums(df.eff.mw[seq(71, 89, 2)])),
                    Product.premium = cbind(df.eff.mw[1:2], df.eff.mw[seq(71, 89, 2)]),
                    Product.count   = cbind(df.eff.mw[1:2], df.eff.mw[seq(70, 88, 2)]),
                    KPI             = cbind(df.eff.mw[1:2], data.frame(Premium.total       = df.eff.mw[50],
                                                                       Premium.p.total     = df.eff.mw[50]/df.eff.mw[45],
                                                                       Premium.cvt.total   = df.eff.mw[67],
                                                                       Count               = df.eff.mw[45],
                                                                       Contract.p.design   = df.eff.mw[30],
                                                                       Age.contractor      = df.eff.mw[68],
                                                                       Age.insurant        = df.eff.mw[69],
                                                                       Contract.p.design   = df.eff.mw[32],
                                                                       Premium.cvt.p.total = df.eff.mw[67]/df.eff.mw[45])))

# write.csv(cbind(res.so.fp.m$Product.ratio, res.so.fp.m$Product.premium[,-c(1:2)], res.so.fp.m$Product.count[,-c(1:2)], res.so.fp.m$KPI[,-c(1:2)]), file = "res.so.fp.m.csv")

res.so.fp.m.six <- data.frame()
for(m in nm.m[nm.m < m.now]){
  id.o.i <- df.eff.mw[,1] %in% tail(nm.m[nm.m <= m], 6)
  df.o.i <- cbind(df.eff.mw[id.o.i, c(1:2, id.so.kpi.m)][,-c(5:6, 10:11)], df.eff.mw[id.o.i, 68:69] * df.eff.mw[id.o.i, 45])
  df.agg <- aggregate(df.o.i[,-c(1:2)], list(df.o.i[,2]), "sum")
  df.agg <- cbind(Eval.m      = m + 1, 
                  FP.id       = df.agg[,1], 
                  P.ratio     = df.agg[,seq(8, 26, 2)]/rowSums(df.agg[,seq(8, 26, 2)]),
                  P.amount    = df.agg[,seq(8, 26, 2)], 
                  P.count     = df.agg[,seq(7, 25, 2)], df.agg[,4:6], 
                  C.avg       = df.agg[,4]/length(tail(nm.m[nm.m <= m], 6)),
                  MP.p.c      = df.agg[,5]/df.agg[,4], 
                  C.p.d       = df.agg[,4]/df.agg[,2]*100,
                  C.p.i       = df.agg[,4]/df.agg[,3]*100, 
                  Age.c       = df.agg[,27]/df.agg[,4],
                  Age.i       = df.agg[,28]/df.agg[,4],
                  P.cvt.p.c   = df.agg[,6]/df.agg[,4])
  res.so.fp.m.six <- rbind(res.so.fp.m.six, df.agg)
}

# write.csv(res.so.fp.m.six, file = "res.so.fp.six.m.csv")


# P2. Sales Activity
res.sa.fp.d <- list(Each.FP    = df.eff.dw[order(df.eff.dw[,2], df.eff.dw[,1]), c(1, 2, id.sa.kpi.d, 56)],
                    Peer.Group = aggregate(df.eff.dw[,c(id.sa.kpi.d, 56)], list(df.eff.dw[,1], df.eff.dw[,9], df.eff.dw[,10]), "mean"),
                    Stdd.Group = aggregate(df.eff.dw[,c(id.sa.kpi.d, 56)], list(df.eff.dw[,1], df.eff.dw[,9], df.eff.dw[,10], df.eff.dw[,13]), "mean"),
                    Crck.Group = aggregate(df.eff.dw[,c(id.sa.kpi.d, 56)], list(df.eff.dw[,1], df.eff.dw[,9], df.eff.dw[,10], df.eff.dw[,14]), "mean"))

# write.csv(res.sa.fp.d$Peer.Group, file = "fp.peer.group.d.csv"); write.csv(res.sa.fp.d$Stdd.Group, file = "fp.stdd.group.d.csv"); write.csv(res.sa.fp.d$Crck.Group, file = "fp.crck.group.d.csv")

res.sa.fp.m <- list(Each.FP    = df.eff.mw[,c(1, 2, id.sa.kpi.m)],
                    Peer.Group = aggregate(df.eff.mw[,id.sa.kpi.m], list(df.eff.mw[,1], df.eff.mw[,8], df.eff.mw[,10]), "mean"),
                    Stdd.Group = aggregate(df.eff.mw[,id.sa.kpi.m], list(df.eff.mw[,1], df.eff.mw[,8], df.eff.mw[,10], df.eff.mw[,13]), "mean"),
                    Crck.Group = aggregate(df.eff.mw[,id.sa.kpi.m], list(df.eff.mw[,1], df.eff.mw[,8], df.eff.mw[,10], df.eff.mw[,14]), "mean"))

# write.csv(res.sa.fp.m$Peer.Group, file = "fp.peer.group.m.csv"); write.csv(res.sa.fp.m$Stdd.Group, file = "fp.stdd.group.m.csv"); write.csv(res.sa.fp.m$Crck.Group, file = "fp.crck.group.m.csv")


# R3. Benchmark
# Refer to res.all.fp & res.sa.fp.m


#########################################################################################################################
### Branch_UI: Branch evaluation
#########################################################################################################################

# Aggregation per Branch
df.br.so.dw <- aggregate(df.eff.dw[,c(36:55, id.so.kpi.d)], list(df.eff.dw[,1], df.eff.dw[,5]), "sum")
df.br.so.mw <- aggregate(cbind(df.eff.mw[,id.so.kpi.m], df.eff.mw[,68:69] * df.eff.mw[,45]), list(df.eff.mw[,1], df.eff.mw[,5]), "sum")
df.br.sa.dw <- aggregate(df.eff.dw[,id.sa.kpi.d], list(df.eff.dw[,1], df.eff.dw[,5]), "sum")
df.br.sa.mw <- aggregate(df.eff.mw[,id.sa.kpi.m], list(df.eff.mw[,1], df.eff.mw[,5]), "sum")

# R1. Sales Outcome
res.so.br.d <- list(Product.ratio   = cbind(df.br.so.dw[1:2], df.br.so.dw[seq(4, 22, 2)]/rowSums(df.br.so.dw[seq(4, 22, 2)])),
                    Product.premium = cbind(df.br.so.dw[1:2], df.br.so.dw[seq(4, 22, 2)]),
                    Product.count   = cbind(df.br.so.dw[1:2], df.br.so.dw[seq(3, 21, 2)]),
                    KPI             = cbind(df.br.so.dw[1:2], data.frame(Premium.total       = df.br.so.dw[37],
                                                                         Premium.p.total     = df.br.so.dw[37]/df.br.so.dw[36],
                                                                         Premium.cvt.total   = df.br.so.dw[40],
                                                                         Count               = df.br.so.dw[36],
                                                                         Contract.p.design   = df.br.so.dw[36]/df.br.so.dw[28]*100,
                                                                         Contract.v.online   = df.br.so.dw[38],
                                                                         Cancel.return       = df.br.so.dw[34],
                                                                         # Age.contractor    = df.br.agg.dw[28]/df.br.agg.dw[4],
                                                                         # Age.insurant      = df.br.agg.dw[29]/df.br.agg.dw[4],
                                                                         Contract.p.design   = df.br.so.dw[36]/df.br.so.dw[29]*100,
                                                                         Premium.cvt.p.total = df.br.so.dw[40]/df.br.so.dw[36])))

# write.csv(cbind(res.so.br.d$Product.ratio, res.so.br.d$Product.premium[,-c(1:2)], res.so.br.d$Product.count[,-c(1:2)], res.so.br.d$KPI[,-c(1:2)]), file = "res.so.br.d.csv")

res.so.br.m <- list(Product.ratio   = cbind(df.br.so.mw[1:2], df.br.so.mw[seq(13, 31, 2)]/rowSums(df.br.so.mw[seq(13, 31, 2)])),
                    Product.premium = cbind(df.br.so.mw[1:2], df.br.so.mw[seq(13, 31, 2)]),
                    Product.count   = cbind(df.br.so.mw[1:2], df.br.so.mw[seq(12, 30, 2)]),
                    KPI             = cbind(df.br.so.mw[1:2], data.frame(Premium.total       = df.br.so.mw[8],
                                                                         Premium.p.total     = df.br.so.mw[8]/df.br.so.mw[7],
                                                                         Premium.cvt.total   = df.br.so.mw[9],
                                                                         Count               = df.br.so.mw[7],
                                                                         Contract.p.design   = df.br.so.mw[7]/df.br.so.mw[3]*100,
                                                                         Age.contractor      = df.br.so.mw[32]/df.br.so.mw[7],
                                                                         Age.insurant        = df.br.so.mw[33]/df.br.so.mw[7],
                                                                         Contract.p.design   = df.br.so.mw[7]/df.br.so.mw[4]*100,
                                                                         Premium.cvt.p.total = df.br.so.mw[9]/df.br.so.mw[7])))

# write.csv(cbind(res.so.br.m$Product.ratio, res.so.br.m$Product.premium[,-c(1:2)], res.so.br.m$Product.count[,-c(1:2)], res.so.br.m$KPI[,-c(1:2)]), file = "res.so.br.m.csv")

res.so.br.m.six <- data.frame()
for(m in nm.m[nm.m < m.now]){
  id.o.i  <- df.eff.mw[,1] %in% tail(nm.m[nm.m <= m], 6)
  df.o.i  <- cbind(df.eff.mw[id.o.i, c(1:2, 5, id.so.kpi.m)], df.eff.mw[id.o.i, 68:69] * df.eff.mw[id.o.i, 45])
  df.agg  <- aggregate(df.o.i[,-c(1:3)], list(df.o.i[,3]), "sum")
  res.agg <- data.frame(Eval.m    = m + 1, 
                        BR.id     = df.agg[1], 
                        P.ratio   = df.agg[seq(12, 30, 2)]/rowSums(df.agg[seq(12, 30, 2)]),
                        P.amount  = df.agg[seq(12, 30, 2)], 
                        P.count   = df.agg[seq(11, 29, 2)], df.agg[,6:8], 
                        C.avg     = df.agg[6]/length(tail(nm.m[nm.m <= m], 6)),
                        MP.p.c    = df.agg[7]/df.agg[6],
                        C.p.d     = df.agg[6]/df.agg[2]*100, 
                        C.p.i     = df.agg[6]/df.agg[3]*100, 
                        Age.c     = df.agg[31]/df.agg[6],
                        Age.i     = df.agg[32]/df.agg[6],
                        P.cvt.p.c = df.agg[8]/df.agg[6])
  res.so.br.m.six <- rbind(res.so.br.m.six, res.agg)
  
}

# write.csv(res.so.br.m.six, file = "res.so.br.six.m.csv")


# P2. Sales Activity
df.br.sa.dw[,2] <- as.numeric(df.br.sa.dw[,2])
res.br.sa.pg.dw <- data.frame()
for(i in 1:nrow(df.br.sa.dw)){res.br.sa.pg.dw <- rbind(res.br.sa.pg.dw, df.eff.mw[df.eff.mw[,1] == (m.now - 1) & df.eff.mw[,5] == df.br.sa.dw[i, 2], 6:7][1,])}
res.sa.br.pg.dw <- aggregate(df.br.sa.dw[,-c(1:2)], list(df.br.sa.dw[,1], res.br.sa.pg.dw[,1], res.br.sa.pg.dw[,2]), "mean")

# write.csv(df.br.sa.dw, file = "res.sa.br.d.csv"); write.csv(res.sa.br.pg.dw, file = "br.peer.group.d.csv")

res.br.sa.pg.mw <- data.frame()
for(i in 1:nrow(df.br.sa.mw)){res.br.sa.pg.mw <- rbind(res.br.sa.pg.mw, df.eff.mw[df.eff.mw[,1] == df.br.sa.mw[i, 1] & df.eff.mw[,5] == df.br.sa.mw[i, 2], 6:7][1,])}
res.sa.br.pg.mw <- aggregate(df.br.sa.mw[,-c(1:2)], list(df.br.sa.mw[,1], res.br.sa.pg.mw[,1], res.br.sa.pg.mw[,2]), "mean")

# write.csv(df.br.sa.mw, file = "res.sa.br.m.csv"); write.csv(res.sa.br.pg.mw, file = "br.peer.group.m.csv")


# R3. FP comparison
br.id         <- 7003
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

# Branch data aggregation
df.agg.br <- cbind(aggregate(df.eff.mw[,c(id.x.s1, id.y.s1, id.y.s2[2], id.x.s3, id.y.s3)], list(df.eff.mw[,1], df.eff.mw[,5]), "sum"),
                   aggregate(df.eff.mw[,id.y.s2[1]], list(df.eff.mw[,1], df.eff.mw[,5]), "mean")[3],
                   aggregate(df.eff.mw[,c(28:29, 67, 45)], list(df.eff.mw[,1], df.eff.mw[,5]), "sum")[,-c(1:2)])

# Plates
res.all.br <- data.frame()

# Loop for all periods
for(m in nm.m){
  
  # Data of interest
  df.br.mw <- df.agg.br[df.agg.br[,1] == m,]
  
  # Sub-grouping
  br.group     <- matrix(unlist(apply(df.br.mw[2], 1, function(x) subset(df.eff.mw, df.eff.mw[1] == m & df.eff.mw[5] == x)[1, 6:7])), byrow = T, ncol = 2)
  id.inc.all   <- which(br.group[,1] == 1)
  id.nor.01.19 <- which(br.group[,1] == 2 & br.group[,2] == 1)
  id.nor.20.00 <- which(br.group[,1] == 2 & br.group[,2] == 2)
  
  # Plates
  res.all.m <- data.frame()
  
  # Productivity analysis
  for(i in 1:3){

    # Data of interest
    id.o.i     <- if(i == 1) id.inc.all else if(i == 2) id.nor.01.19 else id.nor.20.00
    df.temp.mw <- df.br.mw[id.o.i,]
    
    # IDs for evaluation
    id.calc.s1 <- which(df.temp.mw[,6] > 0)
    id.excd.s1 <- setdiff(1:nrow(df.temp.mw), id.calc.s1)
    id.calc.s2 <- which(apply(df.temp.mw[,c(8, 13)], 1, function(x) sum(x > 0) == 2))
    id.excd.s2 <- setdiff(1:nrow(df.temp.mw), id.calc.s2)
    id.calc.s3 <- which(df.temp.mw[,10] > 0)
    id.excd.s3 <- setdiff(1:nrow(df.temp.mw), id.calc.s3)
    
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
    res.s1    <- dm.sbm(df.temp.mw[id.calc.s1, 3:5], 
                        cbind(df.temp.mw[id.calc.s1, 6], 
                              max(df.temp.mw[id.calc.s1, 7]) + 1 - df.temp.mw[id.calc.s1, 7]), 
                        "drs", "o")
    res.s1.se <- dm.sbm(df.temp.mw[id.calc.s1, 3:5], 
                        cbind(df.temp.mw[id.calc.s1, 6], 
                              max(df.temp.mw[id.calc.s1, 7]) + 1 - df.temp.mw[id.calc.s1, 7]), 
                        "drs", "o", se = T)
    
    # Stage 2
    res.s2    <- dm.sbm(df.temp.mw[id.calc.s2, 6], 
                        df.temp.mw[id.calc.s2, c(8, 13)],
                        "drs", "o")
    res.s2.se <- dm.sbm(df.temp.mw[id.calc.s2, 6], 
                        df.temp.mw[id.calc.s2, c(8, 13)],
                        "drs", "o", se = T)
    
    # Stage 3
    res.s3    <- dm.sbm(df.temp.mw[id.calc.s3, 9], 
                        cbind(df.temp.mw[id.calc.s3, 10], 
                              max(df.temp.mw[id.calc.s3, 11]) + 1 - df.temp.mw[id.calc.s3, 11],
                              max(df.temp.mw[id.calc.s3, 12]) + 1 - df.temp.mw[id.calc.s3, 12]),
                        "drs", "o")
    res.s3.se <- dm.sbm(df.temp.mw[id.calc.s3, 9], 
                        cbind(df.temp.mw[id.calc.s3, 10], 
                              max(df.temp.mw[id.calc.s3, 11]) + 1 - df.temp.mw[id.calc.s3, 11],
                              max(df.temp.mw[id.calc.s3, 12]) + 1 - df.temp.mw[id.calc.s3, 12]),
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
    w.s1.ef.ws <- -c(0.5, 0.5) * c(500, 10); w.s1.y1.y2 <- c(0.7, 0.3)
    res.rank.s1[id.excd.s1] <- nrow(df.temp.mw)
    ts.s1.xs <- summary(df.temp.mw[id.calc.s1, 6])[2]
    ts.s1.s  <- summary(df.temp.mw[id.calc.s1, 6])[3]
    ts.s1.m  <- summary(df.temp.mw[id.calc.s1, 6])[5]
    id.s1.xs <- which(df.temp.mw[id.calc.s1, 6] <= ts.s1.xs)
    id.s1.s  <- which(df.temp.mw[id.calc.s1, 6] >  ts.s1.xs & df.temp.mw[id.calc.s1, 6] <= ts.s1.s)
    id.s1.m  <- which(df.temp.mw[id.calc.s1, 6] >  ts.s1.s  & df.temp.mw[id.calc.s1, 6] <= ts.s1.m)
    id.s1.l  <- which(df.temp.mw[id.calc.s1, 6] >  ts.s1.m)
    if(length(id.s1.xs) > 0) rank.calc.s1[id.s1.xs] <- round(rank(res.score.s1[id.calc.s1][id.s1.xs] * w.s1.ef.ws[1] + as.matrix(df.temp.mw[id.calc.s1,][id.s1.xs, 14:15]) %*% w.s1.y1.y2 * w.s1.ef.ws[2])) + length(id.s1.l) + length(id.s1.m) + length(id.s1.s)
    if(length(id.s1.s ) > 0) rank.calc.s1[id.s1.s ] <- round(rank(res.score.s1[id.calc.s1][id.s1.s ] * w.s1.ef.ws[1] + as.matrix(df.temp.mw[id.calc.s1,][id.s1.s , 14:15]) %*% w.s1.y1.y2 * w.s1.ef.ws[2])) + length(id.s1.l) + length(id.s1.m)
    if(length(id.s1.m ) > 0) rank.calc.s1[id.s1.m ] <- round(rank(res.score.s1[id.calc.s1][id.s1.m ] * w.s1.ef.ws[1] + as.matrix(df.temp.mw[id.calc.s1,][id.s1.m , 14:15]) %*% w.s1.y1.y2 * w.s1.ef.ws[2])) + length(id.s1.l)
    if(length(id.s1.l ) > 0) rank.calc.s1[id.s1.l ] <- round(rank(res.score.s1[id.calc.s1][id.s1.l ] * w.s1.ef.ws[1] + as.matrix(df.temp.mw[id.calc.s1,][id.s1.l , 14:15]) %*% w.s1.y1.y2 * w.s1.ef.ws[2]))
    res.group.s1[id.calc.s1][id.s1.xs] <- "Q1"; res.group.s1[id.calc.s1][id.s1.s] <- "Q2"; res.group.s1[id.calc.s1][id.s1.m] <- "Q3"; res.group.s1[id.calc.s1][id.s1.l] <- "Q4"
    if(length(id.s1.xs) > 0) res.wsum.s1[id.calc.s1][id.s1.xs] <- as.matrix(df.temp.mw[id.calc.s1,][id.s1.xs, 14:15]) %*% w.s1.y1.y2
    if(length(id.s1.s ) > 0) res.wsum.s1[id.calc.s1][id.s1.s ] <- as.matrix(df.temp.mw[id.calc.s1,][id.s1.s , 14:15]) %*% w.s1.y1.y2
    if(length(id.s1.m ) > 0) res.wsum.s1[id.calc.s1][id.s1.m ] <- as.matrix(df.temp.mw[id.calc.s1,][id.s1.m , 14:15]) %*% w.s1.y1.y2
    if(length(id.s1.l ) > 0) res.wsum.s1[id.calc.s1][id.s1.l ] <- as.matrix(df.temp.mw[id.calc.s1,][id.s1.l , 14:15]) %*% w.s1.y1.y2
    # View(cbind(df.temp.mw[id.calc.s1, c(3:5, 14:15, 6:7)], res.score.s1[id.calc.s1], rank.calc.s1))
    
    # Stage 2
    # XS(<Q1) vs S(<Q2) vs M(<Q3) vs L
    w.s2.ef.ws <- -c(0.5, 0.5) * c(500, 10); w.s2.y1.y2 <- c(0.3, 0.7)
    res.rank.s2[id.excd.s2] <- nrow(df.temp.mw)
    ny.s2    <- data.frame(apply(df.temp.mw[id.calc.s2, c(8, 13)], 2, function(x) nor(x)), 
                           nor(df.temp.mw[id.calc.s2, 8]) * w.s2.y1.y2[2] + nor(df.temp.mw[id.calc.s2, 13]) * w.s2.y1.y2[1])
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
    # View(cbind(df.temp.mw[id.calc.s2, c(6, 8, 13)], res.score.s2[id.calc.s2], rank.calc.s2))
    
    # Stage 3
    # XS(<Q1) vs S(<Q2) vs M(<Q3) vs L
    w.s3.ef.ws <- -c(0.5, 0.5) * c(500, 10); w.s3.y1.y2 <- c(0.3, 0.7)
    res.rank.s3[id.excd.s3] <- nrow(df.temp.mw)
    ny.s3    <- data.frame(nor(df.temp.mw[id.calc.s3, 10]), 
                           nor(df.temp.mw[id.calc.s3, 10]/df.temp.mw[id.calc.s3, 9]),
                           nor(df.temp.mw[id.calc.s3, 10]) * w.s3.y1.y2[1] + nor(df.temp.mw[id.calc.s3, 10]/df.temp.mw[id.calc.s3, 9]) * w.s2.y1.y2[2])
    ts.s3.xs <- summary(ny.s3[,1])[2]
    ts.s3.s  <- summary(ny.s3[,1])[3]
    ts.s3.m  <- summary(ny.s3[,1])[5]
    id.s3.xs <- which(ny.s3[,1] <= ts.s3.xs)
    id.s3.s  <- which(ny.s3[,1] >  ts.s3.xs & ny.s3[,1] <= ts.s3.s)
    id.s3.m  <- which(ny.s3[,1] >  ts.s3.s  & ny.s3[,1] <= ts.s3.m)
    id.s3.l  <- which(ny.s3[,1] >  ts.s3.m)
    if(length(id.s3.xs) > 0) rank.calc.s3[id.s3.xs] <- round(rank(res.score.s3[id.calc.s3][id.s3.xs] * w.s3.ef.ws[1] + ny.s3[id.s3.xs, 3] * w.s3.ef.ws[2])) + length(id.s3.l) + length(id.s3.m) + length(id.s3.s)
    if(length(id.s3.s ) > 0) rank.calc.s3[id.s3.s ] <- round(rank(res.score.s3[id.calc.s3][id.s3.s ] * w.s3.ef.ws[1] + ny.s3[id.s3.s,  3] * w.s3.ef.ws[2])) + length(id.s3.l) + length(id.s3.m)
    if(length(id.s3.m ) > 0) rank.calc.s3[id.s3.m ] <- round(rank(res.score.s3[id.calc.s3][id.s3.m ] * w.s3.ef.ws[1] + ny.s3[id.s3.m,  3] * w.s3.ef.ws[2])) + length(id.s3.l)
    if(length(id.s3.l ) > 0) rank.calc.s3[id.s3.l ] <- round(rank(res.score.s3[id.calc.s3][id.s3.l ] * w.s3.ef.ws[1] + ny.s3[id.s3.l,  3] * w.s3.ef.ws[2]))
    res.group.s3[id.calc.s3][id.s3.xs] <- "Q1"; res.group.s3[id.calc.s3][id.s3.s] <- "Q2"; res.group.s3[id.calc.s3][id.s3.m] <- "Q3"; res.group.s3[id.calc.s3][id.s3.l] <- "Q4"
    if(length(id.s3.xs) > 0) res.wsum.s3[id.calc.s3][id.s3.xs] <- ny.s3[id.s3.xs, 3]
    if(length(id.s3.s ) > 0) res.wsum.s3[id.calc.s3][id.s3.s ] <- ny.s3[id.s3.s,  3]
    if(length(id.s3.m ) > 0) res.wsum.s3[id.calc.s3][id.s3.m ] <- ny.s3[id.s3.m,  3]
    if(length(id.s3.l ) > 0) res.wsum.s3[id.calc.s3][id.s3.l ] <- ny.s3[id.s3.l,  3]
    # View(cbind(df.temp.mw[id.calc.s3, 9:12], res.wsum.s3[id.calc.s3], res.score.s3[id.calc.s3], rank.calc.s3))
    
    # Rank
    res.rank.s1[id.calc.s1] <- rank.calc.s1
    res.rank.s2[id.calc.s2] <- rank.calc.s2
    res.rank.s3[id.calc.s3] <- rank.calc.s3
    
    # Grade
    res.grade.s1 <- if(length(res.rank.s1) > 4) ifelse(res.rank.s1 < length(res.rank.s1)/3, "A", ifelse(res.rank.s1 < length(res.rank.s1)*2/3, "B", "C")) else rep(NA, length(res.rank.s1))
    res.grade.s2 <- if(length(res.rank.s2) > 4) ifelse(res.rank.s2 < length(res.rank.s2)/3, "A", ifelse(res.rank.s2 < length(res.rank.s2)*2/3, "B", "C")) else rep(NA, length(res.rank.s2))
    res.grade.s3 <- if(length(res.rank.s3) > 4) ifelse(res.rank.s3 < length(res.rank.s3)/3, "A", ifelse(res.rank.s3 < length(res.rank.s3)*2/3, "B", "C")) else rep(NA, length(res.rank.s3))
    
    
    #################
    # Targeting
    #################
    # Stage 1
    # SBM target
    # tar.calc.sbm.s1 <- res.s1$lambda %*% as.matrix(cbind(df.temp.mw[id.calc.s1, 6], 
    #                                                      max(df.temp.mw[id.calc.s1, 7]) + 1 - df.temp.mw[id.calc.s1, 7]))
    # res.tar.sbm.s1[id.calc.s1,] <- as.matrix(data.frame(ceiling(tar.calc.sbm.s1[,1]), floor(max(df.temp.mw[id.calc.s1, 7]) + 1 - round(tar.calc.sbm.s1[,2], 4))))
    # res.tar.sbm.s1[which(res.rank.s1 == 1),] <- as.matrix(df.temp.mw[id.calc.s1, 6:7][which(res.rank.s1[id.calc.s1] == 1),])
    
    # Stage 2
    # SBM target
    # tar.calc.sbm.s2 <- res.s2$lambda %*% as.matrix(df.temp.mw[id.calc.s2, c(8, 13)])
    # res.tar.sbm.s2[id.calc.s2,] <- as.matrix(tar.calc.sbm.s2)
    # res.tar.sbm.s2[which(res.rank.s2 == 1),] <- as.matrix(df.temp.mw[id.calc.s2, c(8, 13)][which(res.rank.s2[id.calc.s2] == 1),])
    
    # Stage 3
    # SBM target
    # tar.calc.sbm.s3 <- res.s3$lambda %*% as.matrix(cbind(df.temp.mw[id.calc.s3, 10], 
    #                                                      max(df.temp.mw[id.calc.s3, 11]) + 1 - df.temp.mw[id.calc.s3, 11],
    #                                                      max(df.temp.mw[id.calc.s3, 12]) + 1 - df.temp.mw[id.calc.s3, 12]))
    # res.tar.sbm.s3[id.calc.s3,] <- as.matrix(data.frame(tar.calc.sbm.s3[,1], 
    #                                                     round(max(df.temp.mw[id.calc.s3, 11]) + 1 - tar.calc.sbm.s3[,2]),
    #                                                     round(max(df.temp.mw[id.calc.s3, 12]) + 1 - tar.calc.sbm.s3[,3])))
    res.tar.sbm.s3[which(res.rank.s3 == 1),] <- as.matrix(df.temp.mw[id.calc.s3, 10:12][which(res.rank.s3[id.calc.s3] == 1),])
    
    # Scaled-up target at once
    # nl.data <- df.temp.mw[,id.n.l.t];ng.data <- df.temp.mw[,id.n.g.t]; fx.data <- df.temp.mw[,id.flex]; w <- c(1, 1, 1, 1.1, 1.1, 1.1); max.slack <- NULL; time.out <- 10
    # res.who     <- target.scale.up(df.temp.mw[,c(6, 16, 17)], df.temp.mw[,7], df.temp.mw[,3:5], w = c(1, 1, 1, 1.1, 1.1, 1.1), time.out = 10)$res.target
    # res.tar.sup <- df.temp.mw[res.who, 2]
    # 
    # Results
    res.all.m <- rbind(res.all.m, data.frame(Closed.m    = m,
                                             B.unit.id   = apply(df.temp.mw[2], 1, function(x) subset(df.eff.mw, df.eff.mw[1] == m & df.eff.mw[5] == x)[1, 4]),
                                             Branch.id   = df.temp.mw[,2],
                                             Branch.type = i,
                                             Rank.s1     = res.rank.s1,
                                             n.s1        = length(res.rank.s1),
                                             Grade.s1    = res.grade.s1,
                                             Rank.s2     = res.rank.s2,
                                             n.s2        = length(res.rank.s2),
                                             Grade.s2    = res.grade.s2,
                                             Rank.s3     = res.rank.s3,
                                             n.s3        = length(res.rank.s3),
                                             Grade.s3    = res.grade.s3))

  }
  # write.csv(res.all.m[order(res.all.m[,2]),], file = "benchmark.csv")
  res.all.br <- rbind(res.all.br, res.all.m[order(res.all.m[,1], res.all.m[,2], res.all.m[,3]),])
}
# write.csv(res.all.br[complete.cases(res.all.br),], file = "res.all.br.csv")



#########################################################################################################################
### Business_UI: Business unit evaluation
#########################################################################################################################

# Aggregation per Business Unit
df.bu.so.dw <- aggregate(df.eff.dw[,c(36:55, id.so.kpi.d)], list(df.eff.dw[,1], df.eff.dw[,3]), "sum")
df.bu.so.mw <- aggregate(cbind(df.eff.mw[,id.so.kpi.m], df.eff.mw[,68:69] * df.eff.mw[,45]), list(df.eff.mw[,1], df.eff.mw[,4]), "sum")
df.bu.sa.dw <- aggregate(df.eff.dw[,id.sa.kpi.d], list(df.eff.dw[,1], df.eff.dw[,3]), "sum")
df.bu.sa.mw <- aggregate(df.eff.mw[,id.sa.kpi.m], list(df.eff.mw[,1], df.eff.mw[,4]), "sum")

# R1. Sales Outcome
res.so.bu.d <- list(Product.ratio   = cbind(df.bu.so.dw[1:2], df.bu.so.dw[seq(4, 22, 2)]/rowSums(df.bu.so.dw[seq(4, 22, 2)])),
                    Product.premium = cbind(df.bu.so.dw[1:2], df.bu.so.dw[seq(4, 22, 2)]),
                    Product.count   = cbind(df.bu.so.dw[1:2], df.bu.so.dw[seq(3, 21, 2)]),
                    KPI             = cbind(df.bu.so.dw[1:2], data.frame(Premium.total       = df.bu.so.dw[37],
                                                                         Premium.p.total     = df.bu.so.dw[37]/df.bu.so.dw[36],
                                                                         Premium.cvt.total   = df.bu.so.dw[40],
                                                                         Count               = df.bu.so.dw[36],
                                                                         Contract.p.design   = df.bu.so.dw[36]/df.bu.so.dw[28]*100,
                                                                         Contract.v.online   = df.bu.so.dw[38],
                                                                         Cancel.return       = df.bu.so.dw[34],
                                                                         # Age.contractor    = df.br.agg.dw[28]/df.br.agg.dw[4],
                                                                         # Age.insurant      = df.br.agg.dw[29]/df.br.agg.dw[4],
                                                                         Contract.p.design   = df.bu.so.dw[36]/df.bu.so.dw[29]*100,
                                                                         Premium.cvt.p.total = df.bu.so.dw[40]/df.bu.so.dw[36])))

# write.csv(cbind(res.so.bu.d$Product.ratio, res.so.bu.d$Product.premium[,-c(1:2)], res.so.bu.d$Product.count[,-c(1:2)], res.so.bu.d$KPI[,-c(1:2)]), file = "res.so.bu.d.csv")

res.so.bu.m <- list(Product.ratio   = cbind(df.bu.so.mw[1:2], df.bu.so.mw[seq(13, 31, 2)]/rowSums(df.bu.so.mw[seq(13, 31, 2)])),
                    Product.premium = cbind(df.bu.so.mw[1:2], df.bu.so.mw[seq(13, 31, 2)]),
                    Product.count   = cbind(df.bu.so.mw[1:2], df.bu.so.mw[seq(12, 30, 2)]),
                    KPI             = cbind(df.bu.so.mw[1:2], data.frame(Premium.total       = df.bu.so.mw[8],
                                                                         Premium.p.total     = df.bu.so.mw[8]/df.bu.so.mw[7],
                                                                         Premium.cvt.total   = df.bu.so.mw[9],
                                                                         Count               = df.bu.so.mw[7],
                                                                         Contract.p.design   = df.bu.so.mw[7]/df.bu.so.mw[3]*100,
                                                                         Age.contractor      = df.bu.so.mw[32]/df.bu.so.mw[7],
                                                                         Age.insurant        = df.bu.so.mw[33]/df.bu.so.mw[7],
                                                                         Contract.p.design   = df.bu.so.mw[7]/df.bu.so.mw[4]*100,
                                                                         Premium.cvt.p.total = df.bu.so.mw[9]/df.bu.so.mw[7])))

# write.csv(cbind(res.so.bu.m$Product.ratio, res.so.bu.m$Product.premium[,-c(1:2)], res.so.bu.m$Product.count[,-c(1:2)], res.so.bu.m$KPI[,-c(1:2)]), file = "res.so.bu.m.csv")

res.so.bu.m.six <- data.frame()
for(m in nm.m[nm.m < m.now]){
  id.o.i  <- df.eff.mw[,1] %in% tail(nm.m[nm.m <= m], 6)
  df.o.i  <- cbind(df.eff.mw[id.o.i, c(1:2, 4, id.so.kpi.m)], df.eff.mw[id.o.i, 68:69] * df.eff.mw[id.o.i, 45])
  df.agg  <- aggregate(df.o.i[,-c(1:3)], list(df.o.i[,3]), "sum")
  res.agg <- data.frame(Eval.m    = m + 1, 
                        BR.id     = df.agg[1], 
                        P.ratio   = df.agg[seq(12, 30, 2)]/rowSums(df.agg[seq(12, 30, 2)]),
                        P.amount  = df.agg[seq(12, 30, 2)], 
                        P.count   = df.agg[seq(11, 29, 2)], df.agg[,6:8], 
                        C.avg     = df.agg[6]/length(tail(nm.m[nm.m <= m], 6)),
                        MP.p.c    = df.agg[7]/df.agg[6],
                        C.p.d     = df.agg[6]/df.agg[2]*100, 
                        C.p.i     = df.agg[6]/df.agg[3]*100, 
                        Age.c     = df.agg[31]/df.agg[6],
                        Age.i     = df.agg[32]/df.agg[6],
                        P.cvt.p.c = df.agg[8]/df.agg[6])
  res.so.bu.m.six <- rbind(res.so.bu.m.six, res.agg)
  
}

# write.csv(res.so.bu.m.six, file = "res.so.bu.six.m.csv")


# P2. Sales Activity
df.bu.sa.dw[,2] <- as.numeric(df.bu.sa.dw[,2])
res.sa.bu.pg.dw <- aggregate(df.bu.sa.dw[,-c(1:2)], list(df.bu.sa.dw[,1]), "mean")

# write.csv(df.bu.sa.dw, file = "res.sa.bu.d.csv"); write.csv(res.sa.bu.pg.dw, file = "bu.peer.group.d.csv")

res.sa.bu.pg.mw <- aggregate(df.bu.sa.mw[,-c(1:2)], list(df.bu.sa.mw[,1]), "mean")

# write.csv(df.bu.sa.mw, file = "res.sa.bu.m.csv"); write.csv(res.sa.bu.pg.mw, file = "bu.peer.group.m.csv")


# R3. BR comparison
bu.id         <- 73
br.id.all     <- unique(res.all.br[res.all.br[,2] == bu.id, 3])
res.grade.all <- data.frame(BR.id = rep(br.id.all, each = 3),
                            Stage = rep(1:3, length(br.id.all)),
                            Process = rep(c("New.contract.closing", "Profitability", "Contract.mgt"), length(br.id.all)))
for(i in unique(res.all.br[,1])){
  grade   <- matrix(NA, nrow = length(br.id.all) * 3)
  df.temp <- res.all.br[res.all.br[,1] == i & res.all.br[,2] == bu.id,]
  for(k in br.id.all){
    if(k %in% df.temp[,3]){
      grade[((which(k == br.id.all) - 1)*3 + 1):((which(k == br.id.all) - 1)*3 + 3),] <- unlist(df.temp[df.temp[,3] == k, c(7, 10, 13)])
    }else{
      next
    }
  }
  res.grade.all <- cbind(res.grade.all, grade)
}
names(res.grade.all)[4:ncol(res.grade.all)] <- unique(res.all.br[,1])
res.grade.m <- cbind(res.grade.all[,1:3], res.grade.all[,match(tail(nm.m[nm.m <= m], 6), names(res.grade.all))])
res.grade.m <- res.grade.m[apply(res.grade.m[,-c(1:3)], 1, function(x) sum(is.na(x)) != ncol(res.grade.m[,-c(1:3)])),]
res.grade.m[order(res.grade.m[,1]),]




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
