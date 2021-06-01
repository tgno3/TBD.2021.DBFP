#########################################################################################################################
### Project  : FP Performance Evaluation
### Script   : DBFP on GIT.R
### Contents : FP Performance Evaluation Model - Two stages
#########################################################################################################################

#########################################################################################################################
### Setting up environment
#########################################################################################################################

# Load library
pkgs <- c("DJL")
sapply(pkgs, require, character.only = T)

# Load data & parameters
load("DBFP.2003.2012.Rdata")


#########################################################################################################################
### Data cleansing
#########################################################################################################################

# IDs for exclusion
id.out.level   <- which( is.na(match(df.raw[,3], c(100, 101, 102, 109, 112))))
id.out.branch  <- which(!is.na(match(df.raw[,5], c(8262, 8312, 8335, 8354))))
id.out.bunit   <- which(!is.na(match(df.raw[,4], c(74, NA))))
id.out.month   <- which(df.raw[,10] == 1)
id.out.zero.s1 <- which(rowSums(df.raw[,c(17, 18)]) == 0)

# Effective data
id.out.all <- unique(c(id.out.level, id.out.branch, id.out.bunit, id.out.month, id.out.zero.s1))
df.temp    <- df.raw[-id.out.all,]
df.eff.mw  <- df.temp[order(df.temp[,1], df.temp[,2]),]

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


#########################################################################################################################
### FP Evaluation
#########################################################################################################################

# Month of interest
m        <- 202012
df.fp.mw <- df.eff.mw[df.eff.mw[,1] == m,]
df.fp.sa <- df.eff.sa[df.eff.sa[,1] == m,]

# Only consider the same FP for mw
df.fp.mw <- df.fp.mw[df.fp.mw[,2] %in% df.fp.sa[,2],]

# IDs
id.x.s1 <- c(13:16)
id.y.s1 <- c(17:19)
id.x.s2 <- c(17:18)
id.y.s2 <- c(20:21)
id.m.02.04.mw <- which(df.fp.mw[,10] < 5)
id.m.05.07.mw <- which(df.fp.mw[,10] > 4 & df.fp.mw[,10] < 8)
id.m.08.13.mw <- which(df.fp.mw[,10] > 7 & df.fp.mw[,10] < 14)
id.m.14.00.mw <- which(df.fp.mw[,10] > 13)
id.m.02.04.sa <- which(df.fp.sa[,10] < 5)
id.m.05.07.sa <- which(df.fp.sa[,10] > 4 & df.fp.sa[,10] < 8)
id.m.08.13.sa <- which(df.fp.sa[,10] > 7 & df.fp.sa[,10] < 14)
id.m.14.00.sa <- which(df.fp.sa[,10] > 13)

# Productivity analysis
res.eff.fp.mw <- res.eff.fp.sa <- data.frame(); res.s1.l.mw <- res.s2.l.mw <- res.s1.l.sa <- res.s2.l.sa <- list()
for(i in 1:2){
  for(j in 1:4){
    id.type.mw    <- if(i == 1) which(df.fp.mw[,9] == 1) else which(df.fp.mw[,9] == 0)
    id.type.sa    <- if(i == 1) which(df.fp.sa[,9] == 1) else which(df.fp.sa[,9] == 0)
    id.m.mw       <- if(j == 1) id.m.02.04.mw else if(j == 2) id.m.05.07.mw else if(j == 3) id.m.08.13.mw else id.m.14.00.mw
    id.m.sa       <- if(j == 1) id.m.02.04.sa else if(j == 2) id.m.05.07.sa else if(j == 3) id.m.08.13.sa else id.m.14.00.sa
    id.eff.mw     <- intersect(id.type.mw, id.m.mw)
    id.eff.sa     <- intersect(id.type.sa, id.m.sa)
    adj.min.mw    <- apply(df.fp.mw[id.eff.mw, id.y.s2], 2, function(x) if(min(x) < 0) -min(x) else 0)
    adj.min.sa    <- apply(df.fp.sa[id.eff.sa, id.y.s2], 2, function(x) if(min(x) < 0) -min(x) else 0)
    df.temp.mw    <- df.fp.mw[id.eff.mw,]; df.temp.mw[,id.y.s2] <- t(t(df.temp.mw[,id.y.s2]) + adj.min.mw)
    df.temp.sa    <- df.fp.sa[id.eff.sa,]; df.temp.sa[,id.y.s2] <- t(t(df.temp.sa[,id.y.s2]) + adj.min.sa)
    g.mw          <- cbind(matrix(rep(0, length(id.eff.mw) * length(id.x.s1)), ncol = length(id.x.s1)), df.temp.mw[,id.y.s1])
    g.sa          <- cbind(matrix(rep(0, length(id.eff.sa) * length(id.x.s1)), ncol = length(id.x.s1)), df.temp.sa[,id.y.s1])
    wd            <- c(0, 0, 1)
    id.calc.mw    <- which(rowSums(df.temp.mw[,id.y.s2]) > 0)
    id.calc.sa    <- which(rowSums(df.temp.sa[,id.y.s2]) > 0)
    res.s1.mw     <- dm.sf (df.temp.mw[,id.x.s1], df.temp.mw[,id.y.s1], "vrs", g.mw, wd)
    res.s1.sa     <- dm.sf (df.temp.sa[,id.x.s1], df.temp.sa[,id.y.s1], "vrs", g.sa, wd)
    res.s2.mw     <- dm.dea(df.temp.mw[,id.x.s2], df.temp.mw[,id.y.s2], "vrs", "o", o = id.calc.mw)
    res.s2.sa     <- dm.dea(df.temp.sa[,id.x.s2], df.temp.sa[,id.y.s2], "vrs", "o", o = id.calc.sa)
    res.s1.l.mw   <- list(res.s1.l.mw, res.s1.mw$lambda)
    res.s1.l.sa   <- list(res.s1.l.sa, res.s1.sa$lambda)
    res.s2.l.mw   <- list(res.s2.l.mw, res.s2.mw$lambda)
    res.s2.l.sa   <- list(res.s2.l.sa, res.s2.sa$lambda)
    res.eff.fp.mw <- rbind(res.eff.fp.mw, data.frame(FP.type  = i,
                                                     FP.month = j,
                                                     FP.id    = df.temp.mw[,2],
                                                     Closed.m = m,
                                                     Eff.s1   = res.s1.mw$eff + 1,
                                                     Eff.s2   = res.s2.mw$eff))
    res.eff.fp.sa <- rbind(res.eff.fp.sa, data.frame(FP.type  = i,
                                                     FP.month = j,
                                                     FP.id    = df.temp.sa[,2],
                                                     Closed.m = m,
                                                     Eff.s1   = res.s1.sa$eff + 1,
                                                     Eff.s2   = res.s2.sa$eff))
  }
}

# Cleanse lambda list
res.l.mw <- list(list(res.s1.l.mw[[1]][[1]][[1]][[2]], res.s1.l.mw[[1]][[1]][[2]], res.s1.l.mw[[1]][[2]], res.s1.l.mw[[2]]),
                 list(res.s2.l.mw[[1]][[1]][[1]][[2]], res.s2.l.mw[[1]][[1]][[2]], res.s2.l.mw[[1]][[2]], res.s2.l.mw[[2]]))
res.l.sa <- list(list(res.s1.l.sa[[1]][[1]][[1]][[2]], res.s1.l.sa[[1]][[1]][[2]], res.s1.l.sa[[1]][[2]], res.s1.l.sa[[2]]),
                 list(res.s2.l.sa[[1]][[1]][[1]][[2]], res.s2.l.sa[[1]][[1]][[2]], res.s2.l.sa[[1]][[2]], res.s2.l.sa[[2]]))

# Comparison of the results
res.eff <- data.frame(res.eff.fp.mw, res.eff.fp.sa[,5:6], 
                      Diff.r.s1 = round(res.eff.fp.mw[,5] / res.eff.fp.sa[,5], 2),
                      Diff.r.s2 = round(res.eff.fp.mw[,6] / res.eff.fp.sa[,6], 2))

# Stats
temp.s1 <- aggregate(res.eff[res.eff[,1] == 1, 5:8], list(res.eff[res.eff[,1] == 1, 2]), mean, na.rm = T)
temp.s2 <- aggregate(res.eff[res.eff[,1] == 2, 5:8], list(res.eff[res.eff[,1] == 2, 2]), mean, na.rm = T)
round(temp.s1[,2] - temp.s1[,4], 2);round(temp.s1[,3] - temp.s1[,5], 2)
round(temp.s2[,2] - temp.s2[,4], 2);round(temp.s2[,3] - temp.s2[,5], 2)

# Extract extreme differences
res.eff[c(which(res.eff$Diff.r.s1 == min(res.eff$Diff.r.s1)), which(res.eff$Diff.r.s1 == max(res.eff$Diff.r.s1)),
          which(res.eff$Diff.r.s2 == min(res.eff$Diff.r.s2, na.rm = T)), which(res.eff$Diff.r.s2 == max(res.eff$Diff.r.s2, na.rm = T))),]

# Further investigation
id.fp <- 840225; m <- 202009
id.fp <-   2553; m <- 202010
id.fp <- 850471; m <- 202010
id.fp <- 871031; m <- 202009

t(rbind(df.eff.mw[df.eff.mw[,1] == (m - 2) & df.eff.mw[,2] == id.fp, c(1, id.x.s1, id.y.s1, id.y.s2)],
        df.eff.mw[df.eff.mw[,1] == (m - 1) & df.eff.mw[,2] == id.fp, c(1, id.x.s1, id.y.s1, id.y.s2)],
        df.eff.mw[df.eff.mw[,1] == m       & df.eff.mw[,2] == id.fp, c(1, id.x.s1, id.y.s1, id.y.s2)],
        df.eff.sa[df.eff.sa[,1] == m       & df.eff.sa[,2] == id.fp, c(1, id.x.s1, id.y.s1, id.y.s2)]))


#########################################################################################################################
### Branch Evaluation
#########################################################################################################################

# Descriptive stats: Incu vs Normal, 20>= vs 20<
table(df.eff[,6], df.eff[,8])

# Data aggregation
v.all         <- c(id.x.s1, id.y.s1, id.y.s2)
id.inc        <- which(df.eff[,6] == 1)
id.nor.sm     <- which(df.eff[,6] == 0 & df.eff[,8] == 1)
id.nor.lg     <- which(df.eff[,8] == 2)
df.eff.inc    <- aggregate(df.eff[id.inc, v.all], list(df.eff[id.inc, 5]), sum)
df.eff.nor.sm <- aggregate(df.eff[id.nor.sm, v.all], list(df.eff[id.nor.sm, 5]), sum)
df.eff.nor.lg <- aggregate(df.eff[id.nor.lg, v.all], list(df.eff[id.nor.lg, 5]), sum)

# Productivity analysis
type           <- 1 # 1:incu, 2:normal.small, 3:normal.large
df.temp        <- if(type == 1) df.eff.inc else if(type == 2) df.eff.nor.sm else df.eff.nor.lg
adj.min        <- apply(df.temp[,9:10], 2, function(x) if(min(x) < 0) -min(x) else 0)
df.temp[,9:10] <- t(t(df.temp[,9:10]) + adj.min)
g              <- cbind(matrix(rep(0, nrow(df.temp) * length(id.x.s1)), ncol = length(id.x.s1)), df.temp[,6:8])
wd             <- c(0, 0, 1)
res.s1         <- dm.sf (df.temp[,2:5], df.temp[,6:8], "vrs", g, wd)
res.s2         <- dm.dea(df.temp[,6:7], df.temp[,9:10], "vrs", "o")
res.eff.br     <- data.frame(BR.id  = df.temp[,1],
                             Eff.s1 = res.s1$eff + 1,
                             Eff.s2 = res.s2$eff)





