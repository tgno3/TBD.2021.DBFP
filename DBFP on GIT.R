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

# Analysis type
type <- 1 # 1: month-wise, 2: three-months average

# Input/Output
id.x.s1 <- c(13:16)
id.y.s1 <- c(17:19)
id.x.s2 <- c(17:18)
id.y.s2 <- c(20:21)

# Data for analysis
if(type == 1){
  df.2d <- df.raw
}else{
  df.2d <- data.frame()  
  for(i in 202006:202012){
    id.eff <- c()
    for(j in df.raw[which(df.raw[,1] == i), 2]){
      if(sum(df.raw[which(df.raw[,1] %in% c(i - 2, i - 1)), 2] == j) == 2){
        id.eff <- c(id.eff, j)
      }else{
        next
      }
    }

    df.temp.cr <- subset(df.raw, df.raw[,1] == i       & df.raw[,2] %in% id.eff)
    df.temp.p1 <- subset(df.raw, df.raw[,1] == (i - 1) & df.raw[,2] %in% id.eff)
    df.temp.p2 <- subset(df.raw, df.raw[,1] == (i - 2) & df.raw[,2] %in% id.eff)
    df.temp.cr <- df.temp.cr[order(df.temp.cr[,2]),]
    df.temp.p1 <- df.temp.p1[order(df.temp.p1[,2]),]
    df.temp.p2 <- df.temp.p2[order(df.temp.p2[,2]),]
    
    df.temp.cr[, c(id.x.s1, id.y.s1, id.y.s2)] <- 
      df.temp.cr[, c(id.x.s1, id.y.s1, id.y.s2)] + 
      df.temp.p1[, c(id.x.s1, id.y.s1, id.y.s2)] +
      df.temp.p2[, c(id.x.s1, id.y.s1, id.y.s2)]
    
    df.2d <- rbind(df.2d, df.temp.cr)
  }
}

# IDs for exclusion
id.out.level  <- which( is.na(match(df.2d[,3], c(100, 101, 102, 109, 112))))
id.out.branch <- which(!is.na(match(df.2d[,5], c(8262, 8312, 8335, 8354))))
id.out.bunit  <- which(!is.na(match(df.2d[,4], c(74, NA))))
id.out.month  <- which(df.2d[,10] == 1)

# Zero output
id.out.zero.s1 <- which(rowSums(df.2d[,c(17, 18)]) == 0)
#id.out.zero.s2 <- which(rowSums(df.2d[,c(20, 21)]) == 0)

# Month of interest
moi        <- 202006
id.out.moe <- which(df.2d[,1] != moi)

# Effective data
id.out.all <- unique(c(id.out.level, id.out.branch, id.out.bunit, id.out.month,
                       id.out.zero.s1, id.out.moe))
df.eff     <- df.2d[-id.out.all,]

# Class
id.nb      <- which(df.eff[,9] == 1)
id.ob      <- which(df.eff[,9] == 0)
id.m.02.04 <- which(df.eff[,10] > 1 & df.eff[,10] < 5)
id.m.05.07 <- which(df.eff[,10] > 4 & df.eff[,10] < 8)
id.m.08.13 <- which(df.eff[,10] > 7 & df.eff[,10] < 14)
id.m.14.00 <- which(df.eff[,10] > 13)

# Count
length(intersect(id.nb, id.m.02.04))
length(intersect(id.nb, id.m.05.07))
length(intersect(id.nb, id.m.08.13))
length(intersect(id.nb, id.m.14.00))
length(intersect(id.ob, id.m.02.04))
length(intersect(id.ob, id.m.05.07))
length(intersect(id.ob, id.m.08.13))
length(intersect(id.ob, id.m.14.00))


#########################################################################################################################
### FP Evaluation
#########################################################################################################################

# Productivity analysis
res.eff.fp <- data.frame(); res.s1.l <- res.s2.l <- list()
for(i in 1:2){
  for(j in 1:4){
    id.type    <- if(i == 1) id.nb else id.ob
    id.month   <- if(j == 1) id.m.02.04 else if(j == 2) id.m.05.07 else if(j == 3) id.m.08.13 else id.m.14.00
    id.eff     <- intersect(id.type, id.month)
    adj.min    <- apply(df.eff[id.eff, id.y.s2], 2, function(x) if(min(x) < 0) -min(x) else 0)
    df.temp    <- df.eff[id.eff, ]; df.temp[,id.y.s2] <- t(t(df.temp[,id.y.s2]) + adj.min)
    g          <- cbind(matrix(rep(0, length(id.eff) * length(id.x.s1)), ncol = length(id.x.s1)), df.temp[,id.y.s1])
    wd         <- c(0, 0, 1)
    id.calc    <- which(rowSums(df.temp[,id.y.s2]) > 0)
    xdata.s1   <- df.temp[,id.x.s1]
    ydata.s1   <- df.temp[,id.y.s1]
    xdata.s2   <- df.temp[,id.x.s2]
    ydata.s2   <- df.temp[,id.y.s2]
    res.s1     <- dm.sf (xdata.s1, ydata.s1, "vrs", g, wd)
    res.s2     <- dm.dea(xdata.s2, ydata.s2, "vrs", "o", o = id.calc)
    res.s1.l   <- list(res.s1.l, res.s1$lambda)
    res.s2.l   <- list(res.s2.l, res.s2$lambda)
    res.eff.fp <- rbind(res.eff.fp,
                        data.frame(FP.type  = i,
                                   FP.month = j,
                                   FP.id    = df.temp[,2],
                                   Eff.s1   = res.s1$eff + 1,
                                   Eff.s2   = res.s2$eff))
  }
}

# Cleanse lambda list
res.lambda <- list(list(res.s1.l[[1]][[1]][[1]][[2]], res.s1.l[[1]][[1]][[2]], res.s1.l[[1]][[2]], res.s1.l[[2]]),
                   list(res.s2.l[[1]][[1]][[1]][[2]], res.s2.l[[1]][[1]][[2]], res.s2.l[[1]][[2]], res.s2.l[[2]]))

# List of efficient FP
res.eff[which(rowSums(res.eff[,c(4:5)]) == 2),]


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





