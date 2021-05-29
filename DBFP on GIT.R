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
id.out.level  <- which( is.na(match(df.2d[,3], c(100, 101, 102, 109, 112))))
id.out.branch <- which(!is.na(match(df.2d[,5], c(8262, 8312, 8335, 8354))))
id.out.bunit  <- which(!is.na(match(df.2d[,4], c(74, NA))))
id.out.month  <- which(df.2d[,10] == 1)

# Input/Output
id.x.s1    <- c(13:16)
id.y.s1    <- c(17:19)
id.x.s2    <- c(17:18)
id.y.s2    <- c(20:21)

# Zero outputs
id.out.zero.s1 <- which(rowSums(df.2d[,c(17, 18)]) == 0)
id.out.zero.s2 <- which(rowSums(df.2d[,c(20, 21)]) == 0)

# Effective data
id.out.all <- unique(c(id.out.level, id.out.branch, id.out.bunit, id.out.month,
                       id.out.zero.s1, id.out.zero.s2))
df.eff     <- df.2d[-id.out.all,]

# Class
id.nb      <- which(df.eff[,9] == 1)
id.ob      <- which(df.eff[,9] == 0)
id.m.2.4   <- which(df.eff[,10] > 1 & df.eff[,10] < 5)
id.m.5.7   <- which(df.eff[,10] > 4 & df.eff[,10] < 8)
id.m.8.13  <- which(df.eff[,10] > 7 & df.eff[,10] < 14)
id.m.14.00 <- which(df.eff[,10] > 13)

# Count
length(intersect(id.nb, id.m.2.4))
length(intersect(id.nb, id.m.5.7))
length(intersect(id.nb, id.m.8.13))
length(intersect(id.nb, id.m.14.00))
length(intersect(id.ob, id.m.2.4))
length(intersect(id.ob, id.m.5.7))
length(intersect(id.ob, id.m.8.13))
length(intersect(id.ob, id.m.14.00))


#########################################################################################################################
### Analysis
#########################################################################################################################

# Month-wise Efficiency analysis
res.eff <- data.frame()
for(i in 1:2){
  for(j in 1:4){
    id.type  <- if(i == 1) id.nb else id.ob
    id.month <- if(j == 1) id.m.2.4 else if(j == 2) id.m.5.7 else if(j == 3) id.m.8.13 else id.m.14.00
    id.eff   <- intersect(id.type, id.month)
    g        <- cbind(matrix(rep(0, length(id.eff) * length(id.x.s1)), ncol = length(id.x.s1)),
                      df.eff[id.eff, id.y.s1])
    wd       <- c(0, 0, 1)
    id.calc  <- which(rowSums(df.eff[id.eff, id.y.s2]) > 0)
    xdata.s1 <- df.eff[id.eff, id.x.s1]
    ydata.s1 <- df.eff[id.eff, id.y.s1]
    xdata.s2 <- df.eff[id.eff, id.x.s2]
    ydata.s2 <- df.eff[id.eff, id.y.s2]
    res.eff  <- rbind(res.eff,
                      data.frame(FP.type  = i,
                                 FP.month = j,
                                 FP.id    = df.eff[id.eff, 2],
                                 Eff.s1   = dm.sf (xdata.s1, ydata.s1, "vrs", g, wd)$eff + 1,
                                 Eff.s2   = dm.dea(xdata.s2, ydata.s2, "vrs", "o", o = id.calc)$eff))
  }
}



