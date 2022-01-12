#########################################################################################################################
### DNDEA.v2.0 - max.cp of 2
#########################################################################################################################

dm.dynamic.network <- function(xdata.s1, ydata.s1 = NULL, zdata, xdata.s2 = NULL, ydata.s2, 
                               rts = "crs", orientation = "i", type = "nc", leader = "1st", 
                               alpha = c(1, 0, 0), max.cp = 0, LB = NULL, UB = NULL, t.w = NULL, o = NULL, exhaust = T){
  
  # Initial checks
  if(is.na(match(rts, c("crs", "vrs", "irs", "drs"))))          stop('rts must be "crs", "vrs", "irs", or "drs".')
  if(is.na(match(orientation, c("i", "o"))))                    stop('orientation must be either "i" or "o".')
  if(is.na(match(type, c("co", "nc"))))                         stop('type must be "co" or "nc".')
  if(is.na(match(leader, c("1st", "2nd"))))                     stop('leader must be "1st" or "2nd".')
  if(!is.null(t.w) && dim(xdata.s1)[3] != length(t.w))          stop('t.w must have a length of the period.')
  if(!is.null(o) && !all(o <= nrow(xdata.s1)))                  stop('o must be element(s) of n.')
  if(!isTRUE(alpha == "free") && sum(alpha) != 1)               stop('alpha must sum to one.')
  
  # Parameters
  xdata.s1 <- if(length(dim(xdata.s1)) != 3) array(xdata.s1, c(dim(xdata.s1)[1], 1, dim(xdata.s1)[2])) else as.array(xdata.s1)
  ydata.s2 <- if(length(dim(ydata.s2)) != 3) array(ydata.s2, c(dim(ydata.s2)[1], 1, dim(ydata.s2)[2])) else as.array(ydata.s2)
  zdata    <- if(length(dim(zdata   )) != 3) array(zdata,    c(dim(zdata)[1],    1, dim(zdata)[2]))    else as.array(zdata)
  if(!is.null(ydata.s1)){ydata.s1 <- if(length(dim(ydata.s1)) != 3) array(ydata.s1, c(dim(ydata.s1)[1], 1, dim(ydata.s1)[2])) else as.array(ydata.s1)}
  if(!is.null(xdata.s2)){xdata.s2 <- if(length(dim(xdata.s2)) != 3) array(xdata.s2, c(dim(xdata.s2)[1], 1, dim(xdata.s2)[2])) else as.array(xdata.s2)}
  n        <- nrow(xdata.s1)
  m.s1     <- ncol(xdata.s1)
  s.s1     <- ifelse(is.null(ydata.s1), 0, ncol(ydata.s1))
  p        <- ncol(zdata)
  m.s2     <- ifelse(is.null(xdata.s2), 0, ncol(xdata.s2))
  s.s2     <- ncol(ydata.s2)
  o        <- if(is.null(o))  c(1:n) else as.vector(o)
  max.cp   <- ifelse(is.numeric(alpha), length(alpha) - 1, max.cp)
  t        <- dim(xdata.s1)[3]
  t.w      <- if(is.null(t.w)) rep(1, t) else c(t.w)
  LB       <- if(is.null(LB)) array(rep(0, p*(max.cp + 1)*t), c(p, (max.cp + 1), t)) else aperm(array(rep(LB, p*t), c((max.cp + 1), p, t)), c(2, 1, 3))
  UB       <- if(is.null(UB)) array(rep(1, p*(max.cp + 1)*t), c(p, (max.cp + 1), t)) else aperm(array(rep(UB, p*t), c((max.cp + 1), p, t)), c(2, 1, 3))
  
  # Data frames
  #res.eff.sys <- array(NA, c(n,    1, t))
  res.eff.s1 <- array(NA, c(n,    1, t))
  res.eff.s2 <- array(NA, c(n,    1, t))
  res.v.s1   <- array(NA, c(n, m.s1, t)) 
  res.u.s1   <- array(NA, c(n, s.s1, t)) 
  res.f.s1   <- array(NA, c(n,    1, t)) 
  res.v.s2   <- array(NA, c(n, m.s2, t)) 
  res.u.s2   <- array(NA, c(n, s.s2, t)) 
  res.f.s2   <- array(NA, c(n,    1, t))
  res.w      <- array(NA, c(n,    p, t)) 
  res.r.0    <- array(NA, c(n,    p, t)) 
  res.r.1    <- array(NA, c(n,    p, t))
  res.r.2    <- array(NA, c(n,    p, t))
  res.alpha  <- array(NA, c(n, p*(max.cp + 1), t)) 
  
  # Indices for coding convenience
  no.dv.t <- (m.s1 + s.s1 + p + p + p + p + 1 + m.s2 + s.s2 + 1) 
  id.v.s1 <- array(c(1:m.s1 + rep(no.dv.t * 0:(t-1), each = m.s1)), c(1, m.s1, t))
  id.u.s1 <- if(is.null(ydata.s1)) 0 else array(c((m.s1 + 1):(m.s1 + s.s1) + rep(no.dv.t * 0:(t-1), each = s.s1)), c(1, s.s1, t))
  id.w    <- array(c((m.s1 + s.s1 + 1):(m.s1 + s.s1 + p) + rep(no.dv.t * 0:(t-1), each = p)), c(1, p, t))
  id.a    <- id.w + p; id.b <- id.a + p; id.c <- id.b + p
  id.r    <- array(rbind(id.a, id.b, id.c), c(3, p, t))
  id.f.s1 <- array(c(m.s1 + s.s1 + p + p + p + p + 1 + no.dv.t * 0:(t-1)), c(1, 1, t))
  id.v.s2 <- if(is.null(xdata.s2)) 0 else array(c((id.f.s1[,,1] + 1):(id.f.s1[,,1] + m.s2) + rep(no.dv.t * 0:(t-1), each = m.s2)), c(1, m.s2, t))
  id.u.s2 <- array(c((id.f.s1[,,1] + m.s2 + 1):(id.f.s1[,,1] + m.s2 + s.s2) + rep(no.dv.t * 0:(t-1), each = s.s2)), c(1, s.s2, t))
  id.f.s2 <- array(c(no.dv.t * 1:t), c(1, 1, t))
  id.z    <- z.ag <- list()
  for(i in 1:t){
    if(i == 1){
      id.z[[i]] <- c(id.a[,,i]) 
      z.ag[[i]] <- matrix(zdata[,,1], n)
    }else if(i == 2){
      id.z[[i]] <- c(id.a[,,i], id.b[,,i - 1])
      z.ag[[i]] <- cbind(zdata[,,i], zdata[,,i - 1])
    }else{
      id.z[[i]] <- c(id.a[,,i], id.b[,,i - 1], if(max.cp > 1) id.c[,,i - 2])
      z.ag[[i]] <- cbind(zdata[,,i], zdata[,,i - 1], if(max.cp > 1) zdata[,,i - 2])
    }
  }
  
  # Fixed carryovers for coding convenience
  if(is.numeric(alpha) & isTRUE(exhaust)){
    alpha.ar <- array(alpha, c(p, max.cp + 1, t))
    if(max.cp > 0){
      alpha.ar[,2:(max.cp + 1), t - 1] <- matrix(c(rowSums(alpha.ar[,2:(max.cp + 1), t - 1, drop = F]), if(max.cp == 2) rep(0, p)), p)  
    }
    alpha.ar[,,t] <- c(rep(1, p), rep(0, p*max.cp))
  } 
  
  # Model
  if(type == "nc"){
    if(leader == "1st"){
      for(k in o){
        # LP
        lp.p1 <- make.lp(0, no.dv.t * t) # v1 (+ u1) + w + r0 + r1 + r2 + f1 (+ v2) + u2 + f2
        
        # Labelling
        temp <- c()
        for(i in 1:t){temp <- c(temp, 
                                paste0(i, "v1", 1:m.s1), 
                                if(!is.null(ydata.s1)) paste0(i, "u1", 1:s.s1), 
                                paste0(i, "w", 1:p), paste0(i, "r0", 1:p), paste0(i, "r1", 1:p), paste0(i, "r2", 1:p), 
                                paste0(i, "f1"), 
                                if(!is.null(xdata.s2)) paste0(i, "v2", 1:m.s2), 
                                paste0(i, "u2", 1:s.s2), paste0(i, "f2"))}
        dimnames(lp.p1)[[2]] <- temp
        
        #####################################
        ### PHASE I. Leader - 1st stage
        #####################################
        
        # Set objective
        if(orientation == "i"){ # w((-u1y1) - wz + f1)
          if(is.null(ydata.s1)){
            set.objfn(lp.p1, c(-zdata[k,,] * t.w, t.w), indices = c(id.w, id.f.s1))
          }else{
            set.objfn(lp.p1, c(-ydata.s1[k,,] * t.w, -zdata[k,,] * t.w, t.w), indices = c(id.u.s1, id.w, id.f.s1))
          }
        }
        if(orientation == "o"){ # w(v1x1 + f1)
          set.objfn(lp.p1, c(xdata.s1[k,,] * t.w, t.w), indices = c(id.v.s1, id.f.s1))
        }
        
        # Constraints (exclusive in PHASE I)
        for(i in 1:t){
          # Orientation specific constraints
          if(orientation == "i"){
            # v1x1 = 1 for DMU o
            add.constraint(lp.p1, xdata.s1[k,,i], indices = id.v.s1[,,i], "=", 1)
          }
          if(orientation == "o"){
            # wz (+ u1y1) = 1 for DMU o
            if(is.null(ydata.s1)){
              add.constraint(lp.p1, zdata[k,,i], indices = id.w[,,i], "=", 1)
            }else{
              add.constraint(lp.p1, c(zdata[k,,i], ydata.s1[k,,i]), indices = c(id.w[,,i], id.u.s1[,,i]), "=", 1)
            }
          }
        }
        
        # Constraints (shared with PHASE II)
        for(i in 1:t){
          # Common constraints
          # -v1x1 (+ u1y1) + wz - f1 <= 0 for all
          for(d in o){
            if(is.null(ydata.s1)){
              add.constraint(lp.p1, c(-xdata.s1[d,,i], zdata[d,,i], -1), 
                             indices = c(id.v.s1[,,i], id.w[,,i], id.f.s1[,,i]), "<=", 0)
            }else{
              add.constraint(lp.p1, c(-xdata.s1[d,,i], ydata.s1[d,,i], zdata[d,,i], -1), 
                             indices = c(id.v.s1[,,i], id.u.s1[,,i], id.w[,,i], id.f.s1[,,i]), "<=", 0)
            }
          }
        }
        
        # RTS
        if(rts == "crs") add.constraint(lp.p1, rep(1, 2*t), indices = c(id.f.s1, id.f.s2), "=", 0)
        #if(rts == "irs") TBA
        #if(rts == "drs") TBA
        
        # Bounds
        temp.lb <- rep(0, no.dv.t * t)
        if(rts == "vrs"){temp.lb[c(id.f.s1, id.f.s2)] <- -Inf}
        set.bounds(lp.p1, lower = temp.lb)  
        
        # Solve
        solve.lpExtPtr(lp.p1)
        
        # Get results
        res.all <- get.variables(lp.p1)
        for(i in 1:t){
          if(orientation == "i"){ # (u1y1 +) wz - f1
            if(is.null(ydata.s1)){
              res.eff.s1[k,,i] <- sum(res.all[id.w[,,i]] * zdata[k,,i]) - res.all[id.f.s1[,,i]]
            }else{
              res.eff.s1[k,,i] <- sum(res.all[id.u.s1[,,i]] * ydata.s1[k,,i]) + sum(res.all[id.w[,,i]] * zdata[k,,i]) - res.all[id.f.s1[,,i]]
            }
          }
          if(orientation == "o"){ # v1x1 + f1
            res.eff.s1[k,,i] <- sum(res.all[id.v.s1[,,i]] * xdata.s1[k,,i]) + res.all[id.f.s1[,,i]]
          }
        }
        
        #####################################
        ### PHASE II. Follower - 2nd stage
        #####################################
        
        # Copy LP object for recycling
        lp.p2 <- lp.p1
        
        # Re-set objective
        if(orientation == "i"){ # w(-u2y2 + f2)
          set.objfn(lp.p2, c(-ydata.s2[k,,] * t.w, t.w), indices = c(id.u.s2, id.f.s2))
        }
        if(orientation == "o"){ # w(∑rz (+ v2x2) + f2)
          if(is.null(xdata.s2)){
            set.objfn(lp.p2, c(rep(c(zdata[k,,] * rep(t.w, each = p)), max.cp + 1), t.w), 
                      indices = c(c(id.a, id.b, id.c)[1:(p*t*(max.cp + 1))], id.f.s2))            
          }else{
            set.objfn(lp.p2, c(rep(c(zdata[k,,] * rep(t.w, each = p)), max.cp + 1), xdata.s2[k,,] * t.w, t.w),
                      indices = c(c(id.a, id.b, id.c)[1:(p*t*(max.cp + 1))], id.v.s2, id.f.s2))
          }
        }
        
        # Delete PHASE I specific constraints
        delete.constraint(lp.p2, 1:t)
        
        # Constraints of PHASE II
        for(i in 1:t){
          # Common constraint
          # (-v2x2) - ∑rz + u2y2 - f2 <= 0 for all
          for(d in o){
            if(is.null(xdata.s2)){
              add.constraint(lp.p2, c(-z.ag[[i]][d,], ydata.s2[d,,i], -1), 
                             indices = c(id.z[[i]], id.u.s2[,,i], id.f.s2[,,i]), "<=", 0)
            }else{
              add.constraint(lp.p2, c(-xdata.s2[d,,i], -z.ag[[i]][d,], ydata.s2[d,,i], -1), 
                             indices = c(id.v.s2[,,i], id.z[[i]], id.u.s2[,,i], id.f.s2[,,i]), "<=", 0)
            }
          }
          
          # Orientation specific constraints
          if(orientation == "i"){
            # (v2x2) + ∑rz = 1 for DMU o
            if(is.null(xdata.s2)){
              add.constraint(lp.p2, z.ag[[i]][k,], indices = id.z[[i]], "=", 1)  
            }else{
              add.constraint(lp.p2, c(xdata.s2[k,,i], z.ag[[i]][k,]), indices = c(id.v.s2[,,i], id.z[[i]]), "=", 1)
            }
            
            # -v1x1*E1 + wz (+ u1y1) - f1 = 0 for DMU o
            if(is.null(ydata.s1)){
              add.constraint(lp.p2, c(-xdata.s1[k,,i]*res.eff.s1[k,,i], zdata[k,,i], -1),
                             indices = c(id.v.s1[,,i], id.w[,,i], id.f.s1[,,i]), "=", 0)
            }else{
              add.constraint(lp.p2, c(-xdata.s1[k,,i]*res.eff.s1[k,,i], zdata[k,,i], ydata.s1[k,,i], -1),
                             indices = c(id.v.s1[,,i], id.w[,,i], id.u.s1[,,i], id.f.s1[,,i]), "=", 0)
            }
          }
          if(orientation == "o"){
            # u2y2 = 1 for DMU o
            add.constraint(lp.p2, ydata.s2[k,,i], indices = id.u.s2[,,i], "=", 1)
            
            # -v1x1/E1 - f1/E1 (+ u1y1) + wz = 0 for DMU o
            if(is.null(ydata.s1)){
              add.constraint(lp.p2, c(-xdata.s1[d,,i]/res.eff.s1[k,,i], -1/res.eff.s1[k,,i], zdata[d,,i]), 
                             indices = c(id.v.s1[,,i], id.f.s1[,,i], id.w[,,i]), "=", 0)
            }else{
              add.constraint(lp.p2, c(-xdata.s1[d,,i]/res.eff.s1[k,,i], -1/res.eff.s1[k,,i], ydata.s1[d,,i], zdata[d,,i]), 
                             indices = c(id.v.s1[,,i], id.f.s1[,,i], id.u.s1[,,i], id.w[,,i]), "=", 0)
            }
          } 
          
          # Carry-over constraints
          if(!is.numeric(alpha)){
            for(j in 1:p){
              # w = sum(r) for all periods
              add.constraint(lp.p2, c(1, rep(-1, max.cp + 1)), indices = c(id.w[,j,i], id.r[1:(max.cp + 1),j,i]), "=", 0)
              
              # r+ = r++ = 0 when no carry-over
              if(max.cp == 0){
                add.constraint(lp.p2, rep(1, length(c(id.b, id.c))), indices = c(id.b, id.c), "=", 0)
                LB[LB > 0] <- 0; UB[UB < 1] <- 1
              }
              
              # r++ = 0 for the period second to the last when exhaust is T
              if(isTRUE(exhaust) & i == (t - 1)){
                add.constraint(lp.p2, 1, indices = id.c[,j,i], "=", 0)
                LB[j, -c(1:2), i] <- 0; UB[j, -c(1:2), i] <- 0
              }
              
              # w = r0 for the last period when exhaust is T
              if(isTRUE(exhaust) & i == t){
                add.constraint(lp.p2, c(1, -1), indices = c(id.w[,j,i], id.a[,j,i]), "=", 0)
                LB[j,,i] <- 0; UB[j, -1, i] <- 0
              }
              
              # w*LB <= r0 & r+ & r++ <= w*UB
              for(r in 1:(max.cp + 1)){
                # w*LB <= r
                add.constraint(lp.p2, c(LB[j,r,i], -1), indices = c(id.w[,j,i], id.r[r,j,i]), "<=", 0)    
                
                # r <= w*UB
                add.constraint(lp.p2, c(-UB[j,r,i], 1), indices = c(id.w[,j,i], id.r[r,j,i]), "<=", 0)  
              }
            }
          }else{
            for(j in 1:p){
              # w * alpha = r
              for(r in 1:(max.cp + 1)){
                if(r == 1){add.constraint(lp.p2, c(alpha.ar[j,1,i], -1), indices = c(id.w[,j,i], id.a[,j,i]), "=", 0)}
                if(r == 2){add.constraint(lp.p2, c(alpha.ar[j,2,i], -1), indices = c(id.w[,j,i], id.b[,j,i]), "=", 0)}
                if(r == 3){add.constraint(lp.p2, c(alpha.ar[j,3,i], -1), indices = c(id.w[,j,i], id.c[,j,i]), "=", 0)}
              }
            }
          }
        }
        
        # Solve
        solve.lpExtPtr(lp.p2)
        
        # Get results
        res.all <- get.variables(lp.p2)
        for(i in 1:t){
          if(orientation == "i"){ # u2y2 - f2
            res.eff.s2[k,,i] <- sum(res.all[id.u.s2[,,i]] * ydata.s2[k,,i]) - res.all[id.f.s2[,,i]]
          }
          if(orientation == "o"){ # ∑rz (+ v2x2) + f2
            if(is.null(xdata.s2)){
              res.eff.s2[k,,i] <- sum(res.all[id.z[[i]]] * z.ag[[i]][k,]) + res.all[id.f.s2[,,i]]
            }else{
              res.eff.s2[k,,i] <- sum(res.all[id.z[[i]]] * z.ag[[i]][k,]) + sum(res.all[id.v.s2[,,i]] * xdata.s2[k,,i]) + res.all[id.f.s2[,,i]]
            }
          }
        }
        res.v.s1[k,,]      <- res.all[id.v.s1]
        res.u.s1[k,,]      <- if(is.null(ydata.s1)) NULL else res.all[id.u.s1]
        res.f.s1[k,,]      <- res.all[id.f.s1]
        res.v.s2[k,,]      <- if(is.null(xdata.s2)) NULL else res.all[id.v.s2]
        res.u.s2[k,,]      <- res.all[id.u.s2]
        res.f.s2[k,,]      <- res.all[id.f.s2]
        res.w[k,,]         <- res.all[id.w]
        res.r.0[k,,]       <- res.all[id.a]
        res.r.1[k,,]       <- res.all[id.b]
        res.r.2[k,,]       <- res.all[id.c]
        res.alpha[k, 1:p,] <- res.all[id.a]/res.all[id.w]
        res.alpha[k, -c(1:p),] <- if(max.cp > 0) res.all[id.b]/res.all[id.w]
        res.alpha[k, -c(1:(p*(max.cp + 1))),] <- if(max.cp > 1) res.all[id.c]/res.all[id.w]
      }
    }else if(leader == "2nd"){
      for(k in o){
        # LP
        lp.p1 <- make.lp(0, no.dv.t * t) # v1 (+ u1) + w + r0 + r1 + r2 + f1 (+ v2) + u2 + f2
        
        # Labelling
        temp <- c()
        for(i in 1:t){temp <- c(temp, 
                                paste0(i, "v1", 1:m.s1), 
                                if(!is.null(ydata.s1)) paste0(i, "u1", 1:s.s1), 
                                paste0(i, "w", 1:p), paste0(i, "r0", 1:p), paste0(i, "r1", 1:p), paste0(i, "r2", 1:p), 
                                paste0(i, "f1"), 
                                if(!is.null(xdata.s2)) paste0(i, "v2", 1:m.s2), 
                                paste0(i, "u2", 1:s.s2), paste0(i, "f2"))}
        dimnames(lp.p1)[[2]] <- temp
        
        #####################################
        ### PHASE I. Leader - 2nd stage
        #####################################
        
        # Set objective
        if(orientation == "i"){set.objfn(lp.p1, c(-ydata.s2[k,,] * t.w, t.w), indices = c(id.u.s2, id.f.s2))}
        if(orientation == "o"){
          if(is.null(xdata.s2)){
            set.objfn(lp.p1, c(rep(c(zdata[k,,] * rep(t.w, each = p)), max.cp + 1), t.w), 
                      indices = c(c(id.a, id.b, id.c)[1:(p*t*(max.cp + 1))], id.f.s2))            
          }else{
            set.objfn(lp.p1, c(xdata.s2[k,,] * t.w, rep(c(zdata[k,,] * rep(t.w, each = p)), max.cp + 1), t.w),
                      indices = c(id.v.s2, c(id.a, id.b, id.c)[1:(p*t*(max.cp + 1))], id.f.s2))
          }
        }
        
        # Constraints (exclusive in PHASE I)
        for(i in 1:t){
          # Orientation specific constraints
          if(orientation == "i"){
            # (v2x2 +) ∑rz = 1 for DMU o
            if(is.null(xdata.s2)){
              add.constraint(lp.p1, z.ag[[i]][k,], indices = id.z[[i]], "=", 1)
            }else{
              add.constraint(lp.p1, c(xdata.s2[k,,i], z.ag[[i]][k,]), indices = c(id.v.s2[,,i], id.z[[i]]), "=", 1)
            }
          }
          if(orientation == "o"){
            # u2y2 = 1 for DMU o
            add.constraint(lp.p1, ydata.s2[k,,i], indices = id.u.s2[,,i], "=", 1)
          }
        }
        
        # Constraints (shared with PHASE II)
        for(i in 1:t){
          # Orientation specific constraints
          if(orientation == "i"){
            # -v1x1 (+ u1y1) + wz - f1 <= 0 for DMU o
            if(is.null(ydata.s1)){
              add.consraint(lp.p1, c(-xdata.s1[k,,i], zdata[k,,i], -1),
                            indices = c(id.v.s1[,,i], id.w[,,i], id.f.s1[,,i]), "<=", 0)
            }else{
              add.consraint(lp.p1, c(-xdata.s1[k,,i], ydata.s1[k,,i], zdata[k,,i], -1),
                            indices = c(id.v.s1[,,i], id.u.s1[,,i], id.w[,,i], id.f.s1[,,i]), "<=", 0)
            }
          }
          if(orientation == "o"){
            # (u1y1 +) wz = 1 for DMU o
            if(is.null(ydata.s1)){
              add.constraint(lp.p1, zdata[k,,i], indices = id.w[,,i], "=", 1)    
            }else{
              add.constraint(lp.p1, c(ydata.s1[k,,i], zdata[k,,i]), indices = c(id.u.s1[,,i], id.w[,,i]), "=", 1)
            }
          }
          
          # Common constraints
          # (-v2x2 +) - ∑rz + u2y2 - f2 <= 0 for all
          for(d in o){
            if(is.null(xdata.s2)){
              add.constraint(lp.p1, c(-z.ag[[i]][d,], ydata.s2[d,,i], -1), 
                             indices = c(id.z[[i]], id.u.s2[,,i], id.f.s2[,,i]), "<=", 0)
            }else{
              add.constraint(lp.p1, c(-xdata.s2[d,,i], -z.ag[[i]][d,], ydata.s2[d,,i], -1), 
                             indices = c(id.v.s2[,,i], id.z[[i]], id.u.s2[,,i], id.f.s2[,,i]), "<=", 0)  
            }
          }
          
          # Carry-over constraints
          if(alpha == "free"){
            for(j in 1:p){
              # w = sum(r) for all periods
              add.constraint(lp.p1, c(1, rep(-1, max.cp + 1)), indices = c(id.w[,j,i], id.r[1:(max.cp + 1),j,i]), "=", 0)
              
              # r+ = r++ = 0 when no carry-over
              if(max.cp == 0){
                add.constraint(lp.p1, rep(1, length(c(id.b, id.c))), indices = c(id.b, id.c), "=", 0)
                LB[LB > 0] <- 0; UB[UB < 1] <- 1
              }
              
              # r++ = 0 for the period second to the last when exhaust is T
              if(isTRUE(exhaust) & i == (t - 1)){
                add.constraint(lp.p1, 1, indices = id.c[,j,i], "=", 0)
                LB[j, -c(1:2), i] <- 0; UB[j, -c(1:2), i] <- 0
              }
              
              # w = r0 for the last period when exhaust is T
              if(isTRUE(exhaust) & i == t){
                add.constraint(lp.p1, c(1, -1), indices = c(id.w[,j,i], id.a[,j,i]), "=", 0)
                LB[j,,i] <- 0; UB[j, -1, i] <- 0
              }
              
              # w*LB <= r0 & r+ & r++ <= w*UB
              for(r in 1:(max.cp + 1)){
                # w*LB <= r
                add.constraint(lp.p1, c(LB[j,r,i], -1), indices = c(id.w[,j,i], id.r[r,j,i]), "<=", 0)    
                
                # r <= w*UB
                add.constraint(lp.p1, c(-UB[j,r,i], 1), indices = c(id.w[,j,i], id.r[r,j,i]), "<=", 0)  
              }
            }
          }else if(is.numeric(alpha)){
            for(j in 1:p){
              # w * alpha = r
              for(r in 1:(max.cp + 1)){
                if(r == 1){add.constraint(lp.p1, c(alpha.ar[j,1,i], -1), indices = c(id.w[,j,i], id.a[,j,i]), "=", 0)}
                if(r == 2){add.constraint(lp.p1, c(alpha.ar[j,2,i], -1), indices = c(id.w[,j,i], id.b[,j,i]), "=", 0)}
                if(r == 3){add.constraint(lp.p1, c(alpha.ar[j,3,i], -1), indices = c(id.w[,j,i], id.c[,j,i]), "=", 0)}
              }
            }
          }
        }
        
        # RTS
        if(rts == "crs") add.constraint(lp.p1, rep(1, 2*t), indices = c(id.f.s1, id.f.s2), "=", 0)
        #if(rts == "irs") TBA
        #if(rts == "drs") TBA
        
        # Bounds
        temp.lb <- rep(0, no.dv.t * t)
        if(rts == "vrs"){temp.lb[c(id.f.s1, id.f.s2)] <- -Inf}
        set.bounds(lp.p1, lower = temp.lb)  
        
        # Solve
        solve.lpExtPtr(lp.p1)
        
        # Get results
        res.all <- get.variables(lp.p1)
        for(i in 1:t){
          if(orientation == "i"){ # u2y2 - f2
            res.eff.s2[k,,i] <- sum(res.all[id.u.s2[,,i]] * ydata.s2[k,,i]) - res.all[id.f.s2[,,i]]
          }
          if(orientation == "o"){ # (v2x2 +) ∑rz + f2
            if(is.null(xdata.s2)){
              res.eff.s2[k,,i] <- sum(res.all[id.z[[i]]] * z.ag[[i]][k,]) + res.all[id.f.s2[,,i]]
            }else{
              res.eff.s2[k,,i] <- sum(res.all[id.v.s2[,,i]] * xdata.s2[k,,i]) + sum(res.all[id.z[[i]]] * z.ag[[i]][k,]) + res.all[id.f.s2[,,i]]
            }
          }
        }
        
        #####################################
        ### PHASE II. Follower - 1st stage
        #####################################
        
        # Copy LP object for recycling
        lp.p2 <- lp.p1
        
        # Re-set objective
        if(orientation == "i"){ # w((-u1y1) - wz + f1)
          if(is.null(ydata.s1)){
            set.objfn(lp.p2, c(-zdata[k,,] * t.w, t.w), indices = c(id.w, id.f.s2))
          }else{
            set.objfn(lp.p2, c(-ydata.s1[k,,] * t.w, -zdata[k,,] * t.w, t.w), indices = c(id.u.s1, id.w, id.f.s2))
          }
        }
        if(orientation == "o"){ # w(v1x1 + f1)
          set.objfn(lp.p2, c(xdata.s1[k,,] * t.w, t.w), indices = c(id.v.s1, id.f.s1))
        }
        
        # Delete PHASE I specific constraints
        # delete.constraint(lp.p2, 1:t)
        
        # Constraints of PHASE II
        for(i in 1:t){
          if(orientation == "i"){
            # v1x1 = 1 for DMU o
            add.constraint(lp.p2, xdata.s1[k,,i], indices = id.v.s1[,,i], "=", 1)
            
            # (-v2x2*E2) - ∑rz*E2 + u2y2 - f2 = 0 for DMU o
            if(is.null(xdata.s2)){
              add.constraint(lp.p2, c(-z.ag[[i]][k,]*res.eff.s2[k,,i], ydata.s2[k,,i], -1), 
                             indices = c(id.z[[i]], id.u.s2[,,i], id.f.s2[,,i]), "=", 0)  
            }else{
              add.constraint(lp.p2, c(-xdata.s2[k,,i]*res.eff.s2[k,,i], -z.ag[[i]][k,]*res.eff.s2[k,,i], ydata.s2[k,,i], -1), 
                             indices = c(id.v.s2[,,i], id.z[[i]], id.u.s2[,,i], id.f.s2[,,i]), "=", 0)
            }
          }
          if(orientation == "o"){
            # -v1x1 - f1 (+ u1y1) + wz <= 0 for all
            for(d in o){
              if(is.null(ydata.s1)){
                add.constraint(lp.p2, c(-xdata.s1[d,,i], -1, zdata[d,,i]), 
                               indices = c(id.v.s1[,,i], id.f.s1[,,i], id.w[,,i]), "<=", 0)
              }else{
                add.constraint(lp.p2, c(-xdata.s1[d,,i], -1, ydata.s1[d,,i], zdata[d,,i]), 
                               indices = c(id.v.s1[,,i], id.f.s1[,,i], id.u.s1[,,i], id.w[,,i]), "<=", 0)
              }
            }
            
            # (-v2x2/E2) - ∑rz/E2 - f2/E2 + u2y2 = 0 for DMU o
            if(is.null(xdata.s2)){
              add.constraint(lp.p2, c(-z.ag[[i]][k,]/res.eff.s2[k,,i], -1/res.eff.s2[k,,i], ydata.s2[k,,i]), 
                             indices = c(id.z[[i]], id.f.s2[,,i], id.u.s2[,,i]), "=", 0)  
            }else{
              add.constraint(lp.p2, c(-xdata.s2[k,,i]/res.eff.s2[k,,i], -z.ag[[i]][k,]/res.eff.s2[k,,i], -1/res.eff.s2[k,,i], ydata.s2[k,,i]), 
                             indices = c(id.v.s2[,,i], id.z[[i]], id.f.s2[,,i], id.u.s2[,,i]), "=", 0)  
            }
          } 
        }
        
        # Solve
        solve.lpExtPtr(lp.p2)
        
        # Get results
        res.all <- get.variables(lp.p2)
        for(i in 1:t){
          if(orientation == "i"){ # (-u1y1) - wz + f1
            if(is.null(ydata.s1)){
              res.eff.s1[k,,i] <- -sum(res.all[id.w[,,i]] * zdata[k,,i]) + res.all[id.f.s1[,,i]]
            }else{
              res.eff.s1[k,,i] <- -sum(res.all[id.u.s1[,,i]] * ydata.s1[k,,i]) - sum(res.all[id.w[,,i]] * zdata[k,,i]) + res.all[id.f.s1[,,i]]
            }
          }
          if(orientation == "o"){ # v1x1 + f1
            res.eff.s1[k,,i] <- sum(res.all[id.v.s1[,,i]] * xdata.s1[k,,i]) + res.all[id.f.s1[,,i]]
          }
        }
        res.v.s1[k,,]      <- res.all[id.v.s1]
        res.u.s1[k,,]      <- if(is.null(ydata.s1)) NULL else res.all[id.u.s1]
        res.f.s1[k,,]      <- res.all[id.f.s1]
        res.v.s2[k,,]      <- if(is.null(xdata.s2)) NULL else res.all[id.v.s2]
        res.u.s2[k,,]      <- res.all[id.u.s2]
        res.f.s2[k,,]      <- res.all[id.f.s2]
        res.w[k,,]         <- res.all[id.w]
        res.r.0[k,,]       <- res.all[id.a]
        res.r.1[k,,]       <- res.all[id.b]
        res.r.2[k,,]       <- res.all[id.c]
        res.alpha[k, 1:p,] <- res.all[id.a]/res.all[id.w]
        res.alpha[k, -c(1:p),] <- if(max.cp > 0) res.all[id.b]/res.all[id.w]
        res.alpha[k, -c(1:(p*(max.cp + 1))),] <- if(max.cp > 1) res.all[id.c]/res.all[id.w]
      }
    }
  }
  
  # Pack results
  results <- list(eff.s1 = res.eff.s1, eff.s2 = res.eff.s2, 
                  v.s1 = res.v.s1, u.s1 = res.u.s1, f.s1 = res.f.s1,
                  v.s2 = res.v.s2, u.s2 = res.u.s2, f.s2 = res.f.s2, 
                  w = res.w, r.0 = res.r.0, r.1 = res.r.1, r.2 = res.r.2, alpha = res.alpha)
  
  return(results)
}