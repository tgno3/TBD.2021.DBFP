target.scale.up <- function(xdata, ydata, eff, rank, pt = NULL, bo = NULL, nd = F, efc = F, o = NULL){
    
  # Parameters
  xdata <- as.matrix(xdata)
  ydata <- as.matrix(ydata)
  n     <- nrow(xdata)
  m     <- ncol(xdata)
  s     <- ncol(ydata)
  v     <- m + s
  o     <- if(is.null(o)) c(1:n) else as.vector(o)
  dw    <- if(is.null(pt)) rep(1, v + 1 + v + 1) else c(rep(1, v + 1), if(length(pt) == 1) rep(pt, v + 1) else pt)
  bo    <- if(is.null(bo)) 0 else bo
  
  # Data frames
  res.who <- matrix(NA, nrow = n, ncol = 1)
  res.p.d <- matrix(NA, nrow = n, ncol = v + 1) 
  res.n.d <- matrix(NA, nrow = n, ncol = v + 1)

  # LP
  for(k in o){
    # Declare LP
    lp.tsu <- make.lp(0, n + v + 1 + v + 1) # lambda + p.d + n.d
    
    # Set objective
    set.objfn(lp.tsu, dw, indices = (n + 1):(n + v + 1 + v + 1))

    # Set type
    set.type(lp.tsu, 1:n, "binary")
    
    # Lambda sum to one
    add.constraint(lp.tsu, rep(1, n), indices = 1:n, "=", 1)
    
    # Input constraints
    for(i in 1:m){
      if(isTRUE(nd)){ # No decrease of input allowed
        add.constraint(lp.tsu, c(xdata[,i], -1), indices = c(1:n, n + i), "=", xdata[k, i])  
      }else{ # Some decrease of input allowed
        add.constraint(lp.tsu, c(xdata[,i], -1, 1), indices = c(1:n, n + i, n + v + 1 + i), "=", xdata[k, i])  
      }
    }

    # Output constraints
    for(r in 1:s){ # No decrease of output allowed
      if(r %in% bo){
        add.constraint(lp.tsu, c(ydata[,r],  1), indices = c(1:n, n + v + 1 + m + r), "=", ydata[k, r])
        add.constraint(lp.tsu, c(1), indices = c(n + m + r), "=", 0)
      }else{
        add.constraint(lp.tsu, c(ydata[,r], -1), indices = c(1:n, n + m + r), "=", ydata[k, r] + 1)
        add.constraint(lp.tsu, c(1), indices = c(n + v + 1 + m + r), "=", 0)
      }
    }
    
    # Eff/Rank constraints
    if(isTRUE(efc)){ # Some decrease of efficiency score allowed
      add.constraint(lp.tsu, c(eff, -1, 1), indices = c(1:n, n + v + 1, n + v + 1 + v + 1), "=", eff[k])  
      # add.constraint(lp.tsu, rank, indices = 1:n, "<=", rank[k] * 0.99)
    }else{ # No changes of efficiency score allowed
      add.constraint(lp.tsu, c(1, 1), indices = c(n + v + 1, n + v + 1 + v + 1), "=", 0)
      if(eff[k] < 100){
        add.constraint(lp.tsu, eff, indices = 1:n, ">=", eff[k] * 1.01)
      }else{
        add.constraint(lp.tsu, rank, indices = 1:n, "<=", rank[k] * 0.99)
      }  
    }
    
    # # Eff/Rank constraints
    # if(eff[k] < 100){
    #   add.constraint(lp.tsu, eff, indices = 1:n, ">=", eff[k] * 1.01)
    # }else{
    #   add.constraint(lp.tsu, rank, indices = 1:n, "<=", rank[k] * 0.99)
    # }

    # Solve
    if(solve.lpExtPtr(lp.tsu) == 0){
      solve.lpExtPtr(lp.tsu)  
      res.temp    <- get.variables(lp.tsu)
      res.who[k,] <- which(res.temp[1:n] > 0)
      res.p.d[k,] <- res.temp[(n + 1):(n + v + 1)]
      res.n.d[k,] <- res.temp[(n + v + 2):(n + v + 1 + v + 1)]
    }else if(rank[k] == 1 | max(rowSums(cbind(xdata, 0)[which(rank <= rank[k]),, drop = F])) == rowSums(cbind(xdata, 0))[k]){
      res.who[k,] <- k
    }else{
      res.who[k,] <- which(rank == max(rank[rank < rank[k]]))
    }
  }
  results <- list(res.target = res.who, res.pos.d = res.p.d, res.neg.d = res.n.d, res.eff.gap = eff[res.who] - eff)
  return(results)
}