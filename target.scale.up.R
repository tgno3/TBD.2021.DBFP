target.scale.up <- function(nl.data, ng.data, fx.data, w = NULL, max.slack = NULL, time.out = 60){
  
  # Parameters
  nl.data   <- as.matrix(nl.data)
  ng.data   <- as.matrix(ng.data)
  fx.data   <- as.matrix(fx.data)
  n         <- nrow(nl.data)
  n.nl      <- ncol(nl.data)
  n.ng      <- ncol(ng.data)
  n.fx      <- ncol(fx.data)
  n.all     <- sum(n.nl, n.ng, n.fx)
  w         <- if(is.null(w)) rep(1, n.fx * 2) else as.vector(w)
  max.slack <- if(is.null(max.slack)) rep(apply(fx.data, 2, max), 2) else rep(max.slack, n.fx * 2)

  # Plates
  res.who <- matrix(NA, nrow = n, ncol = 1)
  
  # LP
  for(k in 1:n){
    
    # cbind(nl.data.eff, ng.data.eff, fx.data.eff)
    
    # Effective set
    id.eff.nl   <- which(apply(nl.data, 1, function(x) sum(x >= nl.data[k,]) == n.nl) & apply(nl.data, 1, function(x) sum(x) > 0))
    id.eff.ng   <- which(apply(ng.data, 1, function(x) sum(x <= ng.data[k,]) == n.ng))
    id.eff      <- intersect(id.eff.nl, id.eff.ng)
    n.eff       <- length(id.eff)
    k.eff       <- which(id.eff == k)
    nl.data.eff <- nl.data[id.eff,, drop = F]
    ng.data.eff <- ng.data[id.eff,, drop = F]
    fx.data.eff <- fx.data[id.eff,, drop = F]
    
    # Declare LP
    lp.tsu <- make.lp(0, n.eff + n.fx * 2) # lambda + d+ + d-
    
    # Set obj.F
    set.objfn(lp.tsu, w, indices = (n.eff + 1):(n.eff + n.fx * 2))
    
    # FDH
    set.type(lp.tsu, 1:n.eff, "binary")
    add.constraint(lp.tsu, rep(1, n.eff), indices = 1:n.eff, "=", 1)
    
    # Constraints for No-less-than variables
    for(i in 1:n.nl){
      add.constraint(lp.tsu, nl.data.eff[,i], indices = 1:n.eff, ">=", nl.data[k, i] + ifelse(nl.data[k, i] == max(nl.data.eff[,i]), 0, 0.1))  
    }
    
    # Constraints for No-greater-than variables
    for(j in 1:n.ng){
      add.constraint(lp.tsu, ng.data.eff[,j], indices = 1:n.eff, "<=", ng.data[k, j] - ifelse(ng.data[k, j] == min(ng.data.eff[,j]), 0, 0.1))  
    }
    
    # Constraints for flexible variables
    for(q in 1:n.fx){
      add.constraint(lp.tsu, c(fx.data.eff[,q], -1, 1), indices = c(1:n.eff, n.eff + q, n.eff + n.fx + q), "=", fx.data[k, q])  
    }
    
    # Bounds
    set.bounds(lp.tsu, upper = c(rep(1, n.eff), max.slack))
    
    # Solve
    lp.control(lp.tsu, timeout = time.out)
    status <- solve.lpExtPtr(lp.tsu)
    if(status == 0){
      res.who[k,] <- id.eff[get.variables(lp.tsu)[1:n.eff] > 0]  
    }else if(status == 2){ 
      
      # Alternative model 1. from less-than to no-greater-than
      lp.tsu.alt.1 <- lp.tsu
      
      # Remove less-than constraints
      delete.constraint(lp.tsu.alt.1, (1 + n.nl + 1):(1 + n.nl + n.ng))
      
      # Add no-greater-than constraints
      for(j in 1:n.ng){add.constraint(lp.tsu.alt.1, ng.data.eff[,j], indices = 1:n.eff, "<=", ng.data[k, j])}
      
      # Bounds (remedy for an lpSolve bug)
      set.bounds(lp.tsu.alt.1, lower = c(rep(-0.1, n.eff), rep(0, length(max.slack))))
      
      # Re-Solve
      status.a1 <- solve.lpExtPtr(lp.tsu.alt.1)
      
      if(status.a1 == 0){
        res.who[k,] <- id.eff[get.variables(lp.tsu.alt.1)[1:n.eff] > 0]  
      }else if(status.a1 == 2){
        
        # Alternative model 2. from greater-than to no-less-than
        lp.tsu.alt.2 <- lp.tsu # This does not work for some reasons!
        
        # Remove greater-than & less-than constraints
        delete.constraint(lp.tsu.alt.2, 2:(1 + n.nl))
        delete.constraint(lp.tsu.alt.2, (1 + n.fx + 1):(1 + n.fx + n.ng))
        
        # Add no-less-than constraints
        for(i in 1:n.nl){add.constraint(lp.tsu.alt.2, nl.data.eff[,i], indices = 1:n.eff, ">=", nl.data.eff[k.eff, i])}
        
        # Add less-than constraints
        for(j in 1:n.ng){
          add.constraint(lp.tsu.alt.2, ng.data.eff[,j], indices = 1:n.eff, "<=", ng.data[k, j] - ifelse(ng.data[k, j] == min(ng.data.eff[,j]), 0, 0.1))  
        }
        
        # Bounds (remedy for an lpSolve bug)
        set.bounds(lp.tsu.alt.2, lower = c(rep(-0.1, n.eff), rep(0, length(max.slack))))
        
        # Re-Solve
        status.a2 <- solve.lpExtPtr(lp.tsu.alt.2)
        
        if(status.a2 == 0){
          res.who[k,] <- id.eff[get.variables(lp.tsu.alt.2)[1:n.eff] > 0]  
        }else{
          res.who[k,] <- k
        }
      }
    }else{
      stop("Target is not identified.")
    }
  }
  results <- list(res.target = res.who)
  return(results)
}