  curva_de_vida <- function(x,n,m,curva,log.sobrev=F) {
  long <- length(n) 
  l <- n / n [ 1 ]  
  l
  
  d <- c(l [ 2:long] / l [1:(long-1)], 0) 
  d
  
  s <- 1 - d                            
  s
  n1 <- n 
  if(log.sobrev){n1 <- log(n)} 
  
  if(curva==T){plot(x = x, y = n1, type = "l",lwd = 3, col = "red",
                    xlab = "edad en años",ylab = ifelse(log.sobrev,"log(sobrev)","sobrev"),
                    main = "curva de vida")
    
  }else{data.frame("rango edad" = x,"ind. sobreviven" = n,
                   "natalidad" = m,"vivos" = l, "mortalidad" = d, "supervivencia" = s)
  }
}

