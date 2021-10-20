#Martes 12 octubre 2021
# Autoras: emily gonzalez y sara castro

#Funcion cuadros de punnett.


Cuadros.pnt <- function(alelo.d,alelo.r,res.fin){ rasg.n <- length(alelo.d)
                                                        switch (rasg.n,
                                                        { 
                                                          #CALCULOS
                                                          comb1 <- rep(alelo.d,2)
                                                          comb2 <- rep(alelo.r,2)
                                                          comb3 <- rep(c(alelo.d,alelo.r))
                                                          
                                                          #matriz
                                                          pos.comb <- rbind(c(comb1),c(comb2),c(comb3))
                                                          pos.comb
                                                          colnames(pos.comb) <- c("PADRE","MADRE")
                                                         
                                                        },
                                                        
                                                        {
                                                          
                                                          #CALCULOS
                                                          c1<- rep(alelo.d[1],2)
                                                          c2 <- rep(alelo.d[2],2)
                                                          comb1 <- c(c1,c2)
                                                          
                                                          c1.2 <- rep(alelo.d[1],2)
                                                          c2.2 <- rep(alelo.d[2])
                                                          c2.2.3 <- rep(alelo.r[2])
                                                          comb2 <- c(c1.2,c2.2,c2.2.3)
                                                          
                                                          c1.3 <- rep(alelo.d[1])
                                                          c2.3 <- rep(alelo.r[1])
                                                          c3.3 <- rep(alelo.d[2],2)
                                                          comb3 <- c(c1.3,c2.3,c3.3)
                                                          
                                                          c1.4 <- rep(alelo.d[1])
                                                          c2.4 <- rep(alelo.r[1])
                                                          c3.4 <- rep(alelo.d[2])
                                                          c4.4 <- rep(alelo.r[2])
                                                          comb4 <- c(c1.4,c2.4,c3.4,c4.4)
                                                          
                                                          c1.5<- rep(alelo.d[1],2)
                                                          c2.5 <- rep(alelo.r[2],2)
                                                          comb5 <- c(c1.5,c2.5)
                                                          
                                                          c1.6 <- rep(alelo.d[1])
                                                          c2.6 <- rep(alelo.r[1])
                                                          c3.6 <- rep(alelo.r[2],2)
                                                          comb6 <- c(c1.6,c2.6,c3.6)
                                                          
                                                          c1.7 <- rep(alelo.r[1],2)
                                                          c2.7 <- rep(alelo.d[2],2)
                                                          comb7 <- c(c1.7,c2.7)
                                                          
                                                          c1.8 <- rep(alelo.r[1],2)
                                                          c2.8 <- rep(alelo.d[2])
                                                          c3.8 <- rep(alelo.r[2])
                                                          comb8 <- c(c1.8,c2.8,c3.8)
                                                          
                                                          c1.9 <- rep(alelo.r[1],2)
                                                          c2.9 <- rep(alelo.r[2],2)
                                                          comb9 <- c(c1.9,c2.9)
                                                          
                                                          #matriz
                                                          pos.comb <- rbind(c(comb1),c(comb2),c(comb3),c(comb4),c(comb5),c(comb6),c(comb7),c(comb8),c(comb9))
                                                          pos.comb
                                                          colnames(pos.comb) <- c("PADRE","MADRE","PADRE","MADRE")
                                                        },
                                                        {
                                                          
                                                          #CALCULOS
                                                          c1 <-  rep(alelo.d[1],2)
                                                          c2 <- rep(alelo.d[2],2)
                                                          c3 <- rep(alelo.d[3],2)
                                                          comb1 <- c(c1,c2,c3)
                                                          
                                                          c1.2 <- rep(alelo.d[1],2)
                                                          c2.2 <- rep(alelo.d[2],2)
                                                          c3.2 <- rep(alelo.d[3])
                                                          c4.2 <- rep(alelo.r[3])
                                                          comb2 <- c(c1.2,c2.2,c3.2,c4.2)
                                                          
                                                          c1.3 <- rep(alelo.d[1],2)
                                                          c2.3 <- rep(alelo.d[2])
                                                          c3.3 <- rep(alelo.r[2])
                                                          c4.3 <- rep(alelo.d[3],2)
                                                          comb3 <- c(c1.3,c2.3,c3.3,c4.3)
                                                          
                                                          c1.4 <- rep(alelo.d[1],2)
                                                          c2.4 <- rep(alelo.d[2])
                                                          c3.4 <- rep(alelo.r[2])
                                                          c4.4 <- rep(alelo.d[3])
                                                          c5.4 <- rep(alelo.r[3])
                                                          comb4 <- c(c1.4,c2.4,c3.4,c4.4,c5.4)
                                                          
                                                          c1.5 <- rep(alelo.d[1])
                                                          c2.5 <- rep(alelo.r[1])
                                                          c3.5 <- rep(alelo.d[2],2)
                                                          c4.5 <- rep(alelo.d[3],2)
                                                          comb5 <- c(c1.5,c2.5,c3.5,c4.5)
                                                          
                                                          c1.6 <- rep(alelo.d[1])
                                                          c2.6 <- rep(alelo.r[1])
                                                          c3.6 <- rep(alelo.d[2],2)
                                                          c4.6 <- rep(alelo.d[3])
                                                          c5.6 <- rep(alelo.r[3])
                                                          comb6 <- c(c1.6,c2.6,c3.6,c4.6,c5.6)
                                                          
                                                          c1.7 <- rep(alelo.d[1])
                                                          c2.7 <- rep(alelo.r[1])
                                                          c3.7 <- rep(alelo.d[2])
                                                          c4.7 <- rep(alelo.r[2])
                                                          c5.7 <- rep(alelo.d[3],2)
                                                          comb7 <- c(c1.7,c2.7,c3.7,c4.7,c5.7)
                                                          
                                                          c1.8 <- rep(alelo.d[1])
                                                          c2.8 <- rep(alelo.r[1])
                                                          c3.8 <- rep(alelo.d[2])
                                                          c4.8 <- rep(alelo.r[2])
                                                          c5.8 <- rep(alelo.d[3])
                                                          c6.8 <- rep(alelo.r[3])
                                                          comb8 <- c(c1.8,c2.8,c3.8,c4.8,c5.8,c6.8)
                                                          
                                                          c1.9 <- rep(alelo.d[1],2)
                                                          c2.9 <- rep(alelo.d[2],2)
                                                          c3.9 <- rep(alelo.r[3],2)
                                                          comb9 <- c(c1.9,c2.9,c3.9)
                                                          
                                                          c1.10 <- rep(alelo.d[1],2)
                                                          c2.10 <- rep(alelo.d[2])
                                                          c3.10 <- rep(alelo.r[2])
                                                          c4.10 <- rep(alelo.r[3],2)
                                                          comb10 <- c(c1.10,c2.10,c3.10,c4.10)
                                                          
                                                          c1.11 <- rep(alelo.d[1])
                                                          c2.11 <- rep(alelo.r[1])
                                                          c3.11 <- rep(alelo.d[2],2)
                                                          c4.11 <- rep(alelo.r[3],2)
                                                          comb11 <- c(c1.11,c2.11,c3.11,c4.11)
                                                          
                                                          c1.12 <- rep(alelo.d[1])
                                                          c2.12 <- rep(alelo.r[1])
                                                          c3.12 <- rep(alelo.d[2])
                                                          c4.12 <- rep(alelo.r[2])
                                                          c5.12 <- rep(alelo.r[3],2)
                                                          comb12 <- c(c1.12,c2.12,c3.12,c4.12,c5.12)
                                                          
                                                          c1.13 <- rep(alelo.d[1],2)
                                                          c2.13 <- rep(alelo.r[2],2)
                                                          c3.13 <- rep(alelo.d[3],2)
                                                          comb13 <- c(c1.13,c2.13,c3.13)
                                                          
                                                          c1.14 <- rep(alelo.d[1],2)
                                                          c2.14 <- rep(alelo.r[2],2)
                                                          c3.14 <- rep(alelo.d[3])
                                                          c4.14 <- rep(alelo.r[3])
                                                          comb14 <- c(c1.14,c2.14,c3.14,c4.14)
                                                          
                                                          c1.15 <- rep(alelo.d[1])
                                                          c2.15 <- rep(alelo.r[1])
                                                          c3.15 <- rep(alelo.r[2],2)
                                                          c4.15 <- rep(alelo.d[3],2)
                                                          comb15 <- c(c1.15,c2.15,c3.15,c4.15)
                                                          
                                                          c1.16 <- rep(alelo.d[1])
                                                          c2.16 <- rep(alelo.r[1])
                                                          c3.16 <- rep(alelo.r[2],2)
                                                          c4.16 <- rep(alelo.d[3])
                                                          c5.16 <- rep(alelo.r[3])
                                                          comb16 <- c(c1.16,c2.16,c3.16,c4.16,c5.16)
                                                          
                                                          c1.17 <- rep(alelo.d[1],2)
                                                          c2.17 <- rep(alelo.r[2],2)
                                                          c3.17 <- rep(alelo.r[3],2)
                                                          comb17 <- c(c1.17,c2.17,c3.17)
                                                          
                                                          c1.18 <- rep(alelo.d[1])
                                                          c2.18 <- rep(alelo.r[1])
                                                          c3.18 <- rep(alelo.r[2],2)
                                                          c4.18 <- rep(alelo.r[3],2)
                                                          comb18 <- c(c1.18,c2.18,c3.18,c4.18)
                                                          
                                                          c1.19 <- rep(alelo.r[1],2)
                                                          c2.19 <- rep(alelo.d[2],2)
                                                          c3.19 <- rep(alelo.d[3],2)
                                                          comb19 <- c(c1.19,c2.19,c3.19)
                                                          
                                                          c1.20 <- rep(alelo.r[1],2)
                                                          c2.20 <- rep(alelo.d[2],2)
                                                          c3.20 <- rep(alelo.d[3])
                                                          c4.20 <- rep(alelo.r[3])
                                                          comb20 <- c(c1.20,c2.20,c3.20,c4.20)
                                                          
                                                          c1.21 <- rep(alelo.r[1],2)
                                                          c2.21 <- rep(alelo.d[2])
                                                          c3.21 <- rep(alelo.r[2])
                                                          c4.21 <- rep(alelo.d[3],2)
                                                          comb21 <- c(c1.21,c2.21,c3.21,c4.21)
                                                          
                                                          c1.22 <- rep(alelo.r[1],2)
                                                          c2.22 <- rep(alelo.d[2])
                                                          c3.22 <- rep(alelo.r[2])
                                                          c4.22 <- rep(alelo.d[3])
                                                          c5.22 <- rep(alelo.r[3])
                                                          comb22 <- c(c1.22,c2.22,c3.22,c4.22,c5.22)
                                                          
                                                          c1.23 <- rep(alelo.r[1],2)
                                                          c2.23 <- rep(alelo.d[2],2)
                                                          c3.23 <- rep(alelo.r[3],2)
                                                          comb23 <- c(c1.23,c2.23,c3.23)
                                                          
                                                          c1.24 <-rep(alelo.r[1],2)
                                                          c2.24 <- rep(alelo.d[2])
                                                          c3.24 <- rep(alelo.r[2])
                                                          c4.24 <- rep(alelo.r[3],2)
                                                          comb24 <- c(c1.24,c2.24,c3.24,c4.24)
                                                          
                                                          c1.25 <- rep(alelo.r[1],2)
                                                          c2.25 <- rep(alelo.r[2],2)
                                                          c3.25 <- rep(alelo.d[3],2)
                                                          comb25 <- c(c1.25,c2.25,c3.25)
                                                          
                                                          c1.26 <- rep(alelo.r[1],2)
                                                          c2.26 <- rep(alelo.r[2],2)
                                                          c3.26 <- rep(alelo.d[3])
                                                          c4.26 <- rep(alelo.r[3])
                                                          comb26 <- c(c1.26,c2.26,c3.26,c4.26)
                                                          
                                                          c1.27 <- rep(alelo.r[1],2)
                                                          c2.27 <- rep(alelo.r[2],2)
                                                          c3.27 <- rep(alelo.r[3],2)
                                                          comb27 <- c(c1.27,c2.27,c3.27)
                                                          
                                                          #matriz
                                                          pos.comb <- matrix(c(comb1,comb2,comb3,comb4,comb5,comb6,comb7,comb8,comb9,comb10,comb11,comb12,comb13,comb14,comb15,comb16,comb17,comb18,comb19,comb20,comb21,comb22,comb23,comb24,comb25,comb26,comb27),nrow=27,byrow = T)
                                                          colnames(pos.comb) <- c("PADRE","MADRE","PADRE","MADRE","PADRE","MADRE")
                                                                    } )
                                                         

if(res.fin==T){apply(pos.comb,1,function(x) paste0(x,collapse = ""))}else{pos.comb}
}   


