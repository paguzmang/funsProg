#19/10/2021
#Susana Gomez,Jose Alejandro Velasquez
 biodiv<-function(x,formula){
  ni<-x
  N <-  sum(ni)
  pi <- ni / N 
  S<-length(ni)
  switch (formula,
          
          shannon= {N <-  sum(ni)    
          pi <- ni / N      
          -sum(pi*log(pi))
          },
          
          simpson={N<-  sum(ni)
          sum(ni*(ni-1))/(N*(N-1))
          },
          
          margalef={S<-length(ni)
          N<-sum(ni)
          (S-1)/(log(N))
          },
          
          dominancia={N<-  sum(ni)
          1- (sum(ni*(ni-1))/(N*(N-1)))
          },
          
          menhinick= { 
            S/sqrt(N)})}