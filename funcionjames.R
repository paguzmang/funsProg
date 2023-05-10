conversion <- function(g,c) {
  if(g==0){
    
    #CONVERSION DE TEMPERATURA.
    #De grados celsius a farenheit.
    F<-(c*(9/5))+32
    
    #De grados celsius a kelvins
    K<-(c+273.15)
    
    conversiont<-data.frame("grados celsius"=c,
                            "grados Farenheit"=F,"grados K"=K)
    
    conversion<-list("Temperatura"=conversiont)
    conversion } else {  
      #CONVERSION DE GRAMOS
      #De gramos a miligramos
      mg<-g*1000
      
      #De gramos a centigramos
      cg<-g*100
      
      #De gramos a decigramos
      dg<-g*10
      
      #gramos
      g<-g
      
      #De Gramos a Decagramos
      Dg<-g/10
      
      #De Hectogramos a gramos
      Hg<-g/100
      
      #De gramos a kilogramos
      Kg<-g/1000
      
      #gramos a toneladas
      Ton<-g/1000000
      
      #conversion de Temperatura
      #grados celsius a farenheit
      f<-(c*(9/5))+32
      
      #grados celsius a kelvins
      k<-(c+273.15)
      
      #Creando un data.frame para las unidades de masa.
      conversionm<-data.frame("mg"=mg,"cg"=cg,"dg"=dg,
                              "g"=g,"Dg"=Dg,"Hg"=Hg,
                              "Kg"=Kg,"Ton"=Ton)
      
      #Creando data.frame para las unidades de Temperatura.
      conversiont<-data.frame("grados celsius"=c,
                              "grados Farenheit"=f,
                              "grados Kelvins"=k)
      
      conversion<-list("Masa"=conversionm,
                       "Temperatura"=conversiont)
      conversion }  
}
#Ejemplo
conversion(0,45)
