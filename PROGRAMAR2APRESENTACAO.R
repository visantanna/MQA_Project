trataCsv <- function(caminhoInput , caminhoOutput){
  library(plyr)
  library(ggplot2)
  listaNomeCorruptos <- c()
  tamanhoOriginal <- c()
  tamanhoSemNA  <-c()  
  
  NomesDespesas <<- matrix( c('AIRPLANE RENT','Aluguel de aviões' 
                              , "FLIGHT TICKET" , "Passagem de avião"
                              , "FUEL COSTS" , "Combustível" 
                              , "PHONE COSTS" , "Telefone" 
                              , "GROUND VEHICLE RENT" , "Aluguel de carro" 
                              ,"TAXI, PARKING AND TOLL COSTS" , "Taxis, estacionamento e pedágio" 
                              ,"MEAL COSTS" , "Alimentação" 
                              ,"MAIL COSTS" ,"Correios" 
                              , "CONSULTING, RESEARCH AND TECHNICAL WORK COSTS" , "consultoria , pesquisa e trabalhos Técnicos" 
                              ,"DISSEMINATION OF PARLIAMENTARY ACTIVITY" , "Disseminação de atividade Parlamentar"
                              ,"SECURITY SERVICE" , "Serviço de Segurança"
                              ,"OFFICE MAINTENANCE" , "Serviço de Manutenção"
                              ,"PUBLICATION SIGNATURE EXPENSES" , "Assinatura de jornais e revistas"
                              ,"GROUND TRANSPORTATION TICKET" , "Passagem de transporte terrestre"
                              ,"WORKSHOP/COURSE/EVENT COSTS" , "Cursos e eventos"
                              ,"SHIP/BOAT RENT" , "Aluguel de barco" 
                              ,"HOTEL COSTS" , "Hotéis"
  ), ncol=2,  byrow = TRUE)
  
  RegioesPorEstado <- matrix(
    c("AC" ,  	"NORTE" 
      ,"AM" ,  "NORTE" 
      ,"AP" ,  "NORTE" 
      ,"PA" ,	 "NORTE" 
      ,"RO" ,	 "NORTE" 
      ,"RR" ,	 "NORTE" 
      ,"TO" ,	"NORTE" 
      ,"AL" ,	"NORDESTE" 
      ,"BA" ,	"NORDESTE" 
      ,"CE" , "NORDESTE" 
      ,"MA", 	"NORDESTE" 
      ,"PB", 	"NORDESTE" 
      ,"PE", 	"NORDESTE" 
      ,"PI", 	"NORDESTE"    
      ,"RN", 	"NORDESTE" 
      ,"SE", 	"NORDESTE" 
      ,"GO", 	"CENTRO-OESTE" 
      ,"MS", 	"CENTRO-OESTE" 
      ,"MT", 	"CENTRO-OESTE" 
      ,"ES", 	"SUDESTE" 
      
      ,"MG", 	"SUDESTE" 
      ,"RJ", 	"SUDESTE" 
      ,"SP", 	"SUDESTE" 
      ,"PR", 	"SUL" 
      ,"RS", 	"SUL" 
      ,"SC", 	"SUL"
      ,"DF" , "CENTRO-OESTE") , ncol = 2 , byrow = TRUE 
  )
  
  dataSet <- read.csv(caminhoInput , dec = "." , sep = "," , header = TRUE , na.strings = "", stringsAsFactors = FALSE)
  setwd(caminhoOutput)
  
  dataSet[c(4,5,6,7,10,11,12,13)] <- list(NULL);
  tamanhoOriginal <- length(dataSet[,1])
  
  dataSet <- na.omit(dataSet);
  tamanhoSemNA <- length(dataSet[,1])
  
  textoAdicional <- paste("Aproximadamente " ,tamanhoOriginal - tamanhoSemNA ,
                          "dos dados foram omitidos por estarem incompletos! portanto, nessas analises estamos trabalhando com apenas "
                          , tamanhoSemNA ," de ", tamanhoOriginal , "registros" )
 
  auxiliar <- 1
  for(x in RegioesPorEstado[,1]){
    ##Identifica a região correspondente ao estado
    indiceEstado<-which(dataSet$deputy_state == x)
    
    ##reescreve o estado pela região
    dataSet[indiceEstado,]$deputy_state <- RegioesPorEstado[auxiliar,2]
    auxiliar = auxiliar +1
  } 
  ##dataSet$refund_date <- substring(dataSet$refund_date , 1 ,7)
  
  dataSet <- aggregate(dataSet$refund_value , by = dataSet[,-4] , FUN = sum) 
  
  
  ##relacionando partido e gasto com o método ANOVA
  anova_partido <- aov(dataSet$x ~ dataSet$party_pg)
  summary(anova_partido)
  
  ##relacionando regiao e gasto com o método ANOVA , teoricamente relevante (valor p baixo)
  anova_regiao <-aov(dataSet$x ~ dataSet$deputy_state)
  summary(anova_regiao)
  TukeyHSD(anova_regiao)
  
  dataSetResumido <- ddply(dataSet , c("deputy_state") , summarize , MEDIA=mean(x/1000) , ERROPADRAO= sqrt(var(x/1000)/length(x)))
  
  dataSetResumido$deputy_state <- reorder(dataSetResumido$deputy_state , dataSetResumido$MEDIA)
  
  ggplot(dataSetResumido) + aes(x=deputy_state , y = MEDIA , colour=deputy_state) +
    geom_point()+ geom_errorbar(aes(ymax =MEDIA+ERROPADRAO , ymin=MEDIA-ERROPADRAO)) +
    labs(colour = "Regiões" , y="Valor médio de Gastos [milhares de R$]" , 
    x= "Região do País" , title = "Gasto médio de deputados por Região")+
    theme(axis.text.x=element_text(size=14) , axis.text.y=element_text(size=14), 
    plot.title = element_text(size=20 ,face ="bold" , hjust = 0.5), 
    axis.title.x = element_text(size = 16), 
    axis.title.y = element_text(size = 16))
  
  ##relacionando posicão política e gasto com o método ANOVA
  anova_posicao <-aov(dataSet$x ~ dataSet$party_position)
  summary(anova_posicao)
} 
