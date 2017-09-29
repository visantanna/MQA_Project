trataCsv <- function(caminhoInput , caminhoOutput){
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
  
  dataSet[c(3,5,6,9,10,11,12,13,14,15,16,17)] <- list(NULL);
  tamanhoOriginal <- length(dataSet)
  
  dataSet <- na.omit(dataSet);
  tamanhoSemNA <- length(dataSet)
  
  textoAdicional <- paste("Aproximadamente " ,tamanhoOriginal - tamanhoSemNA ,
                          "dos dados foram omitidos por estarem incompletos! portanto, nessas analises estamos trabalhando com apenas "
                          , tamanhoSemNA ," de ", tamanhoOriginal , "registros" )
  
  write(textoAdicional,"InformacoesAdicionais.txt",1)
  
  dataSet$refund_date <- sapply(dataSet$refund_date , FUN =function(x) substr(x,0,7))
  
  for(var in unique(dataSet$refund_description) ){
    listaNomeCorruptos <- append(listaNomeCorruptos, geraGraficoGastosMesPorTipo(dataSet[dataSet$refund_description == var , ] , var))        
  }
  
  listaPolitico  <-c()
 
  for(nome in unique(listaNomeCorruptos)){
    listaIndices<-which(dataSet$deputy_name == nome)
    ##Identifica a região correspondente ao estado
    indiceEstado<-which(dataSet[listaIndices[1],]$deputy_state == RegioesPorEstado[,1])
    
    ##reescreve o estado pela região
    dataSet[listaIndices[1],]$deputy_state <- RegioesPorEstado[indiceEstado,2]
    
    ##adiciona em um vetor de politicos
    listaPolitico<- rbind(listaPolitico, dataSet[listaIndices[1],])
  }
  
  dataFrameStackedBar<- data.frame ( Frequencia = rep(1 , length(listaPolitico[,1])) ,Posicionamento_Politico =  listaPolitico$party_position , Regiao = listaPolitico$deputy_state)
  
  dataFrameStackedBar <- aggregate(dataFrameStackedBar$Frequencia, by = list(dataFrameStackedBar$Posicionamento_Politico ,dataFrameStackedBar$Regiao ) , FUN = sum )
  
  library(ggplot2)
  library(plyr)
  library(RColorBrewer)
  
  
  ordemNatural<- c("Farleft","Leftwing" , "Centreleft" , "Centre to centreleft" , "Centre" , "Centre/Centreright" , "Centreright","Rightwing")
  myplot<-ggplot(data = dataFrameStackedBar, aes(x = dataFrameStackedBar[,3], y = dataFrameStackedBar[,1], fill = dataFrameStackedBar[,2] )) + 
    geom_bar(stat="identity") 
  ggsave('DeputadosPorRegiaoEPolitica.pdf')
}

geraGraficoGastosMesPorTipo <- function(dataSet , tipoGasto){
  
  ## agrupa os dados por Nome de deputado e Mês , somando todos os valores que sejam do mesmo deputado e mês
  dataSetPorDeputado <-aggregate(dataSet$refund_value , by=list(dataSet$deputy_name,dataSet$refund_date) , FUN=sum  )
  dataSetSomaMensal <- aggregate(dataSetPorDeputado$x , by=list(dataSetPorDeputado$Group.2) , FUN =sum)
  
  ##Transforma os dados em milhares de reais
  dataSetSomaMensal$x <-sapply(dataSetSomaMensal$x , FUN = function(x) round(x/1000 , digits = 0))
  
  ## pega o nome da despesa em portugues para colocar o nome certo no grafico
  nomeDespesaPt <- NomesDespesas[which(NomesDespesas[,1] == tipoGasto) , 2] 
  nomePasta <- gsub(" " , "" ,nomeDespesaPt , fixed = "TRUE")
  nomeArquivo <- paste(nomePasta,"/",nomeDespesaPt,'_Barras.pdf')
  nomeArquivo<- gsub(" " , "" , nomeArquivo , fixed = "TRUE")
  
  ##cria o arquivo pdf
  nomePasta
  nomeArquivo
  if(!dir.exists(nomePasta)){
    dir.create(nomePasta); 
  }
  pdf(nomeArquivo)
  
  ##cria o gráfico
  ##forçando o ylim terminar como multiplo de 200
  graficoBarras<-barplot(dataSetSomaMensal$x, ylim=c(0,(round((max(dataSetSomaMensal$x)/200) , 0 ))+1)*200 , main = paste("Gastos com " ,nomeDespesaPt )  , xlab = "Data" , names.arg = dataSet$Group.1)
  text(x = graficoBarras ,  y = dataSetSomaMensal$x , label = dataSetSomaMensal$x , pos = 3 , cex = 0.7 , col= "red" )
  dev.off()
  
  nomeDeputadosCorruptos <- geraBoxPlotGastoMensal(dataSetPorDeputado , nomeDespesaPt)
  
  return(nomeDeputadosCorruptos)
  ##write.csv(dataSet, file = "Desktop/R/FuelCostByDeputyByMonth.csv");
}

geraGraficoQtdLancamentos <-function(){
  dataSet <- read.csv("Desktop/R/dirty_deputies_v2.csv" , dec = "." , sep = "," , header = TRUE , na.strings = "", stringsAsFactors = FALSE)
  
  dataSet[c(2,3,5,6,9,10,11,12,13,14,15,16,17,18)] <- list(NULL);
  
  dataSet <- na.omit(dataSet);
  
  colunaDatas <- dataSet[3];
  
  dataSet$refund_date <- sapply(dataSet$refund_date , FUN =function(x) substr(x,0,7))
  
  qtdLancamentos <-table(dataSet$refund_date)
  graficoBarras<-barplot( qtdLancamentos ,ylim=c(0,40000), main = "Quantidade de lançamentos por mês"   , xlab = "Data" )
  legendas <- as.data.frame(qtdLancamentos)
  text(x = graficoBarras ,  y = qtdLancamentos , label = legendas$Freq, pos = 3 , cex = 0.6 , col= "red" )
}

geraBoxPlotGastoMensal <-function(dataSet , nomeDespesas){
  ##Cria vetor com o nome dos outliers
  vetorDeNome = c()
  ##dataSet <- read.csv("Desktop/R/FuelCostByDeputyByMonth.csv" , dec = "." , sep = "," , header = TRUE , na.strings = "", stringsAsFactors = FALSE)
  
  nomeArquivo <- paste(nomeDespesas,"/",nomeDespesas,'_boxPlot.pdf')
  nomeArquivo<- gsub(" " , "" , nomeArquivo , fixed = "TRUE")
  
  ##cria pdf com o grafico
  pdf(nomeArquivo)
  
  boxplot <- boxplot(dataSet$x , main = append("Bloxplot de gasto com ", nomeDespesas)  ,  ylab="gasto em Reais por mês")
  
  dev.off()
  
  vetorDeNome <- c()
  ##retorna todos os outliers
  if(length(boxplot$out) > 0 ){
    valorMinOutLiers <- min(boxplot$out)
    dadoMensal<- dataSet[dataSet$x >= valorMinOutLiers ,]
    vetorDeNome<-append(vetorDeNome , unique(dadoMensal$Group.1))
  }
  return(vetorDeNome)
}

