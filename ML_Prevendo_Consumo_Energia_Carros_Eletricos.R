
# Objetivo: Construir um modelo de Machine Learning capaz de prever
#           o consumo de energia de veículos elétricos.

# Configurando o diretório de trabalho
setwd("C:/Users/jtmc/Documents/DSA/Projeto01")
getwd()


# Para realizar o trabalho, iremos fazer um web scraping e baixar diretamente
# o arquivo do endereço para o R.

# Você pode usar o pacote httr para gerenciar requisições HTTP
# e o readxl para carregar o conteúdo do arquivo.

# Instale os pacotes necessários
#install.packages("httr")
#install.packages("readxl")
#install.packages("ggplot2")
#install.packages("reshape2")
#install.packages("ggcorrplot")
#install.packages("psych")



# Carregando os pacotes necessários
library(httr)
library(readxl)
library(caret)
library(ggplot2)
library(reshape2)
library(ggcorrplot)
library(psych)


# COLETA DOS DADOS

# URL do arquivo
url <- "https://data.mendeley.com/public-files/datasets/tb9yrptydn/files/8dcbbd5f-d7b5-469b-91f2-698093ff6f16/file_downloaded"

# Nome do arquivo para salvar localmente
arquivo_local <- "dados.xlsx"

# Fazendo o download do arquivo
resposta <- GET(url, write_disk(arquivo_local, overwrite = TRUE))

# Verificando se o download foi bem-sucedido
if (status_code(resposta) == 200) {
  # Lendo o arquivo .xlsx baixado e Importando para um dataframe
  dados <- read_excel(arquivo_local)
  
  # Removendo objetos desnecessários para economizar memória
  rm(arquivo_local, resposta, url)
  
  message("Download realizado com sucesso!")
  
  # Visualizando as primeiras linhas
  print(head(dados))
  
} else {
  message("Falha no download. Código de status: ", status_code(resposta))
}

# Lista as worksheet no arquivo Excel
excel_sheets("dados.xlsx")

# Lendo a planilha do Excel
View(dados)
dim(dados)

# PRÉ-PROCESSAMENTO DE DADOS:


# LIMPEZA DE DADOS
# Checar valores ausentes
colSums(is.na(dados))

# Criar um data frame para armazenar as linhas com valores ausentes
dados_ausentes <- dados[!complete.cases(dados), ]

# Remover os registros com valores ausentes do data frame original
dados <- na.omit(dados)
View(dados)

# FEATURE ENGINEERING

# RENOMEAÇÃO DE COLUNAS
# Vamos renomar as colunas para facilitar nosso trabalho
# Nomes das colunas
colnames(dados)

# Grava os nomes das colunas em um vetor
myColumns <- colnames(dados)
myColumns

# Atribue o novo nome para cada coluna
myColumns[1] <- "NomeCarro"  
myColumns[2] <- "Fabricante"  
myColumns[3] <- "Modelo"  
myColumns[4] <- "PrecoMinimoBrutoPLN"  
myColumns[5] <- "PotenciaMotorKM"  
myColumns[6] <- "TorqueMaximoNm"  
myColumns[7] <- "TipoFreios"  
myColumns[8] <- "TipoTracao"  
myColumns[9] <- "CapacidadeBateriaKWh"  
myColumns[10] <- "AutonomiaWLTPkm"  
myColumns[11] <- "DistanciaEntreEixosCm"  
myColumns[12] <- "ComprimentoCm"  
myColumns[13] <- "LarguraCm"  
myColumns[14] <- "AlturaCm"  
myColumns[15] <- "PesoMinimoVazioKg"  
myColumns[16] <- "PesoBrutoPermitidoKg"  
myColumns[17] <- "CapacidadeMaximaCargaKg"  
myColumns[18] <- "NumeroAssentos"  
myColumns[19] <- "NumeroPortas"  
myColumns[20] <- "TamanhoPneuPol"  
myColumns[21] <- "VelocidadeMaximaKmh"  
myColumns[22] <- "CapacidadePortaMalasVDAl"  
myColumns[23] <- "Aceleracao0100KmhSeg"  
myColumns[24] <- "PotenciaMaximaCarregamentoDCkW"  
myColumns[25] <- "MediaConsumoEnergiaKWh100km"  

# Verifica o resultado
myColumns

# Atribui os novos nomes de colunas ao dataframe
colnames(dados) <- myColumns
rm(myColumns)


# CATEGORICAL ENCODING

# Vamos concatenar as variaveis Fabricante, NomeCarro e Modelo 
# Concatenação de variáveis
dados$NomeCarro_Completo <- paste(dados$Fabricante, dados$NomeCarro, dados$Modelo, sep = "_")

# Removendo as colunas originais
dados <- dados[, !names(dados) %in% c("Fabricante", "NomeCarro", "Modelo")]

# Colocando a nova coluna no começo
dados <- dados[, c("NomeCarro_Completo", setdiff(names(dados), "NomeCarro_Completo"))]

# Visualizando o resultado
View(dados)

#Vamos transformar algumas variáveis para o tipo fator
dados$TipoFreios <- factor(dados$TipoFreios)
dados$TipoTracao <- factor(dados$TipoTracao)
dados$NumeroAssentos <- factor(dados$NumeroAssentos)
dados$NumeroPortas <- factor(dados$NumeroPortas)
dados$TamanhoPneuPol <- factor(dados$TamanhoPneuPol)

# Selecionar apenas as colunas que são fatores
dados_fatores <- dados[, sapply(dados, is.factor)]

# Criar o modelo de dummies
dummy_model <- dummyVars(~ ., data = dados_fatores)

# Aplicar a transformação e gerar as dummies
dados_dummies <- data.frame(predict(dummy_model, newdata = dados_fatores))

# Combinar as dummies com as colunas originais que não são fatores (opcional)
dados_dummies <- cbind(dados[, !sapply(dados, is.factor)], dados_dummies)

rm(dados_fatores, dummy_model)

# Movendo a coluna target para o final
dados_dummies <- dados_dummies[, c(setdiff(names(dados_dummies), "MediaConsumoEnergiaKWh100km"), "MediaConsumoEnergiaKWh100km")]

# Visualizando o resultado
View(dados_dummies)


# EXPLORAÇÃO INICIAL DOS DADOS (EDA)

# Variáveis e tipos de dados
str(dados)
str(dados_dummies)
# Estatísticas resumidas
summary(dados) 

# Criar uma lista de resumos, um para cada coluna
summaries <- lapply(dados, summary)

# Loop para imprimir os resumos de cada coluna
for (i in 1:25) {
  cat(paste0(colnames(dados)[i], ":\n"))
  print(summaries[[i]])
  cat("\n")  # Adiciona uma linha em branco para separar os resumos
}


# 1. Matriz de Correlação

# Selecionar apenas colunas numéricas
numerical_vars <- dados[sapply(dados, is.numeric)]
cor_matrix <- cor(numerical_vars, use = "complete.obs")

ggcorrplot(cor_matrix, 
           lab = TRUE, 
           title = "Matriz de Correlação entre Variáveis Numéricas")
pairs()


# Este gráfico fornece mais informações sobre o relacionamento entre as variáveis
pairs.panels(despesas[c("idade", "bmi", "filhos", "gastos")])


# 2. Gráfico de visualização combinada 

# Função para criar os gráficos para cada coluna numérica
criar_graficos <- function(dataset) {graficos <- list()

  # Iterando sobre todas as colunas numéricas
  for (coluna in colnames(dataset)) {
    if (is.numeric(dataset[[coluna]])) {
      
      # Gráfico de caixa
      grafico1 <- ggplot(dataset, aes(x = "", y = .data[[coluna]])) +
        geom_boxplot(fill = "lightgreen", color = "darkgreen") +
        labs(
          title = "",
          y = "",
          x = ""
        ) +
        theme_minimal() + 
        theme(axis.title.y = element_blank())
      
      # Gráfico de violino
      grafico2 <- ggplot(dataset, aes(x = "", y = .data[[coluna]])) +
        geom_violin(fill = "lightblue", color = "darkblue") +
        labs(
          title = "",
          y = "",
          x = ""
        ) +
        theme_minimal() + 
        theme(axis.title.y = element_blank(), axis.text.y = element_blank())
      
      # Armazenando os gráficos
      graficos[[coluna]] <- list(grafico1, grafico2)
    }
  }
  
  return(graficos)
}

# Criando os gráficos
graficos_gerados <- criar_graficos(numerical_vars)

# Exibindo os gráficos
for (coluna in names(graficos_gerados)) {
  grid.arrange(graficos_gerados[[coluna]][[1]], graficos_gerados[[coluna]][[2]], 
               ncol = 2, top = coluna)
}

# 3. Histograma da variável target

ggplot(dados, aes(x = MediaConsumoEnergiaKWh100km)) +
  geom_histogram(fill = "blue", color = "black", alpha = 0.7) +
  geom_density(aes(y = ..count..), color = "red", linetype = "dashed") +
  labs(
    title = "MediaConsumoEnergiaKWh100km",
    x = "MediaConsumoEnergiaKWh100km",
    y = "Frequência"
  ) +
  theme_minimal()

# 3. Gráfico de Dispersão: Potência do Motor vs. Preço Mínimo
ggplot(dados, aes(x = MediaConsumoEnergiaKWh100km, y = )) +
  geom_point(color = "green", size = 3) +
  labs(
    title = "Relação entre Potência do Motor e Preço Mínimo",
    x = "Potência do Motor (KM)",
    y = "Preço Mínimo Bruto (PLN)"
  ) +
  theme_minimal()

# 4. Boxplot do Torque Máximo (Nm)
ggplot(dados, aes(y = TorqueMaximoNm)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(
    title = "Boxplot do Torque Máximo (Nm)",
    y = "Torque Máximo (Nm)"
  ) +
  theme_minimal()




# Identificar a última coluna como alvo
target <- "MediaConsumoEnergiaKWh100km"
