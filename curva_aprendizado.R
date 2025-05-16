# Função para verificar e instalar pacotes automaticamente
instalar_pacotes <- function(pacotes) {
  pacotes_faltantes <- pacotes[!(pacotes %in% installed.packages()[, "Package"])]
  if (length(pacotes_faltantes) > 0) {
    cat("Instalando pacotes necessários...\n")
    install.packages(pacotes_faltantes)
  }
  lapply(pacotes, library, character.only = TRUE)
}

# Lista de pacotes necessários
pacotes_necessarios <- c("ggplot2", "dplyr", "scales", "openxlsx", "ggthemes")
instalar_pacotes(pacotes_necessarios)

# Solicitar entrada do usuário
cat("Digite o custo unitário inicial (A) (exemplo: 10000000 para R$ 10.000.000,00): ")
A <- as.numeric(gsub(",", ".", readline())) 

cat("Digite o total de unidades produzidas (X_total): ")
X_total <- as.numeric(readline())

cat("Digite a inclinação da curva de aprendizado (em %) (exemplo: 95 para 95%): ")
learning_rate <- as.numeric(readline()) / 100  

# Verificar se os valores foram inseridos corretamente
if (is.na(A) | is.na(X_total) | is.na(learning_rate)) {
  stop("Erro: Digite apenas números válidos sem pontos ou vírgulas.")
}

# Calcular b (coeficiente da curva de aprendizado)
b <- log(learning_rate) / log(2)

# Criar vetor unitário de unidades produzidas (1 até X_total) - GARANTINDO QUE NÃO INCLUA ZERO
X <- seq(1, X_total, by = 1)  

# Calcular o custo médio usando a fórmula correta
Custo_Medio <- A * (X^b)

# Calcular a redução percentual correta
Reducao_Percentual <- (1 - (Custo_Medio / A)) * 100

# Criar tabela corrigida sem erro de formatação
tabela_resultados <- data.frame(
  Unidades_Produzidas = X,
  Custo_Medio_Estimado = round(Custo_Medio, 2),  # Mantém como número real
  Reducao = paste0(round(Reducao_Percentual, 1), " %")
)

# Exibir a tabela correta no RStudio
View(tabela_resultados)

# Perguntar ao usuário se deseja exportar os dados
cat("Deseja exportar os dados para Excel na Área de Trabalho? (sim/não): ")
exportar <- tolower(readline())

if (exportar == "sim") {
  # Definir o caminho para a Área de Trabalho
  caminho_arquivo <- file.path(Sys.getenv("USERPROFILE"), "Desktop", "Curva_Aprendizado.xlsx")
  
  # Salvar como Excel sem erro de string
  write.xlsx(tabela_resultados, caminho_arquivo, rowNames = FALSE)
  
  cat("Arquivo salvo com sucesso em:", caminho_arquivo, "\n")
}

# Criar gráfico profissional e corrigido
grafico <- ggplot(data = tabela_resultados, 
                  aes(x = Unidades_Produzidas, 
                      y = Custo_Medio_Estimado)) +
  geom_line(color = "#1E3A8A", linewidth = 1.5, na.rm = TRUE) +  # Linha azul escuro profissional
  geom_point(color = "#1E3A8A", size = 1.5, na.rm = TRUE) +  # Pontos na curva
  geom_hline(yintercept = A, linetype = "dashed", color = "gray40", linewidth = 1) +  # Linha do custo original
  scale_x_continuous(breaks = seq(1, X_total, by = round(X_total / 10))) +  # Eixo X nunca inclui 0
  scale_y_continuous(
    limits = c(min(Custo_Medio, na.rm = TRUE), A),  # Ajuste correto dos limites do eixo Y
    labels = scales::label_number(big.mark = ".", decimal.mark = ",", prefix = "R$ ")
  ) +  # Eixo Y formatado corretamente
  labs(
    title = "Curva de Aprendizado vs. Custo Original",
    subtitle = "Redução progressiva do custo médio por unidade",
    x = "Unidades Produzidas",
    y = "Custo Médio Estimado (R$)"
  ) +
  theme_minimal(base_size = 14) +  # Tema clean e elegante
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#1E3A8A"),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray85"),  # Linhas suaves no fundo
    panel.grid.minor = element_blank()
  ) +
  annotate("text", x = max(X) * 0.75, y = A * 0.98, 
           label = "Custo Original Linear", color = "gray40", size = 5)

# Exibir o gráfico corrigido
print(grafico)


