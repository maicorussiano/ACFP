calcular_adiantamento <- function() {
  
  # Função para formatar valores em reais corretamente
  formatar_real <- function(valor) {
    formatC(valor, format = "f", big.mark = ".", decimal.mark = ",", digits = 2)
  }
  
  # Solicita os dados do usuário para o cálculo
  custo_unitario_str <- readline("Digite o custo de cada blindado (em reais): R$ ")
  custo_unitario <- as.numeric(gsub("\\.", "", gsub(",", ".", custo_unitario_str))) # Remove ponto e substitui vírgula por ponto
  
  num_unidades_adiantamento <- as.integer(readline("Digite o número de unidades que serão pagas adiantadas: "))
  num_unidades_total <- as.integer(readline("Digite o número total de unidades no contrato: "))
  pagamento_anual <- as.integer(readline("Digite o número de unidades pagas por ano (em lotes anuais): "))
  taxa_desconto <- as.numeric(readline("Digite a taxa de desconto anual (em decimal, exemplo: 0.008 para 0.8%): "))
  meses_contrato <- as.integer(readline("Digite o prazo original do contrato (em meses): "))
  meses_adiantamento <- as.integer(readline("Digite o número de meses para o pagamento adiantado (em meses): "))
  
  # Cálculo do valor total do contrato sem desconto
  valor_total_contrato <- custo_unitario * num_unidades_total
  
  # Cálculo do valor total sem desconto para as unidades adiadas
  valor_total_adiantamento <- custo_unitario * num_unidades_adiantamento
  
  # Cálculo do valor presente (adiantamento) com a taxa de desconto
  valor_presente <- valor_total_adiantamento / (1 + taxa_desconto / 12) ^ meses_adiantamento
  
  # Calcular o custo por unidade com o desconto
  custo_por_unidade_adiantada <- valor_presente / num_unidades_adiantamento
  
  # Calcular o valor das parcelas dos outros lotes que não foram adiadas
  num_unidades_restantes <- num_unidades_total - num_unidades_adiantamento
  valor_total_restante <- custo_unitario * num_unidades_restantes
  
  # Cálculo do valor final (adiantado + parcelas restantes)
  valor_final_contrato <- valor_presente + valor_total_restante
  
  # Calcular a redução percentual no valor total do contrato
  reducao_percentual <- ((valor_total_contrato - valor_final_contrato) / valor_total_contrato) * 100
  
  # Criar o resumo do cálculo
  resumo <- sprintf("\nResumo do cálculo:\n")
  resumo <- paste0(resumo, sprintf("1. Do total de %d unidades, você adiantou o pagamento de %d unidades, o que representa pagar à vista o equivalente a %d meses.\n",
                                   num_unidades_total, num_unidades_adiantamento, meses_adiantamento))
  resumo <- paste0(resumo, sprintf("2. Isso significa que você deveria ter um desconto neste adiantamento, saindo cada unidade neste pagamento por R$ %s, totalizando R$ %s a serem pagos adiantados.\n",
                                   formatar_real(custo_por_unidade_adiantada), formatar_real(valor_presente)))
  resumo <- paste0(resumo, sprintf("3. No custo original de todo o contrato, que seria R$ %s, agora o custo total é de R$ %s, o que significa uma redução de %.2f%%.\n",
                                   formatar_real(valor_total_contrato), formatar_real(valor_final_contrato), reducao_percentual))
  
  # Exibir o resumo no console
  cat(resumo)
  
  # Perguntar ao usuário se deseja exportar para TXT
  exportar <- readline("\nDeseja exportar o resultado para um arquivo TXT na área de trabalho? (S/N): ")
  
  if (toupper(exportar) == "S") {
    # Definir o caminho da área de trabalho (Windows e Mac/Linux)
    caminho <- ifelse(.Platform$OS.type == "windows",
                      file.path(Sys.getenv("USERPROFILE"), "Desktop", "resumo_calculo.txt"),
                      file.path(Sys.getenv("HOME"), "Desktop", "resumo_calculo.txt"))
    
    # Salvar o arquivo
    writeLines(resumo, caminho)
    cat("\nArquivo salvo com sucesso em:", caminho, "\n")
  } else {
    cat("\nOperação encerrada. O resultado não foi exportado.\n")
  }
}

# Chama a função para calcular
calcular_adiantamento()


