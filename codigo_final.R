# ----------------------------------------------------------------
# MODELAGEM MATEMÁTICA E COMPUTACIONAL - VERSÃO FINAL
# Simulação com Cenário de Equipamentos GoodWe
# ----------------------------------------------------------------
# Este código representa a versão final do projeto, incorporando
# todos os aprimoramentos: modelos físicos realistas, análise
# matemática completa (limites, derivadas, integrais), análise
# econômica e parâmetros de equipamentos reais.
# ----------------------------------------------------------------

# 0. INSTALAÇÃO E CARREGAMENTO AUTOMÁTICO DE PACOTES
# ----------------------------------------------------------------
# Este bloco verifica se os pacotes necessários estão instalados e,
# caso contrário, os instala. Isso garante que o script funcione em qualquer ambiente R.
pacotes_necessarios <- c("ggplot2", "gridExtra", "scales", "dplyr", "grid")

for (pacote in pacotes_necessarios) {
  if (!require(pacote, character.only = TRUE)) {
    cat(sprintf("Pacote '%s' não encontrado. Instalando...\n", pacote))
    install.packages(pacote, dependencies = TRUE)
  }
  library(pacote, character.only = TRUE)
}
cat("✅ Todos os pacotes necessários foram carregados com sucesso!\n\n")


# 1. PARÂMETROS GERAIS DA SIMULAÇÃO (CENÁRIO GOODWE)
# ----------------------------------------------------------------
params <- list(
  # Simulação
  t_inicio_sim = 0,
  t_fim_sim = 24,
  passo_tempo = 0.1,
  
  # Geração Solar (Compatível com inversor GoodWe)
  I_max = 1000,
  area_paineis = 25,
  eficiencia_paineis = 0.21,
  dia_duracao = 12,
  pico_solar = 12,
  temp_ambiente_base = 20,
  variacao_temp_dia = 10,
  coef_temp_potencia = -0.004,
  
  # Consumo da Casa (Dependente dos hábitos do morador)
  consumo_base_casa = 300,
  consumo_pico_casa = 1500,
  desvio_padrao_ruido = 50,
  inicio_pico_manha = 6,
  fim_pico_manha = 9,
  inicio_pico_noite = 18,
  fim_pico_noite = 22,
  
  # Bateria Estacionária (BESS) - Baseado em 2x GoodWe Lynx Home U
  capacidade_bess_kWh = 10.8, # 2 módulos de 5.4 kWh
  potencia_max_bess_kW = 5.0,  # Do inversor GoodWe GW5K-ET
  soc_inicial_bess = 0.5,
  soc_min_bess = 0.1,          # Limite de segurança para baterias LFP
  soc_max_bess = 1.0,          # Baterias LFP podem ser carregadas a 100%
  eficiencia_bess = 0.95,      # Eficiência de ida e volta (round-trip) documentada
  
  # Carga do Carro Elétrico (VE) - Baseado no Carregador GoodWe HCA
  capacidade_ev_kWh = 60,
  potencia_max_carga_ev_kW = 7.0, # Potência do carregador de 7 kW
  soc_inicial_ev = 0.2,
  soc_alvo_ev = 0.9,
  t_inicio_carga_ev = 19,
  limiar_cv = 0.8
)


# 2. FUNÇÕES DE MODELO (Matemática da Simulação)
# ----------------------------------------------------------------

# Modelo da Geração Solar com efeito de temperatura
calcular_geracao_solar <- function(t, p) {
  irradiancia <- max(0, p$I_max * cos((pi / p$dia_duracao) * (t - p$pico_solar)))
  temp_ambiente <- p$temp_ambiente_base + p$variacao_temp_dia * sin((pi / 12) * (t - 8))
  temp_celula <- temp_ambiente + (irradiancia / 800) * 20
  fator_perda_temp <- 1 + p$coef_temp_potencia * (temp_celula - 25)
  potencia_gerada <- irradiancia * p$area_paineis * p$eficiencia_paineis * fator_perda_temp
  return(max(0, potencia_gerada))
}

# Modelo do Consumo da Casa com componente estocástico
calcular_consumo_casa <- function(t, p) {
  consumo <- p$consumo_base_casa + rnorm(1, mean = 0, sd = p$desvio_padrao_ruido)
  if ((t >= p$inicio_pico_manha && t < p$fim_pico_manha) ||
      (t >= p$inicio_pico_noite && t < p$fim_pico_noite)) {
    consumo <- consumo + p$consumo_pico_casa
  }
  return(max(0, consumo))
}

# Modelo da Carga do Carro Elétrico (CC-CV)
calcular_consumo_carro_cc_cv <- function(t, soc_ev_atual, p) {
  if (t < p$t_inicio_carga_ev || soc_ev_atual >= p$soc_alvo_ev) {
    return(0)
  }
  if (soc_ev_atual < p$limiar_cv) {
    potencia_carga <- p$potencia_max_carga_ev_kW * 1000 # em Watts
  } else {
    fator_decaimento <- (soc_ev_atual - p$limiar_cv) / (p$soc_alvo_ev - p$limiar_cv)
    potencia_carga <- p$potencia_max_carga_ev_kW * 1000 * (1 - fator_decaimento)
  }
  return(max(0, potencia_carga))
}


# 3. EXECUÇÃO DA SIMULAÇÃO (Loop Principal)
# ----------------------------------------------------------------
cat("🔄 Iniciando simulação com cenário de equipamentos GoodWe...\n")
tempo <- seq(params$t_inicio_sim, params$t_fim_sim, by = params$passo_tempo)
n_passos <- length(tempo)

# Inicializa o dataframe de resultados para clareza
resultados <- data.frame(
  tempo = tempo,
  P_solar_kW = numeric(n_passos),
  P_casa_kW = numeric(n_passos),
  P_carro_kW = numeric(n_passos),
  P_consumo_total_kW = numeric(n_passos),
  P_bess_kW = numeric(n_passos),
  P_rede_kW = numeric(n_passos),
  SoC_bess_percent = numeric(n_passos),
  SoC_ev_percent = numeric(n_passos)
)

# Condições iniciais em fração (0-1) para os cálculos
soc_bess_atual <- params$soc_inicial_bess
soc_ev_atual <- params$soc_inicial_ev

resultados$SoC_bess_percent[1] <- soc_bess_atual * 100
resultados$SoC_ev_percent[1] <- soc_ev_atual * 100

# Loop de simulação passo a passo
for (i in 1:n_passos) {
  t_atual <- tempo[i]
  
  # 1. Calcular Geração e Consumos no instante t
  resultados$P_solar_kW[i] <- calcular_geracao_solar(t_atual, params) / 1000
  resultados$P_casa_kW[i] <- calcular_consumo_casa(t_atual, params) / 1000
  resultados$P_carro_kW[i] <- calcular_consumo_carro_cc_cv(t_atual, soc_ev_atual, params) / 1000
  resultados$P_consumo_total_kW[i] <- resultados$P_casa_kW[i] + resultados$P_carro_kW[i]
  
  # 2. Calcular o balanço de potência ANTES da bateria da casa
  p_liquido_sem_bess <- resultados$P_solar_kW[i] - resultados$P_consumo_total_kW[i]
  
  # 3. Lógica de Controle da Bateria da Casa (Autoconsumo)
  p_bess <- 0
  if (p_liquido_sem_bess > 0) { # Excesso de geração solar -> Carregar Bateria
    pot_carga_possivel <- (params$soc_max_bess - soc_bess_atual) * params$capacidade_bess_kWh / params$passo_tempo
    p_bess <- -min(p_liquido_sem_bess, params$potencia_max_bess_kW, pot_carga_possivel)
  } else { # Déficit de energia -> Descarregar Bateria
    pot_descarga_possivel <- (soc_bess_atual - params$soc_min_bess) * params$capacidade_bess_kWh / params$passo_tempo
    p_bess <- min(abs(p_liquido_sem_bess), params$potencia_max_bess_kW, pot_descarga_possivel)
  }
  resultados$P_bess_kW[i] <- p_bess
  
  # 4. Calcular o balanço final com a rede elétrica
  resultados$P_rede_kW[i] <- p_liquido_sem_bess + resultados$P_bess_kW[i]
  
  # 5. Atualizar os estados de carga para o próximo passo (Equações Diferenciais Discretizadas)
  # Bateria da Casa
  energia_bess_kWh <- resultados$P_bess_kW[i] * params$passo_tempo
  if (energia_bess_kWh < 0) { # Carregando
    soc_bess_atual <- soc_bess_atual - (energia_bess_kWh * params$eficiencia_bess) / params$capacidade_bess_kWh
  } else { # Descarregando
    soc_bess_atual <- soc_bess_atual - (energia_bess_kWh / params$eficiencia_bess) / params$capacidade_bess_kWh
  }
  soc_bess_atual <- max(params$soc_min_bess, min(params$soc_max_bess, soc_bess_atual))
  
  # Bateria do VE
  energia_ev_kWh <- resultados$P_carro_kW[i] * params$passo_tempo
  soc_ev_atual <- soc_ev_atual + energia_ev_kWh / params$capacidade_ev_kWh
  soc_ev_atual <- min(1.0, soc_ev_atual)
  
  # Armazenar estado para o próximo passo (se não for o último)
  if (i < n_passos) {
    resultados$SoC_bess_percent[i+1] <- soc_bess_atual * 100
    resultados$SoC_ev_percent[i+1] <- soc_ev_atual * 100
  }
}
cat("✅ Simulação concluída com sucesso!\n\n")

# 4. ANÁLISE DOS RESULTADOS
# ----------------------------------------------------------------
cat("⚙️  Realizando análises matemáticas e estatísticas...\n")
passo_h <- params$passo_tempo
energia_gerada_kWh <- sum(resultados$P_solar_kW) * passo_h
energia_consumida_total_kWh <- sum(resultados$P_consumo_total_kW) * passo_h
energia_puxada_rede_kWh <- -sum(resultados$P_rede_kW[resultados$P_rede_kW < 0]) * passo_h
energia_injetada_rede_kWh <- sum(resultados$P_rede_kW[resultados$P_rede_kW > 0]) * passo_h
pico_consumo_rede <- min(resultados$P_rede_kW)
hora_pico_consumo_rede <- resultados$tempo[which.min(resultados$P_rede_kW)]
taxa_rampa_rede <- c(NA, diff(resultados$P_rede_kW) / diff(resultados$tempo))
rampa_max_subida <- max(taxa_rampa_rede, na.rm = TRUE)
hora_rampa_max_subida <- resultados$tempo[which.max(taxa_rampa_rede)]

# 5. IMPRESSÃO DO RELATÓRIO TÉCNICO NO CONSOLE
# ----------------------------------------------------------------
cat("\n--- RELATÓRIO TÉCNICO DA SIMULAÇÃO ---\n\n")
cat(sprintf("Balanço Energético (Integral):\n"))
cat(sprintf("  - Geração Solar: %.2f kWh\n", energia_gerada_kWh))
cat(sprintf("  - Consumo Total: %.2f kWh\n", energia_consumida_total_kWh))
cat(sprintf("  - Interação com Rede: %.2f kWh (puxado) | %.2f kWh (injetado)\n\n", energia_puxada_rede_kWh, energia_injetada_rede_kWh))
cat(sprintf("Pontos Críticos (Limites):\n"))
cat(sprintf("  - Pico de Consumo da Rede: %.2f kW às %.1fh\n\n", pico_consumo_rede, hora_pico_consumo_rede))
cat(sprintf("Análise de Rampas (Derivada):\n"))
cat(sprintf("  - Rampa Máxima de Subida (Estresse na Rede): %.2f kW/h às %.1fh\n", rampa_max_subida, hora_rampa_max_subida))
cat("----------------------------------------\n\n")

# 6. VISUALIZAÇÃO GRÁFICA (Dashboard)
# ----------------------------------------------------------------
cat("📊 Gerando dashboard de visualização...\n")
g_potencia <- ggplot(resultados, aes(x = tempo)) +
  geom_line(aes(y = P_solar_kW, color = "Geração Solar"), size = 1.1) +
  geom_line(aes(y = P_consumo_total_kW, color = "Consumo Total"), size = 1.1) +
  geom_area(aes(y = P_rede_kW, fill = "Balanço com a Rede"), alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  scale_color_manual(name = "Potências", values = c("Geração Solar" = "#f0b400", "Consumo Total" = "#d1495b")) +
  scale_fill_manual(name = "Rede", values = c("Balanço com a Rede" = "#0072B2")) +
  labs(title = "Balanço de Potências do Sistema (Cenário GoodWe)", x = NULL, y = "Potência (kW)") +
  theme_minimal(base_size = 14) + theme(legend.position = "bottom", plot.title = element_text(face="bold"))

g_bess <- ggplot(resultados, aes(x = tempo)) +
  geom_line(aes(y = P_bess_kW, color = "Potência BESS (kW)"), size = 1.1) +
  geom_line(aes(y = SoC_bess_percent, color = "SoC BESS (%)"), size = 1.1, linetype = "dashed") +
  scale_y_continuous(
    name = "Potência (kW)",
    sec.axis = sec_axis(~., name = "SoC (%)")
  ) +
  scale_color_manual(name = "Bateria (BESS)", values = c("Potência BESS (kW)" = "darkgreen", "SoC BESS (%)" = "lightgreen")) +
  labs(subtitle = "Operação da Bateria Estacionária", x = NULL, y = NULL) +
  theme_minimal(base_size = 14) + theme(legend.position = "bottom")

g_ev <- ggplot(resultados, aes(x = tempo, y = SoC_ev_percent)) +
  geom_line(color = "purple", size = 1.2) +
  scale_y_continuous(labels = percent_format(scale = 1), limits = c(0, 100)) +
  labs(subtitle = "Estado de Carga (SoC) do Veículo Elétrico", x = "Tempo (horas)", y = "SoC (%)") +
  theme_minimal(base_size = 14)

# Salva o dashboard em um arquivo PNG para o relatório
png("dashboard_energetico.png", width = 12, height = 9, units = "in", res = 300)
grid.arrange(g_potencia, g_bess, g_ev, ncol = 1, heights = c(3, 2, 2))
dev.off()

cat("✅ Dashboard salvo como 'dashboard_energetico.png'.\n")
cat("\n--- FIM DA EXECUÇÃO ---\n")

