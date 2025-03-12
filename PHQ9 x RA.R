# 1. Pacotes necessários
library(readxl)

# 2. Carregar planilha dos dados 
dados <- read_excel("C:/Users/julin/Documents/FACULDADE/7 período/TCC/Análises em R/Tabelas dos dados/análises específicas (sono X depressão)/RA x PHQ-9.xlsx")  # Substitua pelo caminho correto

# 3. Verificar os nomes das colunas
print(colnames(dados))

# 4. Renomeando colunas, se necessário
colnames(dados) <- c("RA", "PHQ9")

# 5. Converter os dados para numérico, substituindo vírgulas por pontos
dados$RA <- as.numeric(dados$RA)

# 6. Criar os grupos com base no PHQ-9
grupo_sem_depressao <- dados$RA[dados$PHQ9 < 10]   # Mães sem depressão
grupo_com_depressao <- dados$RA[dados$PHQ9 >= 10]  # Mães com depressão

# 7. Teste de normalidade de Shapiro-Wilk
shapiro_sem_dep <- shapiro.test(grupo_sem_depressao)
shapiro_com_dep <- shapiro.test(grupo_com_depressao)

# 8. Resultados do teste de normalidade
print(shapiro_sem_dep)
print(shapiro_com_dep)

# 9. Teste de Mann-Whitney (Wilcoxon Rank-Sum Test) para comparar os grupos
mann_whitney <- wilcox.test(grupo_sem_depressao, grupo_com_depressao)

# 10. Resultado do teste de Mann-Whitney
print(mann_whitney)

# 11. Boxplot para visualizar a distribuição dos grupos
boxplot(RA ~ (PHQ9 >= 10), data = dados,
        names = c("Sem Depressão", "Com Depressão"),
        main = "Distribuição da Amplitude Relativa (RA) por Grupo",
        xlab = "Grupo (0 = Sem Depressão, 1 = Com Depressão)",
        ylab = "Amplitude Relativa (RA)",
        col = c("lightblue", "salmon"))
