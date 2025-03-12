# 1. Pacotes necessários
library(readxl)

# 2. Carregar planilha dos dados 
dados <- read_excel("C:/Users/julin/Documents/FACULDADE/7 período/TCC/Análises em R/Tabelas dos dados/análises específicas (sono X depressão)/M10 x PHQ-9.xlsx")

# 3. Verificar os nomes das colunas
print(colnames(dados))

# 4. Renomeando colunas, se necessário
colnames(dados) <- c("M10", "PHQ9")

# 5. Converter os dados para numérico, substituindo vírgulas por pontos
dados$M10 <- as.numeric(dados$M10)

# 6. Criar os grupos com base no PHQ-9
grupo_sem_depressao <- dados$M10[dados$PHQ9 < 10]  # PHQ-9 < 10 indica menor probabilidade de depressão
grupo_com_depressao <- dados$M10[dados$PHQ9 >= 10] # PHQ-9 >= 10 indica maior probabilidade de depressão

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
boxplot(M10 ~ (PHQ9 >= 10), data = dados,
        names = c("Sem Depressão", "Com Depressão"),
        main = "Distribuição de M10 por Grupo (PHQ-9)",
        xlab = "Grupo (0 = Sem Depressão, 1 = Com Depressão)",
        ylab = "M10 (Maior Atividade em 10 horas)",
        col = c("lightblue", "salmon"))
