# 1. Pacotes necessários
library(readxl)

# 2. Carregar planilha dos dados 
dados <- read_excel("C:/Users/julin/Documents/FACULDADE/7 período/TCC/Análises em R/Tabelas dos dados/análises específicas (sono X depressão)/L5 x EPDS.xlsx")

# 3. Verificar os nomes das colunas
print(colnames(dados))

# 4. Renomeando colunas, se necessário 
colnames(dados) <- c("L5", "EPDS")

# 5. Converter os dados para numérico, substituindo vírgulas por pontos
dados$L5 <- as.numeric(dados$L5)

# 6. Criar grupos com base no score EPDS
grupo_sem_depressao <- dados$L5[dados$EPDS < 12]
grupo_com_depressao <- dados$L5[dados$EPDS >= 12]

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
boxplot(L5 ~ (EPDS >= 12), data = dados,
        names = c("Sem Depressão", "Com Depressão"),
        main = "Distribuição de L5 por Grupo",
        xlab = "Grupo (0 = Sem Depressão, 1 = Com Depressão)",
        ylab = "L5 (Menor Atividade em 5 horas)",
        col = c("lightblue", "salmon"))
