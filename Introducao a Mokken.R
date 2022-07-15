###===--- Uma introdução à Análise de Escala de Mokken ---===###

# Start: Be carefull as these delete all your content on current R section
rm(list=ls()) 
graphics.off()
cat("\014")

### Passo a passo====
## Carregar o pacote
require(mokken)

## Carregar o banco de dados
data("transreas")
data <- transreas[,2:ncol(transreas)] # Selecionar apenas os itens de teste

## Primeiro passo: Análise de dimensionalidade====
AISP <- aisp(data,  search="ga", lowerbound=seq(.3,.8,by=.05))
AISP
Escala1 <- data[,colnames(data)[which(AISP[,"0.45"] == 1)]]

## Segundo passo: Análise de monotonicidade latente====
MonLat <- check.monotonicity(data, minsize = 85)
summary(MonLat)

MonLat <- check.monotonicity(Escala1, minsize = 85)
summary(MonLat)
MonLat <- check.monotonicity(Escala1, minsize = 425/7)
summary(MonLat)
MonLat <- check.monotonicity(Escala1, minsize = 7)
summary(MonLat)
Escala1.1 <- Escala1[,-7]

## Terceiro passo: Análise de não-interseção====
NI <- check.pmatrix(data)
summary(NI)

NI <- check.pmatrix(Escala1.1)
summary(NI)

## Quarto passo: Análise de independência local====
IL <- check.ca(Escala1.1, Windex=T, MINSIZE=4)
test <- function(w) (quantile(w,.75) + (3 * (quantile(w,.75) - median(w))))
W1 <- IL$Index[[1]]$W1
W1[lower.tri(W1)] > test(W1[lower.tri(W1)])
W1[upper.tri(W1)] > test(W1[upper.tri(W1)])
W2 <- IL$Index[[1]]$W2; W2 > test(W2)
W3 <- IL$Index[[1]]$W3; W3[lower.tri(W3)] > test(W3[lower.tri(W3)])

IL <- check.ca(Escala1.1[,-5], Windex=T, MINSIZE=4)
test <- function(w) (quantile(w,.75) + (3 * (quantile(w,.75) - median(w))))
W1 <- IL$Index[[1]]$W1
W1[lower.tri(W1)] > test(W1[lower.tri(W1)])
W1[upper.tri(W1)] > test(W1[upper.tri(W1)])
W2 <- IL$Index[[1]]$W2; W2 > test(W2)
W3 <- IL$Index[[1]]$W3; W3[lower.tri(W3)] > test(W3[lower.tri(W3)])

####====---- FIM! ----====####