library(tidyverse)
library(ModelMetrics)
library(readr)
library(frm)
Base <- na.omit(read_csv("D:/Проекты/Язык R/Заметки на habr/З5 - Цензурированная регрессия/hfi_cc_2020.csv"))
Base <- as.data.frame(Base[, c(6:113)])
# Выбрана переменная 38
y <- as.matrix(Base[,38]/10)
sum(y==0)
sum(y==1)
# Зависимые переменные - 49,106,51
X1 <- Base[,c(49,106)]
X2 <- Base[,c(51,55)]
# Однокомпонентные модели
m_1_1 <- frm(y,X1,linkfrac="logit")
m_1_2 <- frm(y,X2,linkfrac="probit")
m_1_3 <- frm(y,X1,linkfrac="cauchit")
m_1_4 <- frm(y,X2,linkfrac="loglog")
m_1_5 <- frm(y,cbind(X1,X2),linkfrac="cloglog")
# Двухкомпонентные модели
m_2 <- frm(y,X2,X1,linkbin = "cauchit",linkfrac="logit", type = "2P")
m_2_0 <- frm(y,X1,X1,linkbin = "logit",linkfrac="logit", type = "2P",inflation = 1)
#Тесты
frm.reset(m_1_1,2:3,c("Wald","LM"))
frm.reset(m_1_5,2:5,c("LM")) # 2-4 номера коэффициентов модели

frm.ggoff(m_1_5,c("Wald","LM"))

frm.ptest(m_1_1,m_1_3)
frm.ptest(m_2,m_2_0)
