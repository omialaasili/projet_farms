data = read.csv("../Documents/AAMaster1 sem1/Data_science/farms_train.csv")
data_test = read.csv("../Documents/AAMaster1 sem1/Data_science/farms_test.csv")

test = glm(DIFF~., data = data, family = binomial)
summary(test)

library(tidyverse)  
library(car)
vif(test)

test2 =  step(test, direction = "backward")
summary(test2)

predicts = predict(test2, type = "response", newdata = data)
predicts

alpha=1/2
predictions_01 = predicts > alpha
as.numeric(predictions_01)

table(as.numeric(predictions_01),data$DIFF)

# il y a 32/190 faux positif et 24/201 faux negatif


predictions_02 = predicts > 0.6
table(as.numeric(predictions_02),data$DIFF)

#on passe a 32 et 28 pour 0.6

library(ROCR)

# On créée un objet de type prediction pour ROCR
pred <- prediction(predicts, data$DIFF)

# Calcul des coordonnées des différents points de la courbe ROC
perf <- performance(pred, "tpr", "fpr")

# Tracé de la courbe ROC
plot(perf, col = "blue", lwd = 2,
     main = "Courbe ROC - Modèle logistique")
abline(a = 0, b = 1, col = "red", lty = 2)  # ligne aléatoire

# Calcul de l'AUC
auc <- performance(pred, "auc")@y.values[[1]]
auc


predictstest2 = predict(test2, type = "response", newdata = data_test)
predictstest2
alpha=1/2
predictions_fin = predictstest2 > alpha

x  = predictions_fin
table(as.numeric(predictions_fin))

library(tidyverse)
x<-tibble(x)
x

write.csv(x, file = "Data_science/result.csv", row.names = FALSE)
