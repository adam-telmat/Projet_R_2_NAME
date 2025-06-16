# Test couleurs rapide
library(ggplot2)

# Test basique
p <- ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point() +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))

print(p)

# Si ça marche, le problème n'est pas les couleurs
# Si ça plante, on a trouvé le souci
cat("Test couleurs réussi !") 