library(plotly)
library(MASS)
library(fields)
library(mclust)
library(ggplot2)
library(caTools)



# Chargement Dataset
dataset = read.csv2(file="C:/Users/loic_/Documents/ESILV/A4/Parcours_recherche/parcours recherche/datatourismIleDeFranceV4.csv", header = T)

# Extraxtion des 6 monuments choisis
teiffel_complet = subset(dataset, dataset$name == "Tour Eiffel")
museeLouvre_complet = subset(dataset, dataset$name == "MusÃ©e du Louvre")
placeBastille_complet = subset(dataset, dataset$name == "Place de la Bastille")
arcTriomphe_complet = subset(dataset, dataset$name == "Arc de Triomphe")
pantheon_complet = subset(dataset, dataset$name == "PanthÃ©on")
cathedral_complet = subset(dataset, dataset$name == "CathÃ©drale Notre-Dame de Paris")


# J'enleve les coordonnées qui biaisent les resultats en representant 99% des données d'un monument
teiffel2 = subset(teiffel_complet, teiffel_complet$idLocation != 2593354)
teiffel2 = subset(teiffel2, teiffel2$idLocation != 109768643)
museeLouvre2 = subset(museeLouvre_complet, museeLouvre_complet$idLocation != 683630)
placeBastille2 = subset(placeBastille_complet, placeBastille_complet$idLocation != 7971)
arcTriomphe2 = subset(arcTriomphe_complet, arcTriomphe_complet$idLocation != 2999986)
pantheon2 = subset(pantheon_complet, pantheon_complet$idLocation != 1193231)
cathedral2 = subset(cathedral_complet, cathedral_complet$idLocation != 2999938)


# Je supprime les coordonnées qui sont en double pour créer des vecteur de coordonnées uniques
teiffel_complet[,1] = teiffel_complet[,4]
teiffelU = teiffel_complet[!duplicated(teiffel_complet[,1]),]
museeLouvre_complet[,1] = museeLouvre_complet[,4]
museeLouvreU = museeLouvre_complet[!duplicated(museeLouvre_complet[,1]),]
placeBastille_complet[,1] = placeBastille_complet[,4]
placeBastilleU = placeBastille_complet[!duplicated(placeBastille_complet[,1]),]
arcTriomphe_complet[,1] = arcTriomphe_complet[,4]
arcTriompheU = arcTriomphe_complet[!duplicated(arcTriomphe_complet[,1]),]
pantheon_complet[,1] = pantheon_complet[,4]
pantheonU = pantheon_complet[!duplicated(pantheon_complet[,1]),]
cathedral_complet[,1] = cathedral_complet[,4]
cathedralU = cathedral_complet[!duplicated(cathedral_complet[,1]),]



# Je sépare les vecteur des coordonnées uniques en training et test set
separation_training_test_unique = function() {
  
  split_eiffel = sample.split(teiffelU, SplitRatio = 0.90)
  training_set_eiffel = subset(teiffelU, split_eiffel == TRUE)
  test_set_eiffel = subset(teiffelU, split_eiffel == FALSE)
  
  split_louvre = sample.split(museeLouvreU, SplitRatio = 0.90)
  training_set_louvre = subset(museeLouvreU, split_louvre == TRUE)
  test_set_louvre = subset(museeLouvreU, split_louvre == FALSE)
  
  split_bastille = sample.split(placeBastilleU, SplitRatio = 0.90)
  training_set_bastille = subset(placeBastilleU, split_bastille == TRUE)
  test_set_bastille = subset(placeBastilleU, split_bastille == FALSE)
  
  split_arc = sample.split(arcTriompheU, SplitRatio = 0.90)
  training_set_arc = subset(arcTriompheU, split_arc == TRUE)
  test_set_arc = subset(arcTriompheU, split_arc == FALSE)
  
  split_pantheon = sample.split(pantheonU, SplitRatio = 0.90)
  training_set_pantheon = subset(pantheonU, split_pantheon == TRUE)
  test_set_pantheon = subset(pantheonU, split_pantheon == FALSE)
  
  split_cathedral = sample.split(cathedralU, SplitRatio = 0.90)
  training_set_cathedral = subset(cathedralU, split_cathedral == TRUE)
  test_set_cathedral = subset(cathedralU, split_cathedral == FALSE)
  
  test_set_global <<- rbind(test_set_arc, test_set_bastille, test_set_eiffel, test_set_louvre, test_set_pantheon, test_set_cathedral)

  
  museeLouvre <<- training_set_louvre
  teiffel <<- training_set_eiffel
  cathedral <<- training_set_cathedral
  placeBastille <<- training_set_bastille
  arcTriomphe <<- training_set_arc
  pantheon <<- training_set_pantheon
}


# Je sépare les vecteurs des coordonnées qui ne sont pas uniques en training et test set
separation_training_test = function() {
  
  split_eiffel = sample.split(teiffel2, SplitRatio = 0.90)
  training_set_eiffel = subset(teiffel2, split_eiffel == TRUE)
  test_set_eiffel = subset(teiffel2, split_eiffel == FALSE)
  
  split_louvre = sample.split(museeLouvre2, SplitRatio = 0.90)
  training_set_louvre = subset(museeLouvre2, split_louvre == TRUE)
  test_set_louvre = subset(museeLouvre2, split_louvre == FALSE)
  
  split_bastille = sample.split(placeBastille2, SplitRatio = 0.90)
  training_set_bastille = subset(placeBastille2, split_bastille == TRUE)
  test_set_bastille = subset(placeBastille2, split_bastille == FALSE)
  
  split_arc = sample.split(arcTriomphe2, SplitRatio = 0.90)
  training_set_arc = subset(arcTriomphe2, split_arc == TRUE)
  test_set_arc = subset(arcTriomphe2, split_arc == FALSE)
  
  split_pantheon = sample.split(pantheon2, SplitRatio = 0.90)
  training_set_pantheon = subset(pantheon2, split_pantheon == TRUE)
  test_set_pantheon = subset(pantheon2, split_pantheon == FALSE)
  
  split_cathedral = sample.split(cathedral2, SplitRatio = 0.90)
  training_set_cathedral = subset(cathedral2, split_cathedral == TRUE)
  test_set_cathedral = subset(cathedral2, split_cathedral == FALSE)
  
  test_set_global <<- rbind(test_set_arc, test_set_bastille, test_set_eiffel, test_set_louvre, test_set_pantheon, test_set_cathedral)
  

  museeLouvre <<- training_set_louvre
  teiffel <<- training_set_eiffel
  cathedral <<- training_set_cathedral
  placeBastille <<- training_set_bastille
  arcTriomphe <<- training_set_arc
  pantheon <<- training_set_pantheon
}



# Premiere fonction, elle calcule la probabilité d'appartenance à un lieu en séparant longitude et latitude
# "x" est la longitude | "y" est la latitude 
LonLat = function(x,y,type){
  
  
  # modelise la densité de la longitude pour le vecteur teiffel
  a1 = density(teiffel$longitude,adjust = 3.5, bw = type, kernel = c("epanechnikov"))
  # interople la courbe ci-dessus avec la longitude x 
  val_a1 = approx(a1$x,a1$y,xout=x)
  # modelise la densité de la latitude pour le vecteur teiffel
  b1 = density(teiffel$latitude,adjust = 3.5, bw = type, kernel = c("epanechnikov"))
  # interople la courbe ci-dessus avec la latitude y
  val_b1 = approx(b1$x,b1$y,xout=y)
  
  nom1 = "Tour Eiffel"
  # Pour pouvoir comparer la densité entre chaque monument, je choisis le minimum entre la valeur de la densité de la longitude et de la latitude
  pourc1 = min(val_a1$y, val_b1$y)
  
  
  ## Pour les autres monuments la démarche est identique
  
  a2 = density(museeLouvre$longitude, adjust = 3.5, bw = type, kernel = c("epanechnikov"))
  val_a2 = approx(a2$x,a2$y,xout=x)
  b2 = density(museeLouvre$latitude,adjust = 3.5, bw = type, kernel = c("epanechnikov"))
  val_b2 = approx(b2$x,b2$y,xout=y)

  pourc2 = min(val_a2$y, val_b2$y)
  nom2 = "MusÃ©e du Louvre"

  
  
  a3 = density(cathedral$longitude, adjust = 3.5, bw = type, kernel = c("epanechnikov"))
  val_a3 = approx(a3$x,a3$y,xout=x)
  b3 = density(cathedral$latitude,adjust = 3.5, bw = type, kernel = c("epanechnikov"))
  val_b3 = approx(b3$x,b3$y,xout=y)
  
  pourc3 = min(val_a3$y, val_b3$y)
  nom3 = "CathÃ©drale Notre-Dame de Paris"
  
  
  
  a4 = density(placeBastille$longitude, adjust = 3.5, bw = type, kernel = c("epanechnikov"))
  val_a4 = approx(a4$x,a4$y,xout=x)
  b4 = density(placeBastille$latitude,adjust = 3.5, bw = type, kernel = c("epanechnikov"))
  val_b4 = approx(b4$x,b4$y,xout=y)
  
  pourc4 = min(val_a4$y, val_b4$y)
  nom4 = "Place de la Bastille"
  
  
  
  a5 = density(arcTriomphe$longitude, adjust = 3.5, bw = type, kernel = c("epanechnikov"))
  val_a5 = approx(a5$x,a5$y,xout=x)
  b5 = density(arcTriomphe$latitude,adjust = 3.5, bw = type, kernel = c("epanechnikov"))
  val_b5 = approx(b5$x,b5$y,xout=y)
  
  pourc5 = min(val_a5$y, val_b5$y)
  nom5 = "Arc de Triomphe"
  
  
  
  a6 = density(pantheon$longitude, adjust = 3.5, bw = type, kernel = c("epanechnikov"))
  val_a6 = approx(a6$x,a6$y,xout=x)
  b6 = density(pantheon$latitude,adjust = 3.5, bw = type, kernel = c("epanechnikov"))
  val_b6 = approx(b6$x,b6$y,xout=y)
  
  pourc6 = min(val_a6$y, val_b6$y)
  nom6 = "PanthÃ©on"
  
  
  #Je mets l'ensemble des densité dans un seul vecteur
  dens = c(pourc1, pourc2, pourc3, pourc4, pourc5, pourc6)
  
  # Suite à un bug de R qui notait NA une densité qui était trop petite, je corrige en notant cette densité à 0
  for(i in 1:6) {
    if(is.na(dens[i])){
      dens[i] = 0
    }
  }

  
  #Somme les densité
  somme = sum(dens)
  
  col_nom = c(nom1, nom2, nom3, nom4, nom5, nom6)
  
  # Création du vecteur qui donne la proba d'appartenance de la coordonnée à chaque monument
  proba = c(dens[1]/somme, dens[2]/somme, dens[3]/somme, dens[4]/somme, dens[5]/somme, dens[6]/somme)
  
  #Je combine l'ensemble des vecteurs dans une matrice finale
  fin = cbind(col_nom, proba)
  fin = cbind(fin,dens)

  
  # Un probleme venant de R est qu'il n'arrive pas à comparer des nombres avec des puissances de 10 et trop de chiffres significatifs quand ils sont dans un vecteur
  # La solution est donc de forcer la valeurs des nombre trop petit à 0
  for(i in 1:6) {
    if(fin[i,2] > 1){
      fin[i,2] = 0
      fin[i,3] = 0
    }
  }
  
  #Je cherche à quel monument appartient cette coordonnée en trouvant lequel a la plus haute probabilité
  max = fin[1,2]
  nom = fin[1,1]
  for(i in 2:6) {
    if(fin[i,2] > max){
      max = fin[i,2]
      nom = fin[i,1]
    }
  }
  
  #je verifie qu'il y ait au moins un monument qui a une densité supérieur à 10 sinon je considere que les densités sont trop faible et que le resultat est "non-significatif"
  compteur = 0
  for(i in 1:6){
    if(fin[i,3] > 10){
      compteur = compteur +1
    }
  }
  if(compteur == 0){
    nom = "non significatif"
  }
  
  #Je retourne le nom du lieu auquel la coordonnée appartient
  nom

}


# Vérifie la véracité du test_set_global en utilisant les données du training set pour la fonction LongLat
verif_longlat = function(){
  
  #compteur qui compte le nombre de fois où la fonction trouve le bon monument
  compteur = 0
  
  #Vecteur qui indique le lieu qui est prédit par la fonction LongLat
  lieu_predit = c()
  
  #Vecteur qui comprend deux valeurs "oui"/"non" en indiquant si le lieux prédit est identique ou non au lieu véritable
  justesse = c()
  
  #Pour chaque point dans le test set, lui applique la fonction LongLat et compare par rapport à la véracité 
  for(i in 1:nrow(test_set_global)){
    coordLon = test_set_global[i,6]
    coordLat = test_set_global[i,7]
    
    lieu = LonLat(coordLon,coordLat,"nrd0")
    lieu_predit = append(lieu_predit, lieu)
    
    if(test_set_global[i,3] == lieu){
      justesse = append(justesse, "oui")
      compteur = compteur +1
    }
    else if(test_set_global[i,3] == "non significatif"){
      justesse = append(justesse, "non significatif")
    }
    else{
      justesse = append(justesse, "non")
    }
  }
  
  #Création de la matrice finale
  fin = cbind(test_set_global[,4], type.convert(test_set_global[,3], as.is = TRUE), test_set_global[,6], test_set_global[,7], lieu_predit, justesse, compteur)
  
  fin
  
}



# Deuxieme fonction, elle calcule la probabilité d'appartenance à un lieu en utilisant la fonction kde2d (autrement dit la densité par noyau)
# "x" est la longitude | "y" est la latitude 
kernel2d = function(x,y, taille){
  
  #Je crée un point
  point = data.frame(x=x, y=y)
  
  #Calcule le model de densité par noyau
  kd1 = kde2d(teiffel$longitude, teiffel$latitude, n = taille, h = c(bw(teiffel$longitude), bw(teiffel$latitude)))
  nom1 = "Tour Eiffel"
  # interople le model ci-dessus avec le point pour obtenir la valeur de la densité en ce point
  pourc1 = interp.surface(kd1, point)
  
  
  ## Pour les autres monuments la démarche est identique
  
  kd2 = kde2d(museeLouvre$longitude, museeLouvre$latitude , n = taille, h = c(bw(museeLouvre$longitude), bw(museeLouvre$latitude)))
  pourc2 = interp.surface(kd2, point)
  nom2 = "MusÃ©e du Louvre"
  

  kd3 = kde2d(cathedral$longitude, cathedral$latitude, n = taille, h = c(bw(cathedral$longitude), bw(cathedral$latitude)))
  pourc3 = interp.surface(kd3, point)
  nom3 = "CathÃ©drale Notre-Dame de Paris"
  
  
  kd4 = kde2d(placeBastille$longitude, placeBastille$latitude, n = taille, h = c(bw(placeBastille$longitude), bw(placeBastille$latitude)))
  pourc4 = interp.surface(kd4, point)
  nom4 = "Place de la Bastille"
  
  
  kd5 = kde2d(arcTriomphe$longitude, arcTriomphe$latitude, n = taille, h = c(bw(arcTriomphe$longitude), bw(arcTriomphe$latitude)))
  pourc5 = interp.surface(kd5, point)
  nom5 = "Arc de Triomphe"
  
  
  kd6 = kde2d(pantheon$longitude, pantheon$latitude, n = taille, h = c(bw(pantheon$longitude), bw(pantheon$latitude)))
  pourc6 = interp.surface(kd6, point)
  nom6 = "PanthÃ©on"
  
  
  #Je mets l'ensemble des densité dans un seul vecteur
  dens = c(pourc1, pourc2, pourc3, pourc4, pourc5, pourc6)
  
  #Suite à un bug de R qui notait NA une densité qui était trop petite, je corrige en notant cette densité à 0
  for(i in 1:6) {
    if(is.na(dens[i])){
      dens[i] = 0
    }
  }


  #Somme les densité
  somme = sum(dens)
  
  col_nom = c(nom1, nom2, nom3, nom4, nom5, nom6)
  
  # Création du vecteur qui donne la proba d'appartenance de la coordonnée à chaque monument
  proba = c(dens[1]/somme, dens[2]/somme, dens[3]/somme, dens[4]/somme, dens[5]/somme, dens[6]/somme)
  
  #Je combine l'ensemble des vecteurs dans une matrice finale
  fin = cbind(col_nom, proba)
  fin = cbind(fin,dens)
  
  
  # Un probleme venant de R est qu'il n'arrive pas à comparer des nombres avec des puissances de 10 et trop de chiffres significatifs quand ils sont dans un vecteur
  # La solution est donc de forcer la valeurs des nombre trop petit à 0
  for(i in 1:6) {
    if(fin[i,2] > 1){
      fin[i,2] = 0
      fin[i,3] = 0
    }
  }
  
  #Je cherche à quel monument appartient cette coordonnée en trouvant lequel a la plus haute probabilité
  max = fin[1,2]
  nom = fin[1,1]
  for(i in 2:6) {
    if(fin[i,2] > max){
      max = fin[i,2]
      nom = fin[i,1]
    }
  }

  #je verifie qu'il y ait au moins un monument qui a une densité supérieur à 10 sinon je considere que les densités sont trop faible et que le resultat est "non-significatif"
  compteur = 0
  for(i in 1:6){
    if(fin[i,3] > 10){
      compteur = compteur +1
    }
  }
  if(compteur == 0){
    nom = "non significatif"
  }
  
  #Je retourne le nom du lieu auquel la coordonnée appartient
  nom
  
  
  #Pour utiliser la fonction "graph_taille_kernel" il faut commenter "nom" juste au dessus et décommenter "max" ci-dessous
  #max
}



#C'est une fonction qui permet de verifier à partir de quel taille de grille pour la fonction "kde2d" nous pouvons considerer que celle ci est a atteint sa valeur limite (en l'occurence 300 est un resultat convenable)
#ATTENTION : Pour utiliser cette fonction, il faut changer ce que retourne la fonction "kernel2d" (c'est indiqué dans la fin de la fonction la démarche à suivre)
graph_taille_kernel = function(nombre) {
  abs = c()
  ord = c()
  for(i in 1:nombre){
    abs = append(abs, i*10)
    ord = append(ord, as.double(kernel2d(coordLong, coordLat, i*10)))
  }
  fin = cbind(abs, ord)
  #fin
  plot(fin, ylim = c(0,1))
  lines(fin[,1], fin[,2], lwd = 2, ylim = c(0,1))
}
#Execute la fonction et met environs 1 minute pour le résultat
#graph_taille_kernel(50)



# Modifie le mandwidth de la fonction kernel2d pour pouvoir faire des comparaisons sur leur efficacité
bw = function(x) {
  bandwidth.nrd(x)
}





# Vérifie la véracité du test_set_global en utilisant les données du training set pour la fonction kernel2d
verif_kernel2d = function(){
  
  #compteur qui compte le nombre de fois où la fonction trouve le bon monument
  compteur = 0
  
  #Vecteur qui indique le lieu qui est prédit par la fonction LongLat
  lieu_predit = c()
  
  #Vecteur qui comprend deux valeurs "oui"/"non" en indiquant si le lieux prédit est identique ou non au lieu véritable
  justesse = c()
  
  
  #Pour chaque point dans le test set, lui applique la fonction kernel2d et compare par rapport à la véracité
  for(i in 1:nrow(test_set_global)){
    coordLon = test_set_global[i,6]
    coordLat = test_set_global[i,7]
    
    lieu = kernel2d(coordLon,coordLat,300)
    lieu_predit = append(lieu_predit, lieu)
    
    if(test_set_global[i,3] == lieu){
      justesse = append(justesse, "oui")
      compteur = compteur + 1
    }
    else if(test_set_global[i,3] == "non significatif"){
      justesse = append(justesse, "non significatif")
    }
    else{
      justesse = append(justesse, "non")
    }
  }
  
  #Création de la matrice finale
  fin = cbind(test_set_global[,4], type.convert(test_set_global[,3], as.is = TRUE), test_set_global[,6], test_set_global[,7], lieu_predit, justesse, compteur)
  
  fin
}





# Est utilisé dans la fonction "proba_cluster", permet de trouver à quelle classification appartient la point 
classi = function(coordLong, coordLat, mc) {
  dist = (mc$parameters$mean[1,1] - coordLong)^2 + (mc$parameters$mean[2,1] - coordLat)^2
  class = 1
  if(length(mc$parameters$mean[1,])>1) {
    for(k in 2:length(mc$parameters$mean[1,])){
      distTest = (mc$parameters$mean[1,k] - coordLong)^2 + (mc$parameters$mean[2,k] - coordLat)^2
      if(distTest < dist){
        class = k
      }
    }
  }
  
  class
}

# Est utilisé dans la fonction "MixtureGaussian" : Calcul la densité d'un point pour un modele multi-gaussien à l'aide de la formule pour 2 dimensions
densi_cluster = function(coordLong, coordLat, mc) {
  
  #Permet de trouver à quelle classification appartient la point 
  class = classi(coordLong, coordLat, mc)
  
  #Je recupere les données dont j'ai besoin pour calculer la densité d'un point avec le modele de multi-gaussiennes
  coords = c(coordLong, coordLat)
  moyennes = mc$parameters$mean[,class]
  cov = mc$parameters$variance$sigma[,,class]
  
  #J'applique la formule
  densi = 1/(2*pi) * 1/(det(cov)^(1/2)) * exp( (-1/2) * t(coords - moyennes) %*% solve(cov) %*% (coords - moyennes))
  
  densi
}







# Est utilisé dans la fonction "verif_MixtureGaussian"
MixtureGaussian = function(x,y, densitymc_teiffel, densitymc_louvre, densitymc_cath, densitymc_bastille, densitymc_triomphe, densitymc_pant) {
  
  nom1 = "Tour Eiffel"
  #Je recupere la densité en appliquant la fonction "proba_cluster
  pourc1 = densi_cluster(x,y, densitymc_teiffel)
  
  
  pourc2 = densi_cluster(x,y, densitymc_louvre)
  nom2 = "MusÃ©e du Louvre"
  
  
  pourc3 = densi_cluster(x,y, densitymc_cath)
  nom3 = "CathÃ©drale Notre-Dame de Paris"
  
  
  pourc4 = densi_cluster(x,y, densitymc_bastille)
  nom4 = "Place de la Bastille"
  
  
  pourc5 = densi_cluster(x,y, densitymc_triomphe)
  nom5 = "Arc de Triomphe"
  
  
  pourc6 = densi_cluster(x,y, densitymc_pant)
  nom6 = "PanthÃ©on"
  
  

  #Je mets l'ensemble des densité dans un seul vecteur
  dens = c(pourc1, pourc2, pourc3, pourc4, pourc5, pourc6)
  
  #Suite à un bug de R qui notait NA une densité qui était trop petite, je corrige en notant cette densité à 0
  for(i in 1:6) {
    if(is.na(dens[i])){
      dens[i] = 0
    }
  }
  
  
  #Somme les densités
  somme = sum(dens)
  
  col_nom = c(nom1, nom2, nom3, nom4, nom5, nom6)
  
  # Création du vecteur qui donne la proba d'appartenance de la coordonnée à chaque monument
  proba = c(dens[1]/somme, dens[2]/somme, dens[3]/somme, dens[4]/somme, dens[5]/somme, dens[6]/somme)
  
  #Je combine l'ensemble des vecteurs dans une matrice finale
  fin = cbind(col_nom, proba)
  fin = cbind(fin,dens)
  
  
  # Un probleme venant de R est qu'il n'arrive pas à comparer des nombres avec des puissances de 10 et trop de chiffres significatifs quand ils sont dans un vecteur
  # La solution est donc de forcer la valeurs des nombre trop petit à 0
  for(i in 1:6) {
    if(fin[i,2] > 1){
      fin[i,2] = 0
      fin[i,3] = 0
    }
  }
  
  #Je cherche à quel monument appartient cette coordonnée en trouvant lequel a la plus haute probabilité
  max = fin[1,2]
  nom = fin[1,1]
  for(i in 2:6) {
    if(fin[i,2] > max){
      max = fin[i,2]
      nom = fin[i,1]
    }
  }
  
  #je verifie qu'il y ait au moins un monument qui a une densité supérieur à 10 sinon je considere que les densités sont trop faible et que le resultat est "non-significatif"
  compteur = 0
  for(i in 1:6){
    if(fin[i,3] > 10){
      compteur = compteur +1
    }
  }
  if(compteur == 0){
    nom = "non significatif"
  }
  
  #Je retourne le nom du lieu auquel la coordonnée appartient
  nom
}


# Vérifie la véracité du test_set_global en utilisant les données du training set pour la fonction kernel2d
verif_MixtureGaussian = function(){
  
  # Création des modèles de mélange de gaussiennes
  densitymc_teiffel = densityMclust(cbind(teiffel[,6],teiffel[,7]))
  densitymc_louvre = densityMclust(cbind(museeLouvre[,6],museeLouvre[,7]))
  densitymc_cath = densityMclust(cbind(cathedral[,6],cathedral[,7]))
  densitymc_bastille = densityMclust(cbind(placeBastille[,6],placeBastille[,7]))
  densitymc_triomphe = densityMclust(cbind(arcTriomphe[,6],arcTriomphe[,7]))
  densitymc_pant = densityMclust(cbind(pantheon[,6],pantheon[,7]))
  
  #compteur qui compte le nombre de fois où la fonction trouve le bon monument
  compteur = 0
  
  #Vecteur qui indique le lieu qui est prédit par la fonction LongLat
  lieu_predit = c()
  
  #Vecteur qui comprend deux valeurs "oui"/"non" en indiquant si le lieux prédit est identique ou non au lieu véritable
  justesse = c()
  
  
  #Pour chaque point dans le test set, lui applique la fonction kernel2d et compare par rapport à la véracité
  for(i in 1:nrow(test_set_global)){
    coordLon = test_set_global[i,6]
    coordLat = test_set_global[i,7]
    
    lieu = MixtureGaussian(coordLon,coordLat, densitymc_teiffel, densitymc_louvre, densitymc_cath, densitymc_bastille, densitymc_triomphe, densitymc_pant)
    lieu_predit = append(lieu_predit, lieu)
    
    if(test_set_global[i,3] == lieu){
      justesse = append(justesse, "oui")
      compteur = compteur + 1
    }
    else if(test_set_global[i,3] == "non significatif"){
      justesse = append(justesse, "non significatif")
    }
    else{
      justesse = append(justesse, "non")
    }
  }
  
  fin = cbind(test_set_global[,4], type.convert(test_set_global[,3], as.is = TRUE), test_set_global[,6], test_set_global[,7], lieu_predit, justesse, compteur)
  
  fin
}




#Compte le nombre de photo dans un carré de 200m autour du point 
compterPhoto = function(long, lat){ 
  # 0.0014 équivaut à une distance de 100m à notre latitude/longitude
  distLat = 0.0014
  distLong = 0.0014
  
  # Je crée les limite externe du carré
  minLong = long - distLong
  maxLong = long + distLong
  minLat = lat - distLat
  maxLat = lat + distLat
  
  nom1 = "Tour Eiffel"
  nom2 = "MusÃ©e du Louvre"
  nom3 = "CathÃ©drale Notre-Dame de Paris"
  nom4 = "Place de la Bastille"
  nom5 = "Arc de Triomphe"
  nom6 = "PanthÃ©on"
  
  #Compteur pour compter le nombre de photo de la Tour Eiffel
  compteurEiffel = 0
  #Verifie pour chaque coordonnées du vecteur teiffel si elles sont dans le carré créée
  for(i in 1:nrow(teiffel)){
    if (teiffel$longitude[i] > minLong && teiffel$longitude[i] < maxLong && teiffel$latitude[i] > minLat && teiffel$latitude[i] < maxLat){
      compteurEiffel = compteurEiffel + 1
    }
  }
  
  
  ## Le reste est comme pour le vecteur teiffel ci-dessus
  
  compteurArc = 0
  for(i in 1:nrow(arcTriomphe)){
    if (arcTriomphe$longitude[i] > minLong && arcTriomphe$longitude[i] < maxLong && arcTriomphe$latitude[i] > minLat && arcTriomphe$latitude[i] < maxLat){
      compteurArc = compteurArc + 1
    }
  }
  
  compteurLouvre = 0
  for(i in 1:nrow(museeLouvre)){
    if (museeLouvre$longitude[i] > minLong && museeLouvre$longitude[i] < maxLong && museeLouvre$latitude[i] > minLat && museeLouvre$latitude[i] < maxLat){
      compteurLouvre = compteurLouvre + 1
    }
  }
  
  compteurCath = 0
  for(i in 1:nrow(cathedral)){
    if (cathedral$longitude[i] > minLong && cathedral$longitude[i] < maxLong && cathedral$latitude[i] > minLat && cathedral$latitude[i] < maxLat){
      compteurCath = compteurCath + 1
    }
  }
  
  compteurBastille = 0
  for(i in 1:nrow(placeBastille)){
    if (placeBastille$longitude[i] > minLong && placeBastille$longitude[i] < maxLong && placeBastille$latitude[i] > minLat && placeBastille$latitude[i] < maxLat){
      compteurBastille = compteurBastille + 1
    }
  }
  
  compteurPantheon = 0
  for(i in 1:nrow(pantheon)){
    if (pantheon$longitude[i] > minLong && pantheon$longitude[i] < maxLong && pantheon$latitude[i] > minLat && pantheon$latitude[i] < maxLat){
      compteurPantheon = compteurPantheon + 1
    }
  }
  
  col_nom = c(nom1, nom2, nom3, nom4, nom5, nom6)
  colCompteur = c(compteurEiffel, compteurLouvre, compteurCath, compteurBastille, compteurArc, compteurPantheon)
  fin = cbind(col_nom, colCompteur)
  fin
  
}





#Prend en entrée une des matrices des fonction "verif_ ..."  et compte le ratio de justesse des differentes fonction en fonction des monuments
justesse_par_lieu = function(data) {
  nom1 = "Tour Eiffel"
  nom2 = "MusÃ©e du Louvre"
  nom3 = "CathÃ©drale Notre-Dame de Paris"
  nom4 = "Place de la Bastille"
  nom5 = "Arc de Triomphe"
  nom6 = "PanthÃ©on"
  
  col_nom = c(nom1, nom2, nom3, nom4, nom5, nom6)
  juste = c(0,0,0,0,0,0)
  nombre = c(0,0,0,0,0,0)
  #separateur = c("#","#","#","#","#","#")  #permet d'avoir un séparateur entre les données pour un export vers excel
  
  
  for(i in 1:nrow(data)){
    for(j in 1:6){
      if(data[i,2] == col_nom[j]){
        if(data[i,6] == "oui") {
          #compte le nombre d'occurence de chaque monument qui a été correctement étiqueté dans data
          juste[j] = juste[j] + 1
        }
        #compte le nombre d'occurence de chaque monument dans data
        nombre[j] = nombre[j] + 1
      }
    }
    
  }
  ratio = juste/nombre
  #fin = cbind(col_nom, separateur, juste, separateur , nombre, separateur , ratio)  #permet d'avoir un séparateur entre les données pour un export vers excel
  fin = cbind(col_nom, juste , nombre , ratio)
  fin
}


# Affiche la densité en 3d
affiche_3d = function(data){
  
  # n est la taille de la grille de découpe
  kd <- with(data, MASS::kde2d(longitude, latitude, n = 500))
  
  #Pour chaque donnée je mets un seuil minimum de densité (0.01 ici) car sinon l'echelle logarithme va comparer les puissances de 10 négatives entre elles et le graphique n'aura pas réellement de sens
  for(i in 1:500) {
    for(j in 1:500) {
      if(kd$z[i,j]<0.01) {
        kd$z[i,j] = 0.01
      }
    }
  }
  
  
  # On utulise la fonction plotly pour afficher en 3d
  fig <- plot_ly(x = kd$x, y = kd$y, z = kd$z) %>% add_surface(showscale=FALSE)
  fig <- fig %>% layout(scene = list( zaxis = list(type = "log", title = "Densité"), yaxis = list(title = "Latitude"), xaxis = list(title = "Longitude")))
  
  fig
}



#Effectuer l'une des deux commandes ci-dessous, en fonction de si on veut des coordonnées qui ont tous le meme poids, où si l'on veut qu'elles soient pondérées 
#Environs 2 minutes
separation_training_test()
#separation_training_test_unique()




#Execute les trois fonction sur le test_set (environs 10-15 minutes par )
matrice_LonLat = verif_longlat()
matrice_kernel2d = verif_kernel2d()
matrice_MixtureGaussian = verif_MixtureGaussian()


#entre une latitude et une longitude
coordLat = 48.54309
coordLong = 2.653724


# Execute les fonctions
compterPhoto(coordLong,coordLat)

justesse_par_lieu(matrice_LonLat)

affiche_3d(pantheon)

