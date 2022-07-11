####################################################
# Clusteranalyse Projektseminar II
###################

# Pauline Burkart, Cedric Engberg, Alisa Naumann

setwd("/Users/alisanaumann/Desktop/R")
getwd()

library(readxl)
library(openxlsx)
Jugendstudie <- read_xlsx("js_data_updated.xlsx")


library('ggplot2')
library('factoextra')
library('summarytools')

## Alle interessanten Variablen
CA <-Jugendstudie[,c("ID",
                     "sex_wm",
                     "school",
                     "gms", "hs", "rs", "gym", "sbbz","school_full",
                     "birthcountry_out",
                     "migra",
                     "job_1","job_2",
                     "engagement",                                    # Ehrenamt ja/nein
                     "enga.1","enga.2","enga.3","enga.4","enga.5",
                     "enga.6","enga.7","enga.8","enga.9","enga.10",
                     "enga_barrier",                                  # Barrieren für Ehrenamt ja/nein
                     "enga_bar.1","enga_bar.2","enga_bar.3","enga_bar.4","enga_bar.5","enga_bar.6",
                     "asp_edu_1",                                     # Höchster Bildungswunsch (ordinal absteigend 1 bis 4 - weiß nicht ist 7, etwas anderes 5)
                     "asp_edu_2",                                     # Was am liebsten nach jetziger Schule machen? (nominal)
                     "asp_edu_3",
                     "workfield",                                     # Berufsfelder nominal
                     "workfield.1", "workfield.2", "workfield.3", "workfield.4", "workfield.5",
                     "workfield.6", "workfield.7", "workfield.8", "workfield.9", "workfield.10",
                     "workfield.11", "workfield.12", "workfield.13", "workfield.14", "workfield.15",
                     "workfield.16", "workfield.17", "workfield.18",
                     "career_talk",                                   # Sprechen über Berufswahl (Kontakte) nominal
                     "career_talk.1", "career_talk.2", "career_talk.3", "career_talk.4", "career_talk.5",
                     "career_talk.6", "career_talk.7", "career_talk.8",
                     "worry_job", "worry_job_rec", "worry_job_dicho",  # Sorgen um Arbeitsplatz Skala 1-5 (nicht-sehr)
                     "internet",
                     "well_1","well_2","well_3",
                     "efficacy_1","efficacy_2","efficacy_3","efficacy_4",
                     "anxiety","anxiety_dicho",
                     "mentalhealth",
                     "mentalhealth_dicho")]
variable.names(CA)
str(CA)
summary(CA)
CA$ID <- as.character(CA$ID)

CA$school_full <- as.numeric(CA$school_full)
CA$enga_barrier <- as.numeric(CA$enga_barrier)
CA$asp_edu_1 <- as.numeric(CA$asp_edu_1)
CA$asp_edu_2 <- as.numeric(CA$asp_edu_2)
CA$asp_edu_3 <- as.numeric(CA$asp_edu_3)
CA$worry_job_dicho <- as.numeric(CA$worry_job_dicho)
CA$internet <- as.numeric(CA$internet)
CA$well_3 <- as.numeric(CA$well_3)
CA$efficacy_1 <- as.numeric(CA$efficacy_1)
CA$efficacy_2 <- as.numeric(CA$efficacy_2)
CA$efficacy_3 <- as.numeric(CA$efficacy_3)
CA$efficacy_4 <- as.numeric(CA$efficacy_4)
CA$anxiety_dicho <- as.numeric(CA$anxiety_dicho)
CA$mentalhealth_dicho <- as.numeric(CA$mentalhealth_dicho)

str(CA)

### Umcodieren ###

## Alter gibts nicht?
library(car)             # umcodieren von Variablen

CA$school_full <- recode(CA$school_full, "2=0")
CA$job_1 <- recode(CA$job_1, "2=0")
CA$job_2 <- recode(CA$job_2, "2=0")
CA$engagement <- recode(CA$engagement, "2=0")
CA$enga_barrier <- recode(CA$enga_barrier, "2=0")
CA$ZielStudium <- recode(CA$asp_edu_1, "2:7=0")
CA$ZielAbi <- recode(CA$asp_edu_1, "1=0;2=1;3:7=0")
CA$ZielReal <- recode(CA$asp_edu_1, "1:2=0;3=1;4:7=0")
CA$ZielHaupt <- recode(CA$asp_edu_1, "1:3=0;4=1;5:7=0")
CA$ZielAndere <- recode(CA$asp_edu_1, "1:4=0;5=1;6:7=0")
CA$ZielWeißNicht <- recode(CA$asp_edu_1, "1:5=0;6=1")
CA$Lehre <- recode(CA$asp_edu_2, "2:13=0")
CA$studieren <- recode(CA$asp_edu_2, "1=0;2=1;3:13=0")
CA$arbeiten <- recode(CA$asp_edu_2, "1:2=0;3=1;4:13=0")
CA$weiterfSchule <- recode(CA$asp_edu_2, "1:3=0;4=1;5:13=0")
CA$Praktikum <- recode(CA$asp_edu_2, "1:4=0;5=1;6:13=0")
CA$jobben <- recode(CA$asp_edu_2, "1:5=0;6=1;7:13=0")
CA$BFD.FSJde <- recode(CA$asp_edu_2, "1:6=0;7=1;8:13=0")
CA$BFD.FSJaus <- recode(CA$asp_edu_2, "1:7=0;8=1;9:13=0")
CA$Ausland <- recode(CA$asp_edu_2, "1:8=0;7:9=1;10:13=0")
CA$Auslandgrob <- recode(CA$asp_edu_2, "1:9=0;10=1;11:13=0")
CA$Bundeswehr <- recode(CA$asp_edu_2, "1:10=0;11=1;12:13=0")
CA$Anderes <- recode(CA$asp_edu_2, "1:11=0;12=1;13=0")
CA$WeißNicht <- recode(CA$asp_edu_2, "1:12=0;13=1")
CA$well_1 <- recode(CA$well_1, "2=0")
CA$well_2 <- recode(CA$well_2, "2=0")
CA$well_3 <- recode(CA$well_3, "2=0")

summary(CA)

# Mediansplit Selbstwirksamkeit, fester Berufswunsch, Internetnutzung Jobsuche
CA$efficacy_3 <- recode(CA$efficacy_3, "1=5;2=4;4=2;5=1")
CA$efficacy_4 <- recode(CA$efficacy_4, "1=5;2=4;4=2;5=1")
CA$efficacy <- rowMeans(subset(CA, select = c(efficacy_1, efficacy_2, efficacy_3, efficacy_4)))
summary(CA$efficacy) # Median 3.75
CA$efficacy <- recode(CA$efficacy, "lo:3.75=0;3.76:hi=1")
summary(CA)

CA$asp_edu_3 <- recode(CA$asp_edu_3, "lo:3=0;4:hi=1")

CA$internet <- recode(CA$internet, "lo:4=0;5:hi=1")
summary(CA)

# Benennung workfields

CA$Technik <- CA$workfield.1
CA$Handwerk <- CA$workfield.2
CA$Verkehr <- CA$workfield.3
CA$Soziales <- CA$workfield.4
CA$IT <- CA$workfield.5
CA$Kunst <- CA$workfield.6
CA$Verkauf <- CA$workfield.7
CA$Maschinenbau <- CA$workfield.8
CA$Gesundheit <- CA$workfield.9
CA$Koerperpflege <- CA$workfield.10
CA$Natur.LW <- CA$workfield.11
CA$Bau <- CA$workfield.12
CA$Verwaltung <- CA$workfield.13
CA$Medien <- CA$workfield.14
CA$Produktion <- CA$workfield.15
CA$Recht <- CA$workfield.16
CA$Sicherheit <- CA$workfield.17
CA$Sport <- CA$workfield.18


##### Clusteranalyse 1 ## Clustering der Befragten nach allen für uns interessanten Variablen

CA1 <-CA[,c(#"school",      # Schule nominal
            "ID",
            "sex_wm",                                                # Geschlecht
            "gms", "hs", "rs", "gym", "sbbz",                        # Schultyp
                     "school_full",                                  # Schule Anwesenheit (mind. 3 Tage/Woche 7h/Tag) nein/ja
                     "birthcountry_out",                             # In Dt. geboren nein/ja
                     "migra",                                        # Migrationshintergrund nein/ja
                     "job_1",                                        # Nebenjob nein/ja
                     "job_2",                                        # Ferienarbeit nein/ja
                     "engagement",                                   # Ehrenamt nein/ja
                     "enga_barrier",                                 # Barrieren für Ehrenamt nein/ja
                     "ZielStudium","ZielAbi","ZielReal","ZielHaupt","ZielAndere","ZielWeißNicht", # Bildungsaspirationen
                     "Lehre","studieren","arbeiten","weiterfSchule","Praktikum","jobben",
                     "BFD.FSJde","BFD.FSJaus","Ausland","Auslandgrob","Bundeswehr","Anderes","WeißNicht",
                     "asp_edu_3",                                    # Fester Berufswunsch (Mediansplit)
                     "Technik", "Handwerk", "Verkehr", "Soziales", "IT",
                     "Kunst", "Verkauf", "Maschinenbau", "Gesundheit", "Koerperpflege",
                     "Natur.LW", "Bau", "Verwaltung", "Medien", "Produktion",
                     "Recht", "Sicherheit", "Sport",
                     "worry_job_dicho",                              # Sorge Job nein/ja
                     "internet",                                     # Internet Informationssuche (mediansplit)
                     "well_1",                                       # wohlfühlen in Klasse nein/ja
                     "well_2",                                       # wohlfühlen in Schule nein/ja
                     "well_3",                                       # genug Freundinnen / Freunde nein/ja
                     "efficacy",                                     # Selbstwirksamkeit (Mediansplit)
                     "anxiety_dicho",                                # Sorgen in der letzten Woche nein/ja
                     "mentalhealth_dicho")]                          # depressive Symptome - nein/ja

summary(CA1)


write.xlsx(CA1, "Clusteranalyse1.xlsx")

#### Durchfuehrung ##################################
# NAs entfernen
CA1noNA <- na.omit(CA1)
summary(CA1)
summary(CA1noNA) ### von 2160 fällen nurnoch 1642 Fälle

# Skalieren - Standardisierte Werte (Z-Transformation) erzeugen für Vergleichbarkeit
CAv1 <- scale(CA1noNA[2:60])
CAv1

# Ellbogenkriterium / Silhouette
fviz_nbclust(CAv1, hcut, method = "wss", k.max = 30)
  geom_vline(xintercept = 5, linetype = 2)+
  labs(subtitle = "Elbow method")
# Einziger erkennbare Knick am ehesten bei 2, ab 17 wird die Kurve flacher

fviz_nbclust(CAv1, hcut, method = "silhouette", k.max = 30)+
  labs(subtitle = "Silhouette method")
# 1. Peak bei 6 Clustern, aber Höhepunkt bei 17

# Distanzmatrix
d1 <- dist(CAv1, method = 'euclidean')
d1ausg <- head(as.matrix(d1))

# Dendrogramme erstellen
HC <- hclust(d1, method="complete")
#fviz_dend(HC)
#fviz_dend(HC,2)  # Cluster 1 und 2 unterscheiden sich tendenziell in Bildung und Berufswunsch
#fviz_dend(HC,6)  # erster Peak - Macht sich eine Clusterung durch die Schultypen bemerkbar?
#fviz_dend(HC,17) # eventuell erst bei 17 Clustern ein Großteil der Befragten genauer eingruppiert, sonst viele Extrem-/Sonderfälle?

## Ableiten von Loesungen fuer die Cluster
# Loesung 2 Cluster
cluster2 <- cutree(HC, k = 2) 
head(cluster2)
CA1noNA$cluster2 <- cluster2
summary(CA1noNA)

# Loesung 6 Cluster
cluster6 <- cutree(HC, k = 6) 
head(cluster6)
CA1noNA$cluster6 <- cluster6
summary(CA1noNA)

# Loesung 17 Cluster
cluster17 <- cutree(HC, k = 17) 
head(cluster17)
CA1noNA$cluster17 <- cluster17
summary(CA1noNA)


#### Vergleich der Clustereigenschaften anhand der verwendeten Variablen
# Ein Mittelwertvergleich der jeweiligen Cluster und ihrer Auspraegungen der Variablen + Gegenueberstellung mit transpose(t) 

# 2 Cluster: 
cluster.descr2 <- aggregate(CA1noNA[,2:60], by=list(cluster=CA1noNA$cluster2), mean)
cluster.descr2
Mean2Cluster <- t(cluster.descr2)
write.xlsx(Mean2Cluster, "Mean2Cluster.xlsx")

freq(CA1noNA$cluster2) # Cluster 2 kommt nur 29mal vor (1.77%), C1 1613 mal -> Mainstream?
                       # Die Frage wie es nach der Schule weiter geht hat sehr wenig Aussagekraft (Bildungsaspirationen 2)
                       # Cluster 2 hat einen bedeutend größeren Anteil an Hauptschülern, streben Real oder Studium an, häufiger fester Berufswunsch,
                       # auffaellig: Berufsbild Produktion/Fertigung sehr stark, stärkere Jobsorgen, aber höhere Selbstwirksamkeit

# 6 Cluster:
cluster.descr6 <- aggregate(CA1noNA[,2:60], by=list(cluster=CA1noNA$cluster6), mean)
cluster.descr6
Mean6Cluster <- t(cluster.descr6)
write.xlsx(Mean6Cluster, "Mean6Cluster.xlsx")

freq(CA1noNA$cluster6) # Häufigstes Cluster: 1 (1510 Faelle) -> "Mainstream"?

                       # Cluster 2 (51): eher männlich, Bildung sehr durchmischt, stärkerer Migrationshintergrund,
                       # streben vor allem nach Realschulabschluss, alle wollen ein Praktikum nach der Schule, Technik/Soziales/Gesundheit/Verwaltung

                       # Cluster 3 (26): ueberwiegend Gymnasiasten + wenige Realschüler, kaum Migrationshintergrund, jobben und engagieren sich mehr, Hälfte hat festen Berufswunsch,
                       # streben vor allem Studiengänge/Realschulabschl. an, alle wollen ein BFD in DE nach der Schule, ueberwiegend sozialer Beruf gewünscht, kaum Internetnutzung Jobsuche

                       # Cluster 4 (24): entspricht sehr dem Cluster 2 bei der 2-Clusterloesung (Hauptschueler, streben Real/Studium an, alle Produktion, hohe Jobsorgen, hohe Selbstwirksamkeit)

                       # Cluster 5 (26): ueberwiegend Gymnasiasten/Gesamtschueler, Migrationshintergrund (54%), stärkeres ehrenamtliches Engagement, wenig jobben/Praktikum, Ziel vor allem Studium,
                       # auch Abi, alle wollen ein BFD im Ausland, wenigste haben festen Berufswunsch, Hauptinteressen: soziales, IT, Gesundheit

                       # Cluster 6 (5!!!): alle weiblich, Gesamt-/Realschule (eine HS), alle Migrationshintergrund, keine neben/Ferienjobs, kaum Engagement, "andere" Ziele beim Abschluss,
                       # Nach Schule auslandsaufenthalt/weiterführende Schule/anderes, Alle Interesse Bereich Körperpflege/Beauty + ein anderes, keine Selbstwirksamkeit, dafür Sorgen & hohe Depressionswerte

# 17 Cluster:
cluster.descr17 <- aggregate(CA1noNA[,2:60], by=list(cluster=CA1noNA$cluster17), mean)
cluster.descr17
Mean17Cluster <- t(cluster.descr17)
write.xlsx(Mean17Cluster, "Mean17Cluster.xlsx")

freq(CA1noNA$cluster17) # erst bei der 17-Cluster-Lösung verteilen sich die Faelle besser
                        # Allerdings hat Cluster 16 nur 5 Faelle, das größte Cluster (2) hat 708 Faelle

                        # Cluster 1 (40): 

                        # ... ... ...

# Wenn es nur 1 Cluster gäbe (Variablenmittelwerte)

cluster1 <- cutree(HC, k = 1) 
head(cluster1)
CA1noNA$cluster1 <- cluster1
summary(CA1noNA)

cluster.descr1 <- aggregate(CA1noNA[,2:60], by=list(cluster=CA1noNA$cluster1), mean)
cluster.descr1
Mean1Cluster <- t(cluster.descr1)
write.xlsx(Mean1Cluster, "Mean1Cluster.xlsx")

freq(CA1noNA$cluster1) 

###### Wir bleiben vorerst bei der 17 Cluster Lösung, eventuell Variablen reduzieren

#### Clusteranalyse 2 und 3 ####

# Skalieren - Standardisierte Werte (Z-Transformation) erzeugen nur für Schultypen, Bildungsaspirationen (1-3) und Jobsorgen (ohne Geschlecht, etc.)
CAv2 <- scale(CA1noNA[,c(3:7, 15:53)])
CAv2

# Ellbogenkriterium / Silhouette
fviz_nbclust(CAv2, hcut, method = "wss", k.max = 30)
geom_vline(xintercept = 5, linetype = 2)+
  labs(subtitle = "Elbow method")
# unterscheidet sich kaum von CA1

fviz_nbclust(CAv2, hcut, method = "silhouette", k.max = 30)+
  labs(subtitle = "Silhouette method")

# Distanzmatrix
d2 <- dist(CAv2, method = 'euclidean')
d2ausg <- head(as.matrix(d2))

# Dendrogramme erstellen
HC2 <- hclust(d2, method="complete")
#fviz_dend(HC2, 27)

# Skalieren - Standardisierte Werte (Z-Transformation) erzeugen nur für Schultypen, Bildungsaspirationen (1-3) und Jobsorgen (mit Geschlecht)
CAv3 <- scale(CA1noNA[,c(2:7, 15:53)])
CAv3

# Ellbogenkriterium / Silhouette
fviz_nbclust(CAv3, hcut, method = "wss", k.max = 30)
geom_vline(xintercept = 5, linetype = 2)+
  labs(subtitle = "Elbow method")
# unterscheidet sich kaum von CA1

fviz_nbclust(CAv3, hcut, method = "silhouette", k.max = 30)+
  labs(subtitle = "Silhouette method")

# Distanzmatrix
d3 <- dist(CAv3, method = 'euclidean')
d3ausg <- head(as.matrix(d3))

# Dendrogramme erstellen
HC3 <- hclust(d3, method="complete")
#fviz_dend(HC3, 18)

########### Schlussfolgerungen aus den Versuchen mit weniger Variablen:
### CA 2:
# Höhepunkt der Silhouette-Methode bei 27 Clustern
### CA 3:
# Höhepunkt bei Silhouette-Methode bei 18 Clustern.

## Vermutung: Die Schultypen, Bildungs- und Berufsfeldwünsche splitten die SchülerInnen in sehr viele Gruppen auf,
#             die sich wiederum vor allem durch die Variable Geschlecht, aber auch durch die anderen Variablen stärker zusammenfassen lassen.
#             Angesichts dieser Ergebnisse ist eine Clusterung über die Bildungsaspirationen hinaus, um einzelne Gruppen besser charakterisieren zu können, wichtig.
#             Das Geschlecht scheint viele der Cluster zu erklären!


