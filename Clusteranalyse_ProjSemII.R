###################
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


## Alle interessanten Variablen
CA <-Jugendstudie[,c("ID",
                     "sex_wm",
                     "school",
                     "gms", "hs", "rs", "gym", "sbbz","school_full",
                     "birthcountry_out",
                     "migra",
                     "job_1","job_2",
                     "engagement",                                   # Ehrenamt ja/nein
                     "enga.1","enga.2","enga.3","enga.4","enga.5",
                     "enga.6","enga.7","enga.8","enga.9","enga.10",
                     "enga_barrier",                                  # Barrieren für Ehrenamt ja/nein
                     "enga_bar.1","enga_bar.2","enga_bar.3","enga_bar.4","enga_bar.5","enga_bar.6",
                     "asp_edu_1",                                     # Höchster Bildungswunsch (ordinal absteigend 1 bis 4 - weiß nicht ist 7, etwas anderes 5)
                     "asp_edu_2",                                      # Was am liebsten nach jetziger Schule machen? (nominal)
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

### Clusteranalyse 1 - Schultypen
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
CA$ZielAndere <- recode(CA$asp_edu_1, "1:4=0;5=1;7=0")
CA$ZielWeißNicht <- recode(CA$asp_edu_1, "1:5=0;7=1")
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

# Mediansplit Selbstwirksamkeit
CA$efficacy_3 <- recode(CA$efficacy_3, "1=5;2=4;4=2;5=1")
CA$efficacy_4 <- recode(CA$efficacy_4, "1=5;2=4;4=2;5=1")
CA$efficacy <- rowMeans(subset(CA, select = c(efficacy_1, efficacy_2, efficacy_3, efficacy_4)))
summary(CA$efficacy) # Median 3.75
CA$efficacy <- recode(CA$efficacy, "lo:3.75=0;3.76:hi=1")
summary(CA$efficacy)

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
                     "asp_edu_3",                                    # Fester Berufswunsch Skala 1-5 (nicht-sehr)
                     "Technik", "Handwerk", "Verkehr", "Soziales", "IT",
                     "Kunst", "Verkauf", "Maschinenbau", "Gesundheit", "Koerperpflege",
                     "Natur.LW", "Bau", "Verwaltung", "Medien", "Produktion",
                     "Recht", "Sicherheit", "Sport",
                     "worry_job_rec",                                # Sorgen um Arbeitsplatz Skala 1-5 (nicht-sehr)
                     "worry_job_dicho",                              # Sorge Job niedrig-hoch
                     "internet",                                     # Internet Informationssuche nicht-sehr wichtig
                     "well_1",                                       # wohlfühlen in Klasse nein/ja
                     "well_2",                                       # wohlfühlen in Schule nein/ja
                     "well_3",                                       # genug Freundinnen / Freunde nein/ja
                     "efficacy",                                     # Selbstwirksamkeit (Mediansplit) niedrig/hoch
                     "anxiety_dicho",                                # Sorgen in der letzten Woche nein/ja
                     "mentalhealth")]                                # 1-6 (Skala depressiver Symptome - niedrig-hoch)

summary(CA1)

write.xlsx(CA1, "Clusteranalyse1.xlsx")

#### Tabelle mit Mittelwerten der Schulgruppen ### Bitte ignorieren ##############################
# brauchen wir gerade nicht, da wir nach Befragten Clustern
CAmeans1 <- aggregate(cbind(sex_wm,
                           school_full,
                           birthcountry_out,
                           migra,
                           job_1,
                           job_2,
                           engagement,
                           enga_barrier,
                           ZielStudium,
                           ZielAbi,
                           ZielReal,
                           ZielHaupt,
                           ZielAndere,
                           ZielWeißNicht,
                           Lehre,
                           studieren,
                           arbeiten,
                           weiterfSchule,
                           Praktikum,
                           jobben,
                           BFD.FSJde,
                           BFD.FSJaus,
                           Ausland,
                           Auslandgrob,
                           Bundeswehr,
                           Anderes,
                           WeißNicht,
                           asp_edu_3,
                           Technik, Handwerk, Verkehr, Soziales, IT, Kunst,
                           Verkauf, Maschinenbau, Gesundheit, Koerperpflege,
                           Natur.LW, Bau, Verwaltung, Medien, Produktion,
                           Recht, Sicherheit, Sport,
                           worry_job_rec,
                           worry_job_dicho,
                           internet,
                           well_1,
                           well_2,
                           well_3,
                           efficacy,
                           anxiety_dicho,
                           mentalhealth,
                           mentalhealth_dicho) ~ school, CA1, mean)

#### Durchfuehrung ##################################
# NAs entfernen
CA1noNA <- na.omit(CA1)
summary(CA1)
summary(CA1noNA) ### von 2160 fällen nurnoch 1642 Fälle

# Skalieren - Standardisierte Werte (Z-Transformation) erzeugen für Vergleichbarkeit
CAv1 <- scale(CA1noNA[2:61])
CAv1

# Ellbogenkriterium / Silhouette
fviz_nbclust(CAv1, hcut, method = "wss")
  geom_vline(xintercept = 5, linetype = 2)+
  labs(subtitle = "Elbow method")
# Kein eindeutiger "Knick" erkennbar -> 2/3?

fviz_nbclust(CAv1, hcut, method = "silhouette")+
  labs(subtitle = "Silhouette method")
# Maximum eindeutig bei 2 Clustern

# Distanzmatrix
d1 <- dist(CAv1, method = 'euclidean')
d1ausg <- head(as.matrix(d1))

# Dendrogramme erstellen
HC <- hclust(d1, method="complete")
fviz_dend(HC)
#fviz_dend(HC,2) 
#fviz_dend(HC,3)
#fviz_dend(HC,5) # da wir 5 Schultypen haben und wir schauen wollen ob sich die wesentlichen Unterschiede darin zeigen?
#fviz_dend(HC,15) # eventuell erst ab 20 Clustern eine 15 Cluster wirklich viele Gruppen genauer benannt?
#fviz_dend(HC,20)

## Ableiten der Loesung fuer die Cluster
# Loesung 2 Cluster
cluster2 <- cutree(HC, k = 2) 
head(cluster2)
CA1noNA$cluster2 <- cluster2
#CA1$ID <- row.names(CAv1)
summary(CA1noNA)

# Loesung 5 Cluster
cluster5 <- cutree(HC, k = 5) 
head(cluster5)
CA1noNA$cluster5 <- cluster5
#CA1$ <- row.names(CAv1)
summary(CA1noNA)

#### Vergleich der Clustereigenschaften anhand der verwendeten Variablen
# Ein Mittelwertvergleich der jeweiligen Cluster und ihrer Auspraegungen der Variablen + Gegenueberstellung mit transpose(t) 

# 2 Cluster: 
cluster.descr2 <- aggregate(CA1noNA[,2:61], by=list(cluster=CA1noNA$cluster2), mean)
cluster.descr2
t(cluster.descr2)

#10 Cluster
cluster.descr10 <- aggregate(CA1noNA[,2:61], by=list(cluster=CA1noNA$cluster10), mean)
cluster.descr10
t(cluster.descr10)








######################################## Code USL Data Science letztes Semester
#### Cluster visualisieren und Vergleiche

# 2 Cluster:
ggplot(CAmeans, aes(x=EinkommenHEB, y = Wohnraumgroesse, color = factor(cluster2))) +   
  geom_point() + # zumeist nur geom_point genutzt - Unterschiede am ?bersichtlichsten
  geom_text(aes(label= BerufeHEB), size = 2, hjust = 0, nudge_x = 0.05) 