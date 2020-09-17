library(googlesheets)
library(Hmsc)
library(tidyverse)
library(corrplot)
library(ape)

#read in data####
MA_sheet<-gs_title(x = "SUPs_traits")
traits <- gs_read(ss = MA_sheet, ws=3)

traits <- traits %>%  filter(complete.cases(Mass_kg, Migration_km, meanmedian_km, hr.radius))

#make fake phylogeny### change this later
ns = nrow(traits)
phy = rcoal(n=ns, tip.label = sprintf('species_%.3d',1:ns), br = "coalescent") 
plot(phy, show.tip.label = FALSE, no.margin = TRUE)


phy <- read.tree("./data/TreeforSUPS.tree")
phy$tip.label <- gsub("_"," " , phy$tip.label  ,ignore.case = TRUE)


match(traits$scientific_name, phy$tip.label)
traits$scientific_name[is.na(match(traits$scientific_name, phy$tip.label))]
phy$tip.label[phy$tip.label == "Vulpes vulpes"] <- "Vulpes fulva"
phy$tip.label[phy$tip.label == "Rana sylvatica"] <- "Lithobates sylvaticus"
phy$tip.label[phy$tip.label == "Otospermophilus beecheyi"] <- "Spermophilus beecheyi"
phy$tip.label[phy$tip.label == "Urocitellus columbianus"] <- "Spermophilus columbianus"
phy$tip.label[phy$tip.label == "Antigone canadensis"] <- "Grus canadensis"

phy <- drop.tip(phy, tip = phy$tip.label[is.na(match(phy$tip.label, traits$scientific_name))])
phy.dist <- cophenetic.phylo(phy)
plot(phy)
phy.dist

#specify HMSC model####
Y <- data.matrix(traits %>% 
                   dplyr::select(Migration_km, meanmedian_km, hr.radius) %>% 
                   mutate(Migration_ln = log(Migration_km+1),
                          Dispersal_ln = log(meanmedian_km+1),
                          Foraging_ln = log(hr.radius+1)) %>% 
                   dplyr::select(Migration_ln, Dispersal_ln, Foraging_ln))

XData <- data.frame(Mass_kg_ln = log(traits$Mass_kg))

studyDesign <- data.frame(species = factor(traits$scientific_name), class = factor(traits$class), phylo = factor(traits$scientific_name))

rownames(phy.dist) <- factor(traits$scientific_name[match(phy$tip.label, traits$scientific_name)])
colnames(phy.dist) <- factor(traits$scientific_name[match(phy$tip.label, traits$scientific_name)])

rL_class <- HmscRandomLevel(units = studyDesign$class)
rL_species <- HmscRandomLevel(units = studyDesign$scientific_name)
rL_phylo <-HmscRandomLevel(distMat = phy.dist)

m <- Hmsc(Y = Y, 
          XFormula = Y ~ Mass_kg_ln, 
          XData = XData, 
          studyDesign = studyDesign, 
          ranLevels = list(phylo = rL_phylo),
          distr = "normal")

m.no.phy <- Hmsc(Y = Y, 
          XFormula = Y ~ Mass_kg_ln, 
          XData = XData, 
          studyDesign = studyDesign, 
          ranLevels = list(species = rL_species),
          distr = "normal")


m.class <- Hmsc(Y = Y, 
                 XFormula = Y ~ Mass_kg_ln, 
                 XData = XData, 
                 studyDesign = studyDesign, 
                 ranLevels = list(species = rL_species, class = rL_class),
                 distr = "normal")

m.no.body <- Hmsc(Y = Y, 
                XFormula = Y ~ 1, 
                XData = XData, 
                studyDesign = studyDesign, 
                ranLevels = list(phylo = rL_phylo),
                distr = "normal")

m.no.body.no.phy <- Hmsc(Y = Y, 
                  XFormula = Y ~ 1, 
                  XData = XData, 
                  studyDesign = studyDesign, 
                  ranLevels = list(species = rL_species),
                  distr = "normal")


#run hmsc####
m <- sampleMcmc(m, thin = 1, samples = 500, transient = 500, verbose = 100, nChains = 2)
m.no.phy <- sampleMcmc(m.no.phy, thin = 1, samples = 500, transient = 500, verbose = 100, nChains = 2)
m.class <- sampleMcmc(m.class, thin = 1, samples = 500, transient = 500, verbose = 100, nChains = 2)
m.no.body <- sampleMcmc(m.no.body, thin = 1, samples = 500, transient = 500, verbose = 100, nChains = 2)
m.no.body.no.phy <- sampleMcmc(m.no.body.no.phy, thin = 1, samples = 500, transient = 500, verbose = 100, nChains = 2)


#look at model outputs####
mpost = convertToCodaObject(m)
summary(mpost$Beta)

plot(mpost$Beta)

preds = computePredictedValues(m)
MF = evaluateModelFit(hM = m, predY = preds)
MF

preds.no.phy = computePredictedValues(m.no.phy)
MF.no.phy = evaluateModelFit(hM = m.no.phy, predY = preds.no.phy)
MF.no.phy

preds.class = computePredictedValues(m.class)
MF.class = evaluateModelFit(hM = m.class, predY = preds.class)
MF.class

preds.no.body = computePredictedValues(m.no.body)
MF.no.body = evaluateModelFit(hM = m.no.body, predY = preds.no.body)
MF.no.body

preds.no.body.no.phy = computePredictedValues(m.no.body.no.phy)
MF.no.body.no.phy = evaluateModelFit(hM = m.no.body.no.phy, predY = preds.no.body.no.phy)
MF.no.body.no.phy$R2

partition <- createPartition(hM = m, nfolds = 4,column = "species")
preds_part = computePredictedValues(m, partition = partition)
MF_pred = evaluateModelFit(hM = m, predY = preds_part)

partition.no.phy <- createPartition(hM = m.no.phy, nfolds = 4,column = "species")
preds_part.no.phy = computePredictedValues(m.no.phy, partition = partition.no.phy)
MF_pred.no.phy = evaluateModelFit(hM = m.no.phy, predY = preds_part.no.phy)

partition.class <- createPartition(hM = m.class, nfolds = 4,column = "species")
preds_part.class = computePredictedValues(m.class, partition = partition.class)
MF_pred.class = evaluateModelFit(hM = m.class, predY = preds_part.class)

partition.no.body <- createPartition(hM = m.no.body, nfolds = 4,column = "species")
preds_part.no.body = computePredictedValues(m.no.body, partition = partition.no.body)
MF_pred.no.body = evaluateModelFit(hM = m.no.body, predY = preds_part.no.body)

postBeta = getPostEstimate(m, parName = "Beta")
plotBeta(m, post = postBeta, param = "Support", supportLevel = 0.95)

postBeta = getPostEstimate(m.no.body, parName = "Beta")
plotBeta(m.no.body, post = postBeta, param = "Support", supportLevel = 0.95)

head(m$X)
VP = computeVariancePartitioning(m, group = c(1,1),groupnames=c("Mass_kg_ln"))
plotVariancePartitioning(m, VP = VP)

OmegaCor = computeAssociations(m)
supportLevel = 0.95
toPlot = (OmegaCor[[1]]$mean)

data.frame(associations = c(OmegaCor[[1]]$mean), from = c("Migration", "Dispersal", "Foraging"), to = rep(c("Migration", "Dispersal", "Foraging"), each = 3)) %>% 
  ggplot(aes(x = from, y = to, fill = associations))+
  geom_tile()+
  scale_fill_viridis_c(option = "B")

corrplot(OmegaCor[[1]]$mean, method = "color", col=colorRampPalette(c("blue","white","red"))(200), title=paste("random effect level:",m$rLNames[1]), mar=c(0,0,1,0))

toPlot = ((OmegaCor[[1]]$support>supportLevel)
          + (OmegaCor[[1]]$support<(1-supportLevel))>0)*OmegaCor[[1]]$mean

corrplot(toPlot, method = "color", col=colorRampPalette(c("blue","white","red"))(200), title=paste("random effect level:",m$rLNames[1]), mar=c(0,0,1,0))

gradient <- constructGradient(hM = m,focalVariable = "Mass_kg_ln")
predY <- predict(m, XData = gradient$XDataNew, studyDesign = gradient$studyDesignNew, ranLevels = gradient$rLNew, expected = TRUE)


predictions = apply(preds, FUN=rowMeans, MARGIN=2)
predictions <- left_join(cbind(predictions, studyDesign, XData) %>% 
  gather(key = trait, value = prediction, Migration_ln:Foraging_ln),
  cbind(Y, studyDesign, XData) %>% 
    gather(key = trait, value = value, Migration_ln:Foraging_ln))

ggplot(predictions, aes(x = Mass_kg_ln, y = value, fill = class))+
  geom_point(pch = 21, color = 1, size = 2)+
  facet_wrap(~trait)+
  geom_smooth(aes(y = prediction, color = class), method = "lm")+
  theme_classic()+
  scale_fill_manual(values = c("dodgerblue1", "red1", "gray10"), guide = F)+
  scale_color_manual(values = c("dodgerblue1", "red1", "gray10"), name = "")


plot(Y[,1]~XData$Mass_kg_ln)
points(preds.mean[,1]~XData$Mass_kg_ln, col = 2)
plot(Y[,2]~XData$Mass_kg_ln)
points(preds.mean[,2]~XData$Mass_kg_ln, col = 2)
plot(Y[,3]~XData$Mass_kg_ln)
points(preds.mean[,3]~XData$Mass_kg_ln, col = 2)

