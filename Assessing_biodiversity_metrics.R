# Extent and impacts of mixing metrics in the inference
# of biodiversity responses to habitat change
# Last updated by Mingxin Liu (mingxinl@pku.edu.cn) on 3 Octorber 2022

# load libraries
library(readxl)
library(tidyverse)
library(nlme)
library(vegan)
library(cowplot)
library(MuMIn)
library(yarrr)
library(circlize)
library(data.table)

# set working directory
setwd("/Users/mingxin/PKU/Projects/1. Biases of mixing metrics/Statistical_Analysis/")

# empty global env
rm(list = ls())

########## Fig.1A ##########
lit.scan <- read_xlsx("/Users/mingxin/PKU/Projects/1. Biases of mixing metrics/Scan/Scan_19_October_2022.xlsx", sheet = "Fig1A")
lit.scan$ecosystem <- factor(lit.scan$ecosystem)
lit.scan$habitat_change_type <- factor(lit.scan$habitat_change_type)
lit.scan$mixed_number <- factor(lit.scan$mixed_number)

df.1A <- lit.scan %>% select(habitat_change_type, ecosystem, mixed_status, mixed_number)

df.1A$total <- rep(1, length(df.1A$ecosystem))

df.1A <- df.1A %>% group_by(habitat_change_type, ecosystem, mixed_status, mixed_number) %>% summarise(total = sum(total))

df.1A <- df.1A %>% filter(ecosystem != "Garden" & ecosystem != "Glacier")

# scale_fill_manual(values = c("light grey", "#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a"))

Fig.1A <- ggplot(df.1A, aes(x=mixed_status, y=total, fill=mixed_number)) + geom_bar(position="stack", stat="identity", width=0.9) + facet_grid(habitat_change_type ~ ecosystem, scales = "free") +
                            scale_fill_manual(values = c("#bcbddc", "#ffffcc", "#ffeda0", "#fed976", "#feb24c", "#fd8d3c", "#fc4e2a", "#e31a1c", "#bd0026", "#800026")) + 
                 theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.spacing = unit(0,"cm"), legend.position = "none") +
                 scale_y_continuous(expand = c(0,0), limits=c(0,50))

Fig.1A

########## Fig.1B ##########
metric.chord <- read_xlsx("/Users/mingxin/PKU/Projects/1. Biases of mixing metrics/Scan/Scan_26_August_2022.xlsx", sheet = "Fig1B")

col.names <- c("#8dd3c7", "#8dd3c7", "#8dd3c7", "#8dd3c7", "#8dd3c7", "#8dd3c7", 
            "#ffffb3", "#ffffb3","#ffffb3","#ffffb3",
            "orange", "orange","orange","orange","orange","orange",
            "#80b1d3","#80b1d3","#80b1d3", "#80b1d3","#80b1d3","#80b1d3","#80b1d3","#80b1d3",
            "#9e9ac8", "#9e9ac8","#9e9ac8")

col.metrics <- c("Observed species richness",
                 "Fisher alpha",
                 "Estimated species richness",
                 "Rarefied species richness",
                 "Species density",
                 "Other richness metrics",
                 
                 "Arithmetic species abundance",
                 "Biomass",
                 "Percent cover",
                 "Other abundance metrics",
                 
                 "Shannon diversity",
                 "Simpson diversity",
                 "Evenness",
                 "Pielou index",
                 "Other diversity metrics",
                 "Unspecified diveristy index",
                 
                 "Analysis of similarities",
                 "Jaccard index",
                 "Mantel",
                 "Margalef index",
                 "Morisita–Horn index",
                 "Sorenson index",
                 "Other similarity metrics",
                 "Unspecified similarity index",
                 
                 "Survival",
                 "Fecundity",
                 "Other demographical metrics")

colors <- setNames(col.names, col.metrics)

metrics_order <- data.frame(
  c("Observed species richness",
    "Fisher alpha",
    "Estimated species richness",
    "Rarefied species richness",
    "Species density",
    "Other richness metrics",
    
    "Arithmetic species abundance",
    "Biomass",
    "Percent cover",
    "Other abundance metrics",
    
    "Shannon diversity",
    "Simpson diversity",
    "Evenness",
    "Pielou index",
    "Other diversity metrics",
    "Unspecified diveristy index",
    
    "Analysis of similarities",
    "Jaccard index",
    "Mantel",
    "Margalef index",
    "Morisita–Horn index",
    "Sorenson index",
    "Other similarity metrics",
    "Unspecified similarity index",
    
    "Survival",
    "Fecundity",
    "Other demographical metrics"))

colnames(metrics_order) = "metrics"

circos.par(start.degree = 90, gap.degree = 2, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4))

chordDiagram(x = metric.chord[,3:5], grid.col = colors, transparency = 0.25, order = metrics_order$metrics, directional = 0,
             annotationTrack =  "grid", 
             preAllocateTracks = list(track.height = uh(1, "mm")),
             annotationTrackHeight = c(0.05, 0.1), link.sort = TRUE, link.largest.ontop = TRUE)

Fig.1B <- recordPlot() # record the previous plot

circos.trackPlotRegion(
  track.index = 1, 
  bg.border = NA, 
  panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")
    metrics = metrics_order$metrics[metrics_order$metrics == sector.index]
    
    circos.text(x = mean(xlim), y = 1.6, 
                labels = metrics, facing = "clockwise", cex = 1, niceFacing = TRUE, adj = c(0.8, 0), font = 1)
  }
)   

dev.off()

########## Fig.1C ##########
lit.scan <- read_xlsx("/Users/mingxin/PKU/Projects/1. Biases of mixing metrics/Scan/Scan_19_October_2022.xlsx", sheet = 4)

lit.scan <- lit.scan %>% filter(total_citations > 0 & impact_factor_2021 > 0)

mixed.yes.global <- lit.scan %>% filter(mixed_status == "Yes" & geographical_scale == "Global")
mixed.yes.regional <- lit.scan %>% filter(mixed_status == "Yes" & geographical_scale == "Regional")
mixed.no.global <- lit.scan %>% filter(mixed_status == "No" & geographical_scale == "Global")
mixed.no.regional <- lit.scan %>% filter(mixed_status == "No" & geographical_scale == "Regional")

Fig.1C <- ggplot(NULL) + 
  geom_point(data=mixed.no.regional, aes(x=log(impact_factor_2021), y=log(citations_per_year)), shape =21, size=3, colour="#9e9ac8", fill="#9e9ac8", alpha=0.7) +
  geom_point(data=mixed.no.global, aes(x=log(impact_factor_2021), y=log(citations_per_year)), shape =21, size=3, colour="black", fill="#9e9ac8", stroke=0.6, alpha=0.7) +
  geom_point(data=mixed.yes.regional, aes(x=log(impact_factor_2021), y=log(citations_per_year)), shape =24, size=3, colour="orange", fill="orange", alpha=0.7) +
  geom_point(data=mixed.yes.global, aes(x=log(impact_factor_2021), y=log(citations_per_year)), shape =24, size=3, colour="black", fill="orange", stroke=0.6, alpha=0.7) +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  scale_x_continuous(limits = c(0, 4.248495), breaks = c(0, 2.3025851, 2.995732, 3.401197, 3.688879, 3.912023, 4.094345, 4.248495), labels=c("1", "10", "20", "30", "40", "50", "60", "70")) + 
  scale_y_continuous(limits = c(0, 5.857933), breaks = c(0, 3.912023, 4.60517, 5.010635, 5.298317, 5.521461, 5.703782, 5.857933), labels=c("1", "50", "100", "150", "200", "250", "300", "350")) +
  labs(x="Journal impact factor (2021)", y="Average citations per year")

Fig.1C

########## Fig.3 ##########
# Richness: Observed species richness, Rarefied species richness
# Abundance: Arithmetic mean abundance, Geometric mean abundance
# Diversity: Shannon index, Simpson index

# load the full dataset
PL.NF.raw <- read_xlsx("datasets, ecosystem service and biodiversity under forest restoration_Feb16.xlsx", sheet = "abun_PL_vs_NF")

# delete NAs in baseline_raw_count & focal_raw_count, i.e. delete density data
PL.NF.raw <- PL.NF.raw %>% filter(!is.na(baseline_raw_count) & !is.na(focal_raw_count))

# inspect non-integer numbers in in baseline_raw_count & focal_raw_count, i.e. non-integer mean abundance data
non.integer.1 <- data.frame(PL.NF.raw$baseline_raw_count %% 1 == 0)

#
PL.NF.raw <- PL.NF.raw %>% filter(paired_data_id != "B_4_Set1_1_1_1" & 
                                  paired_data_id != "B_31_Set1_1_2_1" &
                                  paired_data_id != "B_31_Set1_1_2_2" &
                                  paired_data_id != "B_31_Set1_2_1_1" &
                                  paired_data_id != "B_31_Set1_2_1_2" &
                                  paired_data_id != "B_59_Set1_1_1_1")

########## 1.1 meta-analysis of PL vs NFben ##########
# read abundance data (plantation vs native forest benchmark)
PL.NFben.raw <- PL.NF.raw %>% filter(baseline_forest_type == "generic_native_forest" | baseline_forest_type == "primary_forest")

# subset data
# (1) study_id_overall: the site identity of the native forest;
# (2) paired_data_id: the identity of the ecological community;
# (3) focal_forest_type
# (4) baseline_datapoint_ID_overall: the identity of the primary study;
# (5) tree_cover_pair: the combination of tree covers;
# (6) focal_raw_count;
# (7) baseline_raw_count;
# (8) focal_count_adjusted: sampling-effort-corrected species abundance of focal community;
# (9) baseline_count_adjusted: sampling-effort-corrected species abundance of baseline community;
# (10) logR: species-level response ratio;
PL.NFben <- PL.NFben.raw %>% select(study_id_overall, paired_data_id, 
                                    focal_forest_type, baseline_datapoint_ID_overall, 
                                    tree_cover_pair, focal_raw_count, baseline_raw_count,
                                    focal_count_adjusted, baseline_count_adjusted,  
                                    logR)

# create new columns for observed species richness of focal and baseline communities
# if_else function: if abundance > 0, a species is present (=1), otherwise absent (=0)
PL.NFben <- PL.NFben %>% mutate(focal.sp = if_else(.$focal_count_adjusted > 0, 1, 0)) %>%     
  mutate(baseline.sp = if_else(.$baseline_count_adjusted > 0, 1, 0))

PL.NFben$study_id_overall <- as.factor(PL.NFben$study_id_overall)
PL.NFben$paired_data_id <- as.factor(PL.NFben$paired_data_id)
PL.NFben$focal_forest_type <- as.factor(PL.NFben$focal_forest_type)
PL.NFben$baseline_datapoint_ID_overall <- as.factor(PL.NFben$baseline_datapoint_ID_overall)

# from here, only use paired_data_id as the community-level identifier to avoid unexpected error when using group_by
# but later when LRR are calculated for all metrics, needed columns/variables should be added
PL.NFben.comm <- PL.NFben %>% group_by(paired_data_id) %>% 
  summarise(OSR.focal = sum(focal.sp), # Observed species richness of focal community
            OSR.baseline = sum(baseline.sp), # Observed species richness of baseline community
            AMA.focal = mean(focal_count_adjusted), # Arithmetic mean abundance of focal community
            AMA.baseline = mean(baseline_count_adjusted), # Arithmetic mean abundance of baseline community
            LRR.GMA = mean(logR), # Geometric mean abundance
            Shan.focal = diversity(focal_count_adjusted, index = "shannon"), # Shannon index of focal community, the greater the Shannon index, the more even the diversity
            Shan.baseline = diversity(baseline_count_adjusted, index = "shannon"), # Shannon index of baseline community
            Simp.focal = diversity(focal_count_adjusted, index = "simpson"), # Simpson index of focal community
            Simp.baseline = diversity(baseline_count_adjusted, index = "simpson"), # Simpson index of baseline community, returns 1-D, the greater the Simpson index, the higher the diversity
            .groups = "drop")

# create an empty df to store Rarefied species richness
df.PL.NFben.Rare <- data.frame("paired_data_id" = character(0), "Rare.focal" = integer(0),
                                   "Rare.baseline" = integer(0))

# divide df by paired_data_id
X.PL.NFben <- split(PL.NFben, PL.NFben$paired_data_id, drop = TRUE)
# create a list of dataframe by paired_data_id
Y.PL.NFben <- lapply(seq_along(X.PL.NFben), function(x) as.data.frame(X.PL.NFben[[x]])[,c("paired_data_id", "focal_raw_count", "baseline_raw_count")])

# calculate abundance-based Rarefied species richness
for (i in 1:length(Y.PL.NFben)){
  df <- data.frame(Y.PL.NFben[[i]])
  df <- data.frame(transpose(df))
  df <- data.frame(sapply(df[2:3,], as.numeric))
  minCounts <- min(rowSums(df))
  Srare <- rarefy(df, minCounts)
  df.PL.NFben.Rare[nrow(df.PL.NFben.Rare) + 1,] <- c(as.character(Y.PL.NFben[[i]][,1][[1]]),
                                                             Srare[1], Srare[2])
}

df.PL.NFben.Rare$Rare.focal <- as.numeric(df.PL.NFben.Rare$Rare.focal)
df.PL.NFben.Rare$Rare.baseline <- as.numeric(df.PL.NFben.Rare$Rare.baseline)

# join Rarefied species richness to PL.NFben.com by column paired_data_id
PL.NFben.comm <- inner_join(PL.NFben.comm, df.PL.NFben.Rare, by = "paired_data_id")

for (i in 1:length(PL.NFben.comm$paired_data_id)){
  # when both the sum of focal count and baseline count are zero
  if (PL.NFben.comm$OSR.focal[i] == 0 | PL.NFben.comm$OSR.baseline[i] == 0){
    PL.NFben.comm$OSR.focal[i] <- PL.NFben.comm$OSR.focal[i] + 0.0001
    PL.NFben.comm$OSR.baseline[i] <- PL.NFben.comm$OSR.baseline[i] + 0.0001
  }
}

for (i in 1:length(PL.NFben.comm$paired_data_id)){
  # when both the sum of focal count and baseline count are zero
  if (PL.NFben.comm$Rare.focal[i] == 0 | PL.NFben.comm$Rare.baseline[i] == 0){
    PL.NFben.comm$Rare.focal[i] <- PL.NFben.comm$Rare.focal[i] + 0.0001
    PL.NFben.comm$Rare.baseline[i] <- PL.NFben.comm$Rare.baseline[i] + 0.0001
  }
}

for (i in 1:length(PL.NFben.comm$paired_data_id)){
  # when both the sum of focal count and baseline count are zero
  if (PL.NFben.comm$AMA.focal[i] == 0 | PL.NFben.comm$AMA.baseline[i] == 0){
    PL.NFben.comm$AMA.focal[i] <- PL.NFben.comm$AMA.focal[i] + 0.0001
    PL.NFben.comm$AMA.baseline[i] <- PL.NFben.comm$AMA.baseline[i] + 0.0001
  }
}

for (i in 1:length(PL.NFben.comm$paired_data_id)){
  # when both the sum of focal count and baseline count are zero
  if (PL.NFben.comm$Simp.focal[i] == 0 | PL.NFben.comm$Simp.baseline[i] == 0){
    PL.NFben.comm$Simp.focal[i] <- PL.NFben.comm$Simp.focal[i] + 0.0001
    PL.NFben.comm$Simp.baseline[i] <- PL.NFben.comm$Simp.baseline[i] + 0.0001
  }
}

for (i in 1:length(PL.NFben.comm$paired_data_id)){
  # when both the sum of focal count and baseline count are zero
  if (PL.NFben.comm$Shan.focal[i] == 0 | PL.NFben.comm$Shan.baseline[i] == 0){
    PL.NFben.comm$Shan.focal[i] <- PL.NFben.comm$Shan.focal[i] + 0.0001
    PL.NFben.comm$Shan.baseline[i] <- PL.NFben.comm$Shan.baseline[i] + 0.0001
  }
}

# calculate log response ratio (LRR) as effect size
PL.NFben.comm <- PL.NFben.comm %>% mutate(LRR.OSR = log(OSR.focal/OSR.baseline),
                                          LRR.Rare = log(Rare.focal/Rare.baseline),
                                          LRR.AMA = log(AMA.focal/AMA.baseline),
                                          LRR.Shan = log(Shan.focal/Shan.baseline),
                                          LRR.Simp = log(Simp.focal/Simp.baseline))

PL.NFben.comm$paired_data_id <- as.factor(PL.NFben.comm$paired_data_id)

# add needed columns/variables
PL.NFben.cols <- PL.NFben.raw %>% group_by(study_id_overall, paired_data_id, 
                                           focal_forest_type, focal_forest_age, baseline_datapoint_ID_overall, 
                                           tree_cover_pair, species_group, record_weight_v1,
                                           MAT, latitude, longitude) %>% 
  summarise(weight = mean(record_weight_v1), # weighting score
            .groups = "drop")

PL.NFben.cols$paired_data_id <- as.factor(PL.NFben.cols$paired_data_id)

# join LRR with needed columns
PL.NFben.comm <- inner_join(PL.NFben.comm, PL.NFben.cols, by="paired_data_id", na_matches="never")

PL.NFben.comm <- distinct(PL.NFben.comm, paired_data_id, .keep_all = TRUE)

PL.NFben.comm$study_id_overall <- as.factor(PL.NFben.comm$study_id_overall)
PL.NFben.comm$paired_data_id <- as.factor(PL.NFben.comm$paired_data_id)
PL.NFben.comm$focal_forest_type <- as.factor(PL.NFben.comm$focal_forest_type)
PL.NFben.comm$baseline_datapoint_ID_overall <- as.factor(PL.NFben.comm$baseline_datapoint_ID_overall)
PL.NFben.comm$tree_cover_pair <- as.factor(PL.NFben.comm$tree_cover_pair)
PL.NFben.comm$species_group <- as.factor(PL.NFben.comm$species_group)

# 1/3 each of the top three metrics, i.e., observed species richness, arithmetic mean abundance and Shannon diversity
set.seed(123)

ran1LRR.OSR <- PL.NFben.comm %>% filter(paired_data_id %in% c(sample(PL.NFben.comm$paired_data_id, 74))) %>% 
  select(paired_data_id=paired_data_id, LRR.simu1=LRR.OSR)

`%!in%` <- Negate(`%in%`)

ran1SecSamp <- PL.NFben.comm %>% filter(paired_data_id %!in% ran1LRR.OSR$paired_data_id)

ran1LRR.AMA <- ran1SecSamp %>% filter(paired_data_id %in% c(sample(ran1SecSamp$paired_data_id, 74))) %>%
  select(paired_data_id=paired_data_id, LRR.simu1=LRR.AMA)

ran1LRR.Shan <- ran1SecSamp %>% filter(paired_data_id %!in% ran1LRR.AMA$paired_data_id) %>%
  select(paired_data_id=paired_data_id, LRR.simu1=LRR.Shan)

ran1LRR.simu <- bind_rows(ran1LRR.AMA, ran1LRR.OSR, ran1LRR.Shan)

PL.NFben.comm <- inner_join(PL.NFben.comm, ran1LRR.simu, by="paired_data_id")

# 1/5 each Simulation of mixing metrics (excluding GMA)
set.seed(123)

ran2LRR.OSR <- PL.NFben.comm %>% filter(paired_data_id %in% c(sample(PL.NFben.comm$paired_data_id, 44))) %>% 
  select(paired_data_id=paired_data_id, LRR.simu2=LRR.OSR)

ran2SecSamp <- PL.NFben.comm %>% filter(paired_data_id %!in% ran2LRR.OSR$paired_data_id)

ran2.LRR.Rare <- ran2SecSamp %>% filter(paired_data_id %in% c(sample(ran2SecSamp$paired_data_id, 44))) %>% 
  select(paired_data_id=paired_data_id, LRR.simu2=LRR.Rare)

ran2TrdSamp <- ran2SecSamp %>% filter(paired_data_id %!in% ran2.LRR.Rare$paired_data_id)

ran2LRR.AMA <- ran2TrdSamp %>% filter(paired_data_id %in% c(sample(ran2TrdSamp$paired_data_id, 44))) %>%
  select(paired_data_id=paired_data_id, LRR.simu2=LRR.AMA)

ran2fourSamp <- ran2TrdSamp %>% filter(paired_data_id %!in% ran2LRR.AMA$paired_data_id)

ran2LRR.Simp <- ran2fourSamp %>% filter(paired_data_id %in% c(sample(ran2fourSamp$paired_data_id, 44))) %>%
  select(paired_data_id=paired_data_id, LRR.simu2=LRR.Simp)

ran2LRR.Shan <- ran2fourSamp %>% filter(paired_data_id %!in% ran2LRR.Simp$paired_data_id) %>%
  select(paired_data_id=paired_data_id, LRR.simu2=LRR.Shan)

ran2LRR.simu <- bind_rows(ran2LRR.OSR, ran2.LRR.Rare, ran2LRR.AMA, ran2LRR.Shan, ran2LRR.Simp)

PL.NFben.comm <- inner_join(PL.NFben.comm, ran2LRR.simu, by="paired_data_id")

# remove outliers to acheive normal distribution
# Observed species richness
PL.NFben.comm.OSR.out <- boxplot(PL.NFben.comm$LRR.OSR)
sort(PL.NFben.comm.OSR.out$out)
PL.NFben.comm.OSR.subset <- subset(PL.NFben.comm, PL.NFben.comm$LRR.OSR < 1.029619 & PL.NFben.comm$LRR.OSR > -1.386294)
PL.NFben.comm.OSR.subset <- PL.NFben.comm.OSR.subset %>% add_column(LRR.type = "Observed species richness")

# Rarefied species richness
PL.NFben.comm.Rare.out <- boxplot(PL.NFben.comm$LRR.Rare)
sort(PL.NFben.comm.Rare.out$out)
PL.NFben.comm.Rare.subset <- subset(PL.NFben.comm, PL.NFben.comm$LRR.Rare < 0.6208905 & PL.NFben.comm$LRR.Rare > -1.0275018)
PL.NFben.comm.Rare.subset <- PL.NFben.comm.Rare.subset %>% add_column(LRR.type = "Rarefied species richness")

# Arithmetic mean abundance
PL.NFben.comm.AMA.out <- boxplot(PL.NFben.comm$LRR.AMA)
sort(PL.NFben.comm.AMA.out$out)
PL.NFben.comm.AMA.subset <- subset(PL.NFben.comm, PL.NFben.comm$LRR.AMA < 2.890372 & PL.NFben.comm$LRR.AMA > -2.756840)
PL.NFben.comm.AMA.subset <- PL.NFben.comm.AMA.subset %>% add_column(LRR.type = "Arithmetic mean abundance")

# Geometric mean abundance
PL.NFben.comm.GMA.out <- boxplot(PL.NFben.comm$LRR.GMA)
sort(PL.NFben.comm.GMA.out$out)
PL.NFben.comm.GMA.subset <- subset(PL.NFben.comm, PL.NFben.comm$LRR.GMA < 1.888607 & PL.NFben.comm$LRR.GMA > -1.912017)
PL.NFben.comm.GMA.subset <- PL.NFben.comm.GMA.subset %>% add_column(LRR.type = "Geometric mean abundance")

# Shannon index
PL.NFben.comm.Shan.out <- boxplot(PL.NFben.comm$LRR.Shan)
sort(PL.NFben.comm.Shan.out$out)
PL.NFben.comm.Shan.subset <- subset(PL.NFben.comm, PL.NFben.comm$LRR.Shan < 0.5135322 & PL.NFben.comm$LRR.Shan > -0.7423436)
PL.NFben.comm.Shan.subset <- PL.NFben.comm.Shan.subset %>% add_column(LRR.type = "Shannon index")

# Simpson index
PL.NFben.comm.Simp.out <- boxplot(PL.NFben.comm$LRR.Simp)
sort(PL.NFben.comm.Simp.out$out)
PL.NFben.comm.Simp.subset <- subset(PL.NFben.comm, PL.NFben.comm$LRR.Simp < 0.3085156 & PL.NFben.comm$LRR.Simp > -0.3953127)
PL.NFben.comm.Simp.subset <- PL.NFben.comm.Simp.subset %>% add_column(LRR.type = "Simpson index")

# LRR.simu1
PL.NFben.comm.simu1.out <- boxplot(PL.NFben.comm$LRR.simu1)
sort(PL.NFben.comm.simu1.out$out)
PL.NFben.comm.simu1.subset <- subset(PL.NFben.comm, PL.NFben.comm$LRR.simu1 < 1.036559 & PL.NFben.comm$LRR.simu1 > -1.386294)
PL.NFben.comm.simu1.subset <- PL.NFben.comm.simu1.subset %>% add_column(LRR.type = "Simulated mixture 1")

# LRR.simu2
PL.NFben.comm.simu2.out <- boxplot(PL.NFben.comm$LRR.simu2)
sort(PL.NFben.comm.simu2.out$out)
PL.NFben.comm.simu2.subset <- subset(PL.NFben.comm, PL.NFben.comm$LRR.simu2 < 1.252394 & PL.NFben.comm$LRR.simu2 > -1.098612)
PL.NFben.comm.simu2.subset <- PL.NFben.comm.simu2.subset %>% add_column(LRR.type = "Simulated mixture 2")

# fitting linear mixed model
# random effects:
# taxon: species' taxonomic identity as the highest-tier
# tree_cover_pair: the combination of tree covers
# study_id_streamlined: the site identity of the native forest
# baseline_datapoint_ID_overall_streamlined: the identity of the primary study
# weighting scheme: record_weight_v3

# lme for OSR
lme.PL.NFben.OSR <- lme(LRR.OSR ~ 1, random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id,
                          weights = ~I(1/weight), data = PL.NFben.comm.OSR.subset)

lower.PL.NFben.OSR <- intervals(lme.PL.NFben.OSR, which = "fixed")[[1]][[1,1]]
upper.PL.NFben.OSR <- intervals(lme.PL.NFben.OSR, which = "fixed")[[1]][[1,3]]

# heterogeneity test
I2_R2_lme <- function(model) {
  if(length(fixef(model)) == 1){
    phi <- model$sigma^2
    vi <- phi*attr(model$modelStruct$varStruct,"covariate")
    
    # sampling variance
    sigma2_m <-  sum(1/vi) * (dim(model$data)[1] - 1)/(sum(1/vi )^2 - sum((1/vi )^2))
    
    # other variance compn
    sigma2_others <-suppressWarnings(as.numeric(VarCorr(model)[,1]))
    sigma2_others <- sum(sigma2_others[1:length(sigma2_others) %% 2 == 0]) # all the even elements
    I2 <- sum(sigma2_others)/ (sigma2_others + sigma2_m)
    I2
  }
  else{
    # random effects
    sigma2_others <-suppressWarnings(as.numeric(VarCorr(model)[,1]))
    sigma2_others <- sum(sigma2_others[1:length(sigma2_others) %% 2 == 0]) # all the even elements
    
    # fixed effects
    sigma2_f <- var(fitted(model, 0))
    R2 <- sigma2_f/(sigma2_f + sigma2_others)
    R2
  }
} 

I2_R2_lme(lme.PL.NFben.OSR)

# lme for Rare
lme.PL.NFben.Rare <- lme(LRR.Rare ~ 1, random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id,
                           weights = ~I(1/weight), data = PL.NFben.comm.Rare.subset)

lower.PL.NFben.Rare <- intervals(lme.PL.NFben.Rare, which = "fixed")[[1]][[1,1]]
upper.PL.NFben.Rare <- intervals(lme.PL.NFben.Rare, which = "fixed")[[1]][[1,3]]

I2_R2_lme(lme.PL.NFben.Rare)

# lme for AMA
lme.PL.NFben.AMA <- lme(LRR.AMA ~ 1, random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id,
                        weights = ~I(1/weight), data = PL.NFben.comm.AMA.subset)

lower.PL.NFben.AMA <- intervals(lme.PL.NFben.AMA, which = "fixed")[[1]][[1,1]]
upper.PL.NFben.AMA <- intervals(lme.PL.NFben.AMA, which = "fixed")[[1]][[1,3]]

I2_R2_lme(lme.PL.NFben.AMA)

# lme for GMA
lme.PL.NFben.GMA <- lme(LRR.GMA ~ 1, random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall,
                        weights = ~I(1/weight), data = PL.NFben.comm.GMA.subset)

lower.PL.NFben.GMA <- intervals(lme.PL.NFben.GMA, which = "fixed")[[1]][[1,1]]
upper.PL.NFben.GMA <- intervals(lme.PL.NFben.GMA, which = "fixed")[[1]][[1,3]]

I2_R2_lme(lme.PL.NFben.GMA)

# lme for Shan
lme.PL.NFben.Shan <- lme(LRR.Shan ~ 1, random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id,
                           weights = ~I(1/weight), data = PL.NFben.comm.Shan.subset)

lower.PL.NFben.Shan <- intervals(lme.PL.NFben.Shan, which = "fixed")[[1]][[1,1]]
upper.PL.NFben.Shan <- intervals(lme.PL.NFben.Shan, which = "fixed")[[1]][[1,3]]

I2_R2_lme(lme.PL.NFben.Shan)

# lme for Simp
lme.PL.NFben.Simp <- lme(LRR.Simp ~ 1, random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id,
                         weights = ~I(1/weight), data = PL.NFben.comm.Simp.subset)

lower.PL.NFben.Simp <- intervals(lme.PL.NFben.Simp, which = "fixed")[[1]][[1,1]]
upper.PL.NFben.Simp <- intervals(lme.PL.NFben.Simp, which = "fixed")[[1]][[1,3]]

I2_R2_lme(lme.PL.NFben.Simp)

# lme for simu1
lme.PL.NFben.simu1 <- lme(LRR.simu1 ~ 1, random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id,
                            weights = ~I(1/weight), data = PL.NFben.comm.simu1.subset)

lower.PL.NFben.simu1 <- intervals(lme.PL.NFben.simu1, which = "fixed")[[1]][[1,1]]
upper.PL.NFben.simu1 <- intervals(lme.PL.NFben.simu1, which = "fixed")[[1]][[1,3]]

I2_R2_lme(lme.PL.NFben.simu1)

# lme for simu2
lme.PL.NFben.simu2 <- lme(LRR.simu2 ~ 1, random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id,
                            weights = ~I(1/weight), data = PL.NFben.comm.simu2.subset)

lower.PL.NFben.simu2 <- intervals(lme.PL.NFben.simu2, which = "fixed")[[1]][[1,1]]
upper.PL.NFben.simu2 <- intervals(lme.PL.NFben.simu2, which = "fixed")[[1]][[1,3]]

I2_R2_lme(lme.PL.NFben.simu2)

# create a df to store estimated effect size, 95% CI (i.e. lower and upper bound)
df.PL.NFben.es <- data.frame("LRR.type" = character(0), "est.es" = integer(0),
                              "lower.es" = integer(0), "upper.es" = integer(0))

df.PL.NFben.es[nrow(df.PL.NFben.es) + 1,] <- c("Observed species richness", lme.PL.NFben.OSR$coefficients[[1]], lower.PL.NFben.OSR, upper.PL.NFben.OSR)
df.PL.NFben.es[nrow(df.PL.NFben.es) + 1,] <- c("Rarefied species richness", lme.PL.NFben.Rare$coefficients[[1]], lower.PL.NFben.Rare, upper.PL.NFben.Rare)
df.PL.NFben.es[nrow(df.PL.NFben.es) + 1,] <- c("Arithmetic mean abundance", lme.PL.NFben.AMA$coefficients[[1]], lower.PL.NFben.AMA, upper.PL.NFben.AMA)
df.PL.NFben.es[nrow(df.PL.NFben.es) + 1,] <- c("Geometric mean abundance", lme.PL.NFben.GMA$coefficients[[1]], lower.PL.NFben.GMA, upper.PL.NFben.GMA)
df.PL.NFben.es[nrow(df.PL.NFben.es) + 1,] <- c("Shannon index", lme.PL.NFben.Shan$coefficients[[1]], lower.PL.NFben.Shan, upper.PL.NFben.Shan)
df.PL.NFben.es[nrow(df.PL.NFben.es) + 1,] <- c("Simpson index", lme.PL.NFben.Simp$coefficients[[1]], lower.PL.NFben.Simp, upper.PL.NFben.Simp)
df.PL.NFben.es[nrow(df.PL.NFben.es) + 1,] <- c("Simulated mixture 1", lme.PL.NFben.simu1$coefficients[[1]], lower.PL.NFben.simu1, upper.PL.NFben.simu1)
df.PL.NFben.es[nrow(df.PL.NFben.es) + 1,] <- c("Simulated mixture 2", lme.PL.NFben.simu2$coefficients[[1]], lower.PL.NFben.simu2, upper.PL.NFben.simu2)

df.PL.NFben.es$est.es <- as.numeric(df.PL.NFben.es$est.es)
df.PL.NFben.es$lower.es <- as.numeric(df.PL.NFben.es$lower.es)
df.PL.NFben.es$upper.es <- as.numeric(df.PL.NFben.es$upper.es)

# calculate difference percentage
df.PL.NFben.es<- df.PL.NFben.es %>% mutate(perc.est.es = (exp(est.es) - 1) *100,
                                             perc.lower.es = (exp(lower.es) - 1) *100,
                                             perc.upper.es = (exp(upper.es) -1) *100)

cols.PL.NFben <- as.factor(df.PL.NFben.es$LRR.type)

panel.A.theme <- theme(legend.position = "none",
                       plot.title=element_text(size=8,face="bold",hjust = 0.5),
                       axis.title.x = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.text.x = element_blank(),
                       axis.title.y = element_text(size=8),
                       axis.text.y = element_text(colour = "black", size=8),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank())

panel.A <- ggplot(NULL) + 
  geom_jitter(data = PL.NFben.comm.OSR.subset, aes(x=LRR.type, y=LRR.OSR, col=cols.PL.NFben[1], alpha=0.2), width = 0.2) + 
  geom_jitter(data = PL.NFben.comm.Rare.subset, aes(x=LRR.type, y=LRR.Rare, col=cols.PL.NFben[2], alpha=0.2), width = 0.2) + 
  geom_jitter(data = PL.NFben.comm.AMA.subset, aes(x=LRR.type, y=LRR.AMA, col=cols.PL.NFben[3], alpha=0.2), width = 0.2) + 
  geom_jitter(data = PL.NFben.comm.GMA.subset, aes(x=LRR.type, y=LRR.GMA, col=cols.PL.NFben[4], alpha=0.2), width = 0.2) + 
  geom_jitter(data = PL.NFben.comm.Shan.subset, aes(x=LRR.type, y=LRR.Shan, col=cols.PL.NFben[5], alpha=0.2), width = 0.2) + 
  geom_jitter(data = PL.NFben.comm.Simp.subset, aes(x=LRR.type, y=LRR.Simp, col=cols.PL.NFben[6], alpha=0.2), width = 0.2) + 
  geom_jitter(data = PL.NFben.comm.simu1.subset, aes(x=LRR.type, y=LRR.simu1, col=cols.PL.NFben[7], alpha=0.2), width = 0.2) + 
  geom_jitter(data = PL.NFben.comm.simu2.subset, aes(x=LRR.type, y=LRR.simu2, col=cols.PL.NFben[8], alpha=0.2), width = 0.2) + 
  geom_pointrange(data = df.PL.NFben.es, aes(x=LRR.type, y=est.es, ymin=lower.es, ymax=upper.es, fill=cols.PL.NFben, stroke=1), shape=23, size=0.5) +
  geom_hline(yintercept=0, linetype=2) + ylim(-2.5, 2.5) + coord_flip() +
  theme_bw() + labs(x='', y='', title='Plantations v.s. reference native forests')

panel.A + panel.A.theme

########## 1.2 meta-regression of PL vs NFben ##########
lme.predict<-function(mod, newdat, se.mult){
  pred <- as.vector(predict(mod, newdat, level = 0))
  Designmat <- model.matrix(formula(mod)[-2], newdat)
  predvar <- diag(Designmat %*% vcov(mod) %*% t(Designmat)) 
  SE <- sqrt(predvar) 
  upper<- pred + (se.mult*SE)
  lower<- pred - (se.mult*SE) 
  return(data.frame(pred, upper, lower))
}	

plot.CI.func<- function(x.for.plot, pred, upper, lower, line.color, polygon.color){
  polygon.coords<-data.frame(rbind(cbind(x.for.plot[1], lower[1]), 
                                   cbind(x.for.plot, upper), 
                                   cbind(x.for.plot, lower)[rev(order(x.for.plot)),]))
  names(polygon.coords)<-c("x", "y")							
  polygon(polygon.coords$x, polygon.coords$y, col=transparent(polygon.color,0.7), border=NA)
  lines(x.for.plot, pred, col=line.color, lwd=2)         
} 

# lme for OSR
PL.NFben.comm.OSR.reg <- PL.NFben.comm.OSR.subset %>% filter(!is.na(focal_forest_age) & !is.na(MAT))

glb.PL.NFben.comm.OSR.reg <- lme(LRR.OSR ~ log(focal_forest_age) + log(MAT) +
                             log(MAT) * log(focal_forest_age),
                           random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id, 
                           weights = ~I(1/weight),
                           data=PL.NFben.comm.OSR.reg)

mod.glb.PL.NFben.comm.OSR.reg <- dredge(glb.PL.NFben.comm.OSR.reg, trace = TRUE, rank = "AICc", REML = FALSE)
mod.glb.PL.NFben.comm.OSR.reg # 

lme.PL.NFben.comm.OSR.reg <- lme(LRR.OSR ~ log(MAT),
                                 random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id, 
                                 weights = ~I(1/weight),
                                 data=PL.NFben.comm.OSR.reg)

summary(lme.PL.NFben.comm.OSR.reg)

# separate by focal_forest_type
abd.NFben.comm.OSR.reg <- PL.NFben.comm.OSR.reg %>% filter(focal_forest_type == "abandoned_plantation")
mix.NFben.comm.OSR.reg <- PL.NFben.comm.OSR.reg %>% filter(focal_forest_type == "mixed_plantation")
mono.NFben.comm.OSR.reg <- PL.NFben.comm.OSR.reg %>% filter(focal_forest_type == "monoculture_plantation")

# remove focal_forest_type and run with the remaining variables
# abandoned plantation
glb.abd.NFben.comm.OSR.reg <- lme(LRR.OSR ~ log(focal_forest_age) + log(MAT) +
                                 log(MAT) * log(focal_forest_age),
                               random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id, 
                               weights = ~I(1/weight),
                               data=abd.NFben.comm.OSR.reg)

mod.glb.abd.NFben.comm.OSR.reg <- dredge(glb.abd.NFben.comm.OSR.reg, trace = TRUE, rank = "AICc", REML = FALSE)
mod.glb.abd.NFben.comm.OSR.reg # ln(focal_forest_age) was identified

lme.abd.NFben.comm.OSR.reg <- lme(LRR.OSR ~ log(focal_forest_age),
                               random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id, 
                               weights = ~I(1/weight),
                               data=abd.NFben.comm.OSR.reg)

summary(lme.abd.NFben.comm.OSR.reg)

x.abd.NFben.comm.OSR.reg <- seq(11, 80, length.out=27)

predictions.abd.NFben.comm.OSR.reg <- lme.predict(lme.abd.NFben.comm.OSR.reg,
                                         newdat = data.frame(focal_forest_age=x.abd.NFben.comm.OSR.reg), 
                                         se.mult = 1.96)

plot(x=0,y=0,
     pch=16, cex=1, col='white',
     xlim=c(11,80), ylim=c(-1,1),
     xaxt='n', ann = FALSE, yaxt='n', lwd=2, frame.plot = TRUE,main="") +
  points(abd.NFben.comm.OSR.reg$focal_forest_age, abd.NFben.comm.OSR.reg$LRR.OSR,
         bg=transparent("#02818a", 0.4),pch=21,cex =2) +
  plot.CI.func(x.for.plot = x.abd.NFben.comm.OSR.reg, pred=predictions.abd.NFben.comm.OSR.reg[1:27,1], 
               upper = predictions.abd.NFben.comm.OSR.reg[1:27,2], lower = predictions.abd.NFben.comm.OSR.reg[1:27,3],
               polygon.color = transparent("#02818a", 0.4),
               line.color="grey20") +
  axis(1, xlim=c(11,80), at=c(seq(10,80, by=5))) +
  axis(2, ylim= c(-1, 1), at=c(seq(-1,1, by=0.5)))+
  abline(h=0, lty="dotted",col="black",lwd=2)+
  title("Plantations versus Reference native forests",
        xlab="Forest age (year)", ylab="RR-OSR")

# mixed plantation
glb.mix.NFben.comm.OSR.reg <- lme(LRR.OSR ~ log(focal_forest_age) + log(MAT) +
                                 log(MAT) * log(focal_forest_age),
                               random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id, 
                               weights = ~I(1/weight),
                               data=mix.NFben.comm.OSR.reg)

mod.glb.mix.NFben.comm.OSR.reg <- dredge(glb.mix.NFben.comm.OSR.reg, trace = TRUE, rank = "AICc", REML = FALSE)
mod.glb.mix.NFben.comm.OSR.reg # no variable was identified

# mono plantation
glb.mono.NFben.comm.OSR.reg <- lme(LRR.OSR ~ log(focal_forest_age) + log(MAT) +
                                  log(MAT) * log(focal_forest_age),
                                random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id, 
                                weights = ~I(1/weight),
                                data=mono.NFben.comm.OSR.reg)

mod.glb.mono.NFben.comm.OSR.reg <- dredge(glb.mono.NFben.comm.OSR.reg, trace = TRUE, rank = "AICc", REML = FALSE)
mod.glb.mono.NFben.comm.OSR.reg # no variable was identified

lme.mono.NFben.comm.OSR.reg <- lme(LRR.OSR ~ log(focal_forest_age) + log(MAT) + log(focal_forest_age) * log(MAT),
                                   random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id, 
                                   weights = ~I(1/weight),
                                   data=mono.NFben.comm.OSR.reg)

summary(lme.mono.NFben.comm.OSR.reg)

# lme for Rarefied
PL.NFben.comm.Rare.reg <- PL.NFben.comm.Rare.subset %>% filter(!is.na(focal_forest_age) & !is.na(MAT))

glb.PL.NFben.comm.Rare.reg <- lme(LRR.Rare ~ log(focal_forest_age) + log(MAT) +
                              log(MAT) * log(focal_forest_age),
                            random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id, 
                            weights = ~I(1/weight),
                            data=PL.NFben.comm.Rare.reg)

mod.glb.PL.NFben.comm.Rare.reg <- dredge(glb.PL.NFben.comm.Rare.reg, trace = TRUE, rank = "AICc", REML = FALSE)
mod.glb.PL.NFben.comm.Rare.reg # 

lme.PL.NFben.comm.Rare.reg <- lme(LRR.Rare ~ log(focal_forest_age),
                            random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id, 
                            weights = ~I(1/weight),
                            data=PL.NFben.comm.Rare.reg)

summary(lme.PL.NFben.comm.Rare.reg) # significant

# lme for AMA
PL.NFben.comm.AMA.reg <- PL.NFben.comm.AMA.subset %>% filter(!is.na(focal_forest_age) & !is.na(MAT))

glb.PL.NFben.comm.AMA.reg <- lme(LRR.AMA ~ log(focal_forest_age) + log(MAT) +
                                   log(MAT) * log(focal_forest_age),
                                 random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id, 
                                 weights = ~I(1/weight),
                                 data=PL.NFben.comm.AMA.reg)

mod.glb.PL.NFben.comm.AMA.reg <- dredge(glb.PL.NFben.comm.AMA.reg, trace = TRUE, rank = "AICc", REML = FALSE)
mod.glb.PL.NFben.comm.AMA.reg # 

lme.PL.NFben.comm.AMA.reg <- lme(LRR.AMA ~ log(focal_forest_age) + log(MAT) +
                                   log(MAT) * log(focal_forest_age),
                                 random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id, 
                                 weights = ~I(1/weight),
                                 data=PL.NFben.comm.AMA.reg)

summary(lme.PL.NFben.comm.AMA.reg) # 

# lme for GMA
PL.NFben.comm.GMA.reg <- PL.NFben.comm.GMA.subset %>% filter(!is.na(focal_forest_age) & !is.na(MAT))

glb.PL.NFben.comm.GMA.reg <- lme(LRR.GMA ~ log(focal_forest_age) + log(MAT) +
                                   log(MAT) * log(focal_forest_age),
                                 random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id, 
                                 weights = ~I(1/weight),
                                 data=PL.NFben.comm.GMA.reg)

mod.glb.PL.NFben.comm.GMA.reg <- dredge(glb.PL.NFben.comm.GMA.reg, trace = TRUE, rank = "AICc", REML = FALSE)
mod.glb.PL.NFben.comm.GMA.reg # 

lme.PL.NFben.comm.GMA.reg <- lme(LRR.GMA ~ log(MAT),
                                 random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id, 
                                 weights = ~I(1/weight),
                                 data=PL.NFben.comm.GMA.reg)

summary(lme.PL.NFben.comm.GMA.reg)

# plot lme for Shan
PL.NFben.comm.Shan.reg <- PL.NFben.comm.Shan.subset %>% filter(!is.na(focal_forest_age) & !is.na(MAT))

glb.PL.NFben.comm.Shan.reg<- lme(LRR.Shan ~ log(focal_forest_age) + log(MAT) +
                              log(MAT) * log(focal_forest_age),
                            random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id, 
                            weights = ~I(1/weight),
                            data=PL.NFben.comm.Shan.reg)

mod.glb.PL.NFben.comm.Shan.reg <- dredge(glb.PL.NFben.comm.Shan.reg, trace = TRUE, rank = "AICc", REML = FALSE)
mod.glb.PL.NFben.comm.Shan.reg # 

lme.PL.NFben.comm.Shan.reg <- lme(LRR.Shan ~ log(MAT),
                            random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id, 
                            weights = ~I(1/weight),
                            data=PL.NFben.comm.Shan.reg)

summary(lme.PL.NFben.comm.Shan.reg) # 

# lme for Simp
PL.NFben.comm.Simp.reg <- PL.NFben.comm.Simp.subset %>% filter(!is.na(focal_forest_age) & !is.na(MAT))

glb.PL.NFben.comm.Simp.reg <- lme(LRR.Simp ~ log(focal_forest_age) + log(MAT) +
                                    log(MAT) * log(focal_forest_age),
                                  random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id, 
                                  weights = ~I(1/weight),
                                  data=PL.NFben.comm.Simp.reg)

mod.glb.PL.NFben.comm.Simp.reg <- dredge(glb.PL.NFben.comm.Simp.reg, trace = TRUE, rank = "AICc", REML = FALSE)
mod.glb.PL.NFben.comm.Simp.reg # no variable was identified

# lme for simu1
PL.NFben.comm.simu1.reg <- PL.NFben.comm.simu1.subset %>% filter(!is.na(focal_forest_age) & !is.na(MAT))

glb.PL.NFben.comm.simu1.reg <- lme(LRR.simu1 ~ log(focal_forest_age) + log(MAT) +
                                     log(MAT) * log(focal_forest_age),
                                   random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id, 
                                   weights = ~I(1/weight),
                                   data=PL.NFben.comm.simu1.reg)

mod.glb.PL.NFben.comm.simu1.reg <- dredge(glb.PL.NFben.comm.simu1.reg, trace = TRUE, rank = "AICc", REML = FALSE)
mod.glb.PL.NFben.comm.simu1.reg

lme.PL.NFben.comm.simu1.reg <- lme(LRR.simu1 ~ log(MAT),
                                   random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id, 
                                   weights = ~I(1/weight),
                                   data=PL.NFben.comm.simu1.reg)

summary(lme.PL.NFben.comm.simu1.reg)

# lme for simu2
PL.NFben.comm.simu2.reg <- PL.NFben.comm.simu2.subset %>% filter(!is.na(focal_forest_age) & !is.na(MAT))

glb.PL.NFben.comm.simu2.reg <- lme(LRR.simu2 ~ log(focal_forest_age) + log(MAT) +
                                     log(MAT) * log(focal_forest_age),
                                   random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id, 
                                   weights = ~I(1/weight),
                                   data=PL.NFben.comm.simu2.reg)

mod.glb.PL.NFben.comm.simu2.reg <- dredge(glb.PL.NFben.comm.simu2.reg, trace = TRUE, rank = "AICc", REML = FALSE)
mod.glb.PL.NFben.comm.simu2.reg

lme.PL.NFben.comm.simu2.reg <- lme(LRR.simu2 ~ log(MAT),
                                   random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id, 
                                   weights = ~I(1/weight),
                                   data=PL.NFben.comm.simu2.reg)

summary(lme.PL.NFben.comm.simu2.reg)

# separate by focal_forest_type
abd.NFben.comm.simu2.reg <- PL.NFben.comm.simu2.reg %>% filter(focal_forest_type == "abandoned_plantation")
mix.NFben.comm.simu2.reg <- PL.NFben.comm.simu2.reg %>% filter(focal_forest_type == "mixed_plantation")
mono.NFben.comm.simu2.reg <- PL.NFben.comm.simu2.reg %>% filter(focal_forest_type == "monoculture_plantation")

glb.abd.NFben.comm.simu2.reg <- lme(LRR.simu2 ~ log(focal_forest_age) + log(MAT) +
                                      log(MAT) * log(focal_forest_age),
                                    random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id, 
                                    weights = ~I(1/weight),
                                    data=abd.NFben.comm.simu2.reg)

mod.glb.abd.NFben.comm.simu2.reg <- dredge(glb.abd.NFben.comm.simu2.reg, trace = TRUE, rank = "AICc", REML = FALSE)
mod.glb.abd.NFben.comm.simu2.reg

glb.mix.NFben.comm.simu2.reg <- lme(LRR.simu2 ~ log(focal_forest_age) + log(MAT) +
                                      log(MAT) * log(focal_forest_age),
                                    random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id, 
                                    weights = ~I(1/weight),
                                    data=mix.NFben.comm.simu2.reg)

mod.glb.mix.NFben.comm.simu2.reg <- dredge(glb.mix.NFben.comm.simu2.reg, trace = TRUE, rank = "AICc", REML = FALSE)
mod.glb.mix.NFben.comm.simu2.reg

glb.mono.NFben.comm.simu2.reg <- lme(LRR.simu2 ~ log(focal_forest_age) + log(MAT) +
                                      log(MAT) * log(focal_forest_age),
                                    random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id, 
                                    weights = ~I(1/weight),
                                    data=mono.NFben.comm.simu2.reg)

mod.glb.mono.NFben.comm.simu2.reg <- dredge(glb.mono.NFben.comm.simu2.reg, trace = TRUE, rank = "AICc", REML = FALSE)
mod.glb.mono.NFben.comm.simu2.reg

lme.mono.NFben.comm.simu2.reg <- lme(LRR.simu2 ~ focal_forest_age + MAT,
                                     random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id, 
                                     weights = ~I(1/weight),
                                     data=mono.NFben.comm.simu2.reg)

summary(lme.mono.NFben.comm.simu2.reg)

########## 2.1 meta-analysis of PL vs resNF ##########
# read abundance data (plantation vs restored native forests (including native planting and secondary forest))
PL.resNF.raw <- PL.NF.raw %>% filter(baseline_forest_type == "secondary_forest" | baseline_forest_type == "native_planting")

# keep plantations and RNF age contrast within 10 years
PL.resNF.raw$focal_baseline_age_contrast <- as.numeric(PL.resNF.raw$focal_baseline_age_contrast )
PL.resNF.raw <- PL.resNF.raw %>% filter(!is.na(focal_baseline_age_contrast) & focal_baseline_age_contrast >= -10 & focal_baseline_age_contrast <= 10)
# subset data
# (1) study_id_overall: the site identity of the native forest;
# (2) paired_data_id: the identity of the ecological community;
# (3) baseline_datapoint_ID_overall: the identity of the primary study;
# (4) focal_forest_type
# (5) tree_cover_pair: the combination of tree covers;
# (6) focal_raw_count;
# (7)baseline_raw_count;
# (8) focal_count_adjusted: sampling-effort-corrected species abundance of focal community;
# (9) baseline_count_adjusted: sampling-effort-corrected species abundance of baseline community;
# (10) logR: response ration based on geometric mean species abundance;
PL.resNF.comm <- PL.resNF.raw %>% select(study_id_overall, paired_data_id,
                                  baseline_datapoint_ID_overall, focal_forest_type, 
                                  tree_cover_pair, focal_raw_count, baseline_raw_count,
                                  focal_count_adjusted, baseline_count_adjusted, 
                                  logR)

# create new columns for rarafied species richness of focal and baseline communities
# if_else function: if abundance > 0, a species is present (=1), otherwise absent (=0)
PL.resNF.comm <- PL.resNF.comm %>% mutate(focal.sp = if_else(.$focal_count_adjusted > 0, 1, 0)) %>%     
  mutate(baseline.sp = if_else(.$baseline_count_adjusted > 0, 1, 0))

PL.resNF.comm <- PL.resNF.comm %>% group_by(paired_data_id) %>% 
  summarise(OSR.focal = sum(focal.sp), # Observed species richness (OSR) of focal community
            OSR.baseline = sum(baseline.sp), # Observed species richness (OSR) of baseline community
            AMA.focal = mean(focal_count_adjusted), # arithmetic mean abundance (AMA) of focal community
            AMA.baseline = mean(baseline_count_adjusted), # arithmetic mean abundance (AMA) of baseline community
            LRR.GMA = mean(logR), # GMA adding min.comm to zero focal or baseline count
            Shan.focal = diversity(focal_count_adjusted, index = "shannon"), # Shannon index of focal community
            Shan.baseline = diversity(baseline_count_adjusted, index = "shannon"), # Shannon index of baseline community
            Simp.focal = diversity(focal_count_adjusted, index = "simpson"), # Simpson index of focal community
            Simp.baseline = diversity(baseline_count_adjusted, index = "simpson"), # Simpson index of baseline community
            .groups = "drop")

# create an empty df to store Rarefied species richness
df.PL.resNF.comm.Rarefied <- data.frame("paired_data_id" = character(0), "Rare.focal" = integer(0),
                                      "Rare.baseline" = integer(0))

# divide df by paired_data_id
X.PL.resNF.comm <- split(PL.resNF.raw, PL.resNF.raw$paired_data_id, drop = TRUE)
# create a list of dataframe by paired_data_id
Y.PL.resNF.comm <- lapply(seq_along(X.PL.resNF.comm), function(x) as.data.frame(X.PL.resNF.comm[[x]])[,c("paired_data_id", "focal_raw_count", "baseline_raw_count")])

# calculate abundance-based Rarefied species richness
for (i in 1:length(Y.PL.resNF.comm)){
  df <- data.frame(Y.PL.resNF.comm[[i]])
  df <- data.frame(transpose(df))
  df <- data.frame(sapply(df[2:3,], as.numeric))
  minCounts <- min(rowSums(df))
  Srare <- rarefy(df, minCounts)
  df.PL.resNF.comm.Rarefied[nrow(df.PL.resNF.comm.Rarefied) + 1,] <- c(as.character(Y.PL.resNF.comm[[i]][,1][[1]]),
                                                                          Srare[1], Srare[2])
}

df.PL.resNF.comm.Rarefied$Rare.focal <- as.numeric(df.PL.resNF.comm.Rarefied$Rare.focal)
df.PL.resNF.comm.Rarefied$Rare.baseline <- as.numeric(df.PL.resNF.comm.Rarefied$Rare.baseline)

# join Chao1 species richness to PL.resNF.comm by column paired_data_id
PL.resNF.comm <- inner_join(PL.resNF.comm, df.PL.resNF.comm.Rarefied, by = "paired_data_id")

for (i in 1:length(PL.resNF.comm$paired_data_id)){
  if (PL.resNF.comm$OSR.focal[i] == 0 | PL.resNF.comm$OSR.baseline[i] == 0){
    PL.resNF.comm$OSR.focal[i] <- PL.resNF.comm$OSR.focal[i] + 0.0001
    PL.resNF.comm$OSR.baseline[i] <- PL.resNF.comm$OSR.baseline[i] + 0.0001
  }
}

for (i in 1:length(PL.resNF.comm$paired_data_id)){
  if (PL.resNF.comm$Rare.focal[i] == 0 | PL.resNF.comm$Rare.baseline[i] == 0){
    PL.resNF.comm$Rare.focal[i] <- PL.resNF.comm$Rare.focal[i] + 0.0001
    PL.resNF.comm$Rare.baseline[i] <- PL.resNF.comm$Rare.baseline[i] + 0.0001
  }
}

for (i in 1:length(PL.resNF.comm$paired_data_id)){
  if (PL.resNF.comm$AMA.focal[i] == 0 | PL.resNF.comm$AMA.baseline[i] == 0){
    PL.resNF.comm$AMA.focal[i] <- PL.resNF.comm$AMA.focal[i] + 0.0001
    PL.resNF.comm$AMA.baseline[i] <- PL.resNF.comm$AMA.baseline[i] + 0.0001
  }
}

for (i in 1:length(PL.resNF.comm$paired_data_id)){
  if (PL.resNF.comm$Shan.focal[i] == 0 | PL.resNF.comm$Shan.baseline[i] == 0){
    PL.resNF.comm$Shan.focal[i] <- PL.resNF.comm$Shan.focal[i] + 0.0001
    PL.resNF.comm$Shan.baseline[i] <- PL.resNF.comm$Shan.baseline[i] + 0.0001
  }
}

for (i in 1:length(PL.resNF.comm$paired_data_id)){
  if (PL.resNF.comm$Simp.focal[i] == 0 | PL.resNF.comm$Simp.baseline[i] == 0){
    PL.resNF.comm$Simp.focal[i] <- PL.resNF.comm$Simp.focal[i] + 0.0001
    PL.resNF.comm$Simp.baseline[i] <- PL.resNF.comm$Simp.baseline[i] + 0.0001
  }
}

# calculate log response ratio (LRR) as effect size
PL.resNF.comm <- PL.resNF.comm %>% mutate(LRR.OSR = log(OSR.focal/OSR.baseline),
                                          LRR.Rare = log(Rare.focal/Rare.baseline),
                                          LRR.AMA = log(AMA.focal/AMA.baseline),
                                          LRR.Shan = log(Shan.focal/Shan.baseline),
                                          LRR.Simp = log(Simp.focal/Simp.baseline))

# add needed columns
PL.resNF.cols <- PL.resNF.raw %>% group_by(study_id_overall, paired_data_id, 
                                       focal_forest_type, focal_forest_age, baseline_datapoint_ID_overall, 
                                       tree_cover_pair, species_group, record_weight_v1,
                                       MAT, focal_baseline_age_contrast, latitude, longitude) %>% 
  summarise(weight = mean(record_weight_v1), # weighting score
            .groups = "drop")

PL.resNF.cols$paired_data_id <- as.factor(PL.resNF.cols$paired_data_id)

PL.resNF.comm <- inner_join(PL.resNF.comm, PL.resNF.cols, by="paired_data_id")

PL.resNF.comm <- distinct(PL.resNF.comm, paired_data_id, .keep_all = TRUE)

PL.resNF.comm$study_id_overall <- as.factor(PL.resNF.comm$study_id_overall)
PL.resNF.comm$focal_forest_type <- as.factor(PL.resNF.comm$focal_forest_type)
PL.resNF.comm$baseline_datapoint_ID_overall <- as.factor(PL.resNF.comm$baseline_datapoint_ID_overall)
PL.resNF.comm$tree_cover_pair <- as.factor(PL.resNF.comm$tree_cover_pair)
PL.resNF.comm$paired_data_id <- as.factor(PL.resNF.comm$paired_data_id)
PL.resNF.comm$species_group <- as.factor(PL.resNF.comm$species_group)
PL.resNF.comm$focal_baseline_age_contrast <- as.numeric(PL.resNF.comm$focal_baseline_age_contrast)

# 1/3 simulated 
ran3LRR.OSR <- PL.resNF.comm %>% filter(paired_data_id %in% c(sample(PL.resNF.comm$paired_data_id, 5))) %>%
  select(paired_data_id=paired_data_id, LRR.simu3=LRR.OSR)

ran3SecSamp <- PL.resNF.comm %>% filter(paired_data_id %!in% ran3LRR.OSR$paired_data_id)

ran3LRR.AMA <- ran3SecSamp %>% filter(paired_data_id %in% c(sample(ran3SecSamp$paired_data_id, 5))) %>%
  select(paired_data_id=paired_data_id, LRR.simu3=LRR.AMA)

ran3LRR.Shan <- ran3SecSamp %>% filter(paired_data_id %!in% ran3LRR.AMA$paired_data_id) %>% 
  select(paired_data_id=paired_data_id, LRR.simu3=LRR.Shan)

ran3LRR.simu <- bind_rows(ran3LRR.OSR, ran3LRR.AMA, ran3LRR.Shan)

PL.resNF.comm <- inner_join(PL.resNF.comm, ran3LRR.simu, by="paired_data_id")

# 1/5 simulated
ran4LRR.OSR <- PL.resNF.comm %>% filter(paired_data_id %in% c(sample(PL.resNF.comm$paired_data_id, 3))) %>%
  select(paired_data_id=paired_data_id, LRR.simu4=LRR.OSR)

ran4SecSamp <- PL.resNF.comm %>% filter(paired_data_id %!in% ran4LRR.OSR$paired_data_id)

ran4LRR.Rare <- ran4SecSamp %>% filter(paired_data_id %in% c(sample(ran4SecSamp$paired_data_id, 3))) %>%
  select(paired_data_id=paired_data_id, LRR.simu4=LRR.Rare)

ran4TrdSamp <- ran4SecSamp %>% filter(paired_data_id %!in% ran4LRR.Rare$paired_data_id)

ran4LRR.AMA <- ran4TrdSamp %>% filter(paired_data_id %in% c(sample(ran4TrdSamp$paired_data_id, 3))) %>%
  select(paired_data_id=paired_data_id, LRR.simu4=LRR.AMA)

ran4FourSamp <- ran4TrdSamp %>% filter(paired_data_id %!in% ran4LRR.AMA$paired_data_id)

ran4LRR.Shan <- ran4FourSamp %>% filter(paired_data_id %in% c(sample(ran4FourSamp$paired_data_id, 3))) %>% 
  select(paired_data_id=paired_data_id, LRR.simu4=LRR.Shan)

ran4LRR.Simp <- ran4FourSamp %>% filter(paired_data_id %!in% ran4LRR.Shan$paired_data_id) %>%
  select(paired_data_id=paired_data_id, LRR.simu4=LRR.Simp)

ran4LRR.simu <- bind_rows(ran4LRR.OSR, ran4LRR.Rare, ran4LRR.AMA, ran4LRR.Shan, ran4LRR.Simp)

PL.resNF.comm <- inner_join(PL.resNF.comm, ran4LRR.simu, by="paired_data_id")

# remove outliers to achieve normal distribution
# OSR
PL.resNF.comm.OSR.out <- boxplot(PL.resNF.comm$LRR.OSR)
sort(PL.resNF.comm.OSR.out$out)
PL.resNF.comm.OSR.subset <- subset(PL.resNF.comm, PL.resNF.comm$LRR.OSR > -1.027153)
PL.resNF.comm.OSR.subset <- PL.resNF.comm.OSR.subset %>% add_column(LRR.type = "Observed species richness")

# Rarefied
PL.resNF.comm.Rare.out <- boxplot(PL.resNF.comm$LRR.Rare)
sort(PL.resNF.comm.Rare.out$out)
PL.resNF.comm.Rare.subset <- subset(PL.resNF.comm, PL.resNF.comm$LRR.Rare < 100)
PL.resNF.comm.Rare.subset <- PL.resNF.comm.Rare.subset %>% add_column(LRR.type = "Rarefied species richness")

# AMA
PL.resNF.comm.AMA.out <- boxplot(PL.resNF.comm$LRR.AMA)
sort(PL.resNF.comm.AMA.out$out)
PL.resNF.comm.AMA.subset <- subset(PL.resNF.comm, PL.resNF.comm$LRR.AMA > -2.563923)
PL.resNF.comm.AMA.subset <- PL.resNF.comm.AMA.subset %>% add_column(LRR.type = "Arithmetic mean abundance")

# GMA
PL.resNF.comm.GMA.out <- boxplot(PL.resNF.comm$LRR.GMA)
sort(PL.resNF.comm.GMA.out$out)
PL.resNF.comm.GMA.subset <- subset(PL.resNF.comm, PL.resNF.comm$LRR.GMA < 0.1707204 & PL.resNF.comm$LRR.GMA > -0.9716707)
PL.resNF.comm.GMA.subset <- PL.resNF.comm.GMA.subset %>% add_column(LRR.type = "Geometric mean abundance")

# Shan
PL.resNF.comm.Shan.out <- boxplot(PL.resNF.comm$LRR.Shan)
sort(PL.resNF.comm.Shan.out$out)
PL.resNF.comm.Shan.subset <- subset(PL.resNF.comm, PL.resNF.comm$LRR.Shan > -100 & PL.resNF.comm$LRR.Shan < 100)
PL.resNF.comm.Shan.subset <- PL.resNF.comm.Shan.subset %>% add_column(LRR.type = "Shannon index")

# Simp
PL.resNF.comm.Simp.out <- boxplot(PL.resNF.comm$LRR.Simp)
sort(PL.resNF.comm.Simp.out$out)
PL.resNF.comm.Simp.subset <- subset(PL.resNF.comm, PL.resNF.comm$LRR.Simp > -0.3308425 & PL.resNF.comm$LRR.Simp < 0.1635030)
PL.resNF.comm.Simp.subset <- PL.resNF.comm.Simp.subset %>% add_column(LRR.type = "Simpson index")

# simu3
PL.resNF.comm.simu3.out <- boxplot(PL.resNF.comm$LRR.simu3)
sort(PL.resNF.comm.simu3.out$out)
PL.resNF.comm.simu3.subset <- subset(PL.resNF.comm, PL.resNF.comm$LRR.simu3 > -0.6211428 & PL.resNF.comm$LRR.simu3 < 0.5557865)
PL.resNF.comm.simu3.subset <- PL.resNF.comm.simu3.subset %>% add_column(LRR.type = "Simulated mixture 3")

# simu4
PL.resNF.comm.simu4.out <- boxplot(PL.resNF.comm$LRR.simu4)
sort(PL.resNF.comm.simu4.out$out)
PL.resNF.comm.simu4.subset <- subset(PL.resNF.comm, PL.resNF.comm$LRR.simu4 > -100)
PL.resNF.comm.simu4.subset <- PL.resNF.comm.simu4.subset %>% add_column(LRR.type = "Simulated mixture 4")

# fitting linear mixed model
# random effects:
# taxon: species' taxonomic identity as the highest-tier
# tree_cover_pair: the combination of tree covers
# study_id_streamlined: the site identity of the native forest
# baseline_datapoint_ID_overall_streamlined: the identity of the primary study
# paired_data_id: the identity of the ecological community as the lowest-tier nested random effect: 
# weighting scheme: record_weight_v3

# lme for OSR
lme.PL.resNF.comm.OSR <- lme(LRR.OSR ~ 1, random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall,
                            weights = ~I(1/weight), data = PL.resNF.comm.OSR.subset)

lower.PL.resNF.comm.OSR <- intervals(lme.PL.resNF.comm.OSR, which = "fixed")[[1]][[1,1]]
upper.PL.resNF.comm.OSR <- intervals(lme.PL.resNF.comm.OSR, which = "fixed")[[1]][[1,3]]

I2_R2_lme(lme.PL.resNF.comm.OSR)

# lme for Rarefied
lme.PL.resNF.comm.Rare <- lme(LRR.Rare ~ 1, random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall,
                              weights = ~I(1/weight), data = PL.resNF.comm.Rare.subset)

lower.PL.resNF.comm.Rare <- intervals(lme.PL.resNF.comm.Rare, which = "fixed")[[1]][[1,1]]
upper.PL.resNF.comm.Rare <- intervals(lme.PL.resNF.comm.Rare, which = "fixed")[[1]][[1,3]]

I2_R2_lme(lme.PL.resNF.comm.Rare)

# lme for AMA
lme.PL.resNF.comm.AMA <- lme(LRR.AMA ~ 1, random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall,
                             weights = ~I(1/weight), data = PL.resNF.comm.AMA.subset)

lower.PL.resNF.comm.AMA <- intervals(lme.PL.resNF.comm.AMA, which = "fixed")[[1]][[1,1]]
upper.PL.resNF.comm.AMA <- intervals(lme.PL.resNF.comm.AMA, which = "fixed")[[1]][[1,3]]

I2_R2_lme(lme.PL.resNF.comm.AMA)

# lme for GMA
lme.PL.resNF.comm.GMA <- lme(LRR.GMA ~ 1, random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall,
                             weights = ~I(1/weight), data = PL.resNF.comm.GMA.subset)

lower.PL.resNF.comm.GMA <- intervals(lme.PL.resNF.comm.GMA, which = "fixed")[[1]][[1,1]]
upper.PL.resNF.comm.GMA <- intervals(lme.PL.resNF.comm.GMA, which = "fixed")[[1]][[1,3]]

I2_R2_lme(lme.PL.resNF.comm.GMA)

# lme for Shan
lme.PL.resNF.comm.Shan <- lme(LRR.Shan ~ 1, random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall,
                             weights = ~I(1/weight), data = PL.resNF.comm.Shan.subset)

lower.PL.resNF.comm.Shan <- intervals(lme.PL.resNF.comm.Shan, which = "fixed")[[1]][[1,1]]
upper.PL.resNF.comm.Shan <- intervals(lme.PL.resNF.comm.Shan, which = "fixed")[[1]][[1,3]]

I2_R2_lme(lme.PL.resNF.comm.Shan)

# lme for Simp
lme.PL.resNF.comm.Simp <- lme(LRR.Simp ~ 1, random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall,
                              weights = ~I(1/weight), data = PL.resNF.comm.Simp.subset)

lower.PL.resNF.comm.Simp <- intervals(lme.PL.resNF.comm.Simp, which = "fixed")[[1]][[1,1]]
upper.PL.resNF.comm.Simp <- intervals(lme.PL.resNF.comm.Simp, which = "fixed")[[1]][[1,3]]

I2_R2_lme(lme.PL.resNF.comm.Simp)

# lme for simu3
lme.PL.resNF.comm.simu3 <- lme(LRR.simu3 ~ 1, random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall,
                                 weights = ~I(1/weight), data = PL.resNF.comm.simu3.subset)

lower.PL.resNF.comm.simu3 <- intervals(lme.PL.resNF.comm.simu3, which = "fixed")[[1]][[1,1]]
upper.PL.resNF.comm.simu3 <- intervals(lme.PL.resNF.comm.simu3, which = "fixed")[[1]][[1,3]]

I2_R2_lme(lme.PL.resNF.comm.simu3)

# lme for simu4
lme.PL.resNF.comm.simu4 <- lme(LRR.simu4 ~ 1, random = ~1|species_group/tree_cover_pair/study_id_overall,
                                 weights = ~I(1/weight), data = PL.resNF.comm.simu4.subset)

lower.PL.resNF.comm.simu4 <- intervals(lme.PL.resNF.comm.simu4, which = "fixed")[[1]][[1,1]]
upper.PL.resNF.comm.simu4 <- intervals(lme.PL.resNF.comm.simu4, which = "fixed")[[1]][[1,3]]

I2_R2_lme(lme.PL.resNF.comm.simu4)

# create a df to store estimated effect size, 95% CI (i.e. lower and upper bound)
df.PL.resNF.es <- data.frame("LRR.type" = character(0), "est.es" = integer(0),
                              "lower.es" = integer(0), "upper.es" = integer(0))

df.PL.resNF.es[nrow(df.PL.resNF.es) + 1,] <- c("Observed species richness", lme.PL.resNF.comm.OSR$coefficients[[1]], lower.PL.resNF.comm.OSR, upper.PL.resNF.comm.OSR)
df.PL.resNF.es[nrow(df.PL.resNF.es) + 1,] <- c("Rarefied species richness", lme.PL.resNF.comm.Rare$coefficients[[1]], lower.PL.resNF.comm.Rare, upper.PL.resNF.comm.Rare)
df.PL.resNF.es[nrow(df.PL.resNF.es) + 1,] <- c("Arithmetic mean abundance", lme.PL.resNF.comm.AMA$coefficients[[1]], lower.PL.resNF.comm.AMA, upper.PL.resNF.comm.AMA)
df.PL.resNF.es[nrow(df.PL.resNF.es) + 1,] <- c("Geometric mean abundance", lme.PL.resNF.comm.GMA$coefficients[[1]], lower.PL.resNF.comm.GMA, upper.PL.resNF.comm.GMA)
df.PL.resNF.es[nrow(df.PL.resNF.es) + 1,] <- c("Shannon index", lme.PL.resNF.comm.Shan$coefficients[[1]], lower.PL.resNF.comm.Shan, upper.PL.resNF.comm.Shan)
df.PL.resNF.es[nrow(df.PL.resNF.es) + 1,] <- c("Simpson index", lme.PL.resNF.comm.Simp$coefficients[[1]], lower.PL.resNF.comm.Simp, upper.PL.resNF.comm.Simp)
df.PL.resNF.es[nrow(df.PL.resNF.es) + 1,] <- c("Simulated mixture 3", lme.PL.resNF.comm.simu3$coefficients[[1]], lower.PL.resNF.comm.simu3, upper.PL.resNF.comm.simu3)
df.PL.resNF.es[nrow(df.PL.resNF.es) + 1,] <- c("Simulated mixture 4", lme.PL.resNF.comm.simu4$coefficients[[1]], lower.PL.resNF.comm.simu4, upper.PL.resNF.comm.simu4)

df.PL.resNF.es$est.es <- as.numeric(df.PL.resNF.es$est.es)
df.PL.resNF.es$lower.es <- as.numeric(df.PL.resNF.es$lower.es)
df.PL.resNF.es$upper.es <- as.numeric(df.PL.resNF.es$upper.es)

# calculate difference percentage
df.PL.resNF.es <- df.PL.resNF.es %>% mutate(perc.est.es = (exp(est.es) - 1) *100,
                                             perc.lower.es = (exp(lower.es) - 1) *100,
                                             perc.upper.es = (exp(upper.es) -1) *100)

cols.PL.resNF <- as.factor(df.PL.resNF.es$LRR.type)

panel.B.theme <- theme(legend.position = "none",
                  plot.title=element_text(size=8,face="bold",hjust = 0.5),
                  axis.title.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.title.y = element_text(size=8),
                  axis.text.y = element_text(colour = "black", size=8),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())

panel.B <- ggplot(NULL) + 
  geom_jitter(data = PL.resNF.comm.OSR.subset, aes(x=LRR.type, y=LRR.OSR, col=cols.PL.resNF[1], alpha=0.1), width = 0.2) + 
  geom_jitter(data = PL.resNF.comm.Rare.subset, aes(x=LRR.type, y=LRR.Rare, col=cols.PL.resNF[2], alpha=0.1), width = 0.2) + 
  geom_jitter(data = PL.resNF.comm.AMA.subset, aes(x=LRR.type, y=LRR.AMA, col=cols.PL.resNF[3], alpha=0.1), width = 0.2) + 
  geom_jitter(data = PL.resNF.comm.GMA.subset, aes(x=LRR.type, y=LRR.GMA, col=cols.PL.resNF[4], alpha=0.1), width = 0.2) + 
  geom_jitter(data = PL.resNF.comm.Shan.subset, aes(x=LRR.type, y=LRR.Shan, col=cols.PL.resNF[5], alpha=0.1), width = 0.2) + 
  geom_jitter(data = PL.resNF.comm.Simp.subset, aes(x=LRR.type, y=LRR.Simp, col=cols.PL.resNF[6], alpha=0.1), width = 0.2) + 
  geom_jitter(data = PL.resNF.comm.simu3.subset, aes(x=LRR.type, y=LRR.simu3, col=cols.PL.resNF[7], alpha=0.1), width = 0.2) + 
  geom_jitter(data = PL.resNF.comm.simu4.subset, aes(x=LRR.type, y=LRR.simu4, col=cols.PL.resNF[8], alpha=0.1), width = 0.2) + 
  geom_pointrange(data = df.PL.resNF.es, aes(x=LRR.type, y=est.es, ymin=lower.es, ymax=upper.es, fill=cols.PL.resNF, stroke=1), shape=23, size=0.5) +
  geom_hline(yintercept=0, linetype=2) + ylim(-2.5, 2.5) + coord_flip() +
  theme_bw() + labs(x='', y='Effect size (RR)', title='Plantations v.s. restored native forests')
  
panel.B + panel.B.theme

########## 2.2 meta-regression of PL vs resNF ##########
# lme for OSR
PL.resNF.comm.OSR.reg <- PL.resNF.comm.OSR.subset %>% filter(focal_baseline_age_contrast != "NA" & MAT != "NA")

glb.PL.resNF.comm.OSR.reg <- lme(LRR.OSR ~ log(MAT),
                            random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall,
                            weights = ~I(1/weight), data = PL.resNF.comm.OSR.reg)

mod.glb.PL.resNF.comm.OSR.reg <- dredge(glb.PL.resNF.comm.OSR.reg, trace = TRUE, rank = "AICc", REML = FALSE)
mod.glb.PL.resNF.comm.OSR.reg 

# lme for Rarefied
glb.PL.resNF.comm.Rare.reg <- lme(LRR.Rare ~ log(MAT),
                            random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id,
                            weights = ~I(1/weight), data = PL.resNF.comm.Rare.subset)

mod.glb.PL.resNF.comm.Rare.reg <- dredge(glb.PL.resNF.comm.Rare.reg, trace = TRUE, rank = "AICc", REML = FALSE)
mod.glb.PL.resNF.comm.Rare.reg # no variable was identified

lme.PL.resNF.comm.Rare.reg <- lme(LRR.Rare ~ log(MAT),
                                  random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id,
                                  weights = ~I(1/weight), data = PL.resNF.comm.Rare.subset)

summary(lme.PL.resNF.comm.Rare.reg)

# lme for AMA
glb.PL.resNF.comm.AMA.reg <- lme(LRR.AMA ~ log(MAT),
                                 random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id,
                                 weights = ~I(1/weight), data = PL.resNF.comm.AMA.subset)

mod.glb.PL.resNF.comm.AMA.reg <- dredge(glb.PL.resNF.comm.AMA.reg, trace = TRUE, rank = "AICc", REML = FALSE)
mod.glb.PL.resNF.comm.AMA.reg # no variable was identified

lme.PL.resNF.comm.AMA.reg <- lme(LRR.AMA ~ log(MAT),
                                 random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id,
                                 weights = ~I(1/weight), data = PL.resNF.comm.AMA.subset)

summary(lme.PL.resNF.comm.AMA.reg)

# lme for GMA
glb.PL.resNF.comm.GMA.reg <- lme(LRR.GMA ~ log(MAT),
                                 random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id,
                                 weights = ~I(1/weight), data = PL.resNF.comm.GMA.subset)

mod.glb.PL.resNF.comm.GMA.regA <- dredge(glb.PL.resNF.comm.GMA.reg, trace = TRUE, rank = "AICc", REML = FALSE)
mod.glb.PL.resNF.comm.GMA.regA # no variable was identified

# lme for Shan
glb.PL.resNF.comm.Shan.reg <- lme(LRR.Shan ~ log(MAT),
                             random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall,
                             weights = ~I(1/weight), data = PL.resNF.comm.Shan.subset)

mod.glb.PL.resNF.comm.Shan.reg <- dredge(glb.PL.resNF.comm.Shan.reg, trace = TRUE, rank = "AICc", REML = FALSE)
mod.glb.PL.resNF.comm.Shan.reg # no variable was identified

# lme for Simp
glb.PL.resNF.comm.Simp.reg <- lme(LRR.Simp ~ log(MAT),
                                  random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id,
                                  weights = ~I(1/weight), data = PL.resNF.comm.Simp.subset)

mod.glb.PL.resNF.comm.Simp.reg <- dredge(glb.PL.resNF.comm.Simp.reg, trace = TRUE, rank = "AICc", REML = FALSE)
mod.glb.PL.resNF.comm.Simp.reg

lme.PL.resNF.comm.Simp.reg <- lme(LRR.Simp ~ log(MAT),
                                  random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id,
                                  weights = ~I(1/weight), data = PL.resNF.comm.Simp.subset)

summary(lme.PL.resNF.comm.Simp.reg)

# simu3
glb.PL.resNF.comm.simu3.reg <- lme(LRR.simu3 ~ log(MAT),
                                  random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall,
                                  weights = ~I(1/weight), data = PL.resNF.comm.simu3.subset)

mod.glb.PL.resNF.comm.simu3.reg <- dredge(glb.PL.resNF.comm.simu3.reg, trace = TRUE, rank = "AICc", REML = FALSE)
mod.glb.PL.resNF.comm.simu3.reg

# simu4
glb.PL.resNF.comm.simu4.reg <- lme(LRR.simu4 ~ log(MAT),
                                   random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id,
                                   weights = ~I(1/weight), data = PL.resNF.comm.simu4.subset)

mod.glb.PL.resNF.comm.simu4.reg <- dredge(glb.PL.resNF.comm.simu4.reg, trace = TRUE, rank = "AICc", REML = FALSE)
mod.glb.PL.resNF.comm.simu4.reg

########## 3.1 meta-analysis of SF vs OGF ##########
SF.OGF.raw <- read_xlsx("../../2. BES native forest restoration/Biodiversity/Miao/Species_abundance_SF_vs_OGF_26_Sep_2022.xlsx", sheet = 1)

SF.OGF.raw <- SF.OGF.raw %>% filter(!is.na(focal_raw_count) & !is.na(baseline_raw_count))

# inspect non-integer numbers in focal_raw_count and baseline_raw_count
non.integer.2 <- data.frame(SF.OGF.raw$focal_raw_count %% 1 == 0)

SF.OGF.raw <- SF.OGF.raw %>% filter(paired_data_id != "B_12_Set1_1_1_1" &
                                paired_data_id != "B_31_Set1_1_1_1")

# subset data
# (1) paired_data_id: the identity of the ecological community;
# (2) focal_forest_type
# (3) baseline_datapoint_ID_overall: the identity of the primary study;
# (4) tree_cover_pair: the combination of tree covers;
# (5) focal_count_streamlined: sampling-effort-corrected species abundance of focal community;
# (6) baseline_count_streamlined: sampling-effort-corrected species abundance of baseline community;
# (7) logR: response ration based on geometric mean species abundance;

# convert abundance to presence-absence 
SF.OGF.comm <- SF.OGF.raw %>% mutate(focal.sp = if_else(.$focal_count_adjusted > 0, 1, 0)) %>%     
  mutate(baseline.sp = if_else(.$baseline_count_adjusted > 0, 1, 0))

SF.OGF.comm <- SF.OGF.comm %>% group_by(paired_data_id) %>% 
  summarise(OSR.focal = sum(focal.sp), # Observed species richness (OSR) of focal community
            OSR.baseline = sum(baseline.sp), # Observed species richness (OSR) of baseline community
            AMA.focal = mean(focal_count_adjusted), # Arithmetic mean abundance of focal community
            AMA.baseline = mean(baseline_count_adjusted), # Arithmetic mean abundance of baseline community
            LRR.GMA = mean(logR), # GMA adding min.comm to zero focal or baseline count
            Shan.focal = diversity(focal_count_adjusted, index = "shannon"), # Shannon index of focal community
            Shan.baseline = diversity(baseline_count_adjusted, index = "shannon"), # Shannon index of baseline community
            Simp.focal = diversity(focal_count_adjusted, index = "simpson"), # Simpson index of focal community
            Simp.baseline = diversity(baseline_count_adjusted, index = "simpson"), # Simpson index of baseline community
            .groups = "drop")

# create an empty df to store Chao1 species richness
df.SF.OGF.comm.Rare <- data.frame("paired_data_id" = character(0), "Rare.focal" = integer(0),
                                   "Rare.baseline" = integer(0))

# divide df by paired_data_id
X.SF.OGF.comm <- split(SF.OGF.raw, SF.OGF.raw$paired_data_id, drop = TRUE)
# create a list of dataframe by paired_data_id
Y.SF.OGF.comm <- lapply(seq_along(X.SF.OGF.comm), function(x) as.data.frame(X.SF.OGF.comm[[x]])[,c("paired_data_id", "focal_raw_count", "baseline_raw_count")])

# calculate abundance-based Chao1 species richness
for (i in 1:length(Y.SF.OGF.comm)){
  df <- data.frame(Y.SF.OGF.comm[[i]])
  df <- data.frame(transpose(df))
  df <- data.frame(sapply(df[2:3,], as.numeric))
  minCounts <- min(rowSums(df))
  Srare <- rarefy(df, minCounts)
  df.SF.OGF.comm.Rare[nrow(df.SF.OGF.comm.Rare) + 1,] <- c(as.character(Y.SF.OGF.comm[[i]][,1][[1]]),
                                                             Srare[1], Srare[2])
  
}

df.SF.OGF.comm.Rare$Rare.focal <- as.numeric(df.SF.OGF.comm.Rare$Rare.focal)
df.SF.OGF.comm.Rare$Rare.baseline <- as.numeric(df.SF.OGF.comm.Rare$Rare.baseline)

# join Rare species richness to PL.resNF.1.com by column paired_data_id
# inspect if any cells are zero
SF.OGF.comm <- inner_join(SF.OGF.comm, df.SF.OGF.comm.Rare, by = "paired_data_id")

# calculate log response ratio (LRR) as effect size
SF.OGF.comm <- SF.OGF.comm %>% mutate(LRR.OSR = log(OSR.focal/OSR.baseline),
                                      LRR.Rare = log(Rare.focal/Rare.baseline),    
                                      LRR.AMA = log(AMA.focal/AMA.baseline),
                                      LRR.Shan = log(Shan.focal/Shan.baseline),
                                      LRR.Simp = log(Simp.focal/Simp.baseline))

# add needed columns
SF.OGF.comm.cols <- SF.OGF.raw %>% group_by(tree_cover_pair, study_id, study_id_overall,
                                         paired_data_id, focal_forest_age_streamlined, MAT,
                                       focal_forest_type, baseline_datapoint_ID_overall,
                                       species_group, record_weight_v1, latitude, longitude) %>% 
  summarise(weight = mean(record_weight_v1), # weighting score
            .groups = "drop")

SF.OGF.comm.cols$paired_data_id <- as.factor(SF.OGF.comm.cols$paired_data_id)

SF.OGF.comm <- inner_join(SF.OGF.comm, SF.OGF.comm.cols, by="paired_data_id")

SF.OGF.comm <- distinct(SF.OGF.comm, paired_data_id, .keep_all = TRUE)

SF.OGF.comm$paired_data_id <- as.factor(SF.OGF.comm$paired_data_id)
SF.OGF.comm$focal_forest_type <- as.factor(SF.OGF.comm$focal_forest_type)
SF.OGF.comm$baseline_datapoint_ID_overall <- as.factor(SF.OGF.comm$baseline_datapoint_ID_overall)
SF.OGF.comm$species_group <- as.factor(SF.OGF.comm$species_group)

# 1/3 each Simulated mixture 1
set.seed(123)

ran5LRR.OSR <- SF.OGF.comm %>% filter(paired_data_id %in% c(sample(SF.OGF.comm$paired_data_id, 65))) %>%
  select(paired_data_id=paired_data_id, LRR.simu5=LRR.OSR)

ran5SecSamp <- SF.OGF.comm %>% filter(paired_data_id %!in% ran5LRR.OSR$paired_data_id)

ran5LRR.Shan <- ran5SecSamp %>% filter(paired_data_id %in% c(sample(ran5SecSamp$paired_data_id, 65))) %>%
  select(paired_data_id=paired_data_id, LRR.simu5=LRR.Shan)

ran5LRR.AMA <- ran5SecSamp %>% filter(paired_data_id %!in% ran5LRR.Shan$paired_data_id) %>%
  select(paired_data_id=paired_data_id, LRR.simu5=LRR.AMA)

ran5LRR.simu <- bind_rows(ran5LRR.OSR, ran5LRR.Shan, ran5LRR.AMA)

SF.OGF.comm <- inner_join(SF.OGF.comm, ran5LRR.simu, by="paired_data_id")

# 1/5 each of five metrics excluding GMA
set.seed(123)

ran6LRR.OSR <- SF.OGF.comm %>% filter(paired_data_id %in% c(sample(SF.OGF.comm$paired_data_id, 39))) %>%
  select(paired_data_id=paired_data_id, LRR.simu6=LRR.OSR)

ran6SecSamp <- SF.OGF.comm %>% filter(paired_data_id %!in% ran6LRR.OSR$paired_data_id)

ran6LRR.Rare <- ran6SecSamp %>% filter(paired_data_id %in% c(sample(ran6SecSamp$paired_data_id, 39))) %>%
  select(paired_data_id=paired_data_id, LRR.simu6=LRR.Rare)

ran6TrdSamp <- ran6SecSamp %>% filter(paired_data_id %!in% ran6LRR.Rare$paired_data_id)

ran6LRR.AMA <- ran6TrdSamp %>% filter(paired_data_id %in% c(sample(ran6TrdSamp$paired_data_id, 39))) %>%
  select(paired_data_id=paired_data_id, LRR.simu6=LRR.AMA)

ran6FourSamp <- ran6TrdSamp %>% filter(paired_data_id %!in% ran6LRR.AMA$paired_data_id)

ran6LRR.Shan <- ran6FourSamp %>% filter(paired_data_id %in% c(sample(ran6FourSamp$paired_data_id, 39))) %>%
  select(paired_data_id=paired_data_id, LRR.simu6=LRR.Shan)

ran6LRR.Simp <- ran6FourSamp %>% filter(paired_data_id %!in% ran6LRR.Shan$paired_data_id) %>%
  select(paired_data_id=paired_data_id, LRR.simu6=LRR.Simp)

ran6LRR.simu <- bind_rows(ran6LRR.OSR, ran6LRR.Rare, ran6LRR.Simp, ran6LRR.Shan, ran6LRR.AMA)

SF.OGF.comm <- inner_join(SF.OGF.comm, ran6LRR.simu, by="paired_data_id")

# remove outliers to acheive normal distribution
# OSR
SF.OGF.comm.OSR.out <- boxplot(SF.OGF.comm$LRR.OSR)
sort(SF.OGF.comm.OSR.out$out)
SF.OGF.comm.OSR.subset <- subset(SF.OGF.comm, SF.OGF.comm$LRR.OSR > -2.140066)
SF.OGF.comm.OSR.subset <- SF.OGF.comm.OSR.subset %>% add_column(LRR.type = "Observed species richness")

# Rare
SF.OGF.comm.Rare.out <- boxplot(SF.OGF.comm$LRR.Rare)
sort(SF.OGF.comm.Rare.out$out)
SF.OGF.comm.Rare.subset <- subset(SF.OGF.comm, SF.OGF.comm$LRR.Rare < 0.9162907 & SF.OGF.comm$LRR.Rare > -1.0487727)
SF.OGF.comm.Rare.subset <- SF.OGF.comm.Rare.subset %>% add_column(LRR.type = "Rarefied species richness")

# AMA
SF.OGF.comm.AMA.out <- boxplot(SF.OGF.comm$LRR.AMA)
sort(SF.OGF.comm.AMA.out$out)
SF.OGF.comm.AMA.subset <- subset(SF.OGF.comm, SF.OGF.comm$LRR.AMA < 1.545534 & SF.OGF.comm$LRR.AMA > -1.717353)
SF.OGF.comm.AMA.subset <- SF.OGF.comm.AMA.subset %>% add_column(LRR.type = "Arithmetic mean abundance")

# GMA
SF.OGF.comm.GMA.out <- boxplot(SF.OGF.comm$LRR.GMA)
sort(SF.OGF.comm.GMA.out$out)
SF.OGF.comm.GMA.subset <- subset(SF.OGF.comm, SF.OGF.comm$LRR.GMA < 1.337082 & SF.OGF.comm$LRR.GMA > -1.868798)
SF.OGF.comm.GMA.subset <- SF.OGF.comm.GMA.subset %>% add_column(LRR.type = "Geometric mean abundance")

# Shan
SF.OGF.comm.Shan.out <- boxplot(SF.OGF.comm$LRR.Shan)
sort(SF.OGF.comm.Shan.out$out)
SF.OGF.comm.Shan.subset <- subset(SF.OGF.comm, SF.OGF.comm$LRR.Shan > -0.6893613 & SF.OGF.comm$LRR.Shan < 0.4646697)
SF.OGF.comm.Shan.subset <- SF.OGF.comm.Shan.subset %>% add_column(LRR.type = "Shannon index")

# Simp
SF.OGF.comm.Simp.out <- boxplot(SF.OGF.comm$LRR.Simp)
sort(SF.OGF.comm.Simp.out$out)
SF.OGF.comm.Simp.subset <- subset(SF.OGF.comm, SF.OGF.comm$LRR.Simp > -0.2090753 & SF.OGF.comm$LRR.Simp < 0.1368271)
SF.OGF.comm.Simp.subset <- SF.OGF.comm.Simp.subset %>% add_column(LRR.type = "Simpson index")

# simu5
SF.OGF.comm.simu5.out <- boxplot(SF.OGF.comm$LRR.simu5)
sort(SF.OGF.comm.simu5.out$out)
SF.OGF.comm.simu5.subset <- subset(SF.OGF.comm, SF.OGF.comm$LRR.simu5 < 1.201303 & SF.OGF.comm$LRR.simu5 > -1.348073)
SF.OGF.comm.simu5.subset <- SF.OGF.comm.simu5.subset %>% add_column(LRR.type= "Simulated mixture 5")

# simu6
SF.OGF.comm.simu6.out <- boxplot(SF.OGF.comm$LRR.simu6)
sort(SF.OGF.comm.simu6.out$out)
SF.OGF.comm.simu6.subset <- subset(SF.OGF.comm, SF.OGF.comm$LRR.simu6 < 0.8495994 & SF.OGF.comm$LRR.simu6 > -1.0474962)
SF.OGF.comm.simu6.subset <- SF.OGF.comm.simu6.subset %>% add_column(LRR.type= "Simulated mixture 6")

# lme for OSR
lme.SF.OGF.comm.OSR <- lme(LRR.OSR ~ 1, random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id,
                            weights = ~I(1/weight), data = SF.OGF.comm.OSR.subset)

lower.SF.OGF.comm.OSR <- intervals(lme.SF.OGF.comm.OSR, which = "fixed")[[1]][[1,1]]
upper.SF.OGF.comm.OSR <- intervals(lme.SF.OGF.comm.OSR, which = "fixed")[[1]][[1,3]]

I2_R2_lme(lme.SF.OGF.comm.OSR)

# lme for Rare
lme.SF.OGF.comm.Rare <- lme(LRR.Rare ~ 1, random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id,
                              weights = ~I(1/weight), data = SF.OGF.comm.Rare.subset)

lower.SF.OGF.comm.Rare <- intervals(lme.SF.OGF.comm.Rare, which = "fixed")[[1]][[1,1]]
upper.SF.OGF.comm.Rare <- intervals(lme.SF.OGF.comm.Rare, which = "fixed")[[1]][[1,3]]

I2_R2_lme(lme.SF.OGF.comm.Rare)

# lme for AMA
lme.SF.OGF.comm.AMA <- lme(LRR.AMA ~ 1, random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id,
                           weights = ~I(1/weight), data = SF.OGF.comm.AMA.subset)

lower.SF.OGF.comm.AMA <- intervals(lme.SF.OGF.comm.AMA, which = "fixed")[[1]][[1,1]]
upper.SF.OGF.comm.AMA <- intervals(lme.SF.OGF.comm.AMA, which = "fixed")[[1]][[1,3]]

I2_R2_lme(lme.SF.OGF.comm.AMA)

# lme for GMA
lme.SF.OGF.comm.GMA <- lme(LRR.GMA ~ 1, random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id,
                           weights = ~I(1/weight), data = SF.OGF.comm.GMA.subset)

lower.SF.OGF.comm.GMA <- intervals(lme.SF.OGF.comm.GMA, which = "fixed")[[1]][[1,1]]
upper.SF.OGF.comm.GMA <- intervals(lme.SF.OGF.comm.GMA, which = "fixed")[[1]][[1,3]]

I2_R2_lme(lme.SF.OGF.comm.GMA)

# lme for Shan
lme.SF.OGF.comm.Shan <- lme(LRR.Shan ~ 1, random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id,
                             weights = ~I(1/weight), data = SF.OGF.comm.Shan.subset)

lower.SF.OGF.comm.Shan <- intervals(lme.SF.OGF.comm.Shan, which = "fixed")[[1]][[1,1]]
upper.SF.OGF.comm.Shan <- intervals(lme.SF.OGF.comm.Shan, which = "fixed")[[1]][[1,3]]

I2_R2_lme(lme.SF.OGF.comm.Shan)

# lme for Simp
lme.SF.OGF.comm.Simp <- lme(LRR.Simp ~ 1, random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id,
                            weights = ~I(1/weight), data = SF.OGF.comm.Simp.subset)

lower.SF.OGF.comm.Simp <- intervals(lme.SF.OGF.comm.Simp, which = "fixed")[[1]][[1,1]]
upper.SF.OGF.comm.Simp <- intervals(lme.SF.OGF.comm.Simp, which = "fixed")[[1]][[1,3]]

I2_R2_lme(lme.SF.OGF.comm.Simp)

# lme for simu5
lme.SF.OGF.comm.simu5 <- lme(LRR.simu5 ~ 1, random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall,
                             weights = ~I(1/weight), data = SF.OGF.comm.simu5.subset)

lower.SF.OGF.comm.simu5 <- intervals(lme.SF.OGF.comm.simu5, which = "fixed")[[1]][[1,1]]
upper.SF.OGF.comm.simu5 <- intervals(lme.SF.OGF.comm.simu5, which = "fixed")[[1]][[1,3]]

I2_R2_lme(lme.SF.OGF.comm.simu5)

# lme for simu6
lme.SF.OGF.comm.simu6 <- lme(LRR.simu6 ~ 1, random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id,
                               weights = ~I(1/weight), data = SF.OGF.comm.simu6.subset)

lower.SF.OGF.comm.simu6 <- intervals(lme.SF.OGF.comm.simu6, which = "fixed")[[1]][[1,1]]
upper.SF.OGF.comm.simu6 <- intervals(lme.SF.OGF.comm.simu6, which = "fixed")[[1]][[1,3]]

I2_R2_lme(lme.SF.OGF.comm.simu6)

df.SF.OGF.comm.es <- data.frame("LRR.type" = character(0), "est.es" = integer(0),
                              "lower.es" = integer(0), "upper.es" = integer(0))

df.SF.OGF.comm.es[nrow(df.SF.OGF.comm.es) + 1,] <- c("Observed species richness", lme.SF.OGF.comm.OSR$coefficients[[1]], lower.SF.OGF.comm.OSR, upper.SF.OGF.comm.OSR)
df.SF.OGF.comm.es[nrow(df.SF.OGF.comm.es) + 1,] <- c("Rarefied species richness", lme.SF.OGF.comm.Rare$coefficients[[1]], lower.SF.OGF.comm.Rare, upper.SF.OGF.comm.Rare)
df.SF.OGF.comm.es[nrow(df.SF.OGF.comm.es) + 1,] <- c("Arithmetic mean abundance", lme.SF.OGF.comm.AMA$coefficients[[1]], lower.SF.OGF.comm.AMA, upper.SF.OGF.comm.AMA)
df.SF.OGF.comm.es[nrow(df.SF.OGF.comm.es) + 1,] <- c("Geometric mean abundance", lme.SF.OGF.comm.GMA$coefficients[[1]], lower.SF.OGF.comm.GMA, upper.SF.OGF.comm.GMA)
df.SF.OGF.comm.es[nrow(df.SF.OGF.comm.es) + 1,] <- c("Shannon index", lme.SF.OGF.comm.Shan$coefficients[[1]], lower.SF.OGF.comm.Shan, upper.SF.OGF.comm.Shan)
df.SF.OGF.comm.es[nrow(df.SF.OGF.comm.es) + 1,] <- c("Simpson index", lme.SF.OGF.comm.Simp$coefficients[[1]], lower.SF.OGF.comm.Simp, upper.SF.OGF.comm.Simp)
df.SF.OGF.comm.es[nrow(df.SF.OGF.comm.es) + 1,] <- c("Simulated mixture 5", lme.SF.OGF.comm.simu5$coefficients[[1]], lower.SF.OGF.comm.simu5, upper.SF.OGF.comm.simu5)
df.SF.OGF.comm.es[nrow(df.SF.OGF.comm.es) + 1,] <- c("Simulated mixture 6", lme.SF.OGF.comm.simu6$coefficients[[1]], lower.SF.OGF.comm.simu6, upper.SF.OGF.comm.simu6)

df.SF.OGF.comm.es$est.es <- as.numeric(df.SF.OGF.comm.es$est.es)
df.SF.OGF.comm.es$lower.es <- as.numeric(df.SF.OGF.comm.es$lower.es)
df.SF.OGF.comm.es$upper.es <- as.numeric(df.SF.OGF.comm.es$upper.es)

cols.SF.OGF.comm <- as.factor(df.SF.OGF.comm.es$LRR.type)

panel.C.theme <- theme(legend.position = "none",
                       plot.title=element_text(size=8,face="bold",hjust = 0.5),
                       axis.text.x = element_text(colour = "black", size = 8),
                       axis.title.y = element_text(size=8),
                       axis.text.y = element_text(colour = "black", size=8),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank())

panel.C <- ggplot(NULL) + 
  geom_jitter(data = SF.OGF.comm.OSR.subset, aes(x=LRR.type, y=LRR.OSR, col=cols.SF.OGF.comm[1], alpha=0.2), width = 0.2) + 
  geom_jitter(data = SF.OGF.comm.Rare.subset, aes(x=LRR.type, y=LRR.Rare, col=cols.SF.OGF.comm[2], alpha=0.2), width = 0.2) + 
  geom_jitter(data = SF.OGF.comm.AMA.subset, aes(x=LRR.type, y=LRR.AMA, col=cols.SF.OGF.comm[3], alpha=0.2), width = 0.2) + 
  geom_jitter(data = SF.OGF.comm.GMA.subset, aes(x=LRR.type, y=LRR.GMA, col=cols.SF.OGF.comm[4], alpha=0.2), width = 0.2) + 
  geom_jitter(data = SF.OGF.comm.Shan.subset, aes(x=LRR.type, y=LRR.Shan, col=cols.SF.OGF.comm[5], alpha=0.2), width = 0.2) + 
  geom_jitter(data = SF.OGF.comm.Simp.subset, aes(x=LRR.type, y=LRR.Simp, col=cols.SF.OGF.comm[6], alpha=0.2), width = 0.2) + 
  geom_jitter(data = SF.OGF.comm.simu5.subset, aes(x=LRR.type, y=LRR.simu5, col=cols.SF.OGF.comm[7], alpha=0.2), width = 0.2) + 
  geom_jitter(data = SF.OGF.comm.simu6.subset, aes(x=LRR.type, y=LRR.simu6, col=cols.SF.OGF.comm[8], alpha=0.2), width = 0.2) + 
  geom_pointrange(data = df.SF.OGF.comm.es, aes(x=LRR.type, y=est.es, ymin=lower.es, ymax=upper.es, fill=cols.SF.OGF.comm, stroke=1), shape=23, size=0.5) +
  geom_hline(yintercept=0, linetype=2)  + ylim(-2.5, 2.5) + coord_flip() +
  theme_bw() + labs(x='', y='', title='Restored native forests v.s. old-growth forests')

panel.C + panel.C.theme

########## 3.2 meta-regression of SF vs OGF ##########
# plot lme for OSR
SF.OGF.comm.OSR.subset.2 <- SF.OGF.comm.OSR.subset %>% filter(!is.na(focal_forest_age_streamlined) & !is.na(MAT))

glb.SF.OGF.comm.OSR.subset.2 <- lme(LRR.OSR ~ log(focal_forest_age_streamlined) + log(MAT) +
                             log(MAT) * log(focal_forest_age_streamlined),
                           random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id, 
                           weights = ~I(1/weight),
                           data=SF.OGF.comm.OSR.subset.2)

mod.glb.SF.OGF.comm.OSR.subset.2 <- dredge(glb.SF.OGF.comm.OSR.subset.2, trace = TRUE, rank = "AICc", REML = FALSE)
mod.glb.SF.OGF.comm.OSR.subset.2 # no variable was identified

# plot lme for Rarefied richness
SF.OGF.comm.Rare.subset.2 <- SF.OGF.comm.Rare.subset %>% filter(!is.na(focal_forest_age_streamlined) & !is.na(MAT))

glb.SF.OGF.comm.Rare.subset.2 <- lme(LRR.Rare ~ log(focal_forest_age_streamlined) +log(MAT) +
                                    log(MAT) * log(focal_forest_age_streamlined),
                                     random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id, 
                                     weights = ~I(1/weight),
                                     data=SF.OGF.comm.Rare.subset.2)

mod.glb.SF.OGF.comm.Rare.subset.2 <- dredge(glb.SF.OGF.comm.Rare.subset.2, trace = TRUE, rank = "AICc", REML = FALSE)
mod.glb.SF.OGF.comm.Rare.subset.2 # focal_forest_age_streamlined was identified

lme.SF.OGF.comm.Rare.subset.2 <- lme(LRR.Rare ~ log(focal_forest_age_streamlined),
                                     random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id, 
                                     weights = ~I(1/weight),
                                     data=SF.OGF.comm.Rare.subset.2)

summary(lme.SF.OGF.comm.Rare.subset.2)

# lme for Shannon
SF.OGF.comm.Shan.subset.2 <- SF.OGF.comm.Shan.subset %>% filter(!is.na(focal_forest_age_streamlined) & !is.na(MAT))

glb.SF.OGF.comm.Shan.subset.2 <- lme(LRR.Shan ~ log(focal_forest_age_streamlined) + log(MAT) +
                                        log(MAT) * log(focal_forest_age_streamlined),
                                      random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall, 
                                      weights = ~I(1/weight),
                                      data=SF.OGF.comm.Shan.subset.2)

mod.glb.SF.OGF.comm.Shan.subset.2 <- dredge(glb.SF.OGF.comm.Shan.subset.2, trace = TRUE, rank = "AICc", REML = FALSE)
mod.glb.SF.OGF.comm.Shan.subset.2 # no variable was identified

# lme for Simpson
SF.OGF.comm.Simp.subset.2 <- SF.OGF.comm.Simp.subset %>% filter(!is.na(focal_forest_age_streamlined) & !is.na(MAT))

glb.SF.OGF.comm.Simp.subset.2 <- lme(LRR.Simp ~ log(focal_forest_age_streamlined) + log(MAT) +
                                        log(MAT) * log(focal_forest_age_streamlined),
                                      random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id, 
                                      weights = ~I(1/weight),
                                      data=SF.OGF.comm.Simp.subset.2)

mod.glb.SF.OGF.comm.Simp.subset.2 <- dredge(glb.SF.OGF.comm.Simp.subset.2, trace = TRUE, rank = "AICc", REML = FALSE)
mod.glb.SF.OGF.comm.Simp.subset.2 # 

#
spon.SF.OGF.comm.Simp.subset.2 <- SF.OGF.comm.Simp.subset.2 %>% filter(focal_forest_type == "spontaneously_natural_regeneration")
ass.SF.OGF.comm.Simp.subset.2 <- SF.OGF.comm.Simp.subset.2 %>% filter(focal_forest_type == "assisted_natural_regeneration")
act.SF.OGF.comm.Simp.subset.2 <- SF.OGF.comm.Simp.subset.2 %>% filter(focal_forest_type == "actively_native_planting")

glb.spon.SF.OGF.comm.Simp.subset.2 <- lme(LRR.Simp ~ log(focal_forest_age_streamlined) + log(MAT) +
                                            log(MAT) * log(focal_forest_age_streamlined),
                                          random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall, 
                                          weights = ~I(1/weight),
                                          data=spon.SF.OGF.comm.Simp.subset.2)

mod.glb.spon.SF.OGF.comm.Simp.subset.2 <- dredge(glb.spon.SF.OGF.comm.Simp.subset.2, trace = TRUE, rank = "AICc", REML = FALSE)
mod.glb.spon.SF.OGF.comm.Simp.subset.2

glb.ass.SF.OGF.comm.Simp.subset.2 <- lme(LRR.Simp ~ log(focal_forest_age_streamlined) + log(MAT) +
                                            log(MAT) * log(focal_forest_age_streamlined),
                                          random = ~1|species_group/tree_cover_pair/study_id_overall, 
                                          weights = ~I(1/weight),
                                          data=ass.SF.OGF.comm.Simp.subset.2)

mod.glb.ass.SF.OGF.comm.Simp.subset.2 <- dredge(glb.ass.SF.OGF.comm.Simp.subset.2, trace = TRUE, rank = "AICc", REML = FALSE)
mod.glb.ass.SF.OGF.comm.Simp.subset.2

glb.act.SF.OGF.comm.Simp.subset.2 <- lme(LRR.Simp ~ focal_forest_age_streamlined +
                                            log(focal_forest_age_streamlined) + MAT + log(MAT) +
                                            MAT * focal_forest_age_streamlined,
                                          random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall, 
                                          weights = ~I(1/weight),
                                          data=act.SF.OGF.comm.Simp.subset.2)

mod.glb.act.SF.OGF.comm.Simp.subset.2 <- dredge(glb.act.SF.OGF.comm.Simp.subset.2, trace = TRUE, rank = "AICc", REML = FALSE)
mod.glb.act.SF.OGF.comm.Simp.subset.2

# lme for Arithmetic mean abundance
SF.OGF.comm.AMA.subset.2 <- SF.OGF.comm.AMA.subset %>% filter(!is.na(focal_forest_age_streamlined) & !is.na(MAT))

glb.SF.OGF.comm.AMA.subset.2 <- lme(LRR.AMA ~ log(focal_forest_age_streamlined) + log(MAT) +
                                        log(MAT) * log(focal_forest_age_streamlined),
                                      random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id, 
                                      weights = ~I(1/weight),
                                      data=SF.OGF.comm.AMA.subset.2)

mod.glb.SF.OGF.comm.AMA.subset.2 <- dredge(glb.SF.OGF.comm.AMA.subset.2, trace = TRUE, rank = "AICc", REML = FALSE)
mod.glb.SF.OGF.comm.AMA.subset.2 # focal_forest_age_streamlined, focal_forest_age_streamlined^2 was identified

lme.SF.OGF.comm.AMA.subset.2 <- lme(LRR.AMA ~ log(focal_forest_age_streamlined),
                                      random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id, 
                                      weights = ~I(1/weight),
                                      data=SF.OGF.comm.AMA.subset.2)

summary(lme.SF.OGF.comm.AMA.subset.2)

# lme for Geometric mean abundance
SF.OGF.comm.GMA.subset.2 <- SF.OGF.comm.GMA.subset %>% filter(!is.na(focal_forest_age_streamlined) & !is.na(MAT))

glb.SF.OGF.comm.GMA.subset.2 <- lme(LRR.GMA ~ log(focal_forest_age_streamlined) + log(MAT) +
                                      log(MAT) * log(focal_forest_age_streamlined),
                                     random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id, 
                                     weights = ~I(1/weight),
                                     data=SF.OGF.comm.GMA.subset.2)

mod.glb.SF.OGF.comm.GMA.subset.2 <- dredge(glb.SF.OGF.comm.GMA.subset.2, trace = TRUE, rank = "AICc", REML = FALSE)
mod.glb.SF.OGF.comm.GMA.subset.2 # none was identified

# lme for simu5
SF.OGF.comm.simu5.subset.2 <- SF.OGF.comm.simu5.subset %>% filter(!is.na(focal_forest_age_streamlined) & !is.na(MAT))

glb.SF.OGF.comm.simu5.subset.2 <- lme(LRR.simu5 ~ log(focal_forest_age_streamlined) + log(MAT) +
                                      log(MAT) * log(focal_forest_age_streamlined),
                                    random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id, 
                                    weights = ~I(1/weight),
                                    data=SF.OGF.comm.simu5.subset.2)

mod.glb.SF.OGF.comm.simu5.subset.2 <- dredge(glb.SF.OGF.comm.simu5.subset.2, trace = TRUE, rank = "AICc", REML = FALSE)
mod.glb.SF.OGF.comm.simu5.subset.2 # none was identified

# lme for simu6
SF.OGF.comm.simu6.subset.2 <- SF.OGF.comm.simu6.subset %>% filter(!is.na(focal_forest_age_streamlined) & !is.na(MAT))

glb.SF.OGF.comm.simu6.subset.2 <- lme(LRR.simu6 ~ log(focal_forest_age_streamlined) + log(MAT) +
                                        log(MAT) * log(focal_forest_age_streamlined),
                                      random = ~1|species_group/tree_cover_pair/study_id_overall/baseline_datapoint_ID_overall/paired_data_id, 
                                      weights = ~I(1/weight),
                                      data=SF.OGF.comm.simu6.subset.2)

mod.glb.SF.OGF.comm.simu6.subset.2 <- dredge(glb.SF.OGF.comm.simu6.subset.2, trace = TRUE, rank = "AICc", REML = FALSE)
mod.glb.SF.OGF.comm.simu6.subset.2 # none was identified

########## Fig.2 ##########
PL.resNF.map <- PL.resNF.comm %>% mutate(latitude = as.numeric(latitude), 
                                    longitude = as.numeric(longitude)) %>%
  group_by(latitude, longitude) %>%
  summarise(num = length(unique(paired_data_id))) 

PL.NFben.map <- PL.NFben.comm %>% mutate(latitude = as.numeric(latitude), 
                                    longitude = as.numeric(longitude)) %>%
  group_by(latitude, longitude) %>%
  summarise(num = length(unique(paired_data_id)))

SF.OGF.map <- SF.OGF.comm %>% mutate(latitude = as.numeric(latitude), 
                                longitude = as.numeric(longitude)) %>%
  group_by(latitude, longitude) %>%
  summarise(num = length(unique(paired_data_id)))

glb_map <- map_data("world")

Fig.2 <- ggplot() + 
  geom_polygon(data = glb_map, aes (x=long, y =lat, group = group), fill = "light grey", alpha=0.5) + 
  coord_sf(ylim = c(-50,80), xlim = c(-180,180) ) + scale_radius(range = c(2,10)) + 
  geom_point(data = SF.OGF.map, aes(x=longitude, y = latitude, size = num), fill = "#9e9ac8", shape=21, alpha = 0.8, color="black", stroke=0.6) +
  geom_point(data = PL.NFben.map, aes(x=longitude, y = latitude, size = num), fill = "#008000", shape=21, alpha = 0.6, color="black", stroke=0.6) + 
  geom_point(data = PL.resNF.map, aes(x=longitude, y = latitude, size = num), fill = "#ff8c00", shape=21, alpha = 0.6, color="black", stroke=0.6) +
  theme(axis.title = element_blank(), axis.ticks = element_blank(),
        axis.text = element_blank(), panel.background = element_blank())


Fig.2

END