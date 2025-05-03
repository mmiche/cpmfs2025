library(tidyverse)
# https://modeldata.tidymodels.org/reference/sim_classification.html
library(tidymodels)

# Run (= active) all customized functions, all of which are stored in the script tidymodels_simAndCPM2Funs.R (Funs = Functions)
source("tidymodels_simAndCPM2Funs.R", echo = FALSE)

# ----------------------------------------
# Simulate binary dataset - start here.

set.seed(1)
# binDat = binary outcome dataset
binDat0 <- modeldata::sim_classification(
    num_samples = 100000,
    method = "caret",
    intercept = -5,
    num_linear = 20,
    keep_truth = FALSE
)
dim(binDat0)
names(binDat0)
# Remove nonlinear voriables
idx_nonLin <- grep("non", names(binDat0))
binDat <- binDat0[,-idx_nonLin]
tab2(binDat$class)
set.seed(1)
idxMinorClass <- sample(which(binDat$class=="class_1"), size=40000)
binDat1 <- binDat[-idxMinorClass,]
tab2(binDat1$class)
# Make major class = 0 and minor class = 1
binDat1$outcome <- 0
binDat1$outcome[binDat1$class=="class_1"] <- 1
tab2(binDat1$outcome)
# Remove variable 'class'
idx_class <- grep("class", names(binDat1))
binDat2 <- binDat1[,-idx_class]
# Check column names (no class, no non_linear)
names(binDat2)
head(binDat2)
rms::lrm(outcome ~ ., data=binDat2)
# Event per variable = 215 (well over 10-15)
sum(binDat2$outcome)/22
# Binary dataset - end (use that dataset: binDat2).

# Convert binDat2 from class tibble to class data.frame.
# Reason: Make it compatible with functions of mysml package.
binDat2 <- data.frame(binDat2)
# ----------------------------------------

# # Install mysml from GitHub
# devtools::install_github(repo="https://github.com/mmiche/mysml",
#                          dependencies = "Imports")
library(mysml)

set.seed(1)
# 5-fold CV = 5 80/20 splits; 10 seeds = 10 repetitions of 5 fold = 50 final test results.
seeds <- sample(1:10e6, size=10)
cvLs <- mysml::myRepeatedkFoldcv(data=binDat2, outcome="outcome", folds = 5, stratify = TRUE, seeds=seeds)

TrainLs <- cvLs$TrainLs
TestLs <- cvLs$TestLs

# head(TrainLs[[1]])

# Check stratification: Frequency of outcome (newAud) being closely around 3.15 percent, both in the training and in the test subsets of the total sample.
unique(unlist(lapply(TrainLs, FUN=function(x) mean(x$outcome)))) # 7.9%
unique(unlist(lapply(TestLs, FUN=function(x) mean(x$outcome)))) # 7.9%

# Make empty named list:
predProbsLs <- sapply(c("logreg", "rf"),function(x) NULL) #
fmla <- formula("outcome ~ .")
fmla.f <- formula("factor(outcome) ~ .")
startTime <- Sys.time()
for(m in 1:length(TrainLs)) {
    # Select training and test subset
    Train.m <- TrainLs[[m]]
    Test.m <- TestLs[[m]]
    # head(Train.m)
    
    # Logistic regression
    # -------------------
    logreg_m <- applyLogreg(dataTrain=Train.m, dataTest=Test.m, frmla = fmla, outcome = "outcome")
    # head(logreg_m$TestCV)
    predProbsLs[["logreg"]][[m]] <- logreg_m$TestCV
    
    # Random forest
    # -------------
    set.seed(1)
    rndmFrst_m <- applyRandomForest(dataTrain=Train.m, dataTest=Test.m, frmla.f = fmla.f, outcome = "outcome")
    predProbsLs[["rf"]][[m]] <- rndmFrst_m$TestCV
    
}
endTime <- Sys.time()
difftime(endTime, startTime) # Time difference of 48.01671 mins

# saveRDS(object=predProbsLs, file="predProbsLs20250502.rds")

predProbsLs <- readRDS(file="predProbsLs20250502.rds")

for(i in 1:length(predProbsLs$rf)) {
    predProbsLs$rf[[i]]$predicted <- winsorize_probs(x=predProbsLs$rf[[i]]$predicted)
}

# head(predProbsLs$logreg[[1]])
# head(predProbsLs$rf[[1]])

# ------------------------------------------------

# ?mysml::computeRelevantResults
relRes <- mysml::computeRelevantResults(
    predictionOutputLs = predProbsLs,
    # Select five thresholds
    dcaReasonableThresholds = c(.04, .05, .07, .09, .11))

# relRes$dcaLs$tableDCA[[1]]
# 
# relRes$orderedObsLs$logreg[[1]]

# Make the plot for logreg, first of 50 results:
(glmprobPlot <- plotAllPredProbs(
    data=relRes$orderedObsLs$logreg[[1]],
    thrsh = c(.04, .05, .07, .09, .11)))

# Make the plot for random forest (rf), first of 50 results:
(rfprobPlot <- plotAllPredProbs(
    data=relRes$orderedObsLs$rf[[1]],
    thrsh = c(.04, .05, .07, .09, .11)))
        

# Decision curve analysis (for a SINGLE cross-validation; exemplary)
# -----------------------
allDCA <- relRes$dcaLs$plotDCA[[1]]

# Plot the DCA
useColor <- c("Treat all" = "black", "Treat none" = "darkgrey",
              "Logistic regression" = "red", "Random forest" = "blue")
htbPlot <- function(x) paste0("1:", round((1-x)/x, digits=2))
# Make dca plot
(dcaPlot <- 
        ggplot(data=allDCA, aes(x=threshold, y=net_benefit, colour=label)) +
        geom_line(aes(colour=label), linewidth=1) +
        labs(color=NULL) +
        scale_x_continuous(
            sec.axis = dup_axis(name="Harm-to-benefit ratio", labels=htbPlot)) +
        # Take control of the y-axis: How much of the negative part shall be visible?
        coord_cartesian(ylim=c(-.01, .08)) +
        scale_colour_manual(values = useColor) +
        ylab(label="Net benefit") +
        theme(
            panel.background = element_blank(),
            axis.text.x=element_text(size=16),
            axis.title.x=element_text(size=16),
            axis.text.y=element_text(size=16),
            axis.title.y = element_text(size=16),
            panel.border = element_rect(color="black", fill=NA),
            legend.text = element_text(size=14),
            legend.position = "top",
            legend.title = element_blank()) +
        labs(x="Threshold probability"))


# Decision curve analysis (DCA)
# dcaSelectType: One out of these three options: mnci, mnminmax, mnq1q3.
# mnci: mean net benefit, with lower to upper 95 percent CI.
# mnminmax: mean net benefit, with minimum to maximum value.
# mnq1q3: mean net benefit, with 25th to 75th percentile.
allDCALs <- visualizeMultipleDCA(dcaLs = relRes$dcaLs, dcaSelectType = "mnminmax")
allDCALs$logregNB
allDCALs$rfNB

dcaPublLs <- plotDCALs(allDCA = allDCALs$allDCA, bothModels = TRUE)
dcaPublLs$dcaPlot2

# -------------------------------------------------
# Calibration

calibLRLs <- mysml::makeCalibPlotLs(calibLs=relRes$orderedObsLs, model="logreg")
calibRFLs <- mysml::makeCalibPlotLs(calibLs=relRes$orderedObsLs, model="rf")

calibLR <- mysml::avrgCalibPlotLs(calibPlotLs = calibLRLs, model="logreg")
calibRF <- mysml::avrgCalibPlotLs(calibPlotLs = calibRFLs, model="rf")

calibLRPlot <- mysml::compressCalibPlot(mnSpanLs = calibLR, model = "logreg")
calibRFPlot <- mysml::compressCalibPlot(mnSpanLs = calibRF, model = "rf")

min(calibLRPlot[,-5])
min(calibRFPlot[,-5])

max(calibLRPlot[,-5])
max(calibRFPlot[,-5])

lrCalAllSeeds <- calibPlot(calibData=calibLRPlot, calibXYmax = .5)

rfCalAllSeeds <- calibPlot(calibData=calibRFPlot, calibXYmax = .65)


# Calibration metrics (Note: I reject calibration metrics, I prefer the plot, because it shows all of the relevant calibration performance; enables more comprehensive evaluation than a single summary numeric result)
calibMetricsLs <- lapply(X=relRes$calibLs, FUN = function(x) {
    mysml::myCalib(calibDf=x, outcome = "observed")$calibPerfW
})
calibMetrics <- dplyr::bind_rows(calibMetrics)
colnames(calibMetrics)[9] <- "model"
rownames(calibMetrics) <- NULL

# saveRDS(object=calibMetrics, file="calibrationMetrics.rds")

calibMetrics <- readRDS(file="calibrationMetrics.rds")

# head(calibMetrics)

calibMetrics$model <- factor(calibMetrics$model, levels = c("logreg", "randomForest"))
calibBoxPlot <- 
    ggplot(data=calibMetrics, aes(x=model, y=Emax)) +
    geom_boxplot() +
    theme(panel.background = element_blank(),
          axis.text.x=element_text(size=16),
          axis.title.x=element_text(size=16),
          axis.text.y=element_text(size=16),
          axis.title.y = element_text(size=16),
          panel.border = element_rect(color="black", fill=NA))

# -------------------------------------------------
# Discrimination

logreg_auc <- unlist(lapply(predProbsLs[["logreg"]], FUN=function(x) {
    precrec::auc(precrec::evalmod(scores=x[,"predicted"], labels=x[,"observed"]))$aucs[1]
}))
summary(logreg_auc)

rf_auc <- unlist(lapply(predProbsLs[["rf"]], FUN=function(x) {
    precrec::auc(precrec::evalmod(scores=x[,"predicted"], labels=x[,"observed"]))$aucs[1]
}))
summary(rf_auc)
# -------------------------------------------------