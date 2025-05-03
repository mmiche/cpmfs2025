tab2 <- function(x=NULL) {
    list(table(x),
         prop.table(table(x)))
}

applyLogreg <- function(dataTrain=NULL, dataTest=NULL, frmla=NULL, outcome="y") {
    
    if(is.null(frmla)) {
        glmPred <- glm(y ~ ., family = binomial(link="logit"), data = dataTrain)
    } else {
        glmPred <- glm(frmla, family = binomial(link="logit"), data = dataTrain)
    }
    apparentCV <- predict(object=glmPred, newdata=dataTrain, type = "response")
    glmCV <- predict(object=glmPred, newdata=dataTest, type = "response")
    
    logregOut <- list()
    logregOut[["ApparentCV"]] <- data.frame(observed=dataTrain[,outcome],
                                            predicted=apparentCV,
                                            ids=rownames(dataTrain))
    logregOut[["TestCV"]] <- data.frame(observed=dataTest[,outcome],
                                        predicted=glmCV,
                                        ids=rownames(dataTest))
    return(logregOut)
}


applyRandomForest <- function(dataTrain=NULL, dataTest=NULL, frmla.f=NULL, outcome="y") {
    
    if(is.null(frmla.f)) {
        rfPred <- ranger::ranger(factor(y) ~ ., data = dataTrain, probability = TRUE)
    } else {
        rfPred <- ranger::ranger(frmla.f, data = dataTrain, probability = TRUE)
    }
    apparentCV <- predict(object=rfPred, data=dataTrain, type = "response")$predictions[,"1"]
    rfCV <- predict(object=rfPred, data=dataTest, type = "response")$predictions[,"1"]
    
    randomForestOut <- list()
    randomForestOut[["ApparentCV"]] <- data.frame(observed=dataTrain[,outcome],
                                                  predicted=apparentCV,
                                                  ids=rownames(dataTrain))
    randomForestOut[["TestCV"]] <- data.frame(observed=dataTest[,outcome],
                                              predicted=rfCV,
                                              ids=rownames(dataTest))
    return(randomForestOut)
}

plotAllPredProbs <- function(data=NULL, thrsh=NULL) {
    p0 <- 
        ggplot(data=data, aes(x=predicted, y=id, col=observed)) +
        geom_point() +
        theme(
            panel.background = element_blank(),
            axis.text.x=element_text(size=16),
            axis.title.x=element_text(size=16),
            axis.text.y=element_text(size=16),
            axis.title.y = element_text(size=16),
            legend.text = element_text(size=14),
            legend.title = element_text(size=14),
            panel.border = element_rect(color="grey", fill=NA))
    if(is.null(thrsh)) {
        return(p0)
    } else {
        return(
            p0 +
                geom_vline(xintercept = thrsh, linetype = "dashed")
        )
    }
}

visualizeMultipleDCA <- function(dcaLs = NULL, dcaSelectType = NULL) {
    
    selectedTypes <- c("mnci", "mnminmax", "mnq1q3")
    if(length(dcaSelectType) != 1 || all((selectedTypes %in% dcaSelectType) == FALSE)) {
        stop("Argument dcaSelectedType must be one of these three options: mnci, mnminmax, mnq1q3.")
    }
    
    dcaAllModels <- dplyr::bind_rows(dcaLs$tableDCA)
    nbWideLogreg <- dcaAllModels[dcaAllModels$model=="logreg",]
    nbWideRf <- dcaAllModels[dcaAllModels$model=="randomForest",]
    
    logregNB <- nbWideLogreg %>%
        dplyr::group_by(.data$thrsh) %>%
        dplyr::summarise(
            Median=median(.data$nbModel),
            Mean=mean(.data$nbModel),
            Min=min(.data$nbModel),
            qu1=as.numeric(summary(.data$nbModel)[2]),
            lci=as.numeric(mysml::mnci(.data$nbModel)[2]),
            uci=as.numeric(mysml::mnci(.data$nbModel)[3]),
            qu3=as.numeric(summary(.data$nbModel)[5]),
            Max=max(.data$nbModel)
        )
    
    rfNB <- nbWideRf %>%
        dplyr::group_by(.data$thrsh) %>%
        dplyr::summarise(
            Median=median(.data$nbModel),
            Mean=mean(.data$nbModel),
            Min=min(.data$nbModel),
            qu1=as.numeric(summary(.data$nbModel)[2]),
            lci=as.numeric(mysml::mnci(.data$nbModel)[2]),
            uci=as.numeric(mysml::mnci(.data$nbModel)[3]),
            qu3=as.numeric(summary(.data$nbModel)[5]),
            Max=max(.data$nbModel)
        )
    
    allDCA <- dcaLs$plotDCA[[1]]
    allDCA$net_benefit[c(14:18,20:24)] <- c(logregNB$Mean, rfNB$Mean)
    
    logregMnSpan <- list()
    rfMnSpan <- list()
    
    if(dcaSelectType==selectedTypes[1]) {
        #
        for(i in unique(nbWideLogreg$thrsh)) {
            idx <- nbWideLogreg$thrsh == i
            logregMnSpan[[paste0("threshold",i)]] <- mysml::mnci(x=nbWideLogreg$nbModel[idx])
        }
        
        for(i in unique(nbWideRf$thrsh)) {
            idx <- nbWideRf$thrsh == i
            rfMnSpan[[paste0("threshold",i)]] <- mysml::mnci(x=nbWideRf$nbModel[idx])
        }
    } else if(dcaSelectType==selectedTypes[2]) {
        #
        for(i in unique(nbWideLogreg$thrsh)) {
            idx <- nbWideLogreg$thrsh == i
            logregMnSpan[[paste0("threshold",i)]] <- mysml::mnminmax(x=nbWideLogreg$nbModel[idx])
        }
        
        for(i in unique(nbWideRf$thrsh)) {
            idx <- nbWideRf$thrsh == i
            rfMnSpan[[paste0("threshold",i)]] <- mysml::mnminmax(x=nbWideRf$nbModel[idx])
        }
    } else if(dcaSelectType==selectedTypes[3]) {
        #
        for(i in unique(nbWideLogreg$thrsh)) {
            idx <- nbWideLogreg$thrsh == i
            logregMnSpan[[paste0("threshold",i)]] <- mysml::mnq1q3(x=nbWideLogreg$nbModel[idx])
        }
        for(i in unique(nbWideRf$thrsh)) {
            idx <- nbWideRf$thrsh == i
            rfMnSpan[[paste0("threshold",i)]] <- mysml::mnq1q3(x=nbWideRf$nbModel[idx])
        }
    }
    
    selectedThresholds <- unique(dcaAllModels$thrsh)
    selectedThresholds_len <- length(selectedThresholds)
    
    logreg_ui <- dplyr::bind_rows(logregMnSpan)
    logreg_ui$threshold <- selectedThresholds
    logreg_ui$model <- "logreg"
    
    rf_ui <- dplyr::bind_rows(rfMnSpan)
    rf_ui$threshold <- selectedThresholds
    rf_ui$model <- "randomForest"
    
    uiDf <- dplyr::bind_rows(logreg_ui, rf_ui)
    uiDf$model <- c(rep("Logistic regression", times=selectedThresholds_len),
                    rep("Random forest", times=selectedThresholds_len))
    uiDf$model <- as.factor(uiDf$model)
    uiDf$threshold <- as.factor(uiDf$threshold)
    
    idxLR <- which(uiDf$model %in% "Logistic regression")
    idxRF <- which(uiDf$model %in% "Random forest")
    
    lenAllNone <- length(which(allDCA$label %in% c("Treat all", "Treat none")))
    allDCA$mn <- c(rep(NA, times=lenAllNone), NA, uiDf$mn[idxLR], NA, uiDf$mn[idxRF])
    
    # ---------------
    # Append selected type
    # ---------------
    if(dcaSelectType==selectedTypes[1]) {
        
        allDCA$lci <- c(rep(NA, times=lenAllNone), NA, uiDf$lci[idxLR], NA, uiDf$lci[idxRF])
        allDCA$uci <- c(rep(NA, times=lenAllNone), NA, uiDf$uci[idxLR], NA, uiDf$uci[idxRF])
        
    } else if(dcaSelectType==selectedTypes[2]){
        
        allDCA$min <- c(rep(NA, times=lenAllNone), NA, uiDf$min[idxLR], NA, uiDf$min[idxRF])
        allDCA$max <- c(rep(NA, times=lenAllNone), NA, uiDf$max[idxLR], NA, uiDf$max[idxRF])
        
    } else if(dcaSelectType==selectedTypes[3]) {
        
        allDCA$q1 <- c(rep(NA, times=lenAllNone), NA, uiDf$q1[idxLR], NA, uiDf$q1[idxRF])
        allDCA$q3 <- c(rep(NA, times=lenAllNone), NA, uiDf$q3[idxLR], NA, uiDf$q3[idxRF])
        
    }
    return(list(allDCA=allDCA, logregNB=logregNB, rfNB=rfNB))
}

# allDCA = allDCALs$allDCA; bothModels = TRUE
plotDCALs <- function(allDCA = NULL, bothModels=FALSE) {
    
    # if(bothModels) {
    #     dca <- allDCA
    #     useColor <- c("Treat all" = "black", "Treat none" = "black",
    #                   "Logistic regression" = "black", "Random Forest" = "gray")
    #     useLinetype <- c("solid", "solid", "dashed", "dotted")
    # } else {    
    #     dca <- allDCA[!allDCA$label %in% "Random forest",]
    #     dca$label <- droplevels(dca$label)
    #     useColor <- c("Treat all" = "black", "Treat none" = "black",
    #                   "Logistic regression" = "black")
    #     useLinetype <- c("solid", "dashed", "dotted")
    # }
    # 
    # htbPlot <- function(x) paste0("1:", round((1-x)/x, digits=2))
    # # Make dca plot
    # dcaPlot <- 
    #     ggplot(data=dca, aes(x=threshold, y=net_benefit)) +
    #     # geom_line(aes(colour=label), linewidth=.75) +
    #     geom_line(aes(colour=label, linetype=label), linewidth=.75) +
    #     labs(color=NULL) +
    #     labs(linetype=NULL) +
    #     scale_x_continuous(
    #         sec.axis = dup_axis(name="Harm-to-benefit ratio", labels=htbPlot)) +
    #     # Take control of the y-axis: How much of the negative part shall be visible?
    #     coord_cartesian(ylim=c(0, .08), xlim=c(0, .12)) +
    #     scale_colour_manual(name=NA,
    #                         labels = c("Logistic regression", "Random Forest", "Treat all", "Treat none"),
    #                         values = useColor) +
    #     scale_linetype_manual(name=NA,
    #                           labels = c("Logistic regression", "Random Forest", "Treat all", "Treat none"),
    #                           values = useLinetype) +
    #     ylab(label="Net benefit") +
    #     
    #     # Adapt legend
    #     # https://www.tidyverse.org/blog/2024/02/ggplot2-3-5-0-legends/
    #     
    #     theme(
    #         panel.background = element_blank(),
    #         axis.text.x=element_text(size=16),
    #         axis.title.x=element_text(size=16),
    #         axis.text.y=element_text(size=16),
    #         axis.title.y = element_text(size=16),
    #         panel.border = element_rect(color="black", fill=NA),
    #         legend.text = element_text(size=14),
    #         legend.position = "top",
    #         legend.title = element_blank(),
    #         legend.key.size = unit(.8, units = "cm")) +
    #     labs(x="Threshold probability")
    # 
    # # ggplot2 linetype integers 0-6:
    # # 0 = blank, 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash
    # 
    # if(any(colnames(dca) == "lci")) {
    #     # Mean net benefit and 95% CI:
    #     # ---------------------------
    #     if(bothModels) {
    #         #
    #     } else {
    #         dcaPlotOverlay <- 
    #             dcaPlot +
    #             geom_point(aes(y=mn), size=3) +
    #             geom_errorbar(width=.003, aes(ymin=lci, ymax=uci), linewidth=1) +
    #             guides(color = guide_legend(override.aes = list(shape=NA)))
    #     }
    #     
    # } else if(any(colnames(dca) == "min")){
    #     # Mean and Minimum/Maximum net benefit:
    #     # ------------------------------------
    #     if(bothModels) {
    #         #
    #     } else {
    #         dcaPlotOverlay <- 
    #             dcaPlot +
    #             geom_point(aes(y=mn), size=3) +
    #             geom_errorbar(width=.003, aes(ymin=min, ymax=max), linewidth=1) +
    #             guides(color = guide_legend(override.aes = list(shape=NA)))
    #     }
    #     
    # } else if(any(colnames(dca) == "q1")) {
    #     # Mean and q1/q3 net benefit:
    #     # ------------------------------------
    #     if(bothModels) {
    #         #
    #     } else {
    #         dcaPlotOverlay <- 
    #             dcaPlot +
    #             geom_point(aes(y=mn), size=3) +
    #             geom_errorbar(width=.003, aes(ymin=q1, ymax=q3), linewidth=1) +
    #             guides(color = guide_legend(override.aes = list(shape=NA)))
    #     }
    # }
    # return(list(dcaPlot1=dcaPlot, dcaPlot2=dcaPlotOverlay))
    
    if(bothModels) {
        dca <- allDCA
        useColor <- c("Treat all" = "black", "Treat none" = "darkgrey",
                      "Logistic regression" = "red", "Random forest" = "blue")
    } else {
        dca <- allDCA[!allDCA$label %in% "Random forest",]
        dca$label <- droplevels(dca$label)
        useColor <- c("Treat all" = "black", "Treat none" = "darkgrey",
                      "Logistic regression" = "red")
    }
    htbPlot <- function(x) paste0("1:", round((1-x)/x, digits=2))
    # Make dca plot
    dcaPlot <-
        ggplot(data=dca, aes(x=.data$threshold, y=.data$net_benefit, colour=.data$label)) +
        geom_line(aes(colour=.data$label), linewidth=.75) +
        labs(color=NULL) +
        scale_x_continuous(
            sec.axis = dup_axis(name="Harm-to-benefit ratio", labels=htbPlot)) +
        # Take control of the y-axis: How much of the negative part shall be visible?
        coord_cartesian(ylim=c(0, .08), xlim=c(0, .12)) +
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
        labs(x="Threshold probability")
    
    if(any(colnames(dca) == "lci")) {
        # Mean net benefit and 95% CI:
        # ---------------------------
        if(bothModels) {
            dcaPlotOverlay <-
                dcaPlot +
                geom_point(aes(y=.data$mn), size=3, position = position_dodge(width=.002)) +
                geom_errorbar(width=.003, aes(ymin=.data$lci, ymax=.data$uci), linewidth=1, position=position_dodge(width=0.002)) +
                guides(color = guide_legend(override.aes = list(shape=NA)))
        } else {
            dcaPlotOverlay <-
                dcaPlot +
                geom_point(aes(y=.data$mn), size=3) +
                geom_errorbar(width=.003, aes(ymin=.data$lci, ymax=.data$uci), linewidth=1) +
                guides(color = guide_legend(override.aes = list(shape=NA)))
        }
        
    } else if(any(colnames(dca) == "min")){
        # Mean and Minimum/Maximum net benefit:
        # ------------------------------------
        if(bothModels) {
            dcaPlotOverlay <-
                dcaPlot +
                geom_point(aes(y=.data$mn), size=3, position = position_dodge(width=.002)) +
                geom_errorbar(width=.003, aes(ymin=.data$min, ymax=.data$max), linewidth=1, position=position_dodge(width=0.002)) +
                guides(color = guide_legend(override.aes = list(shape=NA)))
        } else {
            dcaPlotOverlay <-
                dcaPlot +
                geom_point(aes(y=.data$mn), size=3) +
                geom_errorbar(width=.003, aes(ymin=.data$min, ymax=.data$max), linewidth=1) +
                guides(color = guide_legend(override.aes = list(shape=NA)))
        }
        
    } else if(any(colnames(dca) == "q1")) {
        # Mean and q1/q3 net benefit:
        # ------------------------------------
        if(bothModels) {
            dcaPlotOverlay <-
                dcaPlot +
                geom_point(aes(y=.data$mn), size=3, position = position_dodge(width=.002)) +
                geom_errorbar(width=.003, aes(ymin=.data$q1, ymax=.data$q3), linewidth=1, position=position_dodge(width=0.002)) +
                guides(color = guide_legend(override.aes = list(shape=NA)))
        } else {
            dcaPlotOverlay <-
                dcaPlot +
                geom_point(aes(y=.data$mn), size=3) +
                geom_errorbar(width=.003, aes(ymin=.data$q1, ymax=.data$q3), linewidth=1) +
                guides(color = guide_legend(override.aes = list(shape=NA)))
        }
    }
    return(list(dcaPlot1=dcaPlot, dcaPlot2=dcaPlotOverlay))
}

# Calibration plot
calibPlot <- function(calibData=NULL, calibXYmax=NULL) {
    ggplot(calibData, aes(x=mn_x, y=mn_y)) +
        geom_errorbar(width=.005, aes(ymin=lci_y, ymax=uci_y), linewidth=1, color="grey") +
        geom_point(size=2) +
        geom_abline(aes(slope=1, intercept=0, linetype="Perfect calibration"), colour = "black", linewidth=1) +
        scale_linetype_manual(NULL, values=c("Perfect calibration"=2)) +
        
        scale_x_continuous(limits = c(0,calibXYmax)) +
        scale_y_continuous(limits = c(min(calibLRPlot$lci_y),calibXYmax)) +
        
        geom_vline(xintercept=.04, linetype="dashed", linewidth=.5) +
        geom_vline(xintercept=.11, linetype="dashed", linewidth=.5) +
        
        xlab(label="Predicted probability") +
        ylab(label="Observed proportion") +
        
        theme(
            panel.background = element_blank(),
            axis.text.x=element_text(size=16),
            axis.title.x=element_text(size=16),
            axis.text.y=element_text(size=16),
            axis.title.y = element_text(size=16),
            panel.border = element_rect(color="grey", fill=NA),
            legend.text=element_text(size=16),
            legend.position = "top")
}

# ==============================================================================
# Source: Calibration_Sup2-03-calibration-functions.R (Supplementary Code Ojeda et al. 2023, DOI: 10.1002/sim.9921)
# 04) Winsorizes a vector of probabilities. 
#
#     The approach used to winsorize a vector of probabilities is taken from
#     yardstick:::mn_log_loss_multiclass. Which is how that package deals with
#     probabilities close to 0 or 1 when working with the LogLoss.
#
#     ARGUMENTS
#     
#     x: a numeric vector (of probabilities)
#
#     VALUE
#
#     The numeric vector x after winsorization.
#
winsorize_probs <- function(x) {
    eps <- .Machine$double.eps
    pmax(pmin(x, 1 - eps), eps)
}
# ==============================================================================

