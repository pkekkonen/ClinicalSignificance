library(ggplot2) # For plotting

clinsigClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "clinsigClass",
    inherit = clinsigBase,
    private = list(
        .run = function() {
            if ((!is.null(self$options$pre)) & (!is.null(self$options$post))) {

                # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
                #                                            DATA EXTRACTION AND PREPERATION                                              #
                # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

                # Extract pre and post values
                col_index_pre <- grep(self$options$pre, colnames(self$data)) #get the index of the pre column
                values_pre <- self$data[,col_index_pre] #get the values of pre
                col_index_post <- grep(self$options$post, colnames(self$data)) #get the index of the post column
                values_post <- self$data[,col_index_post] #get the values of post

                # Extract groups
                if(!is.null(self$options$groupingVar)) { # Check if grouping variable is present
                    col_index_group <- grep(self$options$groupingVar, colnames(self$data)) #get the index of the groups column
                    values_group <- self$data[,col_index_group] #get the values of groups
                } else {
                    values_group <- rep("", length(values_pre)) # If grouping variable is not present, fill column with nothings's
                }

                # Delete rows that has at least one NA
                dataframe_with_na <- data.frame(values_pre = values_pre, values_post = values_post, values_group=values_group) # Dataframe consisting of pre and postvalues and groups
                dataframe_without_na <- na.omit(dataframe_with_na)

                values_post <- dataframe_without_na$values_post
                values_pre <- dataframe_without_na$values_pre
                values_group <- dataframe_without_na$values_group

                # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
                #                                             MEAN AND STANDARD DEVIATION                                                 #
                # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

                if(self$options$dysNorms == "manualValues") {
                    m_pre <- self$options$dys_mean # Get the inputted mean
                    sd_pre <- self$options$dys_sd # Get the inputted standard deviation
                } else {
                    m_pre <- mean(values_pre) # Calculate the mean of pre values
                    sd_pre <- sd(values_pre) # Calculate the standard deviation of pre values
                }

                # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
                #                                                 CUTOFF POINTS                                                           #
                # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

                if(self$options$cutoffs == "a") {
                    if(self$options$higherBetter) { # Checks if higher score indicates improvement
                        cutoff <- m_pre+2*sd_pre
                    } else {
                        cutoff <- m_pre-2*sd_pre
                    }
                }

                if(self$options$cutoffs == "b") {
                    m_post <- self$options$func_mean
                    sd_post <- self$options$func_sd

                    if(self$options$higherBetter) { # Checks if higher score indicates improvement
                        cutoff <- m_post-2*sd_post
                    } else {
                        cutoff <- m_post+2*sd_post
                    }
                }

                if(self$options$cutoffs == "c") {
                    m_post <- self$options$func_mean
                    sd_post <- self$options$func_sd
                    cutoff <- (sd_post * m_pre + sd_pre * m_post)/(sd_post + sd_pre)
                }


                # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
                #                                                  RCI CALCULATION                                                        #
                # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

                # Calculation for RCI
                reliabilityMeasurement <- self$options$reliabilityMeasurement #Reliability of measurement
                standard_error_of_measurement <- sd_pre*sqrt(1-reliabilityMeasurement)
                s_diff <- sqrt(2*(standard_error_of_measurement ^ 2))

                # We want to create rci boundary lines (y=kx+m) where x = values_pre, y = values_post, k = 1 and m (the interception point) is the negative and positive value of the following
                interception_point <- s_diff*1.96
                interception_point_minus <- interception_point - (interception_point * 2)


                # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
                #                                             PATIENT STATUS CALCULATION                                                  #
                # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

                if(self$options$higherBetter) {
                    patient_status <- c(ifelse(values_post-values_pre >= interception_point, ifelse(values_post >= cutoff,"Recovered","Improved"),ifelse(values_post-values_pre <= interception_point_minus,"Detoriated","Unchanged")))
                } else {
                    patient_status <- c(ifelse(values_post-values_pre <= interception_point_minus,ifelse(values_post <= cutoff,"Recovered","Improved"),ifelse(values_post-values_pre >= interception_point,"Detoriated","Unchanged")))
                }


                # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
                #                                                SCATTER PLOT PREPERATION                                                 #
                # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

                if(self$options$scatterplot) {
                    # Create dataframe with the relevant values for plotting
                    df_scatterplot <- data.frame(values_pre, values_post, values_group, patient_status, cutoff, interception_point, interception_point_minus) # Dataframe consisting of pre and postvalues
                    colnames(df_scatterplot) <- c("values_pre", "values_post", "values_group", "patient_status", "cutoff", "interception_point", "interception_point_minus")

                    # Save dataframe to image
                    image_scatter <- self$results$scatterplot
                    image_scatter$setState(df_scatterplot)
                } else {
                    self$results$scatterplot$setVisible(FALSE)
                }


                # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
                #                                                   BAR PLOT PREPERATION                                                  #
                # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

                df <- data.frame(patient_status = patient_status, values_pre = values_pre, values_group = values_group) # Dataframe consisting of if a patient is above cutoff-point and the patients scores

                frequency_df <- as.data.frame(table(df$patient_status, df$values_group)) # Frequency dataframe of patient status outcomes
                colnames(frequency_df) <- c("patient_status", "values_group", "no_of_patients")

                if(self$options$barplot) {
                    image <- self$results$barplot
                    image$setState(frequency_df)

                } else {
                    self$results$barplot$setVisible(FALSE)
                }

                # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
                #                                              TABLE PREPERATION AND DISPLAY                                              #
                # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

                table_zero <-table(factor(df$patient_status,
                                          levels = c("Detoriated",  "Improved", "Recovered", "Unchanged")),
                                   factor(df$values_group))


                frequency_df_zero <- as.data.frame(table_zero)
                colnames(frequency_df_zero) <- c("patient_status", "values_group", "no_of_patients")

                i <- 0

                if(self$options$showPercentage) {
                    for (group in unique(frequency_df$values_group)){

                        detoriated <- frequency_df_zero$no_of_patients[1 + i]
                        improved <- frequency_df_zero$no_of_patients[2 + i]
                        recovered <- frequency_df_zero$no_of_patients[3 + i]
                        unchanged <- frequency_df_zero$no_of_patients[4 + i]

                        total <- detoriated + improved + recovered + unchanged

                        j <- 1
                        for (col in self$results$table$columns) {

                            if(j>1 && j %% 2 == 1) {
                                col$setVisible(TRUE)
                            } else {
                                col$setSuperTitle(col$name)
                                if(j > 1)
                                    col$setTitle("n")
                                else
                                    col$setTitle(self$options$groupingVar)
                            }

                            j <- j+1
                        }

                        detoriated_percent <- sprintf("%s", format(round(detoriated/total*100, 2), nsmall = 2))
                        improved_percent <- sprintf("%s", format(round(improved/total*100, 2), nsmall = 2))
                        recovered_percent <- sprintf("%s", format(round(recovered/total*100, 2), nsmall = 2))
                        unchanged_percent <- sprintf("%s", format(round(unchanged/total*100, 2), nsmall = 2))

                        self$results$table$addRow(group, values = list(
                            Grouping = group,
                            Detoriated = detoriated,
                            Improved = improved,
                            Recovered = recovered,
                            Unchanged = unchanged,
                            DetoriatedPercent = detoriated_percent,
                            ImprovedPercent = improved_percent,
                            RecoveredPercent = recovered_percent,
                            UnchangedPercent = unchanged_percent
                        ))

                        i <- i + 4
                    }

                } else {
                    for (group in unique(frequency_df$values_group)){

                        j <- 1
                        for (col in self$results$table$columns) {

                            if(j>1 && j %% 2 == 1) {
                                col$setVisible(FALSE)
                            } else {
                                col$setTitle(col$name)
                                col$setSuperTitle(NULL)
                            }
                            j <- j+1

                        }

                        self$results$table$addRow(group, values = list(
                            Grouping = group,
                            Detoriated = frequency_df_zero$no_of_patients[1 + i],
                            Improved = frequency_df_zero$no_of_patients[2 + i],
                            Recovered = frequency_df_zero$no_of_patients[3 + i],
                            Unchanged = frequency_df_zero$no_of_patients[4 + i]
                        ))


                        i <- i + 4
                    }
                }

                if(is.null(self$options$groupingVar)) {
                    self$results$table$columns$Grouping$setVisible(FALSE)
                } else {
                    self$results$table$columns$Grouping$setVisible(TRUE)
                }

                if(self$options$table) {
                    print(nrow(self$results$table))
                } else {
                    self$results$table$setVisible(FALSE)
                    return;
                }

            } else {
                self$results$table$setVisible(FALSE)
                return;
            }
        },

        # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
        #                                                       RENDERING                                                         #
        # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

        .barplot=function(image, ...) {
            plotData <- image$state
            if (!is.null(plotData)) {
                if(self$options$showBarPlotAmount) {
                    labels <- c()
                    
                    i <- 1
                    no_of_rows <- self$results$table$rowCount
                    
                    for(col in self$results$table$columns) {
                        if(i>1 && i %% 2 == 0) {
                            label <- paste0(col$name,":")
                            j <- 1
                            
                            while(j <= no_of_rows) {
                                label <- paste(label, self$results$table$getCell(rowNo=j,col=col$name)$value)
                                
                                if(j < no_of_rows)
                                    label <- paste(label, "/")
                                
                                j <- j+1
                            }
                            labels <-c(labels, label)
                        } 
                        i <- i+1
                        
                    }
                } else {
                    labels <- c("Detoriated", "Improved","Recovered", "Unchanged")
                }
                
                
                
                barplot <- ggplot(data=plotData, aes(x=values_group, y=no_of_patients, fill = patient_status)) +
                    geom_bar(stat="identity", position = position_dodge()) + 
                    scale_fill_manual(labels = labels, values=c("Recovered"="green", "Improved"="blue", "Unchanged"="orange", "Detoriated"="red")) +
                    labs(x = self$options$groupingVar, y = "Number of patients", fill = "Patient status") # Barplot
                print(barplot)
                TRUE
            } else {
                self$results$barplot$setVisible(FALSE)
                return;
            }
        },
        .scatterplot=function(image_scatter, ...) {

            plotData <- image_scatter$state
            if (!is.null(plotData)) {
                cutoff <- image_scatter$state$cutoff
                interception_point <- image_scatter$state$interception_point
                interception_point_minus <- image_scatter$state$interception_point_minus
                values_group <- image_scatter$state$values_group
                patient_status <-plotData$patient_status
                groups <- unique(plotData$values_group)
                max_all <- max(max(plotData$values_pre), max(plotData$values_post))
                min_all <- min(min(plotData$values_pre), min(plotData$values_post))


                if(length(groups) > 5 & !(is.null(self$options$groupingVar))) {  # Show error message if more than five different groupings
                    scatter_plot <-  ggplot() +
                        theme_void() +
                        geom_text(aes(0,0,label="Can only display a maximum of five different groupings at once."), color="red", size=5) +
                        xlab(NULL)
                } else {

                    available_filling_shapes <- c(21,22,23,24,25)
                    used_filling_shapes <- available_filling_shapes[1:length(groups)]

                    scatter_plot <- ggplot(data=plotData, aes(x=plotData$values_pre, y = plotData$values_post)) +
                        coord_cartesian(xlim = c(min_all, max_all), ylim = c(min_all, max_all), expand = TRUE) +
                        geom_abline(aes(intercept = cutoff, slope=0,linetype = "Cutoff point", color="Cutoff point")) +
                        geom_abline( aes(intercept=interception_point, slope=1, linetype="Boundary for reliable change", color="Boundary for reliable change")) + # rci boundary
                        geom_abline(aes(intercept=0, slope=1, linetype = "No change", color = "No change")) + # line indicating no change
                        geom_abline(aes(intercept=interception_point_minus, slope=1, linetype="Boundary for reliable change", color="Boundary for reliable change")) + # rci boundary
                        scale_linetype_manual(values=c("Boundary for reliable change"="dashed", "No change"="solid", "Cutoff point"="solid")) +
                        scale_color_manual(values=c("Boundary for reliable change"="black", "No change"="black", "Cutoff point"="red")) +
                        theme(legend.position = "right")
                    
                    
                    if(self$options$showScatterPlotAmount) {
                        labels <- c()
                        
                        i <- 1
                        no_of_rows <- self$results$table$rowCount
                        
                        for(col in self$results$table$columns) {
                            if(i>1 && i %% 2 == 0) {
                                label <- paste0(col$name,":")
                                j <- 1
                                
                                while(j <= no_of_rows) {
                                    label <- paste(label, self$results$table$getCell(rowNo=j,col=col$name)$value)
                                    
                                    if(j < no_of_rows)
                                        label <- paste(label, "/")
                                    
                                    j <- j+1
                                }
                                labels <-c(labels, label)
                            } 
                            i <- i+1
                            
                        }
                    } else {
                        labels <- c("Detoriated", "Improved","Recovered", "Unchanged")
                    }
                    
                    
                    # Check whether grouping should be with as a variable
                    if (length(groups) == 1) {
                        scatter_plot <- scatter_plot +
                            scale_fill_manual(labels = labels, values=c("Detoriated"="red", "Improved"="blue","Recovered"="green", "Unchanged"="orange")) +
                            geom_point(aes(fill = factor(patient_status)), shape = 21,size=3, stroke=0) +
                            labs(x = "Before treatment", y = "After treatment", linetype = "Line explanations", color = "Line explanations", fill= "Patient status")
                    } else {
                        
                        scatter_plot <- scatter_plot +
                            scale_fill_manual(labels = labels, values=c("Detoriated"="red", "Improved"="blue","Recovered"="green", "Unchanged"="orange")) +
                            geom_point(aes(shape = factor(values_group),fill = factor(patient_status)), size=3, stroke=0) +
                            scale_shape_manual(values=used_filling_shapes) +
                            guides(fill = guide_legend(override.aes = list(shape = 22, size=5)))+
                            guides(shape = guide_legend(override.aes = list(fill = "black")))+
                            labs(x = "Before treatment", y = "After treatment", linetype = "Line explanations", color = "Line explanations", fill= "Patient status", shape = self$options$groupingVar)
                    }

                }
                print(scatter_plot)

                TRUE
            } else {
                self$results$scatterplot$setVisible(FALSE)
                return;
            }}
    )
)

