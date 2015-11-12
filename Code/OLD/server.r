library(shiny)
library(ggplot2)
library(robustbase)
library(reshape)
library(xlsx)
library(grid)
library(fastcluster)
library(ggdendro)
library(gtable)

options(shiny.maxRequestSize = 9*1024^2)

shinyServer(function(input, output) {
	ranges <- reactiveValues(y = NULL)
	show_outliers <- reactiveValues(Names = NULL, Distances = NULL)
	
	my_data <- reactive({
		validate(need(input$data, message = FALSE)) 
		inFile <- input$data 
		if (is.null(inFile))
			return(NULL)
		data <- read.csv(inFile$datapath, header = input$header, sep = input$sep,quote = input$quote, dec = ".")
		data[,1] <- NULL
		data
	})
	
	theme_none <- theme(
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		panel.background = element_blank(),
		axis.title.x = element_text(colour=NA),
		axis.title.y = element_text(size=0),
		axis.text.x = element_blank(),
		axis.text.y = element_blank(),
		axis.line = element_blank(),
		axis.ticks.length = unit(0, "cm")
	)
	
  Data_Heatmap <- function(data, type, bins){
	result <- data[,order(colnames(data))]
	result <- na.omit(result)
	row.names(result) <- paste("Sample",c(1:length(row.names(result))), sep=" ")
	
	if (type == "raw_heatmap"){
	} else if (type == "zscores_heatmap"){		
		result <- scale(result, center = TRUE, scale = TRUE)
	} else if (type == "quantiles_heatmap"){
		result <- apply(result,2,rank)
		result <- result / max(result)
	} else{
		result <- apply(result,2,rank)
	}
	
	temp <- melt(as.matrix(result))
	temp$X1 <- factor(temp$X1, levels = row.names(result))
	temp$lev <- cut(temp$value,bins)

	dd.col <- as.dendrogram(hclust(dist(as.matrix(result))))
	dd.row <- as.dendrogram(hclust(dist(t(as.matrix(result)))))

	ddata_x <- dendro_data(dd.row)
	ddata_y <- dendro_data(dd.col)
		
	p1 <- ggplot(temp, aes(X2, X1, fill = lev)) + geom_tile(alpha = 0.5, colour = "white") + scale_fill_manual(values = color_fun(bins), name = "Z-score",guide=FALSE)
	p1 <- p1 + labs(x = "", y = "") + scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0))# + ggtitle("Column Scaled Z-Score Heatmap")
	p1 <- p1 + theme(axis.ticks = element_blank(), plot.title = element_text(vjust=2), axis.text.x = element_text(angle=90, vjust = 0.6), axis.text.y = element_text(), text = element_text(size = 20), legend.text=element_text(size=20), legend.title = element_text(size = 10))# + guides(fill = guide_colorbar(barwidth = 2, barheight = 10, title.position = "top", title.vjust = 10))
		
	p2 <- ggplot(segment(ddata_x)) +   geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) +   theme_none + theme(axis.title.x=element_blank()) + scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + theme(plot.margin=unit(c(0,0,0,0), "cm"), panel.margin=unit(c(0,0,0,0), "cm"))
		
	p3 <- ggplot(segment(ddata_y)) +   geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) +   coord_flip() + theme_none + scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + theme(plot.margin=unit(c(0.15,1,-0.6,0), "cm"), panel.margin=unit(c(0,0,0,0), "cm"))
		
	cb_df <- data.frame(X1 = 1,X2 = temp$lev)
	cb <- ggplot(cb_df,aes(X1,X2,fill = X2)) + geom_tile(alpha = 0.5) + coord_equal(1/bins * 25) + scale_fill_manual(values = color_fun(bins), guide=FALSE) + theme_none + theme(axis.text.y = element_text())

	gA <- ggplotGrob(p1)
	gB <- ggplotGrob(p3)
	gC <- ggplotGrob(p2)	
	gD <- ggplotGrob(cb)
		
	g <- gtable_add_cols(gA, unit(3,"in"))
	g <- gtable_add_grob(g, gB,t = 2, l = ncol(g), b = 3, r = ncol(g))
	g <- gtable_add_rows(g, unit(3,"in"), 0)
	g <- gtable_add_grob(g, gC,t = 1, l = 4, b = 1, r = 4)
	g <- gtable_add_grob(g, gD,t = 1, l = ncol(g), b = 1, r = ncol(g))


	grid.newpage()
	grid.draw(g)
  }
	
  Marginals <- function(data,name,type){
	validate(need(name, message=FALSE))
	current_column <- which(colnames(data) == name)

	current_mean <- mean(data[,current_column])
	current_median <- median(data[,current_column])

	
	if (type == "hist"){
		p <- ggplot(data, aes_q(x = as.name(name))) + geom_histogram(fill = "deepskyblue2", alpha = 0.2, color = "white") + title("Marginal Distribution") + ylab('Counts')
	} else if (type == "kd"){
		p <- ggplot(data, aes_q(x = as.name(name))) + geom_density(fill = "blue" , alpha = 0.2) + title("Marginal Distribution") + ylab('Density')
	}
	else{
		 p <- ggplot(data, aes_q(x = as.name(name))) + geom_histogram(aes(y = ..density..), fill = "deepskyblue2", color = "white", alpha = 0.2) + geom_density(fill = "blue" , alpha = 0.2) + title("Marginal Distribution") + ylab('Density')
	}
	
	p <- p + theme(text = element_text(size=20)) + geom_vline(xintercept = current_mean, color = "steelblue") +  geom_text(x= current_mean, label="Mean", y = 0, colour="steelblue", angle=90, text=element_text(size=11), vjust=-0.4, hjust=-6.6) + geom_vline(xintercept = current_median, color = "red") +  geom_text(x = current_median , label="Median", y = 0 , colour="red", angle=90, text=element_text(size=11), vjust=-0.4, hjust=-5)
  }
  
  Outliers <- function(data,cutoff_in){
  
	num_cols <- dim(data)[1]

	mahalanobis_dist <- mahalanobis(data,colMeans(data),cov(data), ,tol=1e-20)
	
	cutoff <- qchisq(1 - cutoff_in / 100, dim(data)[2], ncp = 0, lower.tail = TRUE, log.p = FALSE)
	
	outlier <- mahalanobis_dist > cutoff
	
	df_outliers <<- data.frame(x = c(1:dim(data)[1]), y = log(sqrt(mahalanobis_dist)), z = outlier)
	
	
	show_outliers$Names <<- row_names[df_outliers[,3]]
	show_outliers$Distances <<- mahalanobis_dist[df_outliers[,3]]
	
	
	p <- ggplot(df_outliers,aes(x = x,y = y))
	
	p <- p + geom_point(aes(colour = z)) + geom_abline(intercept = log(sqrt(cutoff)), slope = 0,linetype="dashed",colour = "red") + labs(x = "Observation Number",y = "log(Mahalanobis Distances)", title = paste("Outlier Plot")) + scale_colour_manual(name="Type", values = c("FALSE" = "blue","TRUE" = "#FF0080"), breaks=c("TRUE", "FALSE"), labels=c("Outlier", "Inlier"))	
	
	p <- p + theme(plot.title = element_text(vjust=2), text = element_text(size=20))
	
	return(list(df_outliers,p))
  }
  
  Scree_Plot <- function(data){
	result <- prcomp(data, center = TRUE, scale = TRUE)
	retained_variance <- cumsum(unlist(result[1])^2) /  max(cumsum(unlist(result[1])^2))
	
	df <- data.frame(x = c(1:dim(data)[2]), y = retained_variance)
	
	p <- ggplot(df, aes(x = x,y = y)) + xlab('Retained Dimensions') + ylab('Explained Variance') + ggtitle('Scree Plot')
	p <- p + geom_point() + geom_line() + theme(plot.title = element_text(vjust=2), text = element_text(size=20), axis.text.x=element_text(angle=45))	
  }
  
  Correlation <- function(data, type){
	data_t <- data[,order(colnames(data))]
	
	if (type == "p_corr") {
		result <- cor(data_t)

		temp <- result
		temp[lower.tri(temp)] <- NA
		temp <- melt(temp)
		temp <- na.omit(temp)
	
		p <- ggplot(temp, aes(X2, X1, fill = value)) + geom_tile(alpha = 0.5, colour = "white") + scale_fill_gradient2(low = "steelblue", high = "red", mid = "violet", midpoint = 0, limit = c(-1,1), name = "Pearson\ncorrelation\n")
		base_size <- 14
	
		p <- p + theme_grey(base_size = base_size) + labs(x = "", y = "") + scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + ggtitle("Correlation Heatmap")
	
		p <- p + theme(axis.ticks = element_blank(), plot.title = element_text(vjust=2), axis.text.x = element_text(angle=90, vjust = 0.6), axis.text.y = element_text(), text = element_text(size=20), legend.text=element_text(size=20), legend.title = element_text(size = 20)) + guides(fill = guide_colorbar(barwidth = 2, barheight = 10, title.position = "top", title.vjust = 10)) 
	} else{	
		result <-as.matrix(dist(t(data_t)))

		temp <- result
		temp[lower.tri(temp)] <- NA
		temp <- melt(temp)
		temp <- na.omit(temp)
	
		p <- ggplot(temp, aes(X2, X1, fill = value)) + geom_tile(alpha = 0.5, colour = "white") + scale_fill_gradient2(low = "steelblue", high = "red", mid = "violet", name = "Distance\nmatrix\n")
		base_size <- 14
	
		p <- p + theme_grey(base_size = base_size) + labs(x = "", y = "") + scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + ggtitle("Distance Matrix Heatmap")
	
		p <- p + theme(axis.ticks = element_blank(), plot.title = element_text(vjust=2), axis.text.x = element_text(angle=90, vjust = 0.6), axis.text.y = element_text(), text = element_text(size=20), legend.text=element_text(size=20), legend.title = element_text(size = 20)) + guides(fill = guide_colorbar(barwidth = 2, barheight = 10, title.position = "top", title.vjust = 10)) 
	}
  }
  
  Mean_Vectors <- function(data, type){
	 num_vars <- dim(data)[2]
	
	 output_mean <<- vector()
	 output_se <<- vector()
	 for (i in c(1:num_vars)){
		name <- colnames(data)[i]
		
		output_mean[i] <<- mean(data[,i],na.rm = TRUE)	
		output_se[i] <<- sd(data[,i],na.rm = TRUE) / sqrt(length(data[,3][!is.na(data[,3])]))
	 }

	 names_to_use <- colnames(data)
	 
	 boxplot_data <- melt(data)
	 
	 R_score = colMedians(as.matrix(data), na.rm = FALSE)
	 R_score <- R_score / apply(data,2,mad)
	 
	 df <- data.frame(names = names_to_use, means = output_mean, r_score = R_score)
	 
	 if (type == "Raw Scatter"){
		p <- ggplot(df, aes(x = names, y = means))
		p <- p + geom_point() + ylab("Mean") + xlab("") + theme(plot.title = element_text(vjust=2), text = element_text(size=20), axis.text.x=element_text(angle=90, vjust = 0.6)) + ggtitle('Raw Column Means') + coord_cartesian(ylim = ranges$y)
	 } else if(type == "Raw Scatter with error bars"){
		limits <- aes(ymax = output_mean + output_se, ymin=output_mean - output_se)
		p <- ggplot(df, aes(x = names, y = means))
		 p <- p + geom_point() + geom_errorbar(limits, width=0.3) + ylab("Mean") + xlab("") + theme(plot.title = element_text(vjust=2), text = element_text(size=20), axis.text.x=element_text(angle=90, vjust = 0.6)) + ggtitle('Raw Column Means') + coord_cartesian(ylim = ranges$y)
	 } else if(type == "Box Plot"){
		p <- ggplot(boxplot_data,aes(x = variable, y = value)) + geom_boxplot() + ylab("Mean") + xlab("") + theme(plot.title = element_text(vjust=2), text = element_text(size=20), axis.text.x=element_text(angle=90, vjust = 0.6)) + ggtitle('Boxplots') + coord_cartesian(ylim = ranges$y)
	 }  
	 else{
		p <- ggplot(df, aes(x = names, y = r_score)) + geom_point() + ylab("Mean") + xlab("") + theme(plot.title = element_text(vjust=2), text = element_text(size=20), axis.text.x=element_text(angle=90, vjust = 0.6)) + ggtitle('R-Scores') + coord_cartesian(ylim = ranges$y)
	 }
  }
  
  Clustering <- function(data,num){
	clust <- hclust(dist(data), method = "complete")

	memb <- cutree(clust, k = num)
	
	fit <- prcomp(data, center=TRUE, scale = TRUE)
	
	df <- data.frame(x = fit$x[,1], y = fit$x[,2], z = memb)
	
	p <- ggplot(df,aes(x = x,y = y, colour = factor(z)))
	
	p <- p + geom_point(size = 5) + xlab('First Principal Component') + ylab('Second Principle Component') + theme(plot.title = element_text(vjust=2), text = element_text(size=20), axis.text.x = element_text(vjust = 2)) + scale_colour_discrete(name = "Clusters")	
   }
  
  output$MarginalPlot <- renderPlot({
    p <- Marginals(my_data(),input$col_names,input$show_type)
    print(p)
  })
  
  output$Outliers <- renderPlot({
	result <- Outliers(my_data(),input$pval)
	p <- result[2]
	outlier_data <<- result[[1]]
	#assign("outlier_data", result[[1]], envir = .GlobalEnv) 
	print(p)
  })
  
  output$Corr <- renderPlot({
	p <- Correlation(my_data(), input$correlation_dropdown)
	print(p)
  })
  
  output$data_heatmap <- renderPlot({
	p <- Data_Heatmap(my_data(),input$heatmap_type,input$num_bin_data_heatmap)
	print(p)
  })
  
  output$Mean_o <- renderPlot({
	p <- Mean_Vectors(my_data(),input$mean_type)
	print(p)
  })
  
  output$Clust <- renderPlot({
	p <- Clustering(my_data(),input$num_clust)
	print(p)
  })
  
  output$Scree <- renderPlot({
	p <- Scree_Plot(my_data())
	print(p)
  })
  
  output$outlier_info <- renderDataTable({
	#paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
	
    data.frame(Outlier_Names = show_outliers$Names, Distances = show_outliers$Distances)
	#nearPoints(df_outliers[,c(1:2)], input$plot_brush)#, xval = "x", yval = "y")
    # nearPoints() also works with hover and dblclick events
  }, options= list(searching = FALSE))
  
  output$table <- renderDataTable({
    my_data()			 
	#result <- cbind(row_names,input$data)
	#result
  })
  
  observeEvent(my_data(), { 
    output$marginal_column <- renderUI({
	selectInput(inputId = "col_names", label = "Select", colnames(my_data()))
  })
  })
  
  # output$ui <- renderUI({
 # tabPanel("Data Heatmap", value = "HM",
	# sidebarPanel(			
	# selectInput(inputId = "heatmap_type",
				# label = "Select",
				# list("Raw Data" = "raw_heatmap", 
				 # "Z-scores" = "zscores_heatmap", 
				 # "Quantiles" = "quantiles_heatmap",
				 # "Ranks" = "rank_heatmap")),
	# sliderInput(inputId = "num_bin_data_heatmap", label = "Number of Color Bins", min=2, max=16, value=4, step = 1)
  # ),
	# mainPanel(
		# plotOutput("data_heatmap", width = "100%",height = "1800px")
	# )  
  # ),
  # tabPanel("Marginal Distributions", value = "MD",
  
  #Sidebar with a slider input for number of observations
  # sidebarPanel(
	# selectInput(inputId = "col_names",
				# label = "Select",
				# colnames(data)), 
				
	# selectInput(inputId = "show_type",
				# label = "Select",
				# list("Histogram" = "hist", 
				 # "Kernel Density" = "kd", 
				 # "Combined" = "comb")) 
  # ),

#  Show a plot of the generated distribution
  # mainPanel(
	#includeHTML("graph.js")
    #reactiveBar(outputId = "perfbarplot")
    # plotOutput("MarginalPlot")
  # )  
  # ),
  # tabPanel("Outlier Analysis", value = "OA",
	# sidebarPanel(
		# sliderInput(inputId = "pval", label = "Rejection P-Value", min=0, max=10, value=5, step = 1),
		# dataTableOutput(outputId="outlier_info")
	# ),
  # mainPanel(
    # plotOutput("Outliers")
		
  # )
  # ),
  # tabPanel("Correlation Analysis", value = "CA",
  # sidebarPanel(			
	# selectInput(inputId = "correlation_dropdown",
				# label = "Select",
				# list("Pearson's Correlation" = "p_corr",  
				 # "Distance Metric" = "dist_met")) 
  # ),
  # mainPanel(
   # includeHTML("graph.js")
	# plotOutput("Corr", width = "150%",height = "1200px")
  # )
   # ),
  # tabPanel("Mean Vector", value = "MV",
	# sidebarPanel(
	# selectInput(inputId = "mean_type",
				# label = "Select Type of Plot",
				# list("Raw Scatter", "Raw Scatter with error bars", "Box Plot","R-Score")
				# ) 
	 # ),
  # mainPanel(
    # plotOutput("Mean_o", height = "800px", dblclick = "plot1_dblclick",
        # brush = brushOpts(
          # id = "plot1_brush",
          # resetOnNew = TRUE))
  # )
  # ),  
  # tabPanel("Clustering", value = "C",
	# sidebarPanel(
		# plotOutput("Scree")
	# ),
  # mainPanel(
  	# sliderInput(inputId = "num_clust", label = "Number of Clusters", min=1, max=20, value=3, step = 1),
    # plotOutput("Clust")	
	# )
   # )
   # })
  
  # observeEvent(input$plot1_dblclick, {
    # brush <- input$plot1_brush
    # if (!is.null(brush)) {
      # ranges$y <- c(brush$ymin, brush$ymax)

    # } else {
      # ranges$y <- NULL
    # }
  # })
  
})