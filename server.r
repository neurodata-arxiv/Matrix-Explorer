#R file

library(shiny)
library(ggplot2)
library(robustbase)
library(reshape)
library(grid)
library(fastcluster)
library(ggdendro)
library(gtable)
library(tsne)
library(RColorBrewer)

options(shiny.maxRequestSize = 9*1024^2)

shinyServer(function(input, output) {
	ranges <- reactiveValues(y = NULL)
	show_outliers <- reactiveValues(Names = NULL, Distances = NULL, Rows = NULL)
	
	my_data <- reactive({
		validate(need(input$data, message = FALSE)) 
		inFile <- input$data 
		if (is.null(inFile))
			return(NULL)
		df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,quote = input$quote, dec = ".")
		#data[,1] <- NULL
		dataTypes <- vector(mode="character", length=dim(df)[2])  # define a vector to hold each columns data type 
		# we loop through each column and determine its type 
		for (i in 1:dim(df)[2]){
			# first task is to scrub the data 
			df[,i] <- gsub(" ", "", df[,i]) # remove spaces 
			df[,i] <- tolower(df[,i])
			# check to make sure there are no na n/a and we missed this as continuous data 
			na_indi <- which(df[,i] =="na" | df[,i]=="n/a") #CHECK THIS
			if (length(na_indi) > 0 ) # we found some Nas 
			{
				df[na_indi,i] <- NA
			}
    
			na_indi <- sum(is.na(df[,i])) # get initial count of na indices 
    
			# check if it is numeric by converting to it 
			test <- df[,i] # holder variable 
			test <- as.numeric(test) 
			na_indi2 <- sum(is.na(test))
    
			if (na_indi2>na_indi) #must be characters 
			{
				dataTypes[i] <- "character"
      
			} else { 
				dataTypes[i] <- "double"
				df[,i] <- test
			}
		}
  
		# we now look to convert to factors 

		for (i in 1:(dim(df)[2])){
			if (dataTypes[i] == "character"){
				dataTypes[i] = "factor"
				df[,i] <- as.factor(df[,i])
				if (nlevels(df[,i]) > 6) # bad column and we delete 
					{
						# df[,i] <- NULL # remove column 
						dataTypes[i] <- 0 # mark to remove data type
					}
      
			}
		}
		r_indi <- which(dataTypes == 0)
		df <- df[,-r_indi]
		dataTypes <- dataTypes[-r_indi] 
		data <- df
	})


	data_pp <- reactive({
		if(input$rmout == TRUE){
			if (length(show_outliers$Rows) == 0){
				clean_data <- my_data()
			} else{
				clean_data <- my_data()
				clean_data <- clean_data[-show_outliers$Rows]
			}
		} else{
				clean_data <- my_data()
		}
	
		if (input$clust_pp_type == "raw_pp"){
		} else if (input$clust_pp_type == "zscores_pp"){		
			clean_data <- scale(clean_data, center = TRUE, scale = TRUE)
		} else if (input$clust_pp_type == "quantiles_pp"){
			clean_data <- apply(clean_data,2,rank)
			clean_data <- clean_data / max(clean_data)
		} else{
			clean_data <- apply(clean_data,2,rank)
		}
		clean_data
	})
	
	pca_comp <- reactive({	
		fit <- prcomp(data_pp(), center=TRUE, scale = FALSE)
		df <- data.frame(x = fit$x[,1], y = fit$x[,2], z = clust())
		rv <- fit[1]
		list(df,rv)
	})
	
	tsne_comp <- reactive({		
		fit <- as.data.frame(tsne(data_pp(), perplexity=50))
		df <- data.frame(x = fit$V1, y = fit$V2, z = clust())
		rv <- NULL
		list(df,rv)
	})
	
	clust <- reactive({
		memb <- cutree(hclust(dist(my_data()), method = "complete"), k = input$num_clust)
	})
	
	#This does not need to be reactive, as you will switch tabs before color scheme matters. 
	color_fun <- reactive({
		cgrad <- brewer.pal(5,input$colormap);
		c_fun<- colorRampPalette(cgrad)
	})
	
	Scree_Plot <- reactive({
		if (input$embed_type == "PCA") {
			result <- pca_comp()[[2]]
		} else {
			result <- tsne_comp()[[2]]
		}

		retained_variance <- cumsum(unlist(result)^2) /  max(cumsum(unlist(result)^2))
	
		if (length(retained_variance) == 0){
			df <- data.frame(x = NULL,y = NULL)
		} else{
			df <- data.frame(x = c(1:length(retained_variance)), y = retained_variance)
		}
	
		p <- ggplot(df, aes(x = x,y = y)) + xlab('Retained Dimensions') + ylab('Explained Variance') + ggtitle('Scree Plot')
		p <- p + geom_point() + geom_line() + theme(plot.title = element_text(vjust=2), text = element_text(size=20), axis.text.x=element_text(angle=45))	
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
	
	dd.col <- as.dendrogram(hclust(dist(as.matrix(result))))
	dd.row <- as.dendrogram(hclust(dist(t(as.matrix(result)))))

	col.ord <- order.dendrogram(dd.col)
	row.ord <- order.dendrogram(dd.row)
	
	temp <- melt(as.matrix(result[col.ord, row.ord]))
	temp$X1 <- factor(temp$X1, levels = row.names(result)[col.ord])
	temp$lev <- cut(temp$value,bins)
	
	ddata_x <- dendro_data(dd.row)
	ddata_y <- dendro_data(dd.col)
		
	p1 <- ggplot(temp, aes(X2, X1, fill = lev)) + geom_tile(alpha = 0.5, colour = "white") + scale_fill_manual(values = color_fun()(bins), name = "Z-score",guide=FALSE)
	p1 <- p1 + labs(x = "", y = "") + scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0))# + ggtitle("Column Scaled Z-Score Heatmap")
	p1 <- p1 + theme(axis.ticks = element_blank(), plot.title = element_text(vjust=2), axis.text.x = element_text(angle=90, vjust = 0.6), axis.text.y = element_text(), text = element_text(size = 20), legend.text=element_text(size=20), legend.title = element_text(size = 10))# + guides(fill = guide_colorbar(barwidth = 2, barheight = 10, title.position = "top", title.vjust = 10))
		
	p2 <- ggplot(segment(ddata_x)) +   geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) +   theme_none + theme(axis.title.x=element_blank()) + scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + theme(plot.margin=unit(c(0,0,0,0), "cm"), panel.margin=unit(c(0,0,0,0), "cm"))
		
	p3 <- ggplot(segment(ddata_y)) +   geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) +   coord_flip() + theme_none + scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + theme(plot.margin=unit(c(0.15,1,-0.6,0), "cm"), panel.margin=unit(c(0,0,0,0), "cm"))
		
	cb_df <- data.frame(X1 = 1,X2 = temp$lev)
	cb <- ggplot(cb_df,aes(X1,X2,fill = X2)) + geom_tile(alpha = 0.5) + coord_equal(1/bins * 25) + scale_fill_manual(values = color_fun()(bins), guide=FALSE) + theme_none + theme(axis.text.y = element_text())

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
	
	
	show_outliers$Rows <- df_outliers[,1][outlier]
	show_outliers$Names <- row.names(data)[df_outliers[,3]]
	show_outliers$Distances <- mahalanobis_dist[df_outliers[,3]]
	
	
	p <- ggplot(df_outliers,aes(x = x,y = y))
	
	p <- p + geom_point(aes(colour = z)) + geom_abline(intercept = log(sqrt(cutoff)), slope = 0,linetype="dashed",colour = "red") + labs(x = "Observation Number",y = "log(Mahalanobis Distances)", title = paste("Outlier Plot")) + scale_colour_manual(name="Type", values = c("FALSE" = "blue","TRUE" = "#FF0080"), breaks=c("TRUE", "FALSE"), labels=c("Outlier", "Inlier"))	
	
	p <- p + theme(plot.title = element_text(vjust=2), text = element_text(size=20))
	
	return(list(df_outliers,p))
  }
  
  Correlation <- function(){
	if(input$rmout_corr == TRUE){
			if (length(show_outliers$Rows) == 0){
				clean_data <- my_data()
			} else{
				clean_data <- my_data()
				clean_data <- clean_data[-show_outliers$Rows]
			}
		} else{
				clean_data <- my_data()
	}
	
	if (input$corr_type == "raw_corr"){
	} else if (input$corr_type == "zscores_corr"){		
		clean_data <- scale(clean_data, center = TRUE, scale = TRUE)
	} else if (input$corr_type == "quantiles_corr"){
		clean_data <- apply(clean_data,2,rank)
		clean_data <- clean_data / max(clean_data)
	} else{
		clean_data <- apply(clean_data,2,rank)
	}
  
  
	data_t <- clean_data[,order(colnames(clean_data))]
	
	if (input$correlation_dropdown == "p_corr") {
		result <- cor(data_t)

		temp <- result
		temp[lower.tri(temp)] <- NA
		temp <- melt(temp)
		temp <- na.omit(temp)
	
		p <- ggplot(temp, aes(X2, X1, fill = value)) + geom_tile(alpha = 0.5, colour = "white") + scale_fill_gradient2(low = color_fun()(3)[1], high = color_fun()(3)[2], mid = color_fun()(3)[3], midpoint = 0, limit = c(-1,1), name = "Pearson\ncorrelation\n")
		base_size <- 14
	
		p <- p + theme_grey(base_size = base_size) + labs(x = "", y = "") + scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + ggtitle("Correlation Heatmap")
	
		p <- p + theme(axis.ticks = element_blank(), plot.title = element_text(vjust=2), axis.text.x = element_text(angle=90, vjust = 0.6), axis.text.y = element_text(), text = element_text(size=20), legend.text=element_text(size=20), legend.title = element_text(size = 20)) + guides(fill = guide_colorbar(barwidth = 2, barheight = 10, title.position = "top", title.vjust = 10)) 
	} else{	
		result <-as.matrix(dist(t(data_t)))

		temp <- result
		temp[lower.tri(temp)] <- NA
		temp <- melt(temp)
		temp <- na.omit(temp)
	
		p <- ggplot(temp, aes(X2, X1, fill = value)) + geom_tile(alpha = 0.5, colour = "white") + scale_fill_gradient2(low = color_fun()(3)[1], high = color_fun()(3)[2], mid = color_fun()(3)[3], name = "Distance\nmatrix\n")
		base_size <- 14
	
		p <- p + theme_grey(base_size = base_size) + labs(x = "", y = "") + scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + ggtitle("Distance Matrix Heatmap")
	
		p <- p + theme(axis.ticks = element_blank(), plot.title = element_text(vjust=2), axis.text.x = element_text(angle=90, vjust = 0.6), axis.text.y = element_text(), text = element_text(size=20), legend.text=element_text(size=20), legend.title = element_text(size = 20)) + guides(fill = guide_colorbar(barwidth = 2, barheight = 10, title.position = "top", title.vjust = 10)) 
	}
	
	#g <- ggplotGrob(p)
	#grid.newpage()
	#grid.draw(g)
  }
  
  Mean_Vectors <- function(){
	if(input$rmout_mean == TRUE){
			if (length(show_outliers$Rows) == 0){
				clean_data <- my_data()
			} else{
				clean_data <- my_data()
				clean_data <- clean_data[-show_outliers$Rows]
			}
		} else{
				clean_data <- my_data()
	}
  
	num_vars <- dim(clean_data)[2]
  
  
	output_mean <- vector()
	output_se <- vector()
	if (input$mean_pp_type == "raw_mean"){
		for (i in c(1:num_vars)){
			output_mean[i] <- mean(clean_data[,i],na.rm = TRUE)	
			output_se[i] <- sd(clean_data[,i],na.rm = TRUE)# / sqrt(length(data[,i]))
		}
	} else{
		output_mean <- colMedians(as.matrix(clean_data), na.rm = FALSE)
		output_mean <- output_mean / apply(clean_data,2,mad)
		output_se <- rep(0, num_vars)
	}
	 
	 
	 
	names_to_use <- colnames(clean_data)	 
	 
	df <- data.frame(names = names_to_use, means = output_mean, se = output_se)
	 
	if (input$mean_type == "Scatter"){
		p <- ggplot(df, aes(x = names, y = means))
		p <- p + geom_point() + ylab("Mean") + xlab("") + theme(plot.title = element_text(vjust=2), text = element_text(size=20), axis.text.x=element_text(angle=90, vjust = 0.6)) + ggtitle('Raw Column Means') + coord_cartesian(ylim = ranges$y)
	} else if(input$mean_type== "Scatter with error bars"){
		p <- ggplot(df, aes(x = names, y = means))
		p <- p + geom_point() + geom_errorbar(aes(ymax = means + se, ymin=means - se), width=0.3) + ylab("Mean") + xlab("") + theme(plot.title = element_text(vjust=2), text = element_text(size=20), axis.text.x=element_text(angle=90, vjust = 0.6)) + ggtitle('Raw Column Means') + coord_cartesian(ylim = ranges$y)
	} else if(input$mean_type == "Box Plot"){
		boxplot_data <- melt(clean_data)
		p <- ggplot(boxplot_data,aes(x = variable, y = value)) + geom_boxplot() + ylab("Mean") + xlab("") + theme(plot.title = element_text(vjust=2), text = element_text(size=20), axis.text.x=element_text(angle=90, vjust = 0.6)) + ggtitle('Boxplots') + coord_cartesian(ylim = ranges$y)
	}
  }
  
  Clustering <- function(){	
	if (input$embed_type == "PCA") {
		df <- pca_comp()[[1]]
		# fit <- prcomp(clean_data, center=TRUE, scale = FALSE)
		# df <- data.frame(x = fit$x[,1], y = fit$x[,2], z = memb)
		# retained_variance <- cumsum(unlist(fit[1])^2) /  max(cumsum(unlist(fit[1])^2))
	} else {
		df <- tsne_comp()[[1]]
		# fit <- as.data.frame(tsne(data, perplexity=50))
		# df <- data.frame(x = fit$V1, y = fit$V2, z = memb)
		# retained_variance <- NULL
	}

	p <- ggplot(df,aes(x = x,y = y, colour = factor(z)))
	
	p <- p + geom_point(size = 5) + xlab('First Dimension') + ylab('Second Dimension') + theme(plot.title = element_text(vjust=2), text = element_text(size=20), axis.text.x = element_text(vjust = 2)) + scale_colour_discrete(name = "Clusters")	
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
	p <- Correlation()
	print(p)
  })
  
  output$data_heatmap <- renderPlot({
	p <- Data_Heatmap(my_data(),input$heatmap_type,input$num_bin_data_heatmap)
	print(p)
  })
  
  output$Mean_o <- renderPlot({
	p <- Mean_Vectors()
	print(p)
  })
  
  output$Clust <- renderPlot({
	p <- Clustering()
	print(p)
  })
  
  output$Scree <- renderPlot({
	p <- Scree_Plot()
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
})