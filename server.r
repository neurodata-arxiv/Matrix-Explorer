library(shiny)
library(ggplot2)
library(robustbase)
library(reshape)
library(grid)
library(htmltools)
library(fastcluster)
library(ggdendro)
library(gtable)
library(tsne)
library(RColorBrewer)
library(DT)
library(data.table)

options(shiny.maxRequestSize = 2000*1024^2)

shinyServer(function(input, output) {

	#Set up a reactive variable to accept input from interact.js when the user clicks on a row, column, or chooses a column class. Note that when no columns or rows are selected, Vector Explorer automatically analyzes all rows and columns.
	col_sel <- reactive({
		if (length(input$col_sel) > 0){
			input$col_sel	
		} else{
			c(1:dim(my_data()[[1]])[2])
		}
	})
	
	col_class <- reactive({
		#if (input$col_class %in% my_data()[[3]]){
		#	input$col_class
		#	print(input$col_class)
		#} else {
		#	NA
		#}
		input$col_class	
	})
	
	row_sel <- reactive({
		if (length(input$row_sel) > 0){
			input$row_sel	
		} else{
			c(1:dim(my_data()[[1]])[1])
		}
	})
	
	#Shiny reactive variables use lazy evaluation and are only evaluated when called. Shiny observers use eager evaluation and are called whenever changed. Take advantage of this to update the col_sel, col_class, and row_sel reactive variables as soon as input$col_sel, input$row_sel, and input$col_class are changed by the user. 
	observe({
		col_sel()
		row_sel()
	})
	
	observe({
		my_data()
		run_data()
	})
	
	ranges <- reactiveValues(y = NULL)
	show_outliers <- reactiveValues(Names = NULL, Distances = NULL, Rows = NULL)
	
	my_data <- reactive({
		validate(need(input$data, message = FALSE)) 	
		inFile <- input$data 
		if (is.null(inFile)){
			return(NULL)
		}
		
		df <- fread(inFile$datapath, header = input$header, sep = input$sep, na.strings=c("NA","N/A","null"),data.table = FALSE)
		dataTypes <- sapply(df, class)
		
		if(input$proc){
			df <- na.omit(df)
		}
		 
		data_with_factors <- df
		f_indi <- which(dataTypes == "character")	
		if(length(f_indi) > 0) {
			df <- df[,-f_indi]
			dataTypes <- dataTypes[-f_indi]
		}
		
		data <- list(df,data_with_factors,f_indi)
		
	})

	data_pp <- reactive({
		if(input$rmout == TRUE){
			if (length(show_outliers$Rows) == 0){
				clean_data <- my_data()[[1]][unlist(row_sel()),unlist(col_sel())]
			} else{
				clean_data <- my_data()[[1]][unlist(row_sel()),unlist(col_sel())]
				clean_data <- clean_data[-show_outliers$Rows]
			}
		} else{
				clean_data <- my_data()[[1]][unlist(row_sel()),unlist(col_sel())]
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
	
	pca_precomp <- reactive({	
		fit <- prcomp(data_pp(), center=TRUE, scale = FALSE)
		df <- data.frame(x = fit$x[,1], y = fit$x[,2])
		rv <- fit[1]
		list(df,rv)
	})
	
	tsne_precomp <- reactive({		
		fit <- as.data.frame(tsne(data_pp(), perplexity=50))
		df <- data.frame(x = fit$V1, y = fit$V2)
		rv <- NULL
		list(df,rv)
	})
	
	heatmap_precomp <- reactive({
		data <- my_data()[[1]][unlist(row_sel()),unlist(col_sel())]
		
		result_raw <- data[,order(colnames(data))]
		result_raw <- na.omit(result_raw)
		row.names(result_raw) <- paste("Sample",c(1:length(row.names(result_raw))), sep=" ")
		
		result_zscores <- scale(result_raw, center = TRUE, scale = TRUE)
		
		result_q <- apply(result_raw,2,rank)
		result_q <- result_q / max(result_q)
		
		result_r <- apply(result_raw,2,rank)
		
		for(i in c('raw','zscores','q','r')){
			input_res <- eval(parse(text = paste('result_',i,sep = "")))
		
			if(nrow(input_res) < 1000 & ncol(input_res) < 1000){
				dd.col <- as.dendrogram(hclust(dist(as.matrix(input_res))))
				dd.row <- as.dendrogram(hclust(dist(t(as.matrix(input_res)))))
				col.ord <- order.dendrogram(dd.col)
				row.ord <- order.dendrogram(dd.row)
				ddata_x <- dendro_data(dd.row)
				ddata_y <- dendro_data(dd.col)
				temp <- melt(as.matrix(input_res[col.ord, row.ord]))
				assign(paste('temp_',i,sep=""),temp)
				assign(paste('temp_',i,'$X1',sep=""),factor(eval(parse(text = paste('temp_',i,'$X1',sep=""))), levels = row.names(input_res)[col.ord]))
			} else{
				ddata_x <- NA
				ddata_y <- NA
				temp <- melt(as.matrix(input_res))
				assign(paste('temp_',i,sep=""),temp)
				assign(paste('temp_',i,'$X1',sep=""),factor(eval(parse(text = paste('temp_',i,'$X1',sep=""))), levels = row.names(input_res)))
			}

			assign(paste('lev_',i,sep=""),lapply(2:16, function(j) cut(eval(parse(text = paste('temp_',i,'$value',sep=""))),j)))
			assign(paste('ddata_x_',i,sep=""),ddata_x)
			assign(paste('ddata_y_',i,sep=""),ddata_y)
		}
	
		print('Fired')
		
		output <- list(temp_raw,temp_zscores,temp_q,temp_r,ddata_x_raw,ddata_x_zscores,ddata_x_q,ddata_x_r,ddata_y_raw,ddata_y_zscores,ddata_y_q,ddata_y_r,lev_raw,lev_zscores,lev_q,lev_r)
	})
			
	heatmap_comp <- reactive({		
		data <- my_data()[[1]][unlist(row_sel()),unlist(col_sel())]
		type <- input$heatmap_type
		
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
		temp$lev <- cut(temp$value,input$num_bin_data_heatmap)
		#View(temp)
	
		ddata_x <- dendro_data(dd.row)
		ddata_y <- dendro_data(dd.col)
			
		output <- list(temp,ddata_x,ddata_y)
	})
	
	marginal_precomp <- reactive({
		data <- my_data()[[1]][unlist(row_sel()),unlist(col_sel())]
		
		mean_mat <- rep(NA,length(unlist(col_sel())))
		median_mat <- rep(NA,length(unlist(col_sel())))
		
		for(i in c(1:length(unlist(col_sel())))){
			mean_mat[i] <- mean(data[,i])
			median_mat[i] <- median(data[,i])
		}
		
		output <- list(mean_mat,median_mat)		
	})
	
	outlier_precomp <- reactive({
		data <- my_data()[[1]][unlist(row_sel()),unlist(col_sel())]
		num_cols <- dim(data)[1]

		mahalanobis_dist <- mahalanobis(data,colMeans(data),cov(data),tol=1e-20)
		mahalanobis_dist
	})
	
	correlation_precomp <- reactive({
		data_raw <- my_data()[[1]][unlist(row_sel()),unlist(col_sel())]
		
		data_zscores <- scale(data_raw, center = TRUE, scale = TRUE)
		data_q <- apply(clean_data,2,rank)
		data_q <- data_q / max(data_q)
		data_r <- apply(clean_data,2,rank)
		
		for(i in c('p','d')){
			
		}
		
		if(input$rmout_corr == TRUE){
			if (length(show_outliers$Rows) == 0){
				clean_data <- my_data()[[1]][unlist(row_sel()),unlist(col_sel())]
			} else{
				clean_data <- my_data()[[1]][unlist(row_sel()),unlist(col_sel())]
				clean_data <- clean_data[-show_outliers$Rows]
			}
		} else{
				clean_data <- my_data()[[1]][unlist(row_sel()),unlist(col_sel())]
		}
	  
		data_t <- clean_data[,order(colnames(clean_data))]
		
		if (input$correlation_dropdown == "p_corr") {
			result <- cor(data_t)

			temp <- result
			temp[lower.tri(temp)] <- NA
			temp <- melt(temp)
			temp <- na.omit(temp)
		
		} else{	
			result <-as.matrix(dist(t(data_t)))

			temp <- result
			temp[lower.tri(temp)] <- NA
			temp <- melt(temp)
			temp <- na.omit(temp)
		}
	})
	
	run_data <- reactive({
		if(input$precomp){
			heatmap_precomp()
			#pca_precomp()
			#tsne_precomp()
			marginal_precomp()
			outlier_precomp()
		}
	})
	
	clust <- reactive({
		if (input$embed_type == "PCA") {
			df <- pca_comp()[[1]]
		} else {
			df <- tsne_comp()[[1]]
		}
		clusters_result <- kmeans(df, input$num_clust)
		clusters <- cbind(df,z = clusters_result$cluster)
		#memb <- cutree(hclust(dist(my_data()[[1]]), method = "complete"), k = input$num_clust)
	})
	
	#This does not need to be reactive, as you will switch tabs before colour scheme matters. 
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
	
	theme_offset <- theme(
		title = element_text(size=20), 
		plot.title = element_text(vjust = 2), 
		axis.title.x = element_text(vjust = -0.5), 
		axis.title.y = element_text(vjust = 2)
	)
	
  Data_Heatmap <- function(type,bins){
	
	if(input$precomp){
		pre_output <- isolate(heatmap_precomp()) #list(temp_raw,temp_zscores,temp_q,temp_r,ddata_x_raw,ddata_x_zscores,ddata_x_q,ddata_x_r,ddata_y_raw,ddata_y_zscores,ddata_y_q,ddata_y_r,lev_raw,lev_zscores,lev_q,lev_r) 
		output <- list(NA,NA,NA)
		if (type == "raw_heatmap"){	
			output[[1]] <- pre_output[[1]]
			lev <- pre_output[[13]][[bins - 1]]
			output[[1]] <- cbind(output[[1]],lev)
			output[[2]] <- pre_output[[5]]
			output[[3]] <- pre_output[[9]]
		} else if (type == "zscores_heatmap"){		
			output[[1]] <- pre_output[[2]]
			lev <- pre_output[[14]][[bins - 1]]
			output[[1]] <- cbind(output[[1]],lev)
			output[[2]] <- pre_output[[6]]
			output[[3]] <- pre_output[[10]]
		} else if (type == "quantiles_heatmap"){
			output[[1]] <- pre_output[[3]]
			lev <- pre_output[[15]][[bins - 1]]
			output[[1]] <- cbind(output[[1]],lev)
			output[[2]] <- pre_output[[7]]
			output[[3]] <- pre_output[[11]]
		} else{
			output[[1]] <- pre_output[[4]]
			lev <- pre_output[[16]][[bins - 1]]
			output[[1]] <- cbind(output[[1]],lev)
			output[[2]] <- pre_output[[8]]
			output[[3]] <- pre_output[[12]]
		}
	} else{
		output <- heatmap_comp() #output <- list(temp,ddata_x,ddata_y)
	}
  
	p1 <- ggplot(output[[1]], aes(X2, X1, fill = lev)) + geom_tile(alpha = 0.5, colour = "white") + scale_fill_manual(values = color_fun()(bins), name = "Z-score",guide=FALSE)
	p1 <- p1 + labs(x = "", y = "") + scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0))# + ggtitle("Column Scaled Z-Score Heatmap")
	p1 <- p1 + theme(axis.ticks = element_blank(), plot.title = element_text(vjust=2), axis.text.x = element_text(angle=90, vjust = 0.6), axis.text.y = element_text(), text = element_text(size = 20), legend.text=element_text(size=20), legend.title = element_text(size = 10))
	
	cb_df <- data.frame(X1 = 1,X2 = output[[1]]$lev)
	
	cb <- ggplot(cb_df,aes(X1,X2,fill = X2)) + geom_tile(alpha = 0.5) + coord_equal(1/bins * 25) + scale_fill_manual(values = color_fun()(bins), guide=FALSE) + theme_none + theme(axis.text.y = element_text())
	
	gA <- ggplotGrob(p1)
	gD <- ggplotGrob(cb)
		
	g <- gtable_add_cols(gA, unit(3,"in"))
	
	if(is.na(unlist(output[[2]])[1]) == FALSE){
		p2 <- ggplot(segment(output[[2]])) +   geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) +   theme_none + theme(axis.title.x=element_blank()) + scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + theme(plot.margin=unit(c(0,0,0,0), "cm"), panel.margin=unit(c(0,0,0,0), "cm"))
		
		p3 <- ggplot(segment(output[[3]])) +   geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) +   coord_flip() + theme_none + scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + theme(plot.margin=unit(c(0.15,1,-0.6,0), "cm"), panel.margin=unit(c(0,0,0,0), "cm"))
	
		gB <- ggplotGrob(p3)
		gC <- ggplotGrob(p2)	
		
		g <- gtable_add_cols(gA, unit(3,"in"))
		g <- gtable_add_grob(g, gB,t = 2, l = ncol(g), b = 3, r = ncol(g))
		g <- gtable_add_rows(g, unit(3,"in"), 0)
		g <- gtable_add_grob(g, gC,t = 1, l = 4, b = 1, r = 4)
		g <- gtable_add_grob(g, gD,t = 1, l = ncol(g), b = 1, r = ncol(g))
	} else{
		g <- gtable_add_grob(g, gD,t = 1, l = ncol(g), b = 1, r = ncol(g))
	}
	
	grid.newpage()
	grid.draw(g)	
  }
	
  Marginals <- function(data,name,type){
	validate(need(name, message=FALSE))
	
	current_column <- which(colnames(data) == name)

	if(input$precomp){
		result <- isolate(marginal_precomp()) #list(mean_mat,median_mat)	
		current_mean <- result[[1]][current_column]
		current_median <- result[[2]][current_column]
		
	} else{
		current_mean <- mean(data[,current_column])
		current_median <- median(data[,current_column])
	}
	
	if(input$marginal_condition_classes){
		data <- cbind(data,Class = factor(my_data()[[2]][row_sel(),col_class()]))#Check this
		aes_set <- aes_q(x = as.name(name), color = as.name('Class'))#This looks wrong		
	} else{
		aes_set <- aes_q(x = as.name(name))
	}
	
	if (type == "hist"){
		p <- ggplot(data, aes_set) + geom_histogram(fill = "deepskyblue2", alpha = 0.2) + ylab('Counts')
	} else if (type == "kd"){
		p <- ggplot(data, aes_set) + geom_density(fill = "blue" , alpha = 0.2) + ylab('Density')
	}
	else{
		 p <- ggplot(data, aes_set) + geom_histogram(aes(y = ..density..), fill = "deepskyblue2", alpha = 0.2) + geom_density(fill = "blue" , alpha = 0.2) + ylab('Density')
	}
	
	p <- p + theme_offset + ggtitle("Marginal Distribution") 
	
	if(input$marginal_mean){
		p <- p + geom_vline(xintercept = current_mean, color = "steelblue") +  geom_text(size = 8, x= current_mean, label="Mean", y = 0, colour="steelblue", angle=90, vjust=-0.4, hjust=-2.65)		
	}
	
	if(input$marginal_median){
		p <- p + geom_vline(xintercept = current_median, color = "red") +  geom_text(size = 8,x = current_median , label="Median", y = 0 , colour="red", angle=90, vjust=-0.4, hjust=-2)
	}
	
	p
  }
  
  Outliers <- function(data,cutoff_in){
  
	if(input$precomp){
		mahalanobis_dist <- isolate(outlier_precomp())
	} else{
		num_cols <- dim(data)[1]

		mahalanobis_dist <- mahalanobis(data,colMeans(data),cov(data),tol=1e-20)
	}
	
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
				clean_data <- my_data()[[1]][unlist(row_sel()),unlist(col_sel())]
			} else{
				clean_data <- my_data()[[1]][unlist(row_sel()),unlist(col_sel())]
				clean_data <- clean_data[-show_outliers$Rows]
			}
		} else{
				clean_data <- my_data()[[1]][unlist(row_sel()),unlist(col_sel())]
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
		
			p <- p + theme_grey(base_size = base_size) + labs(x = "", y = "") + scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + ggtitle("Euclidean Distance Matrix Heatmap")
		
			p <- p + theme(axis.ticks = element_blank(), plot.title = element_text(vjust=2), axis.text.x = element_text(angle=90, vjust = 0.6), axis.text.y = element_text(), text = element_text(size=20), legend.text=element_text(size=20), legend.title = element_text(size = 20)) + guides(fill = guide_colorbar(barwidth = 2, barheight = 10, title.position = "top", title.vjust = 10)) 
		}
  }
  
  Mean_Vectors <- function(){
	if(input$rmout_mean == TRUE){
			if (length(show_outliers$Rows) == 0){
				clean_data <- my_data()[[1]][unlist(row_sel()),unlist(col_sel())]
			} else{
				clean_data <- my_data()[[1]][unlist(row_sel()),unlist(col_sel())]
				clean_data <- clean_data[-show_outliers$Rows]
			}
		} else{
				clean_data <- my_data()[[1]][unlist(row_sel()),unlist(col_sel())]
	}
  
	num_vars <- dim(clean_data)[2]
  
  
	output_mean <- vector()
	output_se <- vector()
	if (input$mean_pp_type == "raw_mean"){
		for (i in c(1:num_vars)){
			output_mean[i] <- mean(clean_data[,i],na.rm = TRUE)	
			output_se[i] <- sd(clean_data[,i],na.rm = TRUE) / sqrt(length(clean_data[,i]))
		}
	} else{
		output_mean <- colMedians(as.matrix(clean_data), na.rm = FALSE)
		output_mean <- output_mean / apply(clean_data,2,mad)
		clean_data <- sweep(clean_data,2,apply(clean_data,2,mad),`/`) 
		for (i in c(1:num_vars)){
			output_se[i] <- sd(clean_data[,i],na.rm = TRUE) / sqrt(length(clean_data[,i]))
		}
	}	 
	 
	df <- data.frame(names = colnames(clean_data), means = output_mean, se = output_se)
	 
	if (input$mean_type == "Scatter") {
		p <- ggplot(melt(clean_data,0), aes(x = variable, y = value)) + geom_jitter() + xlab("") + ylab("Data Values") + ggtitle('Data Scatter Plot') 
	} else if (input$mean_type == "Mean Vector"){
		p <- ggplot(df, aes(x = names, y = means)) + geom_point() + ylab("Mean") + xlab("") + ggtitle('Raw Column Means') 
	} else if(input$mean_type == "Mean Vector with standard error bars"){
		p <- ggplot(df, aes(x = names, y = means)) + geom_point() + geom_errorbar(aes(ymax = means + se, ymin=means - se), width=0.3) + ylab("Mean") + xlab("") + ggtitle('Raw Column Means')
	} else if(input$mean_type == "Box Plot"){
		p <- ggplot(melt(clean_data,0),aes(x = variable, y = value)) + geom_boxplot() + ylab("Data Values") + xlab("") + ggtitle('Boxplots') 
	} else{
		p <- ggplot(melt(clean_data,0),aes(x = variable, y = value)) + geom_violin() + ylab("Data Values") + xlab("") + ggtitle('Violin Plots') 
	}
	
	p <- p + theme(plot.title = element_text(vjust=2), text = element_text(size=20), axis.text.x=element_text(angle=90, vjust = 0.6)) + coord_cartesian(ylim = ranges$y)
  }
  
  Clustering <- function(){
	
	df <- clust()

	p <- ggplot(df,aes(x = x,y = y, colour = factor(z)))
	
	p <- p + geom_point(size = 5) + xlab('First Dimension') + ylab('Second Dimension') + theme(plot.title = element_text(vjust=2), text = element_text(size=20), axis.text.x = element_text(vjust = 2)) + scale_colour_discrete(name = "Clusters")	
	
   }
  
  output$MarginalPlot <- renderPlot({
    p <- Marginals(my_data()[[1]][unlist(row_sel()),unlist(col_sel())],input$col_names,input$show_type)
    print(p)
  })
  
  output$Outliers <- renderPlot({
	result <- Outliers(my_data()[[1]][unlist(row_sel()),unlist(col_sel())],input$pval * 100)
	p <- result[2]
	outlier_data <<- result[[1]]
	print(p)
  })
  
  output$Corr <- renderPlot({
	p <- Correlation()
	
	print(p)
	
	#seekViewport('spacer.4-3-4-3')
	#axis_width <- convertWidth(unit(1,'npc'), 'inch', TRUE)
	#axis_heigh <- convertHeigh(unit(1,'npc'), 'inch', TRUE)
	
	# seekViewport('panel.3-4-3-4')
	# plot_width <- convertWidth(unit(1,'npc'), 'inch', TRUE)
	# plot_height <- convertWidth(unit(1,'npc'), 'inch', TRUE)
	
	# seekViewport('layout')
	# layout_width <- convertWidth(unit(1,'npc'), 'inch', TRUE)
	# layout_height <- convertWidth(unit(1,'npc'), 'inch', TRUE)
	
	
	#print(axis_width)
	#print(axis_height)
	# print(plot_width)
	# print(plot_height)
	# print(layout_width)
	# print(layout_height)
	
	# print(c(plot_width,plot_height,current.vpTree(all=TRUE)))
  })
  
  output$data_heatmap <- renderPlot({
	p <- Data_Heatmap(input$heatmap_type,input$num_bin_data_heatmap)
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
  
  output$outlier_info <- DT::renderDataTable({	
    data.frame(Outlier_Names = show_outliers$Names, Distances = show_outliers$Distances)
  }, options= list(searching = FALSE))
  
  output$table <- DT::renderDataTable(
	my_data()[[2]],
	class = 'row-border stripe hover order-column',
	#container = table_cont(),
	callback = JS("initTable(table)"),
	filter = 'top', 
	#server = FALSE,
	selection = 'none',
	extensions = c('TableTools','ColVis','ColReorder'),
	options = list(dom = 'RDCT<"clear">lfrtip',tableTools = list(sSwfPath = copySWF('www')),scrollCollapse = TRUE, deferRender = TRUE, scrollX = TRUE)
  )
  
  
  output$corr_location_info <- DT::renderDataTable({
	if (is.null(input$corr_plot_loc$x)) return()
	if (is.null(input$corr_plot_loc$y)) return()
  
	if(input$rmout_corr == TRUE){
		if (length(show_outliers$Rows) == 0){
			clean_data <- my_data()[[1]][unlist(row_sel()),unlist(col_sel())]
		} else{
			clean_data <- my_data()[[1]][unlist(row_sel()),unlist(col_sel())]
			clean_data <- clean_data[-show_outliers$Rows]
		}
	} else{
			clean_data <- my_data()[[1]][unlist(row_sel()),unlist(col_sel())]
	}
	
	col_names <- sort(colnames(clean_data))
	
	location <- data.frame(
	Column = col_names[round((input$corr_plot_loc$x - 0.1) / (0.86 - 0.1) * length(col_sel()) + 0.5)],
	Row = col_names[round((input$corr_plot_loc$y - 0.2) / (0.96 - 0.2) * length(col_sel()) + 0.5)]
	)
	location
	
  }, options= list(pageLength = 1, dom = 't',searching = FALSE), rownames = FALSE)
  
  output$heatmap_location_info <- DT::renderDataTable({
	if (is.null(input$heatmap_plot_loc$x)) return()
	if (is.null(input$heatmap_plot_loc$y)) return()
  	
	data <- my_data()[[1]][unlist(row_sel()),unlist(col_sel())]
	result <- data[,order(colnames(data))]
	result <- na.omit(result)
	
	row_names	<- paste("Sample",c(1:length(row.names(result))), sep=" ")
	col_names <- sort(colnames(result))
	
	location <- data.frame(
	Column = col_names[round((input$heatmap_plot_loc$x - 0.09255) * length(col_sel()) / 0.699575 + 0.5)],
	Row = row_names[round((input$heatmap_plot_loc$y - 0.045) / (0.9 - 0.045) * length(row_sel) + 0.5)]
	)
	location
  }, options= list(pageLength = 1, dom = 't',searching = FALSE), rownames = FALSE)
  
  observeEvent(my_data(), { 
    output$marginal_column <- renderUI({
	selectInput(inputId = "col_names", label = "Select", colnames(my_data()[[1]][,unlist(col_sel())]))
  })
  })
  
  
})