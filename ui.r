# Define UI for application
#We use tabs
shinyUI(navbarPage("MX:Matrix Explorer", id = "tabs",
  #Tab that allows for data upload
  tabPanel("Upload", value = "D",
    sidebarPanel(
		fileInput('data', 'Choose CSV File', accept=c('text/csv', 'text/comma-separated-values,text/plain','.csv')),
		actionButton('demo',"Demo (Iris Dataset)", width = "35%"),
		checkboxInput('header', 'Header', TRUE),
		radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ',',inline = TRUE),
		radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"',inline = TRUE),
		radioButtons('proc', 'Remove rows containing NAs and Infs?',
                   c('Yes'='TRUE',
                     'No'="FALSE"),inline = TRUE),
		hr(),
		checkboxInput('precomp', 'Pre-compute?', FALSE),
		hr(),
		selectInput(inputId = "colormap",
				label = "Select Color Scheme",
				list("Blue" = "Blues",
				"Blue-Purple" = "BuPu", 
				 "Blue-Green" = "BuGn", 
				 "Green-Blue" = "GnBu",
				 "Green" = "Greens",
				 "Grey" = "Greys",
				 "Orange" = "Oranges",
				 "Orange-Red" = "OrRd",
				 "Purple-Blue" = "PuBu",
				 "Purple-Blue-Green" = "PuBuGn",
				 "Purple-Red" = "PuRd",
				 "Purple" = "Purples",
				 "Red-Purple" = "RdPu",
				 "Red" = "Reds",
				 "Yellow-Green" = "YlGn",
				 "Yellow-Green-Blue" = "YlGnBu",
				 "Yellow-Orange-Brown" = "YlOrBr",
				 "Yellow-Orange-Red" = "YlOrRd")),
		strong(a("Code",href = "https://github.com/neurodata/Matrix-Explorer")),
		br(),
		strong(a("Report Issues", href = "https://github.com/neurodata/Matrix-Explorer/issues"))
    ),
	mainPanel(
		DT::dataTableOutput(outputId="table"),
		includeCSS("www/format.css"),
		tags$head(includeScript("www/interact.js"))
	)  
  ),
  #Panel that displays heatmap data
 tabPanel("Heatmap", value = "HM",
	sidebarPanel(			
	selectInput(inputId = "heatmap_type",
				label = "Select",
				list("Raw Data" = "raw_heatmap", 
				 "Z-scores" = "zscores_heatmap", 
				 "Normalize between 0 and 1" = "quantiles_heatmap",
				 "Ranks" = "rank_heatmap")),
	sliderInput(inputId = "num_bin_data_heatmap", label = "Number of Color Bins", min=2, max=16, value=4, step = 1),
	hr(),
	checkboxInput('heatmapy','Show sample labels?',TRUE),
	checkboxInput('heatmapx','Show feature labels?',TRUE),
	checkboxInput('dendro', 'Show dendrogram?', TRUE),
	#checkboxInput('heatmaporder', 'Disable column/row reordering?', FALSE),
	checkboxInput('heatmapsize', 'Resize heatmap to fit screen?', FALSE),
	DT::dataTableOutput(outputId="heatmap_location_info")
    ),
	mainPanel(
		uiOutput("data_heatmap")
	)  
  ),
  #Panel that displays marginals
  tabPanel("Sample Summary", value = "MD",
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
	uiOutput("marginal_column"),
	selectInput(inputId = "show_type",
				label = "Select",
				list("Combined" = "comb",
				"Histogram" = "hist", 
				 "Kernel Density" = "kd")),
	checkboxInput('marginal_condition_classes', 'Condition on classes?', FALSE),
	checkboxInput('marginal_mean', 'Show mean?', TRUE),
	checkboxInput('marginal_median', 'Show median?', TRUE)
	#downloadLink('download_plot', 'Download Plot')	
  ),

  # Show a plot of the generated distribution
  mainPanel(
	#includeHTML("graph.js")
    #reactiveBar(outputId = "perfbarplot")
    plotOutput("MarginalPlot")
  )  
  ),
  #Panel that displays outlier plots
  tabPanel("Outliers", value = "OA",
	sidebarPanel(
		checkboxInput('coloroutlier','Color based on class?',FALSE),
		sliderInput(inputId = "pval", label = "Rejection P-Value", min=0, max=0.1, value=0.05, step = 0.01),
		DT::dataTableOutput(outputId="outlier_info")
	),
  mainPanel(
    plotOutput("Outliers")
		
  )
  ),
  #Panel that displays correlation plots
  tabPanel("Correlation", value = "CA",
  sidebarPanel(	
	checkboxInput('rmout_corr', 'Remove Outliers', TRUE),
	selectInput(inputId = "corr_type",
				label = "Select Scaling",
				list("Raw Data" = "raw_corr", 
				 "Z-scores" = "zscores_corr", 
				 "Normalize between 0 and 1" = "quantiles_corr",
				 "Ranks" = "rank_corr")),
	selectInput(inputId = "correlation_dropdown",
				label = "Select Metric",
				list("Pearson's Correlation" = "p_corr",  
				 "Euclidean Distance Matrix" = "dist_met")),
	hr(),
	checkboxInput('corraxis','Show axis tick labels?',TRUE),
	DT::dataTableOutput(outputId="corr_location_info")
	#verbatimTextOutput("corr_location_info")
  ),
  mainPanel(
	plotOutput("Corr", width = "100%", height = "600px", hover = "corr_plot_loc")
  )
   ),
   #Tab that displays feature summary plots
  tabPanel("Feature Summary", value = "MV",
	sidebarPanel(
	checkboxInput('rmout_mean', 'Remove Outliers', TRUE),
	selectInput(inputId = "mean_type",
				label = "Select Type of Plot",
				list("Scatter", "Line plot","Mean Vector with standard error bars", "Box Plot","Violin Plot")
				),
	selectInput(inputId = "mean_pp_type",
			label = "Select",
			list("Raw Data" = "raw_mean", 
			 "R-scores" = "rscores_mean")),
	hr(),
	checkboxInput('colorfeature','Color based on class?',FALSE)			 
	),
  mainPanel(
    plotOutput("Mean_o", height = "800px", dblclick = "plot1_dblclick",
        brush = brushOpts(
          id = "plot1_brush",
          resetOnNew = TRUE))
  )
  ),  
  #Panel that displays 2D embedding plots
  tabPanel("Embedding & Clustering", value = "C",
	sidebarPanel(sliderInput(inputId = "num_clust", label = "Number of Clusters", min=1, max=20, value=3, step=1),
	checkboxInput('rmout', 'Remove Outliers', TRUE),
	selectInput(inputId = "embed_type",
			label = "Select Dimensionality Reduction Technique",
			list("PCA","t-SNE")
			),
	selectInput(inputId = "clust_pp_type",
				label = "Select",
				list("Raw Data" = "raw_pp", 
				"Z-scores" = "zscores_pp", 
				"Normalize between 0 and 1" = "quantiles_pp",
				"Ranks" = "rank_pp"))
	),
	mainPanel(
		plotOutput("Clust"),
		plotOutput("Scree")
	)
  # fluidPage(
	# plotOutput("Clust"),
  # fluidRow(
    # column(4,
      # wellPanel(
		# sliderInput(inputId = "num_clust", label = "Number of Clusters", min=1, max=20, value=3, step=1),
		# checkboxInput('rmout', 'Remove Outliers', TRUE),
		# selectInput(inputId = "embed_type",
			# label = "Select Dimensionality Reduction Technique",
			# list("PCA","t-SNE")
			# ),
		# selectInput(inputId = "clust_pp_type",
				# label = "Select",
				# list("Raw Data" = "raw_pp", 
				 # "Z-scores" = "zscores_pp", 
				 # "Normalize between 0 and 1" = "quantiles_pp",
				 # "Ranks" = "rank_pp"))
      # )       
    # ),
    # column(8,
      # plotOutput("Scree")
    # )
  # )
  # )
	# sidebarPanel(
		# plotOutput("Scree")
	# ),
  # mainPanel(
  	# sliderInput(inputId = "num_clust", label = "Number of Clusters", min=1, max=20, value=3, step = 1),
    # plotOutput("Clust")	
	# )
  )
)
)
