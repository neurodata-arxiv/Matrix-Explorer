# Define UI for application
shinyUI(navbarPage(title = "VX:Vector Explorer", id = "tabs",
  tabPanel("Data Upload", value = "D",
    sidebarPanel(
		fileInput('data', 'Choose CSV File', accept=c('text/csv', 'text/comma-separated-values,text/plain','.csv')),
		tags$hr(),
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
		actionButton('select_all_rows', 'Select All Rows',width = '200px'),
		actionButton('clear_rows', 'Clear Rows',width = '200px'),	
		br(),
		actionButton('select_all_columns', 'Select All Columns',width = '200px'),
		actionButton('clear_columns', 'Clear Columns',width = '200px'),
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
				 "Yellow-Orange-Red" = "YlOrRd"))
    ),
	mainPanel(
		DT::dataTableOutput(outputId="table"),
		includeCSS("www/format.css"),
		tags$head(includeScript("www/interact.js"))
	)  
  ),
 tabPanel("Data Heatmap", value = "HM",
	sidebarPanel(			
	selectInput(inputId = "heatmap_type",
				label = "Select",
				list("Raw Data" = "raw_heatmap", 
				 "Z-scores" = "zscores_heatmap", 
				 "Quantiles" = "quantiles_heatmap",
				 "Ranks" = "rank_heatmap")),
	sliderInput(inputId = "num_bin_data_heatmap", label = "Number of Color Bins", min=2, max=16, value=4, step = 1)
  ),
	mainPanel(
		plotOutput("data_heatmap", width = "100%",height = "1800px")
	)  
  ),
  tabPanel("Marginal Distributions", value = "MD",
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
	uiOutput("marginal_column"),
	selectInput(inputId = "show_type",
				label = "Select",
				list("Combined" = "comb",
				"Histogram" = "hist", 
				 "Kernel Density" = "kd")),
	checkboxInput('marginal_condition_classes', 'Condition on classes?', FALSE)				 
  ),

  # Show a plot of the generated distribution
  mainPanel(
	#includeHTML("graph.js")
    #reactiveBar(outputId = "perfbarplot")
    plotOutput("MarginalPlot")
  )  
  ),
  tabPanel("Outlier Detection", value = "OA",
	sidebarPanel(
		sliderInput(inputId = "pval", label = "Rejection P-Value", min=0, max=10, value=5, step = 1),
		dataTableOutput(outputId="outlier_info")
	),
  mainPanel(
    plotOutput("Outliers")
		
  )
  ),
  tabPanel("Correlation Analysis", value = "CA",
  sidebarPanel(	
	checkboxInput('rmout_corr', 'Remove Outliers', TRUE),
	selectInput(inputId = "corr_type",
				label = "Select Scaling",
				list("Raw Data" = "raw_corr", 
				 "Z-scores" = "zscores_corr", 
				 "Quantiles" = "quantiles_corr",
				 "Ranks" = "rank_corr")),
	selectInput(inputId = "correlation_dropdown",
				label = "Select Metric",
				list("Pearson's Correlation" = "p_corr",  
				 "Euclidean Distance Matrix" = "dist_met")) ,
	dataTableOutput(outputId="corr_location_info")
  ),
  mainPanel(
	plotOutput("Corr", width = "100%", height = "600px", hover = "corr_plot_loc")
  )
   ),
  tabPanel("Plots of Mean Vector", value = "MV",
	sidebarPanel(
	checkboxInput('rmout_mean', 'Remove Outliers', TRUE),
	selectInput(inputId = "mean_type",
				label = "Select Type of Plot",
				list("Scatter", "Scatter with error bars", "Box Plot")
				),
	selectInput(inputId = "mean_pp_type",
			label = "Select",
			list("Raw Data" = "raw_mean", 
			 "R-scores" = "rscores_mean"))				
	 ),
  mainPanel(
    plotOutput("Mean_o", height = "800px", dblclick = "plot1_dblclick",
        brush = brushOpts(
          id = "plot1_brush",
          resetOnNew = TRUE))
  )
  ),  
  tabPanel("2D-Embedding and Clustering", value = "C",
  fluidPage(
	plotOutput("Clust"),
  fluidRow(
    column(4,
      wellPanel(
		sliderInput(inputId = "num_clust", label = "Number of Clusters", min=1, max=20, value=3, step=1),
		checkboxInput('rmout', 'Remove Outliers', TRUE),
		selectInput(inputId = "embed_type",
			label = "Select Dimensionality Reduction Technique",
			list("PCA","t-SNE")
			),
		selectInput(inputId = "clust_pp_type",
				label = "Select",
				list("Raw Data" = "raw_pp", 
				 "Z-scores" = "zscores_pp", 
				 "Quantiles" = "quantiles_pp",
				 "Ranks" = "rank_pp"))
      )       
    ),
    column(8,
      plotOutput("Scree")
    )
  )
  )
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
