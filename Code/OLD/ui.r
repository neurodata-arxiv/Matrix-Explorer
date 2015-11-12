# Define UI for application
shinyUI(navbarPage("Data Analytics", id = "tabs",
  tabPanel("Data", value = "D",
    sidebarPanel(
      fileInput('data', 'Choose CSV File', accept=c('text/csv', 'text/comma-separated-values,text/plain','.csv')),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"')
    ),
	mainPanel(
		dataTableOutput(outputId="table")
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
				list("Histogram" = "hist", 
				 "Kernel Density" = "kd", 
				 "Combined" = "comb")) 
  ),

  # Show a plot of the generated distribution
  mainPanel(
	#includeHTML("graph.js")
    #reactiveBar(outputId = "perfbarplot")
    plotOutput("MarginalPlot")
  )  
  ),
  tabPanel("Outlier Analysis", value = "OA",
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
	selectInput(inputId = "correlation_dropdown",
				label = "Select",
				list("Pearson's Correlation" = "p_corr",  
				 "Distance Metric" = "dist_met")) 
  ),
  mainPanel(
    #includeHTML("graph.js")
	plotOutput("Corr", width = "150%",height = "1200px")
  )
   ),
  tabPanel("Mean Vector", value = "MV",
	sidebarPanel(
	selectInput(inputId = "mean_type",
				label = "Select Type of Plot",
				list("Raw Scatter", "Raw Scatter with error bars", "Box Plot","R-Score")
				) 
	 ),
  mainPanel(
    plotOutput("Mean_o", height = "800px", dblclick = "plot1_dblclick",
        brush = brushOpts(
          id = "plot1_brush",
          resetOnNew = TRUE))
  )
  ),  
  tabPanel("Clustering", value = "C",
	sidebarPanel(
		plotOutput("Scree")
	),
  mainPanel(
  	sliderInput(inputId = "num_clust", label = "Number of Clusters", min=1, max=20, value=3, step = 1),
    plotOutput("Clust")	
	)
   )
))
