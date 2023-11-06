library(shiny) 
library(shinyhelper) 
library(data.table) 
library(Matrix) 
library(DT) 
library(magrittr) 
sc1conf = readRDS("sc1conf.rds")
sc1def  = readRDS("sc1def.rds")



shinyUI(
	fluidPage(
		titlePanel("shiny test"),
		navbarPage(
			NULL,
			tabPanel(
				"module-1",
				fluidRow(
					column(
						3, '1-1', br(),
				        h4("Dimension Reduction"), 
				        fluidRow( 
				          	column( 
				            12, 
				            selectInput("sc1a1drX", "X-axis:", 
				            	choices = sc1conf[dimred == TRUE]$UI,
				            	selected = sc1def$dimred[1]), 
				            selectInput("sc1a1drY", "Y-axis:", 
				            	choices = sc1conf[dimred == TRUE]$UI,
				            	selected = sc1def$dimred[2])
				            ) 
				        ) 

					),
					column(
						3, '1-2',br(),
				        actionButton("sc1a1togL", "Toggle to subset cells"), 
				        conditionalPanel( 
				        condition = "input.sc1a1togL % 2 == 1", 
				        selectInput("sc1a1sub1", "Cell information to subset:", 
				                    choices = sc1conf[grp == TRUE]$UI, 
				                    selected = sc1def$grp1), 
				        uiOutput("sc1a1sub1.ui"), 
				        actionButton("sc1a1sub1all", "Select all groups", 
				        	class = "btn btn-primary"), 
				        actionButton("sc1a1sub1non", "Deselect all groups", 
				        	class = "btn btn-primary")
				        ) 
					),
					column(
						6, '1-3',br(),
						actionButton("sc1a1tog0", "Toggle graphics controls"), 
				        conditionalPanel( 
				          condition = "input.sc1a1tog0 % 2 == 1", 
				          fluidRow( 
				            column( 
				              6, "1-3-1",br(),
				              sliderInput("sc1a1siz", "Point size:", 
				                           min = 0, max = 4, value = 1.25, step = 0.25), 
				              radioButtons("sc1a1psz", "Plot size:", 
				                           choices = c("Small", "Medium", "Large"), 
				                           selected = "Medium", inline = TRUE), 
				              radioButtons("sc1a1fsz", "Font size:", 
				                           choices = c("Small", "Medium", "Large"), 
				                           selected = "Medium", inline = TRUE) 
				            ), 
				            column( 
				              6,  "1-3-2",br(),
				              radioButtons("sc1a1asp", "Aspect ratio:", 
				                           choices = c("Square", "Fixed", "Free"), 
				                           selected = "Square", inline = TRUE), 
				              checkboxInput("sc1a1txt", "Show axis text", value = FALSE) 
				            ) 
				          ) 
				        )

					)
				),
				fluidRow(
					column(
						6, '2-1', br(), 
						fluidRow(
							column(
								6,
					            selectInput("sc1a1inp1", "Cell information:", 
					                         choices = sc1conf$UI, 
					                         selected = sc1def$meta1) %>%  
					            helper(type = "inline", size = "m", fade = TRUE, 
					                   title = "Cell information to colour cells by", 
					                   content = c("Select cell information to colour cells", 
					                               "- Categorical covariates have a fixed colour palette", 
					                               paste0("- Continuous covariates are coloured in a ",  
					                                      "Blue-Yellow-Red colour scheme, which can be ", 
					                                      "changed in the plot controls")))
							),
					        column( 
						        6, 
						        actionButton("sc1a1tog1", "Toggle plot controls"), 
						        conditionalPanel( 
						          condition = "input.sc1a1tog1 % 2 == 1", 
						          radioButtons("sc1a1col1", "Colour (Continuous data):", 
						                       choices = c("White-Red","Blue-Yellow-Red","Yellow-Green-Purple"), 
						                       selected = "Blue-Yellow-Red"), 
						          radioButtons("sc1a1ord1", "Plot order:", 
						                       choices = c("Max-1st", "Min-1st", "Original", "Random"), 
						                       selected = "Original", inline = TRUE), 
						          checkboxInput("sc1a1lab1", "Show cell info labels", value = TRUE) 
					          	)
					        ) 
						),
				        fluidRow(column(12, uiOutput("sc1a1oup1.ui"))), 
				        downloadButton("sc1a1oup1.pdf", "Download PDF"), 
				        downloadButton("sc1a1oup1.png", "Download PNG"), br(), 
				        div(style="display:inline-block", 
				            numericInput("sc1a1oup1.h", "PDF / PNG height:", width = "138px", 
				                         min = 4, max = 20, value = 6, step = 0.5)), 
				        div(style="display:inline-block", 
				            numericInput("sc1a1oup1.w", "PDF / PNG width:", width = "138px", 
				                         min = 4, max = 20, value = 8, step = 0.5)), 
				        br(), 
				        actionButton("sc1a1tog9", "Toggle to show cell numbers / statistics"), 
				        conditionalPanel( 
				          condition = "input.sc1a1tog9 % 2 == 1", 
				          h4("Cell numbers / statistics"), 
				          radioButtons("sc1a1splt", "Split continuous cell info into:", 
				                       choices = c("Quartile", "Decile"), 
				                       selected = "Decile", inline = TRUE), 
				          dataTableOutput("sc1a1.dt") 
				        ) 
					),
					column(
						6, '2-2', br(),
						h4("Gene expression"), 
				        fluidRow( 
				          column( 
				            6, 
				            selectInput("sc1a1inp2", "Gene name:", choices=NULL) %>%  
				            helper(type = "inline", size = "m", fade = TRUE, 
				                   title = "Gene expression to colour cells by", 
				                   content = c("Select gene to colour cells by gene expression", 
				                               paste0("- Gene expression are coloured in a ", 
				                                      "White-Red colour scheme which can be ", 
				                                      "changed in the plot controls"))) 
				          ), 
				          column( 
				            6, 
				            actionButton("sc1a1tog2", "Toggle plot controls"), 
				            conditionalPanel( 
				              condition = "input.sc1a1tog2 % 2 == 1", 
				              radioButtons("sc1a1col2", "Colour:", 
				                           choices = c("White-Red","Blue-Yellow-Red","Yellow-Green-Purple"), 
				                           selected = "White-Red"), 
				              radioButtons("sc1a1ord2", "Plot order:", 
				                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
				                           selected = "Max-1st", inline = TRUE) 
				            ) 
				          ) 
				        ) ,
				        fluidRow(column(12, uiOutput("sc1a1oup2.ui"))), 
				        downloadButton("sc1a1oup2.pdf", "Download PDF"), 
				        downloadButton("sc1a1oup2.png", "Download PNG"), br(), 
				        div(style="display:inline-block", 
				            numericInput("sc1a1oup2.h", "PDF / PNG height:", width = "138px", 
				                         min = 4, max = 20, value = 6, step = 0.5)), 
				        div(style="display:inline-block", 
				            numericInput("sc1a1oup2.w", "PDF / PNG width:", width = "138px", 
				                         min = 4, max = 20, value = 8, step = 0.5)) 
					)
				),

			),
			tabPanel(
				"module-2"
			)

		)

	)

)