#====================================
#============== UI ==================
#====================================

drilldown_UI <- function(id) {
	ns <- NS(id)
	fluidPage(
		fixedPanel(
			top = 0,
			left = 0,
			width = 300,
			height = '100%',
			wellPanel(
				id = 'controls',
				draggable = FALSE,
				div(
					img(
						src = 'boston_logo.png',
						height = 200,
						width = 200,
						style = 'display: block; margin: 30px auto 20px; position: relative;'
					)
				),
				h4("Visualize Criminality"),
				hr(),
				input(
					'year',
					selectInput(
						inputId = ns("year"),
						label = "Select year",
						choices = 2016:2019,
						selected = 2018,
						width = "100%"
					),
					help = 'Select year to change graph'
				),
				hr(),
				p(
					style = 'text-align: justify',
					"This Shiny dashboard will help you understand the number of different crime types
					in the City of Boston. You will get an overview of the criminality within each district
					and ZIP5 area."
				),
				p(
					style = 'text-align: justify',
					"In this first page, is it possible to get also the number of crimes by types in absolute
					value and its percentage change compared with the previous year. Data is only available
					from 2016 to 2019."
				),
				hr()
			)
		),
		
		div(
			class = 'mainPanel',
			div(id = 'card',
					fluidRow(
						column(12,
									 fluidRow(
									 	column(5, h1("Crimes by Neighborhood")),
									 	column(
									 		7,
									 		br(),
									 		actionLink(
									 			paste0("b", '_help'),
									 			NULL,
									 			icon = icon('circle-question', lib = 'font-awesome',
									 									verify_fa = FALSE)
									 		),
									 		bsPopover(
									 			paste0("b", '_help'),
									 			placement = 'right',
									 			trigger = 'hover',
									 			title = NULL,
									 			content = "Click the bars to expand the district and get the respective values within their ZIP5 area",
									 			options = list(container = 'body')
									 		)
									 	)
									 ), br(),
									 highchartOutput(ns("drilldown"))
									 )
								)
					),
			metrics_UI(
				ns("count1"),
				3,
				"Violence",
				textOutput(ns("sel_year1")),
				ns("get_percent1")
			),
			metrics_UI(
				ns("count2"),
				3,
				"Drugs",
				textOutput(ns("sel_year2")),
				ns("get_percent2")
			),
			metrics_UI(
				ns("count3"),
				3,
				"Thefts",
				textOutput(ns("sel_year3")),
				ns("get_percent3")
			),
			metrics_UI(
				ns("count4"),
				3,
				"Frauds",
				textOutput(ns("sel_year4")),
				ns("get_percent4")
			)
		)
	)
}




#==========================================================
#======================= SERVER ===========================
#==========================================================

drilldown_SERVER <- function(id) {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns
		
		#=============== DRILLDOWN =====================
		# Prepare Data
		bc_year <- reactive({
			BostonCrime |>
				group_by(neighborhood, zip5, year, median_income, poverty_rate) |>
				summarise(
					N = n(),
					median_income = mean(median_income),
					poverty_rate = mean(poverty_rate)
				) |>
				arrange(desc(N)) |>
				mutate(across(where(is.numeric), round, 3)) |> as_tibble() |>
				filter(year %in% input$year)
		})
		
		bc_column <- reactive({
			bc_year() |>
				group_by(neighborhood) |>
				summarise(
					N = sum(N),
					median_income = mean(median_income),
					poverty_rate = mean(poverty_rate)
				) |>
				mutate(across(where(is.numeric), round, 3)) |>
				arrange(desc(N))
		})
		
		bc_drill <- reactive({
			bc_year() |>
				group_nest(neighborhood) |>
				mutate(
					id = neighborhood,
					type = "column",
					# in the drilldown we'll give the mapping via creating the columns
					data = map(data, mutate, name = zip5, y  = N),
					data = map(data, list_parse)
				)
		})
		
		# Chart
		output$drilldown <- renderHighchart({
			bc_column <- bc_column()
			bc_drill <- bc_drill()
			
			hchart(
				bc_column,
				"column",
				hcaes(
					x = neighborhood,
					y = N,
					name = neighborhood,
					drilldown = neighborhood
				),
				name = "Crimes",
				colorByPoint = T
			) |>
				hc_drilldown(
					allowPointDrilldown = TRUE,
					activeAxisLabelStyle = list(textDecoration = "none",
																			color = "black"),
					series = list_parse(bc_drill)
				) |>
				hc_tooltip(
					pointFormat = tooltip_table(
						c("Number of crimes", "Median Income"),
						c(" {point.N}", " {point.median_income}")
					),
					useHTML = TRUE,
					valueDecimals = 0
				) |>
				hc_yAxis(
					title = list(text = "Number of crimes",
											 style = list()),
					labels = list(style = list())
				) |>
				hc_xAxis(title = "Neighborhood",
								 gridLineWidth = 0,
								 tickWidth = 0) |>
				hc_exporting(enabled = TRUE) |>
				hc_credits(text = "Chart created by Alessio Crisafulli Carpani using R and highcharter",
									 enabled = F) |>
				hc_plotOptions(column = list(borderWidth = 0)) |>
				hc_colors(painter::Palette("#22223b", "#F2E9E4", 25))
			
		})
		
		#========================= COUNTER ====================================
		
		observeEvent(ns(input$year), {
			value1 <-  reactive({
				BostonCrime |>
					group_by(crime_type) |>
					filter(year %in% input$year) |>
					filter(crime_type == "violence") |> count() |>
					pull(n)
			})
			
			output$count1 <- renderCountup({
				value1 <- value1()
				countup(value1, duration = 1)
			})
		})
		
		observeEvent(ns(input$year), {
			value2 <-  reactive({
				BostonCrime |>
					group_by(crime_type) |>
					filter(year %in% input$year) |>
					filter(crime_type == "drugs") |> count() |>
					pull(n)
			})
			
			output$count2 <- renderCountup({
				value2 <- value2()
				countup(value2, duration = 1)
			})
		})
		
		observeEvent(ns(input$year), {
			value3 <-  reactive({
				BostonCrime |>
					group_by(crime_type) |>
					filter(year %in% input$year) |>
					filter(crime_type == "theft") |> count() |>
					pull(n)
			})
			
			output$count3 <- renderCountup({
				value3 <- value3()
				countup(value3, duration = 1)
			})
		})
		
		observeEvent(ns(input$year), {
			value4 <-  reactive({
				BostonCrime |>
					group_by(crime_type) |>
					filter(year %in% input$year) |>
					filter(crime_type == "fraud") |> count() |>
					pull(n)
			})
			
			output$count4 <- renderCountup({
				value4 <- value4()
				countup(value4, duration = 1)
			})
		})
		
		#============= Render text in Counter UI ==================
		
		observeEvent(ns(input$year), {
			output$sel_year1 <- renderText({
				paste("Tot. Number of Crimes in", input$year)
			})
			output$sel_year2 <- renderText({
				paste("Tot. Number of Crimes in", input$year)
			})
			output$sel_year3 <- renderText({
				paste("Tot. Number of Crimes in", input$year)
			})
			output$sel_year4 <- renderText({
				paste("Tot. Number of Crimes in", input$year)
			})
		})
		
		#============= Render percent increase in Counter UI ==================
		
		observeEvent(ns(input$year), {
			new <- BostonCrime |>
				group_by(crime_type) |>
				filter(year %in% input$year) |>
				filter(crime_type == "violence") |> count() |>
				pull(n)
			old <- BostonCrime |>
				group_by(crime_type) |>
				filter(year %in% (as.numeric(input$year) - 1)) |>
				filter(crime_type == "violence") |> count() |>
				pull(n)
			
			value1 <- reactive({
				((new - old) / old) * 100
			})
			
			output$get_percent1 <- renderUI({
				value1 <- value1()
				get_percent(value1, invert_colors = TRUE)
			})
		})
		
		observeEvent(ns(input$year), {
			new <- BostonCrime |>
				group_by(crime_type) |>
				filter(year %in% input$year) |>
				filter(crime_type == "drugs") |> count() |>
				pull(n)
			old <- BostonCrime |>
				group_by(crime_type) |>
				filter(year %in% (as.numeric(input$year) - 1)) |>
				filter(crime_type == "drugs") |> count() |>
				pull(n)
			
			value2 <- reactive({
				((new - old) / old) * 100
			})
			
			output$get_percent2 <- renderUI({
				value2 <- value2()
				get_percent(value2, invert_colors = TRUE)
			})
		})
		
		observeEvent(ns(input$year), {
			new <- BostonCrime |>
				group_by(crime_type) |>
				filter(year %in% input$year) |>
				filter(crime_type == "theft") |> count() |>
				pull(n)
			old <- BostonCrime |>
				group_by(crime_type) |>
				filter(year %in% (as.numeric(input$year) - 1)) |>
				filter(crime_type == "theft") |> count() |>
				pull(n)
			
			value3 <-  reactive({
				((new - old) / old) * 100
			})
			
			output$get_percent3 <- renderUI({
				value3 <- value3()
				get_percent(value3, invert_colors = TRUE)
			})
		})
		
		observeEvent(ns(input$year), {
			new <- BostonCrime |>
				group_by(crime_type) |>
				filter(year %in% input$year) |>
				filter(crime_type == "fraud") |> count() |>
				pull(n)
			old <- BostonCrime |>
				group_by(crime_type) |>
				filter(year %in% (as.numeric(input$year) - 1)) |>
				filter(crime_type == "fraud") |> count() |>
				pull(n)
			
			value4 <-  reactive({
				((new - old) / old) * 100
			})
			
			output$get_percent4 <- renderUI({
				value4 <- value4()
				get_percent(value4, invert_colors = TRUE)
			})
		})
		
	})
}