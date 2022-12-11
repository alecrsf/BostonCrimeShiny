#====================================
#============== UI ==================
#====================================
overview_UI <- function(id) {
	ns <- NS(id)
	fluidPage(
	 column(8,
		fluidRow(
			column(width = 9,
						 fluidRow(
						 	input(
						 		'district',
						 		selectInput(
						 			inputId = ns("district"),
						 			label = "Select district",
						 			choices = levels(BostonCrime$neighborhood),
						 			selected = "Roxbury",
						 			width = "100%"
						 		),
						 		help = 'Select district to change graph'
						 	)
						 ),
						 fluidRow(div(id = 'card',
						 						 style = "height: 450px; padding-bottom:0;",
						 						 apexchartOutput(ns("apex1"))
						 						 )
						 				 )
						 ), 
			column(
				width = 3,
					div(
						id = 'card',
						style = "height: 535px; margin: 0px; padding: 0px;",
						apexchartOutput(ns("radar2"), height = "280px"),
						hr(style = "margin: 0px; padding: 0px;"),
					  apexchartOutput(ns("radar"), height = "280px")
				)
			)
		),
		fluidRow(
			metrics_UI2(ns("streetlights"), 3, "Streetlights"),
			metrics_UI2(ns("density"), 3, "Violence Rate"),
			metrics_UI2(ns("median_age"), 3, "Median Age"),
			metrics_UI2(ns("median_income"), 3, "Median Income")
		)
	 ),
	 column(4,
	 			 fluidRow(div(
	 			 	id = 'card',
	 			 	style = "height: 350px;",
	 			 	h1("Offense Description", style = "font-size: 16px; margin-top: 0; padding-bottom:-15px;"),
	 			 	highchartOutput(ns("wordcloud"), width = "110%", height = "320")
	 			 )),
	 			 fluidRow(div(id = 'card',
	 			 						 	style = "height: 320px;",
	 			 					  	 h1("Most committed crimes", style = "font-size: 16px; margin-top: 0; padding-bottom:-15px;"),
	 			 						  highchartOutput(ns("pie"), width = "110%", height = "290")
	 			 						 )
	 			 					)
					)
	)
}


#==========================================================
#======================= SERVER ===========================
#==========================================================

overview_SERVER <- function(id) {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns
		
		#=========== Area =================
		observeEvent(ns(input$district), {
		output$apex1 <- renderApexchart({
		BostonCrime |> 
			group_by(neighborhood, year, month) |>
			filter(crime_type %in% c("violence","robbery", "burglary", "drugs")) |> 
			count(crime_type) |> 
			mutate('date' = lubridate::make_date(year = year, 
																					 month = month)) |> 
			filter(neighborhood == input$district) |> 
			apex(type = "area", 
					 mapping = aes(x = date, y = n, fill = crime_type)
			) |>
			ax_yaxis(decimalsInFloat = 2) |> 
			ax_chart(stacked = TRUE) %>%
			ax_colors(painter::Palette("#22223b", "#F2E9E4", 4))
			})	
		})
		
		#=========== Radar =================
		
		observeEvent(ns(input$variable), {
			df <- BostonCrime %>% group_by(neighborhood) %>% summarise(
				commercial_mix_ratio = mean(commercial_mix_ratio),
				industrial_mix_ratio = mean(industrial_mix_ratio), 
				owner_occupied_ratio = mean(owner_occupied_ratio),
				poverty_rate = mean(poverty_rate), 
				residential_gini_coef = mean(residential_gini_coef),
				#-----------------------------------------------------
				less_than_high_school_perc = mean(less_than_high_school_perc),
				bachelor_degree_or_more_perc = mean(bachelor_degree_or_more_perc),
				enrolled_college_perc = mean(enrolled_college_perc)
			)
			
			y <- reactive({df |> 
				filter(neighborhood == input$district) |> 
				select(enrolled_college_perc) |> pull() |> round(2) * 100
			})
			
			output$radar <- renderApexchart({
			apex(data = NULL, type = "radialBar", 
					 mapping = aes(x="Graduated", y = y())) |> ax_fill(colors = "#22223b") |> 
					ax_plotOptions(
						radialBar = radialBar_opts(
							dataLabels = list(
								name = list(
									fontFamily = "IBM Plex",
									fontSize = "18px",
									color = "#22223b"
								),
								value = list(
									fontFamily = "IBM Plex",
									fontSize = "20px",
									color = "#22223b",
									formatter = htmlwidgets::JS("function (val) {return val + '%';}")
								)
							)
						)
					)
				})
			
			y2 <- reactive({df |> 
					filter(neighborhood == input$district) |> 
					select(poverty_rate) |> pull() |> round(2) * 100
			})
			
			output$radar2 <- renderApexchart({
				apex(data = NULL, type = "radialBar",
						 mapping = aes(x="Poverty rate", y2 = y2())) |> 
					ax_fill(colors = "#22223b") |> 
					ax_plotOptions(
						radialBar = radialBar_opts(
							dataLabels = list(
								name = list(
									fontFamily = "IBM Plex",
									fontSize = "18px",
									color = "#22223b"
								),
								value = list(
									fontFamily = "IBM Plex",
									fontSize = "20px",
									color = "#22223b",
									formatter = htmlwidgets::JS("function (val) {return val + '%';}")
								)
							)
						)
					)
			})
			})
		
		#=========== Wordcloud ==================
		observeEvent(ns(input$district), {
		output$wordcloud <- renderHighchart({
			BostonCrime |> filter(neighborhood == input$district) |> select(offense_description) |> 
				map(str_to_lower) |> 	
				reduce(str_c) |> 
				str_split("\\s+") |> 
				unlist() |> 
				tibble() |> 
				setNames("word") |> 
				count(word, sort = TRUE) |> 
				anti_join(tidytext::stop_words, by = "word") |> 
				filter(word %!in% c(levels(BostonCrime$crime_type), "no", "&", "(no",
														"-", "/", "mv", "mfr", "etc.", "%", "class", "poss")) |>
				mutate(word = str_to_upper(word)) |> 
				hchart("wordcloud", hcaes(name = word, weight = log(n))) |> 
				hc_colors(painter::Palette("#22223b", "#F2E9E4", 50)) 
		 })
		})
		
		#=========== Counters ==================
		
		observeEvent(ns(input$district), {
			streetlights <-  reactive({
				BostonCrime |>
					group_by(neighborhood) |>
					summarize(streetlights = mean(streetlights)) |>
					filter(neighborhood == input$district) |> pull() |> round(0)
			})
			
			output$streetlights <- renderCountup({
				streetlights <- streetlights()
				countup(streetlights, duration = 1,
								options = list(suffix = ' each 100m'))
			})
		})
		
		observeEvent(ns(input$district), {
			density <-  reactive({
				crimes_avg |>
					select(neighborhood, avg_violence) |>
					left_join(
						BostonCrime |>
							select(neighborhood, neighborhood_area_sqmi) |>
							group_by(neighborhood) |>
							summarise(area_sqmi = mean(neighborhood_area_sqmi)),
						by = "neighborhood"
					) |>
					mutate(density = avg_violence / area_sqmi) |>
					filter(neighborhood == input$district) |>
					pull()
			})
			
			output$density <- renderCountup({
				density <- density()
				countup(density,
								duration = 1,
								options = list(suffix = ' per mi2'))
			})
		})
		
		observeEvent(ns(input$district), {
			median_age <-  reactive({
				BostonCrime |>
					group_by(neighborhood) |>
					summarize(median_age = mean(median_age)) |>
					filter(neighborhood == input$district) |> pull()
			})
			
			output$median_age <- renderCountup({
				median_age <- median_age()
				countup(median_age,
								duration = 1,
								options = list(suffix = ' Y.O.'))
			})
		})
		
		observeEvent(ns(input$district), {
			median_income <-  reactive({
				BostonCrime |>
					group_by(neighborhood) |>
					summarize(median_income = mean(median_income)) |>
					filter(neighborhood == input$district) |> pull()
			})
			
			output$median_income <- renderCountup({
				median_income <- median_income()
				countup(
					median_income,
					duration = 1,
					options = list(
						useEasing = TRUE,
						useGrouping = TRUE,
						separator = ",",
						prefix = "$"
					)
				)
			})
			
		})
		
		
		#=========== Pie ==================
		observeEvent(ns(input$district), {
		output$pie <- renderHighchart({
			df <- BostonCrime |> filter(neighborhood == input$district) |> 
				count(crime_type, sort = T) 
			
			highchart() %>%
				hc_add_series(df, type = "pie", hcaes(x = crime_type, y = n), 
											minPointSize= 10, innerSize= '20%') %>%
				hc_tooltip(pointFormat = '<b>{point.percentage:.1f}%</b>') |> 
				hc_colors(painter::Palette("#22223b", "#F2E9E4", 9)) |> 
				hc_exporting(enabled = TRUE) |> 
				hc_plotOptions(
					pie = list(
						dataLabels = list(
							style = list(
								fontFamily = "IBM Plex",
								fontSize = "15px",
								textTransform = "capitalize"
							)
						)
					)
				)
			})
		})
	})
}