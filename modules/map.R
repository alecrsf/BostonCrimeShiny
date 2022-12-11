#====================================
#============== UI ==================
#====================================
map_UI <- function(id) {
	ns <- NS(id)
	fluidPage(fluidRow(column(
		4,
		div(
			id = 'card',
			style = "margin-left:-20px; height: 680px;",
			fluidRow(column(4, h1("Table")),
							 column(
							 	8,
							 	br(),
							 	actionLink(
							 		paste0("a", '_help'),
							 		NULL,
							 		icon = icon('circle-question', lib = 'font-awesome',
							 								verify_fa = FALSE)
							 	),
							 	bsPopover(
							 		paste0("a", '_help'),
							 		placement = 'right',
							 		trigger = 'hover',
							 		title = NULL,
							 		content = "Click the name of the varibale to sort values",
							 		options = list(container = 'body')
							 	)
							 )),
			br(),
			reactableOutput(ns("map_table"), height = 600)
		)
	),
	column(
		8,
		div(
			id = 'card',
			style = "margin-left:-10px; width: 100%; height: 680px",
			h1("Map"),
			hr(),
			fluidRow(
			column(4,
				radioGroupButtons(
					inputId = ns("outUI"),
					label = "Type",
					size = "sm",
					choices = c("Clusters", "Cloropleth"),
					selected = "Clusters",
					justified = TRUE
				)
			),
			column(4,
				dateRangeInput(
					inputId = ns("date"),
					label = "Date",
					start = min(BostonCrime$date),
					end = max(BostonCrime$date),
					format = "d M yyyy"
				)
			),
			column(4,
				pickerInput(
			 	inputId = ns("crime"),
			 	label = "Filter crime types",
			 	choices = levels(BostonCrime$crime_type),
			 	options = list(`actions-box` = TRUE),
			 	multiple = TRUE,
			 	selected = levels(BostonCrime$crime_type)
			 )
			), 
			),
			uiOutput(ns("mapUI"))
		)
	)))
}




#==========================================================
#======================= SERVER ===========================
#==========================================================

map_SERVER <- function(id) {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns
		
		#=============== Table =====================
		output$map_table <- renderReactable({
			crimes |>
				reactable(
					defaultColDef = colDef(
						maxWidth = 90,
						headerClass = "table-header",
						class = JS(
							"function(rowInfo, column, state) {
               // Highlight sorted columns
               for (let i = 0; i < state.sorted.length; i++) {
               if (state.sorted[i].id === column.id) {
               return 'sorted'}
               }}"),
						footer = function(values) {
							if (!is.numeric(values)) return()
							sparkline(values, type = "box", width = 100, height = 30)
						}
					),
					columns = list(neighborhood = colDef(
						width = 110,
						sticky = "left",
						style = list(
							fontWeight = 400,
							backgroundColor = bg_color,
							borderRight = "1px solid #eee"
						)
					)),
					showPageInfo = FALSE,
					showPageSizeOptions = TRUE,
					defaultPageSize = 10,
					showSortIcon = FALSE,
					highlight = TRUE,
					borderless = TRUE,
					striped = TRUE,
					outlined = TRUE,
					searchable = TRUE,
					compact = F,
					wrap = TRUE,
					paginationType = "simple",
					theme = reactableTheme(
						color = text_color,
						backgroundColor = bg_color,
						borderColor = text_color_lighter,
						borderWidth = "1px",
						highlightColor = text_color_lighter,
						cellPadding = "10px 5px",
						cellStyle = list(
							display = "flex",
							flexDirection = "column",
							justifyContent = "center"
						),
						#------ CSS ----------
						style = list(
							fontFamily = "IBM Plex",
							fontSize = "14px",
							'.table-header[aria-sort="ascending"]' = list(boxShadow = "inset 0 10px 0 -6px #a4133c"),
							'.table-header[aria-sort="descending"]' = list(boxShadow = "inset 0 -10px 0 -6px #a4133c"),
							".sorted" = list(backgroundColor = "hsla(0, 0%, 60%, 0.1)")
						),
						headerStyle = list(
							color = text_color_light,
							borderColor = gray,
							borderWidth = "2px",
							fontWeight = 600,
							fontSize = "10px",
							letterSpacing = "0.9px",
							textTransform = "uppercase",
							"&:hover, &:focus" = list(color = text_color)
						),
						searchInputStyle = list(width = "100%"),
						#----------------- Pagination -----------------------------
						paginationStyle = list(color = text_color_light),
						pageButtonHoverStyle = list(backgroundColor = text_color_lighter),
						pageButtonActiveStyle = list(backgroundColor = text_color_lighter)
					)
				)
		})
		
		#=============== Map =====================
		
		# Clusters --------------------------------------
		observeEvent(c(ns(input$date), ns(input$crime)), {
			output$clusters <- renderLeaflet({
				leaflet(BostonCrime |>
									filter(date >= input$date[1] & date <= input$date[2],
												 crime_type %in% input$crime)) |>
					addProviderTiles(providers$CartoDB.Positron) |>
					addProviderTiles(providers$Stamen.TonerLines,
													 options = providerTileOptions(opacity = 0.35)) |>
					addProviderTiles(providers$Stamen.TonerLabels) |>
					addMarkers(clusterOptions = markerClusterOptions(),
										 clusterId = "quakesCluster") |>
					addMeasure(
						position = "bottomleft",
						primaryLengthUnit = "meters",
						primaryAreaUnit = "sqmeters",
						activeColor = "#3D535D",
						completedColor = "#7D4479",
					) |> 
					addMiniMap(
						tiles = providers$Esri.WorldStreetMap,
						toggleDisplay = TRUE,
						width = 100, height = 100
					) |> 
					addEasyButton(easyButton(states = list(
						easyButtonState(
							stateName = "unfrozen-markers",
							icon = "ion-toggle",
							title = "Freeze Clusters",
							onClick = JS("function(btn, map) {
													 var clusterManager = 
													 map.layerManager.getLayer('cluster', 'quakesCluster');
													 clusterManager.freezeAtZoom();
													 btn.state('frozen-markers');}")
						),
						easyButtonState(
							stateName = "frozen-markers",
							icon = "ion-toggle-filled",
							title = "UnFreeze Clusters",
							onClick = JS("function(btn, map) {
													 var clusterManager =
													 map.layerManager.getLayer('cluster', 'quakesCluster');
													 clusterManager.unfreeze();
													 btn.state('unfrozen-markers');}")
							)
						)
					)
				)
			})
		})
		
		# Cloropleth -------------------------------------
		output$cloropleth <- renderLeaflet({
			bins <- c(0, 300, 1000, 2000, 4000, 5000, 7000, 10000, 20000,30000, 39000)
			palette <- colorBin(painter::Palette("#F2E9E4", "#22223b", 11), 
													domain = mapdata$Crimes, bins = bins)
			
			leaflet(mapdata) |>
				addProviderTiles(providers$CartoDB.PositronNoLabels)|> 
				addPolygons(
					fillColor = ~palette(Crimes),
					weight = 2,
					opacity = 1,
					color = "white",
					dashArray = "3",
					fillOpacity = 0.7
				)
		})

		observeEvent(ns(input$outUI), {
		output$mapUI <- renderUI({
			switch(input$outUI,
				"Clusters" = leafletOutput(ns("clusters"), height = "500"),
				"Cloropleth" = leafletOutput(ns("cloropleth"), height = "500")
				)
			})
		})
		
	})
}