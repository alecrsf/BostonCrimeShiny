shinyUI(
	tagList(
	  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
	navbarPage(
	id = 'navbar', collapsible = TRUE,
	windowTitle = app_title,
	title = app_title,
	tabPanel("Intro", drilldown_UI("drilldown")),
	tabPanel("Map", map_UI("map_table")),
	tabPanel("Overview", overview_UI("overview")),
#	tabPanel("Table", table_UI("table")),
 )
)
)