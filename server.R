shinyServer(function(input, output, session) {
	session$onSessionEnded(stopApp)
	
	# Module server ------------
	drilldown_SERVER("drilldown")
	map_SERVER("map_table")
	overview_SERVER("overview")
})
