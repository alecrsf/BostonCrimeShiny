metrics_UI = function(id, width, title, description, get_percent) {
	column(width, style = 'padding: 0; top: 8px; height: 170px;',
				 div(id = 'card',
				 		div(
				 			id = 'metric',
				 			h4(title),
				 			p(description),
				 			h3(countupOutput(id)),
				 			hr(),
				 			div(style = "text-align: center;",
				 					uiOutput(get_percent))
				 		)))
}


metrics_UI2 = function(id, width, title) {
	column(width, style = 'text-align: center; padding: 0; top: 10px; height: 120px;',
				 div(id = 'card',
				 		div(
				 			id = 'metric',
				 			h4(title, style = "font-size: 18px;"),
				 			hr(),
				 			h3(countupOutput(id)),
				 			br()
				 		)))
}
