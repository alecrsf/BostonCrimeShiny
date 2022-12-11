metrics_UI = function(id, width, title, description) {
	ns <- NS(id)
	column(width, style = 'padding: 0; height: 800px;',
				 div(id = 'card',
				 		div(
				 			id = 'metric',
				 			h4(title),
				 			p(description),
				 			h1(countupOutput(ns(id)))
				 		)))
}

metrics_SERVER = function(id) {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns
		
		observeEvent(input_year, {
			value <-  reactive({
				BostonCrime |>
					group_by(crime_type) |>
					filter(year %in% input_year) |>
					filter(crime_type == "violence") |> count() |>
					pull(n)
			})
			
			output$count1 <- renderCountup({
				countup(value(), duration = 2)
			})
		})
	})
}