input = function(input_id, input_element, help) {
	div(column(
		12,
		style = 'padding-left: 0;',
		input_element,
		absolutePanel(
			top = 0,
			right = 15,
			div(
				style = 'font-size: 10pt;',
				actionLink(
					paste0(input_id, '_help'),
					NULL,
					icon = icon('circle-question', lib = 'font-awesome',
											verify_fa = FALSE)
				),
				bsPopover(
					paste0(input_id, '_help'),
					placement = 'right',
					trigger = 'hover',
					title = NULL,
					content = help,
					options = list(container = 'body')
				)
			)
		)
	))
}