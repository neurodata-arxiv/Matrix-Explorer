var initTable = function(table) {
		var col_sel = []; 
		var col_class = 1; 
		var row_sel = [];
	 
		$('#table tbody').on( 'click', 'td', function () {
			var colIdx = table.cell(this).index().column;
		
			if ( table.column( colIdx ).nodes().to$().hasClass( 'col_selected_1' )) {
				table.column( colIdx ).nodes().to$().removeClass( 'col_selected_1' );
				table.column( colIdx ).nodes().to$().addClass( 'col_selected_2' );
				col_sel.splice(col_sel.indexOf(colIdx),1)
				table.column( col_class ).nodes().to$().removeClass( 'col_selected_2' );
				col_class = colIdx;
				Shiny.onInputChange('col_class',col_class);
				Shiny.onInputChange('col_sel',col_sel);
			} else if ( table.column( colIdx ).nodes().to$().hasClass( 'col_selected_2' )){
				table.column( colIdx ).nodes().to$().removeClass( 'col_selected_2' );
			} else {
				table.column( colIdx ).nodes().to$().removeClass( 'col_selected_1' );
				table.column( colIdx ).nodes().to$().removeClass( 'col_selected_2' );
				table.column( colIdx ).nodes().to$().addClass( 'col_selected_1' );
				col_sel.push(colIdx);
				Shiny.onInputChange('col_sel',col_sel);
			};
		})
}

//setTimeout(function(){Shiny.onInputChange('col_sel',20)},0);