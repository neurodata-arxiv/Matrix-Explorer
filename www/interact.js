var initTable = function(table) {
		var col_sel = []; 
		var col_class = "undefined"; 
		var row_sel = [];
		Shiny.onInputChange('row_sel',row_sel);
		Shiny.onInputChange('col_sel',col_sel);
		Shiny.onInputChange('col_class',col_class);

	 
	 
		$('#table tbody').on( 'click.dt', 'td', function () {
			var DT_rows_current = [];
			var colIdx = table.cell(this).index().column;
			
			var data = table.ajax.json().data
			for (var i = 0; i < data.length; i++) {
				DT_rows_current[i] = data[i][0]
			}
			
			if (colIdx == 0){
				var colIdy = table.cell(this).index().row;
				
				if ( table.row( colIdy ).nodes().to$().hasClass( 'row_selected' )) {
					table.row( colIdy ).nodes().to$().removeClass( 'row_selected' );
					row_sel.splice(row_sel.indexOf(DT_rows_current[colIdy]),1)
					Shiny.onInputChange('row_sel',row_sel);
				}
				else{
					table.row( colIdy ).nodes().to$().removeClass( 'row_selected' );
					table.row( colIdy ).nodes().to$().addClass( 'row_selected' );
					row_sel.push(DT_rows_current[colIdy]);
					Shiny.onInputChange('row_sel',row_sel);
				}
				
				
			} else{
				if ( table.column( colIdx ).nodes().to$().hasClass( 'col_selected_1' )) {
					table.column( colIdx ).nodes().to$().removeClass( 'col_selected_1' );
					table.column( colIdx ).nodes().to$().addClass( 'col_selected_2' );
					col_sel.splice(col_sel.indexOf(colIdx),1);
					if (col_class != "undefined"){
						table.column( col_class ).nodes().to$().removeClass( 'col_selected_2' );
					}
					col_class = colIdx;
					Shiny.onInputChange('col_class',col_class);
					Shiny.onInputChange('col_sel',col_sel);
				} else if ( table.column( colIdx ).nodes().to$().hasClass( 'col_selected_2' )){
					table.column( colIdx ).nodes().to$().removeClass( 'col_selected_2' );
					if (col_class == colIdx){
						col_class = "undefined";
					}
				} else {
					table.column( colIdx ).nodes().to$().removeClass( 'col_selected_1' );
					table.column( colIdx ).nodes().to$().removeClass( 'col_selected_2' );
					table.column( colIdx ).nodes().to$().addClass( 'col_selected_1' );
					col_sel.push(colIdx);
					Shiny.onInputChange('col_sel',col_sel);
				};
			}
		})
		
		var reselect_rows = function() {
			var DT_rows_current = [];		

			var data = table.ajax.json().data
			for (var i = 0; i < data.length; i++) {
				DT_rows_current[i] = data[i][0]
			}
				
			
			if (row_sel.length === 0) return;
			table.rows({page: 'current'}).every(function() {
				if (row_sel.indexOf(DT_rows_current[this.index()])!=-1) {
					$(this.node()).addClass('row_selected');
				}
			});
        } 
		
		var reselect_columns = function() {		
			if (col_sel.length === 0 && col_class === "undefined") return;
			table.columns().every(function() {
				if (col_sel.indexOf(this.index())!=-1) {
					$(this.nodes()).addClass('col_selected_1');
				} 
				if (col_class === this.index()){
					$(this.nodes()).addClass('col_selected_2');
				}
			});
        } 
		
		table.on('draw.dt', reselect_rows);
		table.on('draw.dt', reselect_columns);
}

