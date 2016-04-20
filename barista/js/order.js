function add_to_order(items) {
	var current = decookienize('order');
	if(current === false)
		cookienize(items, 'order');
	else{
		new_items = [];
		for(var i = 0; i < items.length; i++){
			var index = current.findIndex(function (element, index, array){ return element[0] === items[i][0] });
			if(index > -1)
				current[index][2] = parseInt(current[index][2]) + parseInt(items[i][2]);
			else
				new_items.push(items[i]);
		}
		cookienize(current.concat(new_items), 'order');
	}
}

function get_order(){
	var current = decookienize('order');
	return current === false ? [] : current;
}


$(function() {
   var shown = false;
   $('#bill_button').on("click", function() {
   		var order = get_order();
   		var order_string = "";
   		for(var i = 0; i < order.length; i++)
   			order_string += "<span class='left'>" + order[i][2] + "x " + order[i][0] + "</span>" + 
   							"<span class='right'> &nbsp&nbsp€" + parseInt(order[i][2])*parseInt(order[i][1]) + "</span>" + 
   							"<br>";

   		console.log(order_string);

    	shown ? $(this).hideBalloon() : $(this).showBalloon( {  contents: order_string,
    															tipSize: 30,
																showDuration: "slow",
																showAnimation: function(d, c) { this.fadeIn(d, c);},
																css: {
																	border: 'solid 4px #5baec0',
																	boxShadow: "2px 2px 2px #888",
																	padding: '10px',
																	fontFamily: 'Open Sans',
																	fontSize: '120%',
																	lineHeight: '2',
																	backgroundColor: '#eaf8fc',
																	color: '#5baec0'
																}
		});
		shown = !shown;
	});
}); 