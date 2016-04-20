function add_to_order(items) {
	var current = decookienize('order');
	if(current === false)
		cookienize(items, 'order');
	else
		cookienize(current.concat(items), 'order');
}

function get_order(){
	var current = decookienize('order');
	return current === false ? [] : current;
}

