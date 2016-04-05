
// Get the modal
var not_implemented_box = document.getElementById("not_implemented_box");

// Get the <span> element that closes the modal
var close_span = document.getElementsByClassName("close")[0];


// When the user clicks on the button, open the modal 
function not_implemented_fn() {
    not_implemented_box.style.display = "block";
}

// When the user clicks on <span> (x), close the modal
close_span.onclick = function() {
    not_implemented_box.style.display = "none";
}

// When the user clicks anywhere outside of the modal, close it
window.onclick = function(event) {
    if (event.target == not_implemented_box) {
        not_implemented_box.style.display = "none";
    }
}


function show_menu(x){
	var menu_section = document.getElementById("section_menu");
	var el_showned = document.getElementById(x);
	if(menu_section.style.right == "45%"){
		menu_section.style.right = 65 + "%";
		menu_section.style.top = 9 + "%";
		el_showned.style.display = "block";
		previous_displayed = el_showned;
	}
	else if(menu_section.style.right == "65%" && el_showned.style.display != "block"){
		previous_displayed.style.display = "none";
		el_showned.style.display = "block";
		previous_displayed = el_showned;
	}
	
	else {
		previous_displayed.style.display = "none";
		menu_section.style.right = 45 + "%";
		menu_section.style.top = 5 + "%";
	}
}