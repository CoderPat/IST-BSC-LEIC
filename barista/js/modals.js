
// Get the modal
var not_implemented_box = document.getElementById("not_implemented_box");
var vote_message = document.getElementById("vote_message");
var not_found_message= document.getElementById("not_found_message");
var none_voted_message= document.getElementById("none_voted_message");

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
	else if(event.target == vote_message){
		vote_message.style.display = "none";
    }
	
	else if(event.target == not_found_message){
		not_found.style.display = "none";
    }
	else if(event.target == none_voted_message){
		none_voted_message.style.display = "none";
    }
}

