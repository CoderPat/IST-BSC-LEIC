
// Get the modal
var not_implemented_box = document.getElementById("not_implemented_box");
var vote_message = document.getElementById("vote_message");
var input = document.getElementById('quant_num');
input.value;

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
}

// When user clicks on one of the menu buttons in pedir page
function show_menu(x){
	var menu_section = document.getElementById("section_menu");
	var el_showned = document.getElementById(x);
	if(menu_section.style.right == "44%"){
		menu_section.style.right = 65 + "%";
		/*menu_section.style.top = 9 + "%";*/
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
		menu_section.style.right = 44 + "%";
		/*menu_section.style.top = 5 + "%";*/
	}
}

//when user clicks on Votar
function voted(){
	vote_message.style.display = "block";
}
document.getElementsByClassName("close")[1].onclick= function() {
    vote_message.style.display = "none";
}

/*When the user cliks on a order*/

function list(id) {
    if(list_box.style.display == 'block')
       list_box.style.display = 'none';
    else
       list_box.style.display = 'block';
}


// When a user cliks on a item
function item(id) {
  if(id.style.display == 'block')
     id.style.display = 'none';
  else
     id.style.display = 'block';
}


function add(v){
  var x = input;
  var y = v;
  var z = x + y;
  document.getElementById("demo").innerHTML = z;
}


// ---------Drag and drop functions

function allowDrop(ev) {
    ev.preventDefault();
}

function drag(ev) {
    ev.dataTransfer.setData("text", ev.target.id);
}

function drop(ev) {
    ev.preventDefault();
    var data = ev.dataTransfer.getData("text");
    ev.target.appendChild(document.getElementById(data));
}
