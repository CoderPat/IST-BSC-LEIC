$(window).load(function(){

	$('.coverflow').coverflow({index:2});

	$('.coverflow').coverflow('option', 'enableClick', true);


	var vote_message = document.getElementById("vote_message");


	document.getElementsByClassName("close")[1].onclick= function() {
	    vote_message.style.display = "none";
	}

	var all_musics = [["wilder mind", "<img class='cover' id='music_1' src='icons/z-m.jpg' />"], 
					  ["just say yes", "<img class='cover' id='music_2' src='icons/snow.jpg' />"],
					  ["communion", "<img class='cover' id='music_3' src='icons/y.jpg' /> "], 
					  ["arcade fire", "<img class='cover' id='music_4' src='icons/su.jpg' /> "],
					  ["barbie girl" ,"<img class='cover' id='music_5' src='icons/z-a.jpg' /> "]]

	var top_musics = ["<img class='cover' id='music_1' src='icons/z-m.jpg' />", 
					  "<img class='cover' id='music_2' src='icons/snow.jpg' />",
					  "<img class='cover' id='music_5' src='icons/z-a.jpg' /> "]


	$('#top_music_button').click(function(){
		$('.coverflow').empty();
		for(var i = 0, len=top_musics.length; i< len; i++){
			$('.coverflow').append(top_musics[i]);
		}
		$('.coverflow').coverflow('index', Math.floor(top_musics.length / 2));
		$('.coverflow').coverflow('refresh');
	});

	$('#all_button').click(function(){


		$('.coverflow').empty();
		for(var i = 0, len=all_musics.length; i< len; i++){
			$('.coverflow').append(all_musics[i][1]);
		}
		$('.coverflow').coverflow('index', Math.floor(all_musics.length / 2));
		$('.coverflow').coverflow('refresh');
	});

	$('#submit').click(function(){
		var search_term = document.getElementById("search").value;

		$('.coverflow').empty();
		for(var i = 0, len=all_musics.length; i< len; i++){
			if(all_musics[i][0].indexOf(search_term) !== -1){
				$('.coverflow').append(all_musics[i][1]);
			}
		}
		$('.coverflow').coverflow('index', 0);
		$('.coverflow').coverflow('refresh');
	});



});

//when user clicks on Votar
function voted(){
	vote_message.style.display = "block";
}
