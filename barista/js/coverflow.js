function get_html_cover_tag(music_info, percentage = false){
	 var html_tag = "<img class='cover' ";

	 if(percentage){
	 	html_tag += "data-percentage='" + percentage + "%'";
	 }

	 html_tag += "data-artist='" + music_info[0] + "' data-title='" + music_info[1] + "' src='" + music_info[2] + "' />";

	 return html_tag;
}

$(window).load(function(){

	$('.coverflow').coverflow({
		index: 			0,
		change:			function(event, cover) {
			var img = $(cover).children().andSelf().filter('img').last();
			$('#artist_name').text(img.data('artist') || 'unknown');
			$('#music_name').text(img.data('title') || 'unknown');
			$('#percentage_votes').html(img.data('percentage') || '&nbsp;');
		}
	});

	$('.coverflow').coverflow('option', 'enableClick', true);


	var vote_message = document.getElementById("vote_message");
	var not_found_message = document.getElementById("not_found_message");
	var none_voted_message = document.getElementById("none_voted_message");
	
	$('#close_vote_message').click( function() {
	    vote_message.style.display = "none";
	});
	
	$('#close_not_found_message').click( function() {
	    not_found_message.style.display = "none";
	});
	
	$('#close_none_voted_message').click( function() {
	    none_voted_message.style.display = "none";
	});

	var all_musics = [["Mumford and Sons", "qualquercoisa", "icons/z-m.jpg", 0], 
					  ["Snow Patrol", "qualquercoisa", "icons/snow.jpg", 0],
					  ["Yes and Yes", "qualquercoisa", "icons/y.jpg", 0], 
					  ["Arcade fire", "qualquercoisa", "icons/su.jpg", 0],
					  ["Aqua" ,"qualquercoisa", "icons/z-a.jpg", 0]];



	$('#top_music_button').click(function(){
		$('.coverflow').empty();
		
		var none_voted = true;
		var none_voted_message = document.getElementById("none_voted_message");
		var top_musics = all_musics.slice();
		var total_votes = top_musics.reduce(function(sum, music_info){return sum + music_info[3]}, 0);
		top_musics.sort(function(music_info1, music_info2){return music_info2[3] - music_info1[3]})

		for(var i = 0, len=top_musics.length; i< len; i++){
			if(top_musics[i][3] !== 0){
				none_voted = false;
				$('.coverflow').append(get_html_cover_tag(top_musics[i], top_musics[i][3]*100/total_votes));
			}
		}
		$('.coverflow').coverflow('index', 0);
		$('.coverflow').coverflow('refresh');
		if(none_voted){
			$('#all_button').trigger('click');
			none_voted_message.style.display = "block";	
		}
	});

	$('#all_button').click(function(){

		$('.coverflow').empty();
		for(var i = 0, len=all_musics.length; i< len; i++){
			$('.coverflow').append(get_html_cover_tag(all_musics[i]));
		}
		$('.coverflow').coverflow('index', Math.floor(all_musics.length / 2));
		$('.coverflow').coverflow('refresh');
	});

	$('#submit').click(function(){
		var search_term = document.getElementById("search").value;
		var message_not_found = document.getElementById("not_found_message");
		var found= false;
		
		$('.coverflow').empty();
		for(var i = 0, len=all_musics.length; i< len; i++){
			if(all_musics[i][0].toLowerCase().indexOf(search_term.toLowerCase()) !== -1 || all_musics[i][1].toLowerCase().indexOf(search_term.toLowerCase()) != -1){
				$('.coverflow').append(get_html_cover_tag(all_musics[i]));
				found = true;
			}
		}
		
		$('.coverflow').coverflow('index', 0);
		$('.coverflow').coverflow('refresh');
		if(!found){
			$('#all_button').trigger('click');
			message_not_found.style.display = "block";	
		}
	});


	$('#all_button').trigger('click');


	//when user clicks on Votar
	$('#vote_button').click( function(){
		vote_message.style.display = "block";
		var voted = $('.coverflow').coverflow('index');
		all_musics[voted][3] += 1;
	});

});
