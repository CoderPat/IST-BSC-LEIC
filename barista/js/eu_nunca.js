var questions = [["Eu nunca precisei de um advogado", 0, 0 ],
				 ["Eu nunca bebi tres shots de seguida", 0, 0],
				 ["Eu nunca fui revistado pela policia", 0, 0]];

function get_random_question(){
	return questions[Math.floor(Math.random()*questions.length)];
}

$(document).ready(function() {
	$('.question h3').text(get_random_question()[0]);
});