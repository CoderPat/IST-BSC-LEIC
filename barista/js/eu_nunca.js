var questions = [["Eu nunca precisei de um advogado", 0, 0 ],
				 ["Eu nunca bebi tres shots de seguida", 0, 0],
				 ["Eu nunca fui revistado pela policia", 0, 0]];

function get_random_question(){
    var i =  (Math.floor(Math.random()*questions.length) + 1) % questions.length;
    return questions[i];
}

$(document).ready(function() {
	console.log("here");
	$('.question h3').text(get_random_question()[0]);

    $('#next_button').click(function(){
        $('.question h3').text(get_random_question()[0]);
    });

    $('#already_button').click(function(){
        for(var i = 0; i < questions.length; i++)
            if($('.question h3').text() === questions[i][0])
                questions[i][1]++;
    });

    $('#never_button').click(function(){
        for(var i = 0; i < questions.length; i++)
            if($('.question h3').text() === questions[i][0])
                questions[i][2]++;
    });

    $('#highscore_button').click(function(){

        item('cart');

        questions.sort(function(question_info1, question_info2){
            var p_never_1 = question_info1[2] === 0 ? 0 : question_info1[2]/(question_info1[1] + question_info1[2]);
            var p_never_2 = question_info2[2] === 0 ? 0 : question_info2[2]/(question_info2[1] + question_info2[2]);
            return p_never_2 - p_never_1;
        });

        $('.perguntas_div').empty();
        for(var i = 0; i < questions.length; i++){
            if (questions[i][2] != 0)
                $('.perguntas_div').append(questions[i][0]+'<br>' + questions[i][2] + '<br>');
        }
    });
});

// When a user cliks on a item
function item(id) {
  id = document.getElementById(id);
  if(id.style.display == 'block')
     id.style.display = 'none';
  else
     id.style.display = 'block';
}