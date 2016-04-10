$(window).load(function(){
$(function() {
    $('#coverflow').coverflow({ active: 2,
								select: function(event, ui) {
									       console.log('here');
								        }
	});


});
});