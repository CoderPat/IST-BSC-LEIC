$(function() {
    $('#coverflow').coverflow({ active: 2,
								select: function(event, ui) {
									       console.log('here');
								        }
	});

	$('#coverflow img').click(function() {
		if (!$(this).hasClass('ui-state-active')) {
				return;
		}

		$('#coverflow').coverflow('next');
	});
});