
	jQuery(function($) {
	
		$('input[placeholder]')
           	.each(function() { if($(this).val() == '') { $(this).val($(this).attr('placeholder')); } })
           	.focusin(function() { if($(this).val() == $(this).attr('placeholder')) { $(this).val(''); } })
           	.focusout(function() { if($(this).val() == '') { $(this).val($(this).attr('placeholder')); } });
			
	});