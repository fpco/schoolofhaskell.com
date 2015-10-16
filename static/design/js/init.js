
	jQuery(function($) {
	
		$(document).on('click','.open-login-btn',function(e) {
			$('#signinContainer').stop(true,true).animate({'margin-top':0},250);
			$('#loginEmail').focus();
			e.preventDefault();
		});
		
		$(document).on('click','#close',function(e) {
			var container = $('#signinContainer');
			container.stop(true,true)
			  .animate({'margin-top': (5 - container.height())}, 250, "swing", function() { container.removeAttr('style'); });
			e.preventDefault();
		});
		
		$(document).on('click','.show a',function(e) {
			var $o = $(this).html();
			var $n = $(this).attr('data-toggle');
			$(this).toggleClass('open').html($n).attr('data-toggle',$o).closest('.show').next('.solution').slideToggle();
			e.preventDefault();
		});
			
	});
