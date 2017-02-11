$(function() {
	$('#draw_sig').signature();
	$('#clear').click(function() {
		$('#draw_sig').signature('clear');
	});
	$('#sig_submit').click(function() {
		$('#sig_svg').replaceWith($('#draw_sig').signature('toSVG'));
		$('#draw_sig').hide();
		$('#submit_button').removeClass('shinyjs-hide');
	});
});