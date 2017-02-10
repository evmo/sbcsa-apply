$(function() {
	$('#sig').signature();
	$('#clear').click(function() {
		$('#sig').signature('clear');
	});
	$('#sig_submit').click(function() {
		$('#sig_json').replaceWith($('#sig').signature('toJSON')).hide();
		$('#sig_svg').replaceWith($('#sig').signature('toSVG'));
	});
});