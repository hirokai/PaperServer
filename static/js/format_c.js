var log = function(d) {console.log(d);};

var pluginEnabled = false;

$(function(){
var getMeta = function(name){
	return $('meta[name="' + name + '"]').prop('content');
};

	var elems = undefined;
	var pid = getMeta('paper_id');
	var h3 = $('h3');
	if(h3.length>=2){
		elems = h3;
	}else{
		elems = $('h4');
	}
	$.each(elems,function(){
		var e = $(this);
		e.prop("id",e.text());
		$('#tocpanel').append("<li><a href='#"+e.text()+"'>"+e.html()+"</a></li>");
	});
});



function showError(msg){
	log("Error: "+msg); //stub
}