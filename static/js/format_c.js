var log = function(d) {console.log(d);};

var pluginEnabled = false;

var refHtml = [];
var paperId = undefined;

var State = Backbone.Model.extend({
	defaults:{
		modal: null,   //"info", "figs", "toc", "tags", "comment", null
		figIdx: null
	},
	initialize: function(d){
		this.on('change:modal',function(m){
			var m = this.get('modal');
			console.log('modal changed.'+m);
			if(m == null){
				$('#btn-info').removeClass('active');
				$('#btn-toc').removeClass('active');
				$('#btn-figs').removeClass('active');
			}
		})
	}
});

var app = {};
app.State = new State();

var getMeta = function(name){
	return $('meta[name="' + name + '"]').prop('content');
};

$(function(){


paperId = getMeta('paper_id');
console.log(paperId);
mkRefList(paperId);

var elems = undefined;
var h3 = $('div.maintext h3');
if(h3.length>=2){
	elems = h3;
}else{
	elems = $('div.maintext h4');
}

$.each(elems,function(){
	var e = $(this);
	e.prop("id",e.text());
	$('#tocpanel').append("<li><a href='#"+e.text()+"'>"+e.html()+"</a></li>");
});


$('a.ref').click(function(e){
	var id = $(e.target).attr('id');
	var r = refHtml[id];
	if(r){
		location.href = r.url;
	}
});


$('a.ref').mouseover(function(e){
	var names = $(e.target).attr('data-refid');
	console.log(names);
	var str = _.map(names,function(name){
		return "<nobr>"+ (refHtml[name] ? refHtml[name].html : '') + '</nobr>';
	}).join('<br>');
	$(e.target).tooltip({title: str != '' ? str : "(Reference not available)", html: true});
});

//$('a.ref').mouseout(function(e){
//	var id = $(e.target).attr('id');
//	statusBar(false);
//});

$('#invert-button').click(function(e){
	$('body').toggleClass('inverted');
	$('nav.navbar').toggleClass('navbar-inverse');
});

$('#btn-info').click(function(e){
	toggleInfo();
});

$('#btn-figs').click(function(e){
	toggleFigs();
});

$('.modal').on('hide.bs.modal',function(){
	app.State.set('modal',null)
});

$('#figTab a').click(function(e){
//	var i = _.indexOf(figIds,$(e.target).attr('id'));
//	console.log($(e.target).attr('href').slice(1),i);
//	app.State.set('figIdx',i);
});

$(window).resize(function(e){
	var modal = app.State.get('modal');
	if(modal=='figs'){
		var el = $('#figModal .modal-dialog');
		var el2 = $('#figModal .modal-body');
		el.width(Math.min(window.innerWidth*0.7,900));
		el2.height(window.innerHeight-250);		
	}else if (modal == 'info'){
		var el = $('#infoModal .modal-dialog');
		var el2 = $('#infoModal .modal-body');
		el.width(Math.min(window.innerWidth*0.7,900));
		el2.height(window.innerHeight-250);
	}
});

$('body').keydown(function(e){
	var modal = app.State.get('modal');
	if(modal == null && !e.altKey && !e.metaKey && !e.ctrlKey && !e.shiftKey){
		switch(e.keyCode){
			case 73: //i
				toggleInfo();
				break;
			case 82: //r
				toggleInfo(1);
				break;
			case 70: //f
				toggleFigs();
				break;
			case 74:
				scrollDelta(400);
				break;
			case 75:
				scrollDelta(-400);
				break;
		}
	}else if (modal == 'figs' && !e.altKey && !e.metaKey && !e.ctrlKey && !e.shiftKey) {
		switch(e.keyCode){
			case 74: //j
				figMove(-1);
				break;
			case 75: //k
				figMove(1);
				break;
		}		
	}
});

figIds = _.map($('#figTab a'),function(el){$(el).attr('id')});

if(figIds.length > 0){
	$('#btn-figs').removeAttr("disabled");
}

$('#figTab a:first').click();
$('#infoTab a:first').click();



/*
$(window).resize(function(){
	console.log($(document).height());
		var el = $('#infoModal .modal-body');
		el.height($(window).height()-500);
});*/

});
// End of $() block.

function availabilityHtml(){
	var t = getMeta('availability');
	var ts = t.split(';');
	return _.map(ts,function(t){
		if(t == "full"){
			return "<span class='label label-primary'>Full text</span>";
		}else if (t == 'figs'){
			return "<span class='label label-success'>Figures</span>";			
		}else if (t == 'refs'){
			return "<span class='label label-warning'>References</span>";			
		}else if (t == 'abs'){
			return "<span class='label label-default'>Abstract</span>";
		}
	}).join('');
}


function toggleInfo(tabIdx){
	$('#btn-info').toggleClass('active');
	var cur = app.State.get('modal');
	if(cur != 'info'){
		app.State.set('modal',"info");
		var el = $('#infoModal .modal-dialog');
		var el2 = $('#infoModal .modal-body');
		el.width(Math.min(window.innerWidth*0.7,900));
		el2.height(window.innerHeight-250);
		$('#info-availability').html(availabilityHtml());
		$('#infoModal').modal();
		if(tabIdx){
			$($('#infoTab a')[tabIdx]).click();
		}
	}else{
		app.State.set('modal',null);
	}
}

function figMove(delta){
	var i = app.State.get('figIdx');
	var i2 = i + delta;
	var i3 = i2 < 0 ? 0 : (i2 >= figIds.length ? figIds.length - 1 : i2);
	app.State.set('figIdx',i3);
	if(i != i3){
		$($('#figTab a')[i3]).click();
	}
}

var figIds;

function toggleFigs(){
	$('#btn-figs').toggleClass('active');
	var cur = app.State.get('modal');
	if(cur != 'figs'){
		app.State.set('modal',"figs");
		var el = $('#figModal .modal-dialog');
		var el2 = $('#figModal .modal-body');
		el.width(Math.min(window.innerWidth*0.7,900));
		el2.height(window.innerHeight-250);
		$('#figModal').modal();
	}else{
		app.State.set('modal',null);
	}
}

function scrollDelta(delta){
	var s = $('body').scrollTop();
	$('body').animate({ scrollTop: (s+delta) },{duration: 100});
}

function mkRefList(pid){
	$.get('/paper/refs/'+pid,function(res){
		_.each(res.refs,function(r){
			refHtml[r.refName] = {url: r.url, html: r.citText, id: r.refId};
		});
		console.log(refHtml);
	});
}

function statusBar(shown,msg){
	var bar = $('#status-bar');
	if(shown){
		if(msg) { bar.html(msg); }
		bar.show();
	}else{
		bar.hide();
	}
}

function showError(msg){
	log("Error: "+msg); //stub
}