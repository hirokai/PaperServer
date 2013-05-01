//format-b-ui.js (From server)

Config = new Object();
Config.debug = true;

var path_reparse = '/paper/single_reparse/';
var path_resourcelist = '/resource_for/'
var path_upload = '/upload_resource';

//mode 0: normal, 1: figure, 2: info
var mode = mNormal;
var mNormal = 0;
var mFig = 1;
var mInfo = 2;

var normalMode = true;
var figMode = false;
var infoMode = false;

var infoShown = nothing; //0: nothing, 1: citation, 2: references 4: figures.
var nothing = 0;
var cit = 1;
var refs = 2;
var figs = 4;

function mydebug(obj) {
	if(Config.debug) console.log(obj);
}

function scrollTo(container,selector){
	p = $(selector).offset().top;
	$(container).animate({scrollTop:p},'fast');
	console.log('jump to '+selector + ", "+ p);
}

var figBoxIndex = 0;

var isAnimating = false;

$(function(){
  setupUI();
});

var help_html = "<h4 class='formatb-help'>Keyboard shurtcuts</h4>" +
    "<table class='formatb-help'><tr>" +
    "<td class='key'>j/k</td><td class='exp'>Down/up scroll</td></tr>" +
    "<td class='key'>Shift+j/k</td><td class='exp'>Faster scroll</td></tr>" +
    "<td class='key'>f</td><td class='exp'>Toggle figure</td></tr>" +
    "<td class='key'>i</td><td class='exp'>Toggle info</td></tr>" +
    "<td class='key'>t</td><td class='exp'>Toggle TOC</td></tr>" +
    "<td class='key'>h/l</td><td class='exp'>Move between tabs</td></tr>" +
    "<td class='key'>Esc</td><td class='exp'>Close info/figure window</td></tr>" +
    "<td class='key'>q</td><td class='exp'>Toggle header bar</td></tr>" +
    "</table>";


var keyDispatchTable = [];

function addKey(mode,code,func) {
  addKeyM(mode,code,false,false,false,false,func);
}

function addKeyM(mode,code,shift,meta,ctrl,alt,func){
  var hashkey = [mode,code,shift,meta,ctrl,alt].join("_");
  keyDispatchTable[hashkey] = func;
}

function registerKeyEvents(){
  addKey(1, 76, function() {
    showFigMove(1);
  });
	addKey(1, 72, function() {
    showFigMove(-1);
  });
  var dismissFig = function() {showFigBox(false)};
  addKey(1,27,dismissFig);
  addKey(1,70,dismissFig);
  addKey(2,72,function() {moveInfoBy(-1);});  // h
  addKey(2,37,function() {moveInfoBy(-1);});  // Left
  addKey(2,76,function() {moveInfoBy(1);}); // l
  addKey(2,39,function() {moveInfoBy(1);}); // Right
  addKey(2,73,function() {showInfo(false);}); // i
  addKey(mNormal,81,function() {toggleHeader();});  // q
  addKey(0,70,function() {showFigBox(true);});  // f
  
  var animateDown = function(e){
    if(!isAnimating){
      var h = $(window).height();
      var current = $('#leftbox').scrollTop();
      var delta = e.shiftKey ? h*3-210 : h - 70;
      $('#leftbox').animate({scrollTop: current+delta}, 100, 'swing',function(){
        isAnimating = false;
      });
      isAnimating = true;
    }
  };
  addKeyM(0,74,false,false,false,false,animateDown);  // j
  addKeyM(0,74,true,false,false,false,animateDown);  // j

  var animateUp = function(e) {
    if(!isAnimating){
      var h = $(window).height();
    var current = $('#leftbox').scrollTop();
    var delta = e.shiftKey ? h*3-210 : h - 70;
    $('#leftbox').animate({scrollTop: current-delta}, 100, 'swing',function(){
      isAnimating = false;
    });
    }
    isAnimating = true;
  }
  addKeyM(0,75,false,false,false,false,animateUp);  // k
  addKeyM(0,75,true,false,false,false,animateUp);  // k
  addKey(0,73,function() {showInfo(true);});  // i
  addKey(0,37,true,false,false,false,function() {   //Shift + Left
    var link = $('meta[name="prev_id"]').attr("content");
    if(link)
      location.href = "./"+link;
    return false; //no default action 
  });
  addKey(0,39,true,false,false,false,function(){  //Shift+Right
    var link = $('meta[name="next_id"]').attr("content");
    if(link)
    location.href = "./"+link;
    return false; //no default action 
  });
  addKey(0,220,function() {toggleColor();});  // \
  addKey(0,82,function() {location.replace("#references")});
}

function setupUI(){


  $('#infoModal').on('hide',function(){
    normalMode = true;
    infoMode = false;
  });

  $('#button-figs').click(function(){
    showFigBox(true);
  });
  
  $('#button-info').click(function(){
    showInfo(true);
  });

  $('#button-help').popover({html:true,content:help_html,
    placement:'bottom',animation: false,trigger:'hover'});
  
  $('#button-reparse').click(function(){
    var pid = $('meta[name="paper_id"]').prop('content');
    $.get(path_reparse,{id:pid},function(res){
      if(res.success){
   //     showMsg("One paper was re-parsed.",2000);
         console.log(res);

         //Somehow this does not work.
          window.location.reload();


      }else{
   //     showAlert("Error occured while reparsing: "+res.message);
      }
    }).error(function(){
      showAlert("Error occured while reparsing.");
    });
    //ToDo: This is too ad hoc.
    window.setTimeout(function(){
      window.location.reload();
    } ,2000);
  });

	$(document).mousedown(function(e) {
		return true;
	});
	$(document).bind('mousewheel',function(e){
		if($('#figbox').is(':visible') && e.shiftKey){
			figBoxIndex -= (e.originalEvent.wheelDelta/120);
			if(figBoxIndex < 0 ) figBoxIndex = 0;
			if(figBoxIndex >= figIds.length) figBoxIndex = figIds.length - 1;
			showFig(figBoxIndex);
			return false;
		}
		return true;
	});

	$(document).keydown(function(e){
//		console.log(e);
    var ret = true;
    var m;
    if(normalMode)
      m = 0;
    else if (figMode)
      m = 1;
    else if (infoMode)
      m = 2;
    var hashkey = [m,e.keyCode,e.shiftKey,e.metaKey,e.ctrlKey,e.altKey].join("_");
    var func = keyDispatchTable[hashkey];
    if(func){
    var ret = func(e);
    if(ret == undefined)
      return true;
    else
      return ret;
    }else
    return true;
  });

  registerKeyEvents();

  figIds = $('.fig').map(function(){return $(this).attr('id');}).get();
	if(figIds[0] == 'figabs'){
		figIds.splice(0,1);
	}	
	//	$('#contentbar').scrollspy();

  $('.link-available').bind('click',function(e){
    me = $(e.target);
    doi = $('meta[name="doi"]').attr('content');
    if(me.parent().hasClass('link-waiting'))
      return;
    me.parent().removeClass('link-available');
    me.parent().addClass('link-waiting');
    me.attr('src','/static/img/jump_wait.gif');
    $.post("http://localhost:3000/prepareLink",
      {doi:doi, citId:me.parent().parent().attr('id')},
      function(res){
//        console.log(res);
 //       if(res.success==true){
          console.log("Download success.")
          me = $(this);
          me.parent().attr('link-doi',res.doi);
          me.parent().removeClass('link-waiting');
          me.unbind('click');
          me.parent().addClass('link-ready');
          me.attr('src','/static/img/jump_ready.png'); // Stub
          me.bind('click',readyLinkEvent)
  //      }
      });
  }); 
  $('.link-ready').bind('click',readyLinkEvent); 

  $('#info').bind('click',function(){
    url = location.href;
    location.href = url.replace("/papers/a/","/papers/info/").replace("/doi/a/","/doi/info/");
  });
  /*
  $('#reparse').bind( 'click',function(){
    url = location.href;
    url = url.replace("/papers/a/","/papers/reparse/").replace("/doi/a/","/doi/reparse/");
    $.get(url,function(){
      window.location.reload();
    });
  });*/

  $('#info-a-cit').click(function() {
    hideAllInfoTabsBut('#info-div-cit');
  });

  $('#info-a-refs').click(function() {
    hideAllInfoTabsBut('#info-div-refs');
  });
  $('#info-a-history').click(function() {
    hideAllInfoTabsBut('#info-div-history');
  });

  $('#info-a-details').click(function() {
    hideAllInfoTabsBut('#info-div-details');
  });

  $('#button-formatted').click(function(){
    changeDisplayMode(0);
  });

  $('#button-original').click(function(){
    changeDisplayMode(1);
  });


	$(document).bind('load',function(){showFig(0);return true;});

  $('#figModal a').click(function(){
    showFigId($(this).attr('data-figID'));
  });
  $('#figModal a[data-fig-prev]').click(function(){
    showFigMove(-1);
  });
  $('#figModal a[data-fig-next]').click(function(){
    showFigMove(1);
  });
}

// 0: format B
// 1: original HTML in iframe
function changeDisplayMode(num) {
  switch(num){
  case 0:
    $('iframe#original').remove(); 
    $('#outerbox').css('display','block');
    break;
  case 1:
    $('#outerbox').css('display','none');
    var url = $("meta[name='original_url']").attr('content');
    var iframe = $("<iframe id='original' src='"+url+"'></iframe>");
    var w = '100%';
    var h = $('#outerbox').css('height');
    iframe.css('width',w);
    iframe.css('height',h);
    $('body').append(iframe);
    break;
  }
}

function readyLinkEvent(e){
  targetDoi = $(e.target).parent().attr('link-doi');
  console.log("readyLinkEvent: Jump to "+targetDoi);
  location.href = "http://localhost:3000/doi/a/"+targetDoi;
}

function showFigMove(delta) {
  var newpos = figBoxIndex + delta;
  if(newpos < 0)
    newpos = figBoxIndex;
  if(newpos >= figIds.length)
    newpos = figBoxIndex;
  figBoxIndex = newpos;
  showFig(newpos);
}

//var FigIds;
function showFig(index) {
  showFigId(figIds[index]);
}

function showFigId(figid) {
  for(var i = 0, len = figIds.length; i < len; i++ ) {
    var id = figIds[i];
    $("#box-"+id).hide();
    $('a[data-figID="'+id+'"]').parent().removeClass('active');
  }
	$("#box-"+figid).show();
  $('a[data-figID="'+figid+'"]').parent().addClass('active');
}


function toggleHeader(){
  if($('.navbar').is(':visible')){
    $('.navbar').hide();
    $('#leftbox').css('top','0px');
    $('#outerbox').css('top','0px');
    $('#leftbox').height($('#leftbox').height()+50);
    $('#outerbox').height($('#outerbox').height()+50);
    //   document.documentElement.webkitRequestFullScreen();
  }else{
    $('.navbar').show();
    $('#leftbox').css('top','50px');
    $('#outerbox').css('top','50px');
    $('#leftbox').height($('#leftbox').height()-50);
    $('#outerbox').height($('#outerbox').height()-50);

  //   document.webkitCancelFullScreen(); 
  }
}

function showAlert(str){
  //stub
  console.log(str);
}
function toggleColor(){
		$('body').toggleClass('white');
}
/*
$(function(){
  var pid = $('meta[name="paper_id"]').prop('content');
  $.get(path_resourcelist+pid, function(res){
    if(res.success){
      //console.log(res.url);
      askPluginToFetchImages(pid,res.url);
    }else{
      console.log(res.error);
    }
  });
});

function askPluginToFetchImages(){
  
}
*/

var rightShown = true;
function toggleRight(){
	rightShown = !rightShown;
		if(rightShown){
			$('#leftbox').removeClass('full');
			$('#rightbox').removeClass('hidden');
			setTimeout(function(){
				$('#rightbox').css('display','block');
			},300);

		}else{
			$('#leftbox').addClass('full');
			$('#rightbox').addClass('hidden');
			setTimeout(function(){
				$('#rightbox').css('display','none');
			},300);
		}
}

/*
//This should not be used too much.
function resizeObjects(): void {

}

$(window).bind('resize',function(){
	resizeObjects();
});
*/

//This is needed in case figModal was closed by other methods than showFigBox()
$('#figModal').bind('hidden',function(){
    console.log('onHide');
    normalMode = true;
    figMode = false;
    mode = 0;
});

function showFigBox(shown){
  if(shown){
    //resizeObjects();
    $('#figModal').modal();
    showFig(figBoxIndex);
    normalMode = false;
    figMode = true;
    mode = 1;
  }else{
    $('#figModal').modal('hide');
    normalMode = true;
    figMode = false;
    mode = 0;
  }
}

function jumpToRef(r){
//		mydebug(elem.innerText)
		var refs = r.split(" ");
		mydebug(refs);
		location.replace('#'+refs[0]);
}

function showInfo(shown) {
  console.log('hey!!');
    normalMode = !shown;
    infoMode = shown;
    mode = shown ? mInfo : mNormal;
    infoShown = shown ? cit : nothing;

    var h = $(window).height() - 200;
    $('#infoModal .modal-body').css('max-height',h);

    $('#infoModal').modal(shown ? 'show' : 'hide');
    showInfoTab(infoModalIndex);
}

var infoModalIds = ['#info-a-cit','#info-a-refs','#info-a-history','#info-a-details'];
var infoModalDivIds = ['#info-div-cit','#info-div-refs','#info-div-history','#info-div-details'];
var infoModalIndex = 0;

function moveInfoBy(delta){
  var newpos = infoModalIndex + delta;
  if(newpos >= 0 && newpos < infoModalIds.length){
    showInfoTab(newpos);
  }
}

function showInfoTab(index) {
  infoModalIndex = index;
  $(infoModalIds[index]).click();
}

function hideAllInfoTabsBut(id) {
  for(var i = 0; i < infoModalIds.length; i++){
    $(infoModalDivIds[i]).hide();
  }
  $(id).show();
}
