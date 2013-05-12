//paperlist_mobile.js

var path_format_ma = "/m/paper/a/";

var ui = new Object();
ui.recordsPerPage = 20;

ui.goto = function(page){
  var str = $('#search').val();

  var param = {"offset": page * ui.recordsPerPage, limit: ui.recordsPerPage,
      "search-logic": "OR","search[0][field]": "any",
      "search[0][type]": "text",
      "search[0][operator]": "is",
      "search[0][value]": str};
  refreshTable(param);
};

$(function(){
  $('#btn-prev').click(function(e){
    movePage(-1);
  });
  $('#btn-next').click(function(e){
    movePage(1);
  });
  $('#search').on("change",function(event){
    ui.goto(0);
  });
  ui.goto(0);
});

function refreshTable(param){
  $.post('/list.json', param, function(data) {
    console.log(data);
    var items = [] ;

    ui.page = data.page;
    ui.total = data.total;
    ui.numPages = Math.ceil(ui.total / ui.recordsPerPage);
    if(ui.totalAll == undefined){
      ui.totalAll = ui.total;  //All papers from the first call of this.
    }

    $('#heading').html(''+ui.total+' / '+ui.totalAll+' papers');
    $('#pageinfo').html(''+(ui.page+1)+' / '+ui.numPages);

    if(ui.page==0){
      $('#btn-prev').addClass('ui-disabled');
    }else{
      $('#btn-prev').removeClass('ui-disabled');
    }
    if(ui.page==ui.numPages-1){
      $('#btn-next').addClass('ui-disabled');
    }else{
      $('#btn-next').removeClass('ui-disabled');
    }

    $.each(data.records, function(idx,rec) {
      items.push(mkItem(rec));
    });

    var html = items.join('');

    $('#paperlist').html(html);
    $('#paperlist').trigger('create');    
    $('#paperlist').listview('refresh');
    
   // console.log($('#paperlist').html());
  });
}
/*
$( window ).on( "swipeleft", function( event ) {
  movePage(1);
});

$( window ).on( "swiperight", function( event ) {
  movePage(-1);
});
*/

function movePage(delta){
  var p = ui.page;
  var newp = p + delta;
  var num_p = ui.numPages;
  if(newp >= num_p){
    newp = num_p - 1;
  }else if (newp < 0) {
    newp = 0;
  }
  ui.goto(newp);
}

function mkItem(rec){
      var pid = rec.id;
      var c = rec.citation;
  return '<li class="ui-shadow" id="' + pid + '">'+
      '<div>' + 
      '<span>' + rec.recid + '</span>'+ 
      '<a href="'+path_format_ma+pid+'" data-ajax="false"><h3>' + rec.citation.title + '</h3></a>' + 
      '</div>'+
      '<div class="citation">' +
      '<i>' + c.journal + '</i>, ' + '<b>' + c.volume + '</b>, ' + c.pageFrom + '-' + c.pageTo + ' (' + c.year + ')' + 
      '</div></li>';

}

$(document).bind('mobileinit', function(){  
//  $.mobile.pushStateEnabled = false;//Ajax off  
});
