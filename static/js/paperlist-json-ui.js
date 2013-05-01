// paperlist-json-ui.js

var status = new Object();

$(function(){
  status.cursor = 0;
  $('.dropdown-menu').find('input').click(function (e) {
    e.stopPropagation();
  });
});


