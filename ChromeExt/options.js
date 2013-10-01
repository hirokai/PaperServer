//options.js

// Saves options to localStorage.
function save_options() {
  var sel = $("#serverlist");
  var input = $("#text-host");
  var host = sel.val();
  if (host == '')
    host = input.val();
  localStorage["hostDomain"] = host;
  // Update status to let user know options were saved.
  var status = document.getElementById("status");
  status.innerHTML = "Options Saved. Now server is: " + host;
  $('#current_server').text(host);
  input.val(host);
  setTimeout(function() {
    status.innerHTML = "";
  }, 2000);
}
// Restores select box state to saved value from localStorage.
function restore_options() {
  var domain = localStorage["hostDomain"];
  if (!domain) {
    domain = 'http://paperserver.jp';
  }
  var placeholder = document.getElementById("current_server");
  placeholder.innerHTML = domain;
  $("#text-host").val(domain);
  $('#current_server').val(domain);
}

$(function(){
  restore_options();
  $('button').bind('click',function(){
    save_options();
  });

});


$(function(){
  var manifest = chrome.runtime.getManifest();
  var el = $('#domain-list');
  _.each(manifest.permissions,function(site){
    el.append('<ul>'+site+'</ul>');
  });
console.log(manifest.version);

});