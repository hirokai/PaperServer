$('#btn-save').click(function(e){
  var tags = $('#tag-default').val().replace(',,','\t');
  chrome.extension.sendRequest({message: "SetDefaultTags", tags:tags});
});

$(function(){
  chrome.extension.sendRequest({message: "GetDefaultTags"},function(res){
    var tags = res.tags.replace('\t',',,');
    console.log(res);
    $('#tag-default').val(tags);
  });
});

