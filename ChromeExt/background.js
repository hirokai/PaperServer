//background.js

var path_addurl = '/paper/add_url';

chrome.tabs.onUpdated.addListener(function(tabId, info, tab){
});

chrome.extension.onRequest.addListener(function(request, sender, sendResponse) {
  var details = new Object();
  var id = sender.tab.id;
  if(request.message == "ReplaceReady"){
    details.path = 'ready.png';
    details.tabId = sender.tab.id;
    chrome.pageAction.show(sender.tab.id);
    chrome.pageAction.setIcon(details);
    //    var c = request.citation;
    //    webkitNotifications.createNotification("","Added: "+c.journal + (c.year ? (" "+ c.year) : ""), c.title).show();
  }else if (request.message == "Supported"){
    details.path = 'target.png';
    details.tabId = sender.tab.id;
    chrome.pageAction.setIcon(details);
    chrome.pageAction.show(sender.tab.id);			
  }else if (request.message == "NotSupported"){
    details.path = 'default.png';
    details.tabId = sender.tab.id;
    chrome.pageAction.setIcon(details);
    chrome.pageAction.show(sender.tab.id);			
  }else if (request.message == "FormattedNow"){
    details.path = 'formatted.png';
    details.tabId = sender.tab.id;
    chrome.pageAction.setIcon(details);
    chrome.pageAction.show(sender.tab.id);			
  }else if (request.message == "GetHost"){
    sendResponse(localStorage['hostDomain']);
  }else if (request.message == "SetDefaultTags"){
    localStorage['defaultTags'] = request.tags;
  }else if (request.message == "GetDefaultTags"){
    sendResponse({tags:localStorage['defaultTags']});
  }else if (request.message == "FetchImages") {
    var host = request.host;
    var id = request.paperId;
    console.log(request);
    setupImgFetching(host,id);
  }
});

chrome.pageAction.onClicked.addListener(function(tab){
  console.log(tab);
  chrome.tabs.sendRequest(tab.id,{message: "PageAction"});
});


var path_resourcelist = '/resource_for/'

//Following are image download by client.
function setupImgFetching(host,pid){
  var url = host+path_resourcelist+pid;
  console.log(url);
  $.get(url, function(res){
    if(res.success){
      console.log(res);
      fetchImages(host,pid,res.url);
    }else{
      console.log(res.message);
      //This error is caused because the parsing hasn't done, so wait a bit and repeat.
      // ToDo: make sure this will terminate at some point.
      // Probably I need to check the kind of error to avoid infinite loops.
      setTimeout(setupImgFetching,1000);
    }
  });
}

function fetchImages(host,pid,urls){
  var img = [];
  console.log("fetchImages(): ",urls);
  if($('#img_fetched').length == 0){
    $('body').append($('<div id="img_fetched" style="display:none;" />'));
  }
  for(var i = 0; i < urls.length; i++){
    img[i] = document.createElement('img');
    $(img[i]).attr('id','bgimg_'+i);
    $('#img_fetched').append(img[i]);
    addImgLoadListener(host,pid,img[i],i);
    img[i].src = urls[i];
  }
}

var path_upload = "/upload_resource"
function addImgLoadListener(host,pid,img,i){
  img.addEventListener('load', function(e){
    // console.log(e);
    // console.log(img);
    var w = img.naturalWidth;
    var h = img.naturalHeight;
    var c=$('<canvas id="image_' + i + '">');
    c.attr('width', w);
    c.attr('height', h);
    var ctx=c.get(0).getContext("2d");
    ctx.drawImage(img,0,0);
    var ftype = 'image/png';   //Stub: ToDo: support jpeg, etc.
    var dat = c.get(0).toDataURL(ftype).replace(/^data:image\/(png|jpg);base64,/, "");
    // console.log(dat);
    var imgurl = img.src;
    $.post(host + path_upload,{id:pid,url:imgurl,type:ftype,data:dat},function(res){
      if(res.success){
        console.log('Upload succeeded.');
      }else{
        cosole.log('Failed: ' + res.message);
      }
    });
    //  console.log(e);
  });

}

