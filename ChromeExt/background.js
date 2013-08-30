//background.js

var log = function(d) {console.log(d);};

var path_addurl = '/paper/add_url';
var path_rawurl = '/paper/raw/'

var hostRex = /^(http:\/\/.+?)\/(.+?)\//

chrome.tabs.onUpdated.addListener(function(tabId, info, tab){
  var m = tab.url.match(hostRex);
  if(m && m.length >= 2){
    var host = m[1];
    var paper = true || (m[2] == "paper");
    if(host == localStorage["hostDomain"] && paper){
      chrome.tabs.executeScript(tabId,{code: "pluginEnabled = true; var parse = function(pid){chrome.extension.sendRequest({message: 'Reparse',id:pid});};", runAt: "document_idle"})
    }    
  }
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
    details.title = 'This paper is not supported.';
    chrome.pageAction.setIcon(details);
    chrome.pageAction.show(sender.tab.id);			
//    chrome.pageAction.setTitle(details);
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
  }else if (request.message == "FetchPubmed"){
    var host = request.host;
    var doi = request.paperDoi;
    var pid = request.paperId;
    setupPubmedFetching(host,pid,doi);
  }else if (request.message == "DoesSupportParsingOnClient") {
    parseSupported(request.url);
  }else if (request.message == "ParseOnClient") {
    var d = parseHtml(request.url,request.html);
    sendResponse(d); 
  }else if (request.message == "Reparse"){
    var pid = request.id;
    var url = request.url;
    log(request);
    if(parseSupported(url)){
      $.get(localStorage["hostDomain"]+path_rawurl+pid,function(html){
        parseHtml(url,html);
      });
    }else{
      reparseOnServer(pid,sendResponse);
    }
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

var path_uploadpubmed = '/pubmed/add'

function setupPubmedFetching(host,pid,doi){
  var url = "http://www.ncbi.nlm.nih.gov/pubmed?term="+encodeURI(doi)+"%5BLocation%20ID%5D&report=xml&format=text";
    $.ajax({
      url: url
      , type: "GET"
      , dataType: 'text'
      , success:function(res){
        var xml = res.replace('&lt;', '<')
                   .replace('&gt;', '>');
      console.log(xml);
      var upload_url = host + path_uploadpubmed;
      console.log(upload_url);
      $.post(upload_url,{id: pid,doi:doi, xml: xml},function(res){
        console.log(res);
      });
      }});
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

function reparseOnServer(pid,callback){
  log('(Parsing not possible on client, so) Reparsing on server...');
  $.get(localStorage["hostDomain"]+'/paper/single_reparse',{id:pid},function(res){
    callback(res);
  });
}




