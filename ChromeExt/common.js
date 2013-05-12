// common.js
// Common content script

"use strict";

var host;
//getHost();


function getHost(fn){
  chrome.extension.sendRequest({message: "GetHost"},function(ret){
    var h = ret;
    if(!h) h = "http://localhost";
    console.log("Host address: "+h)
    fn(h);
  });
}
var paperIdFromServer = null;

var Config = new Object();
Config.debug = true;
Config.loadImgClient = true;

function mydebug(obj) {
  if(Config.debug) console.log(obj);
}

function getHtml(){
  var node = document.doctype;
  var doctype;
  if(node){
    doctype = "<!DOCTYPE "
      + node.name
      + (node.publicId ? ' PUBLIC "' + node.publicId + '"' : '')
      + (!node.publicId && node.systemId ? ' SYSTEM' : '') 
      + (node.systemId ? ' "' + node.systemId + '"' : '')
      + '>';
  }else{
    doctype = "";
  }
  return doctype+document.documentElement.outerHTML;
}

var path_addurl = '/paper/add_url'


/*
document.addEventListener('DOMNodeInserted',function(e){
 // if(e.target.localName == "title"){
    console.log(e);
 // }
});
*/

//Called when the page is loaded.
//$(function(tabId,changeInfo,tab){
window.addEventListener("DOMContentLoaded", function(){
  console.log("Page loaded, next is getHost")
  getHost(function(host){

    //URLs for Yesod server commands
    var addAddress = host+"/paper/add";
    var checkAddress = host+"/paper/supported";
    var changeTagAddress = host+"/paper/add_tags";

    var url = location.href;
    var originalUrl = $('meta[name="original_url"]').prop("content");


    if(url.indexOf(host) == -1){  //We are not on PaperServer.
      console.log("Check paper: "+url);
      $.get(checkAddress,{url: url},function(r){
        console.log(r);
        if(r.supported){
          chrome.extension.sendRequest({message: "Supported"});

          //Request server to add a paper.
          var json = {url: url, serverside: true, html: getHtml()};
         // console.log("Sending the following json to: "+addAddress);
         // console.log(json);

          $.post(addAddress,json).success(function(res){
            console.log(res);
            if(res.success){
              paperIdFromServer = res.summary.id;

              chrome.extension.sendRequest({message: "ReplaceReady",citation:res.summary.citation,paperId: paperIdFromServer});
              if(Config.loadImgClient){

                setupImgFetchingForBGPage(host,paperIdFromServer);
              }else if(res.summary.resource){

              }
            }else{
              if(res.message == "Already exists"){
                console.log("Add paper: already exists.");
                paperIdFromServer = res.summary.id;
                chrome.extension.sendRequest({message: "ReplaceReady",citation:res.summary.citation,paperId: paperIdFromServer});
              }else{
                console.log("Add paper: failed.");
              }
            }
          }).error(function(){
            console.log(addAddress+" ajax error.");
          });
        }else{
          chrome.extension.sendRequest({message: "NotSupported"});
        }
      });
    }else{  //On paper server
      chrome.extension.sendRequest({message: "FormattedNow"});
      if(Config.loadImgClient){
        var token = location.href.split("/");
        var pid = token[token.length - 1]; 
        setupImgFetchingForBGPage(host,pid);
      }
    }
  });
});

//Just for debug printing
function truncateJson(json){
  var cp = JSON.parse(JSON.stringify(json));
  cp.html = cp.html.slice(0,100) + "...";
  return cp;
}

function fetchRequest(url){
  //		mydebug(url);
  getHost(function(host){
    $.post(host+"/resources/fetch",{url: url}); 
  });
}

var citation = new Object();
citation.meta = [];

function findMeta(rec){
  var res = undefined;
  $.each(rec,function(){
    var n = this.localName;
    if(n == 'title'){
      res = this.innerText;
      console.log(res);
      citation.pageTitle = res;
    }else if(n == 'meta'){
      res = {name: this.name, content: this.content};
      console.log(res);
      citation.meta += res;
    }
  });
  return res;
}

$(function(){
  addUrl();
});

function addUrl(){
  if(notAddedYet){
    getHost(function(host){
      $.post(host+path_addurl,{url:location.href, meta: citation},function(res){
        if(res.supported){
          chrome.extension.sendRequest({message: "Supported"});
        }
      });
    });
  }
  notAddedYet = false;
}

var notAddedYet = true;

var observer = new MutationObserver(function(mutations) {
 $.each(mutations,function(idx){
   console.log(this);
  var t = findMeta(this.addedNodes);
  if(t && citation.pageTitle && notAddedYet){
    addUrl();
  }
 });
}).observe(document, {
  attributes: true,
  childList: true,
  subtree: true,
  attributeFilter: ["head"]
});

chrome.extension.onRequest.addListener(function(request, sender, sendResponse) {
  if(request.message == "PageAction"){
    replaceView();
  }
});


var altView = false;
var originalUrl = null;

function replaceView(){
  getHost(function(host){
    var id = paperIdFromServer;
    var current = location.href;
    var isFormatted = (current.indexOf(host)!=-1);
    if(isFormatted && originalUrl){
      location.href = originalUrl;
    }else{
      location.href = host + "/paper/b/" + id;   //Yesod
    }
  });
}

function uniqueArray(array){
  var ret = [];
  for(var i in array){
    if(ret.indexOf(array[i])==-1){
      ret.push(array[i]);
    }
  }
  return ret;
}


function setupImgFetchingForBGPage(host,pid){
  chrome.extension.sendRequest({message: "FetchImages",host: host, paperId: pid});
}


