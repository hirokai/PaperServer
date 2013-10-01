// common.js
// Common content script

"use strict";

var host;

var log = function(d){console.log(d);};

var sendReq = chrome.extension.sendRequest;

var pluginEnabled = true;
function parse(pid){
  chrome.extension.sendRequest({message: 'Reparse',id:pid});
}

function getHost(fn){
  sendReq({message: "GetHost"},function(ret){
    var h = ret;
    if(!h) h = "http://localhost";
    fn(h);
  });
}
var paperIdFromServer = null;

var Config = new Object();
Config.debug = true;
Config.loadImgClient = true;


var Path = {
  addUrl: '/paper/add_url',
  add: '/paper/add',
  check: "/paper/supported",
  addTags: "/paper/add_tags",
  fetch: "/resources/fetch"
};

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

//Called when the page is loaded. DOMContentLoaded is called earlier than onload.
  window.addEventListener("DOMContentLoaded", function(){
//    log("Page loaded, next is getHost")
    getHost(function(host){

    //URLs for Yesod server commands
    var addAddress = host+Path.add;
    var checkAddress = host+Path.check;
    var changeTagAddress = host+Path.addTags;

    var url = location.href;
    var originalUrl = $('meta[name="original_url"]').prop("content");

    if(url.indexOf(host) == -1){  //We are not on PaperServer.
      console.log("Checking if client-side parsing is possible: "+url);
      //First try to parse on client side.
      var html = getHtml();
      sendReq({message: "ParseOnClient",url: location.href, html: html},function(r){
        if(r.success){
          log(r.data);
          var json = {url: url, serverside: false, html: html, parsed: r.data};

          if(r.data.originalHtml){  // If the parser fetched DOM and obtained the "real" HTML (after programmed injection of DOM.)
            json.html = r.data.originalHtml;
          }
          console.log("Posting...");
          console.log(json);
          //Stub: server does not get JSON as it is, so just stringigy it.
          json.parsed = JSON.stringify(json.parsed);          
          $.post(addAddress,json).success(function(res){
            addDone(res);
          }).error(function(){
                log(addAddress+" ajax error.");
          });
          sendReq({message: "ReplaceReady"});
        }else{ //Server-side parsing.
          $.get(checkAddress,{url: url},function(r){
            console.log(r);
            if(r.supported){
              sendReq({message: "Supported"});

              //Request server to add a paper.
              var json = {url: url, serverside: true, html: html,parsed:""};
              console.log('Posting raw data to server...');
              console.log(json);
              $.post(addAddress,json).success(function(res){
                addDone(res);
              }).error(function(){
                console.log(addAddress+" ajax error.");
              });
            }else{
              sendReq({message: "NotSupported"});
            }
          });
        }
      });
    }else{  //On paper server
      document.getElementById('action_reparse').addEventListener("click",function(event){
        var pid = $('meta[name="paper_id"]').attr('content');
        var url = $('meta[name="original_url"]').attr('content');
        sendReq({message: "Reparse",id: pid, url: url},function(res){
            console.log(res);
            if(res.success){
              location.reload();
            }
        });
      });
      sendReq({message: "FormattedNow"});
      if(Config.loadImgClient){
        var token = location.href.split("/");
        var pid = token[token.length - 1]; 
        setupImgFetchingForBGPage(host,pid);
      }
    }
  });
});

function addDone(res){
  if(res.success){
    log("add paper succeeded.");
    paperIdFromServer = res.summary.id;
    log(res);
    sendReq({message: "ReplaceReady",citation:res.summary.citation,paperId: paperIdFromServer});

    if(res.usePubmed){
      getHost(function(host){
        sendReq({message: "FetchPubmed",host: host, paperId: res.summary.id,paperDoi: res.summary.doi, paperTitle: res.summary.title});
      });
    }
    if(res.useGScholar){
      getHost(function(host){
        sendReq({message: "FetchGScholar",host: host, paperId: res.summary.id,paperDoi: res.summary.doi, paperTitle: res.summary.title});
      });      
    }
  }else{
    if(res.message == "Already exists"){
      log("Add paper: already exists.");
      log(res);
      paperIdFromServer = res.summary.id;
      sendReq({message: "ReplaceReady",citation:res.summary.citation,paperId: paperIdFromServer});
    }else{
      log("Add paper: failed: "+res.message);
      sendReq({message: "NotSupported"});
    }
  }
  //No matter if paper was added this time or previously, set up image fetching.
  //Image fetching is smart enough to download only nonexisting files.
  getHost(function(host){
    setupImgFetchingForBGPage(host,paperIdFromServer);
  });
}

//Just for debug printing
function truncateJson(json){
  var cp = JSON.parse(JSON.stringify(json));
  cp.html = cp.html.slice(0,100) + "...";
  return cp;
}

function fetchRequest(url){
  getHost(function(host){
    $.post(host+Path.fetch,{url: url}); 
  });
}

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
      var url = host + "/paper/c/" + id;
      window.open(url);
    }
  });
}

function setupImgFetchingForBGPage(host,pid){
  sendReq({message: "FetchImages",host: host, paperId: pid});
}
