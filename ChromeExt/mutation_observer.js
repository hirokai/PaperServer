// This is only used dynamically loaded pages such as Elsevier.
// Elsevier does not give full DOM at the time of DOMContentLoaded

/*
var observer;

var fragHtml = '';
var frag_count = 0;

//Stub: ad hoc for sciencedirect.
if(location.href.match(/^http:\/\/www\.sciencedirect\.com\//)){
  observer = new MutationObserver(function(mutations) {
    var nodes = _.filter(_.flatten(_.map(mutations,function(m){return m.addedNodes})),function(n){
      return $(n).hasClass('page_fragment');
    });
    if(nodes.length > 0){

    }
    _.each(nodes,function(node){
      var h =  node[0].innerHTML;
      if(h){
        fragHtml += h;
        frag_count += 1;
      }
    });
  }).observe(document, {
    attributes: true,
    childList: true,
    subtree: true,
    attributeFilter: []
  });
}

var citation = new Object();
citation.meta = [];

//Stores metadata to citation.
function findMeta(rec){
  var res = undefined;
 // log(rec);
  _.each(rec,function(r){
    var n = r.localName;
    if(n == 'title'){
      citation.pageTitle = r.innerText;
    }else if(n == 'meta'){
      citation.meta.push({name: r.name, content: r.content});
    }
  });
  //stub
  return (citation.pageTitle);
}

$(function(){
  addUrl();
});

//Send citation data accumulated from MutationObserver events.
function addUrl(){
  if(State.notAddedYet){
    getHost(function(host){
      $.post(host+Path.addUrl,{url:location.href, meta: citation},function(res){
        if(res.supported){
          sendReq({message: "Supported"});
        }
      });
    });
  }
  State.notAddedYet = false;
}

var State = new Object();
State.notAddedYet = true;

*/