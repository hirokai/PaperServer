// This is only used dynamically loaded pages such as Elsevier.
// Elsevier does not give full DOM at the time of DOMContentLoaded

var observer = new MutationObserver(function(mutations) {
 $.each(mutations,function(idx){
   log(this);
  // Stub: need a mechanism to combine multiple calls to this. 
   var completed = findMeta(this.addedNodes);
   if(completed && State.notAddedYet){
    log(citation);
    State.notAddedYet = false;
    //addUrl();
  }
});
}).observe(document, {
  attributes: true,
  childList: true,
  subtree: true,
  attributeFilter: []
});


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
      log(r);
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
