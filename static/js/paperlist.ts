/// <reference path="./jquery.d.ts" />
/// <reference path="./bootstrap.d.ts" />
//From: https://github.com/borisyankov/DefinitelyTyped/blob/master/

//
// paperlist.js
// Based on w2ui
//

// Disable console.log() for release environment.
// console = { 
//   log: function(){}
// };

var path_addtags = '/paper/add_tags';
var path_replacetags = '/paper/replace_tags';
var path_delete = '/paper/delete';
var path_reparse = '/paper/reparse';
var path_reader = '/start_reader'

//mode 0: normal, 1: figure, 2: info, 4: tag, 8: tag input
enum Mode {
  mNormal = 0,
  mSearch = 1,
  mInfo = 2,
  mTag = 4,
  mTagInput = 8,
  mSearchInput = 16,
  mInput = 32   // input in general.
}

//useAccessor 'changeMode' and 'getMode' to change the mode. 
var globalMode: Mode = Mode.mNormal;

function changeMode(m: Mode): void {
  console.log('Mode changed to: ' + m);
  globalMode = m;
  if(globalMode == Mode.mNormal){
    if(currentPopup)
      currentPopup.close();
  }
}

function getMode(): Mode {
  console.log("getMode(): "+globalMode);
  return globalMode;
}

function showMode(): string {
  switch(getMode()){
    case Mode.mNormal:
      return "NORMAL";
    case Mode.mSearch:
      return "SEARCH";
    case Mode.mInfo:
      return "INFO";
    case Mode.mTag:
      return "TAG";
    case Mode.mTagInput:
      return "TAG(INPUT)";
  }
}

enum ShownInfo {
  nothing = 0,
  cit = 1,
  refs = 2,
  figs = 4
}

var infoShown: ShownInfo = ShownInfo.nothing;

// ToDo: Modal window key events that are common to multiple pages should be integrated into a Widget.
var keyDispatchTable: {[s: string]: (model: any) => bool; } = {};


interface ServerResponse extends JQueryXHR {
  success: bool;
}

class UIParts {
search: any;
}

var ui: UIParts;

function addKeyM(mode,code,shift,meta,ctrl,alt,func){
  var hashkey = [mode,code,shift,meta,ctrl,alt].join("_");
  keyDispatchTable[hashkey] = func;
}

function addKeyS(mode: Mode,code: number, func: (event: any) => bool) {
  addKeyM(mode,code,true,false,false,false,func);
}

function addKey(mode: Mode,code: number, func: (event: any) => bool) {
  addKeyM(mode,code,false,false,false,false,func);
}

function checkNotInput(){
  return getMode() == Mode.mNormal &&
      ! $('#grid_paperlist input').is(':focus');
}
function registerKeyEvents(){
  addKey(2,72,moveLeftInfo); // h
  addKey(2,76,moveRightInfo); // l

  var toNormalMode = function(stub) {
    if(getMode() == Mode.mSearch){
      ui.search.blur();
      $('.action-addtag').blur();
    }
    changeMode(Mode.mNormal);
    return true;
  };

  addKey(Mode.mNormal,27,function(){
/*  if($('input').is(':focus')) {
      if($('grid_paperlist table input').is(':focus')){
        var sel = w2ui['paperlist'].getSelection()[0];
        $('input').blur();
        w2ui['paperlist'].select(sel);
      }else{
        $('input').blur();
        w2ui['paperlist'].selectNone();
      }

    } */
  }); //Esc: it's still mNormal when the input is activated in grid.

  //http://www.programming-magic.com/file/20080205232140/keycode_table.html
  //http://www.openspc2.org/reibun/JavaScript_technique/sample/08_event/005/sample/index.htmladdKey(Mode.mSearch,27,toNormalMode);
  addKey(Mode.mInfo,27,toNormalMode);    // Esc
  addKey(Mode.mTag,27,toNormalMode);    // Esc
  addKey(Mode.mTagInput,27,toNormalMode);    // Esc
  addKey(Mode.mTagInput,27,toNormalMode);    // Esc

  addKey(Mode.mNormal,74,function(e){
    if(checkNotInput()){
      return moveDown();
    }
  }); // j
  addKeyS(Mode.mNormal,74,function(e){
    if(checkNotInput()){
      return moveDownSelect();
    }
  }); // J
  addKey(Mode.mNormal,75,function(e){
    if(checkNotInput()){
      return moveUp();
    }
  });   // k
  addKeyS(Mode.mNormal,75,function(e){
    if(checkNotInput()){
      return moveUpSelect();
    }
  }); // K

  addKey(Mode.mNormal,76,function(e){
    if(checkNotInput()){
      return movePage(1);
    }}); // l
  addKey(Mode.mNormal,39,function(e){
    if(checkNotInput()){
      return movePage(1);
    }
  }); // right

  addKey(Mode.mNormal,72,function(e){
    if(checkNotInput()){
      return movePage(-1);}}); // h

  addKey(Mode.mNormal,37,function(e){
    if(checkNotInput()){
      return movePage(-1);
    }
  }); // left
  
  addKey(Mode.mNormal,73,function(e){
    if(checkNotInput()){
      return showInfo();
  }});   // i

  addKey(Mode.mNormal,13,function(e){
    if(checkNotInput()){
    return showPaper();
    }  
      });  // Enter
  addKey(Mode.mTag,13,function(e){return tagModalCheckEnter();});   // Enter
  addKey(Mode.mNormal,65,function(e){
    if(checkNotInput()){
      return toggleTagbar();}});   // a
  addKey(Mode.mNormal,84,function(e){  //t
    if(checkNotInput()){
      showTagAddWindow(true);
      return false; // to prevent key 't' input to the text box
    }
  });
  addKeyS(Mode.mNormal,84,function(e){  //T
    if(checkNotInput()){
      showTagEditWindow(true);
      return false; // to prevent key 't' input to the text box
    }
  });
  //addKey(Mode.mInfo,84,function(){return showTagWindow(false,$scope.papers[$scope.cursor]);});   // t
  //addKey(Mode.mInfo,84,function(){return showTagWindow(false,$scope.papers[$scope.cursor]);});   // t
  addKey(Mode.mNormal,191,function(e){    // /
      searchBar(e);
      return true;
      });
  addKeyM(Mode.mNormal,191,true,false,false,false,function(){
      console.log('showHelp()');
      $('#help').modal({keyboard:true});
      changeMode(Mode.mInfo);
      });
}

class Citation {
  title: string;
}

class Paper {
  citation: Citation;
  doi: string;
  id: string;
  tags: string[];
}

$(document).keydown(function(e){
  //console.log(e);
  var ret = true;
  var hashkey = <any>[getMode(),e.keyCode,e.shiftKey,e.metaKey,e.ctrlKey,e.altKey].join("_");
  var func = keyDispatchTable[hashkey];
  //console.log(e);
  if(func){
    var ret = func(e);
    if(ret == undefined)
      return true;
    else
      return ret;
  }else{
    return true;
  }
});

registerKeyEvents(null);

$('#alert-close').click(function() {
  $('#alert-box').css('visibility','hidden');
});

  console.log( $('ul a'));
  $('ul a').click(function(e){
      console.log(e);
  });


class Config {
  showTagbar: bool;
  constructor() {
    showTagbar = true;
  }
}

var config = new Config();

function toggleTagbar(): void {
  config.showTagbar = !config.showTagbar;
  w2ui['layout'].toggle('left', true);
}

//ToDo: still buggy. Does not refresh correctly.
function showInfo(paper: Paper): void {
  $.get('/paper/info/'+paper.id).success(function(dat){
      var div = $("<div></div>").append(dat).find('#outer-all');
      console.log(div);
      $('#infoModalBody').html(div.html());
      });
  changeMode(Mode.mInfo);
  infoShown = ShownInfo.cit;
  $('#infoModal').modal();
  $('#info-a-cit').click();
}

function paperFromDoi(papers: Paper[],doi: string): Paper {
  var p = undefined;
  angular.forEach(papers, function(paper) {
      if(paper.doi==doi){
      p = paper;
      }
      });
  return p;
}

function paperFromId(papers,id): Paper {
  var p = undefined;
  angular.forEach(papers, function(paper) {
      if(paper.id==id){
      p = paper;
      }
      });
  return p;
}

function showPaper(): void {
  var recid = w2ui['paperlist'].getSelection()[0];
  var id = pidFromRecId(recid);
  if(id != undefined){
    location.href = "/paper/b/" + id;
  }else{
  }
}

function recordFromPid(pid: string): string {
  var rs = w2ui['paperlist'].records;
  var len = rs.length;
  var res = undefined;
  for(var i=0;i < len; i++){
    if(rs[i].id == pid){
      res = rs[i];
      break;
    }
  }
  return res;
}

function pidFromRecId(ri): string {
  var rs = w2ui['paperlist'].records;
  var len = rs.length;
  var res = undefined;
  for(var i=0;i < len; i++){
    if(rs[i].recid == ri){
      res = rs[i].id;
      break;
    }
  }
  return res;
}

function showAlert(msg: string, delay?: number): void {
  $('#alert-place').html(msg);
  $('#alert-box').css('visibility','visible');
  if(delay != undefined){
    setTimeout(function(){
        $('#alert-box').css('visibility','hidden');
        },delay);
  }
}

function showMsg(msg: string,delay: number): void {
  $('#alert-place').html(msg);
  $('#alert-box').css('visibility','visible');
  if(delay){
    setTimeout(function(){
        $('#alert-box').css('visibility','hidden');
        },delay);
  }
}


function moveDown() {
  moveBy(1);
}

function moveUp() {
  moveBy(-1);
}

function moveUpSelect(){ moveBySelect(-1);}
function moveDownSelect(){ moveBySelect(1);}

//FIXME: It's still buggy, but this is just a matter of selection algorithm.
//Interfacing with UI seems correct.
function moveBySelect(delta){
  var is = [];
  var ui = w2ui['paperlist'];
  var sels = ui.getSelection().sort();
  if(sels.length > 0){
  var idx = (delta < 0 ? sels[0] : sels[sels.length-1]);
  var sign = (delta > 0) ? 1 : -1;
  for(var i = 0; i <= Math.abs(delta);i++){
    ui.select(idx+i*sign);
  }
  }
}

function moveBy(delta){
  var ui = w2ui['paperlist'];
  var sels = ui.getSelection().sort();
  var idx = undefined;
  if(sels.length == 0){
    idx = 0;
  }else{
    idx = delta > 0 ? sels[sels.length-1] : sels[0];
  }
  var len = ui.recordsPerPage;
  var ni = idx + delta;
  if (ni < 1) ni = 1;
  if (ni > len) ni = len;
  console.log(idx);
  ui.selectNone();
  ui.select(ni);

}

function scrollToSelected($scope) {
  scrollTo($scope.cursor);
}

function scrollTo(idx) {
  var elem = idx >= 3 ? '#paper-entry-'+(idx-3) : "body";
  var top = $(elem).offset().top;
  var doc = $('body');
  //console.log(top);
  doc.animate({
scrollTop: top
},{ duration: 0 });
}

function searchBar(e: Event) {
  ui.search.focus();
  e.preventDefault();
  changeMode(Mode.mSearch);
}

function moveLeftInfo(){
  if(infoShown == ShownInfo.refs){
    infoShown = ShownInfo.cit;
    $('#info-a-cit').click();
  }else if(infoShown == ShownInfo.figs){
    infoShown = ShownInfo.refs;
    $('#info-a-refs').click();
  }
}

function moveRightInfo(){
  if(infoShown == ShownInfo.cit){
    infoShown = ShownInfo.refs;
    $('#info-a-refs').click();
  }else if(infoShown == ShownInfo.refs){
    infoShown = ShownInfo.figs;
    $('#info-a-figs').click();
  }
}

//Add tags. Works for multi selection.
function showTagAddWindow(shown): bool {
  if(shown){
    changeMode(Mode.mTag);
    var str = "<p>Input tags to add. Separate tags by comma.</p>";
    str += "<p>If you chose multiple papers, tags will be added to all of them.</p>"
    str += "<input id='tag-input'>";
    $('#tagModal .modal-body').html(str);
    $('#tagModal').modal();
    var input = $('#tagModal #tag-input');
    input.focus();
    input.val('');
    input.attr('data-mode','add');
  } else {
    changeMode(Mode.mNormal);
    $('#tagModal').modal('hide');
  }
  return false; // To prevent 'T' key input.
}

//Edit tags. Not working for multi selection.
function showTagEditWindow(shown): bool {
  //console.log('showTagEditWindow');
  var rs = selectedRecords();  
  if(shown && rs.length == 1){
    var tagtxt = rs[0].tags.join(', ');
    
    changeMode(Mode.mTag);
    var str = "<p>Update tags. Separate tags by comma.</p>";
    str += "<input id='tag-input'>";
    $('#tagModal .modal-body').html(str);
    $('#tagModal').modal();
    var input = $('#tagModal #tag-input');
    input.focus();
    input.val(tagtxt);
    input.attr('data-mode','update');
  } else {
    changeMode(Mode.mNormal);
    $('#tagModal').modal('hide');
  }
  return false; // To prevent 'T' key input.
}

$('#tagModal #tag-input').bind('shown',function(e){
  //  console.log('Hey')
    e.target.val('');
});

$('#tagModal').on('hidden', function () {
   changeMode(Mode.mNormal);
   $('input').blur();
});

function splitTagText(txt: string): string {
    var tags = txt.split(',');
    for(var i = 0; i < tags.length; i ++ ){
      tags[i] = tags[i].replace(/^\s*(.+?)\s*$/, "$1");
    }
    return tags;
}

function tagModalCheckEnter() {
  var input = $("#tagModal #tag-input");
  var mode = input.attr('data-mode');
  if(input.is(":focus")){
    $('#tagModal').modal('hide');
    var ids = selectedIds();
    var tagstext = splitTagText(input.val()).join("\n");
    var idtext = unsplitIds(ids);
    var token = getSecretToken();
    console.log(tagstext);

    var url = '';
    if(mode == 'add'){
      url = path_addtags;
    }else if(mode == 'update'){
      url = path_replacetags;
    }else{
      console.log('Mode not speficied.');
      return;
    }
    // Look at postAddTagsR and formHandlerWithIdtextAndParams  
    $.post(url,{id: idtext, params: tagstext,_token: token},function(res){
       console.log(res);
       w2ui['paperlist'].reload();
       reloadTagList();
    });
  }
}

function updateTagFilter() {
  selectedTags = [];
  $("ul.stacked li.active").each(function(){
      selectedTags.push($("span.tagname",this).text());
  });
  w2ui['paperlist'].search([{field: 'tags', value: selectedTags.join('\n'),type: "text",operator: "is"}]);
}

var path_gettags: string = '/list/alltags';

var selectedTags: string[] = [];

function tagSelected(tag: string): bool {
  var flag = false;
  for(var i = 0; i < selectedTags.length; i++){
    if(selectedTags[i] == tag){
      flag = true;
    }
  }
  return flag;
}

function reloadTagList(): void {
   $.get(path_gettags,function(res){
    var len = res.length;
    var elem = "<div id='taglist-heading'>'AND' filters</div><ul class='stacked nav nav-pills nav-stacked'>"
    for(var i=0; i<len; i++){
      var tag = res[i].tag;
      var act = '';
      if(tagSelected(tag)){
        act = ' class="active" '
      }
      elem += '<li'+act+'><a data-tag="'+attrEscape(tag)+'"><span class="tagname">'+tag+
              '</span><span class="tagnum">'+res[i].numPapers+'</span></a></li>';
    }

    //Note: You need string (not jQuery object) like this.
    w2ui['layout'].content('left', elem);

    $('ul.stacked li').click(function(e){
      console.log(e);
      //$(e.target).removeClass('.active');
      $(this).toggleClass('active');
      updateTagFilter();
    });
  }); 
}



//ToDo: This is a bit ad hoc,
//since this assumes name "_token" for hidden token and common to all forms.
//I need to check whenever Yesod is updated.
function getSecretToken(): string {
  return $('#hidden-forms input[name="_token"]').val();
}

function allSuccess(rs): bool {
  var flag = true;
  angular.forEach(rs, function(res) {
      if (!res.success){
      flag = false;
      }
      });
  return flag;
}

function batch_operation(path,verb,ids){
  if(ids.length == 0){
    showAlert('No paper is selected.');
  }else{
    var idtext = unsplitIds(ids);
    var token = getSecretToken();
    $.post(path,{id:idtext,_token:token}).success(function(res: ServerResponse[]){
        if(allSuccess(res)){
        if(res.length == 1){
        showMsg("One paper was "+verb+"ed.",2000);
        console.log(res);
        }else{
        showMsg(""+res.length + " papers were "+verb+"ed.",2000);
        }
        }else{
        showAlert("Error occured while "+verb+"ing: "+res.message);
        console.log(res);
        }
        }).error(function(){
          showAlert("Error occured while "+verb+"ing.");
          });
  }
}

function paper_delete(ids){
  batch_operation(path_delete,"delet",ids);
}

function reparse(ids) {
  batch_operation(path_reparse,"repars",ids);
}

function selectedRecords() {
  var allrs = w2ui['paperlist'].records;
  var sels = w2ui['paperlist'].getSelection();
  var rs = [];
  var len = sels.length;
  for(var i = 0; i < len; i ++){
    rs.push(allrs[sels[i]-1]);
  }
  return rs;
}


function selectedIds(): string[] {
  var rs = w2ui['paperlist'].records;
  var sels = w2ui['paperlist'].getSelection();
  var ids: string[] = [];
  var len = sels.length;
  //console.log(rs,sels,ids);
  for(var i = 0; i < len; i ++){
    ids.push(rs[sels[i]-1].id);
  }
  //console.log(ids);
  return ids;
}

function unsplitIds(ids: string[]): string {
  return ids.join('\n');
}

function attrEscape(str: string): string {
  return str.replace('"','').replace("'","");
}

function findRecordFromRecId(rs,id: number) {
  var len = rs.length;
  for(var i=0;i<len;i++){
    if(rs[i].recid == id){
      return rs[i];
    }
  }
  return undefined;
}



function movePage(delta: number): void {
  console.log("movePage");
  var ui = w2ui['paperlist'];
  var p = ui.page;
  var newp = p + delta;
  var num_p = Math.ceil(ui.total / ui.recordsPerPage);
  if(newp >= num_p){
    newp = num_p - 1;
  }else if (newp < 0) {
    newp = 0;
  }
  ui.goto(newp);
}


// Examples:
// http://w2ui.com/web/demos/#!grid/grid-2

var currentPopup = undefined;

$(function(){
  showMsg("Welcome",5000);
$('#maindiv').w2layout({
    name: 'layout',
    panels: [
        { type: 'left', size: 150, style: '', content: 'Loading...' },
        { type: 'main', style: 'padding: 5px;' }
    ]
});
  $('#button-help').popover({html:true,content:help_html,
    placement:'bottom',animation: false,trigger:'hover'});

  reloadTagList();

    w2ui['layout'].content('main',$().w2grid({
  name    : 'paperlist',
  url: '/list.json',
  show: { selectColumn: true,
    footer: true,
  	toolbar: true,	
		toolbarDelete: true,
    toolbarSearch: false
    },
  //  multiSearch: false,
  columns: [
    { field: 'recid', caption: '#', size: '20px', sortable: false  }
    , { field: 'title', caption: 'Title', size: '60%', resizable: true, sortable: true  }
    , { field: 'cittxt', caption: 'Citation', size: '250px' , resizable: true, sortable: true }
    , { field: 'type', caption: 'type', size: '100px', sortable: true, render: function (record, index) {
          return record.citation.type;
      }}
    , { field: 'tags', caption: 'Tags', size: '100px', sortable: false, editable: { type: 'text' },
        render: function (record, index) {
          return record.tags.join(', ');
    }}
    , { field: 'abstract_available', caption: 'Abs', size: '30px', sortable: true, render: function (record, index) {
       //  console.log(record);
         if(record.available.abstract){
            return '<i class="icon-ok"></i> ';
         }else{
            return "";
         }
    }}
    , { field: 'fulltext_available', caption: 'Full', size: '30px', sortable: true, render: function (record, index) {
        //  console.log(record);
         if(record.available.fulltext){
            return '<i class="icon-ok"></i> ';
         }else{
            return "";
         }
    }}
    , { field: 'refs_available', caption: 'Ref', size: '30px', sortable: true, render: function (record, index) {
         if(record.available.references){
            return '<i class="icon-ok"></i> ';
         }else{
            return "";
         }
    }}
    , { field: 'figs_available', caption: 'Fig', size: '30px', sortable: true, render: function (record, index) {
        //  console.log(record);
         if(record.available.figures){
            return '<i class="icon-ok"></i> ';
         }else{
            return "";
         }
    }}
    , { field: 'date', caption: 'Date added', size: '100px', sortable: true, hidden: true  }
    , { field: 'id', caption: 'Database ID', size: '100px', resizable: true, sortable: true, hidden: true  }
  ],
    	multiSearch: true,
  	searches: [
		{ field: 'any', caption: 'Any field', type: 'text' }
		, { field: 'tags', caption: 'Tags', type: 'text' }
		,{ field: 'date', caption: 'Date added', type: 'date' }
		,{ field: 'type', caption: 'Type', type: 'list', items: ['Any','Letter','Article','Review'] }
		, { field: 'tags', caption: 'Tag', type: 'text' }
	],
  //This is used before list.json ajax loading. You need to add all fields.
  records: [
    { recid: 1, title: 'Loading...', cittxt: 'Loading...', date: '', citation: {type: ""},tags: []
      ,available: {abstract: false,fulltext: false,figures: false,citation: false}}
  ]
  , recordsPerPage: 20  // Stub: temp for test
  , onDblClick: function(target, e) {
      console.log(e);
      var r = findRecordFromRecId(w2ui['paperlist'].records,e.recid);
      location.href = "/paper/b/" + r.id;
	}
  ,onDelete: function(target, eventData) {
		console.log(eventData);
    currentPopup = $().w2popup({
	title   : 'Delete papers',
	body    : 'Are you sure you want to delete the selected paper(s)?',
	buttons : '<button id="button-delete-ok">OK</button><button id="button-delete-cancel">Cancel</button>',
  speed: 0
});
    $('#button-delete-ok').click(function(){
      //delete selected papers.
      var ids = selectedIds();
      paper_delete(ids);
      currentPopup.close();
      w2ui['paperlist'].reload();
      });
    $('#button-delete-cancel').click(function(){
      currentPopup.close();
      });
    eventData.stop = true;
	}
  , onChange: function(target, eventData) {
		//console.log(eventData);
    var changed = w2ui['paperlist'].getChanged();
    var len = changed.length;
    var json = [];
    var count = 0;
    for(var i=0;i<len;i++){
      var tagtxt = splitTagText(changed[i].tags).join('\n');
      var pid = pidFromRecId(changed[i].recid);
      $.post(path_replacetags, {id: pid, params: tagtxt, _token: getSecretToken()},function(res){
        count += 1;
        if(count >= len){
          //Disable for now, because this causes pause.
          //w2ui['paperlist'].reload();

          // Currently only the first id is used.
          console.log(res);
          //ToDo: FIXME: This does not work yet.
          recordFromPid(res.ids[0]).tags = changed.tags;
          
          //ToDo: Do this while keeping tag filtering.
          reloadTagList();
        }
      });
    }
	}	
}));

});

$('#button-reader').click(function(){
  var ids = selectedIds();
  //location.href=path_reader+"&paper_id="+ids.join(':');
});

//
// Array utillity functions
//

function unique(origArr) {  
  var newArr = [],  
      origLen = origArr.length,  
      found,  
      x, y;  
  for ( x = 0; x < origLen; x++ ) {  
    found = undefined;  
    for ( y = 0; y < newArr.length; y++ ) {  
      if ( origArr[x].name === newArr[y].name ) {  
        found = true;  
        break;  
      }  
    }  
    if ( !found) newArr.push( origArr[x] );  
  }  
  return newArr;  
}

function removeFromArray(arr: any[],x: any): any[] {
  var len: number = arr.length;
  var ret: any[] = [];
  for(var i = 0; i < len; i++){
    if(arr[i] != x)
      ret.push(arr[i]);
  }
  return ret;
}

var help_html = "<h4 class='paperlist-help'>Keyboard shurtcuts</h4>" +
    "<table class='paperlist-help'><tr>" +
    "<td class='key'>j/k</td><td class='exp' rowspan='2'>Down/up scroll</td></tr>" +
    "<td class='key'>down/up</td></tr>" +
    "<td class='key'>h/l</td><td class='exp' rowspan='2'>Move to prev page</td></tr>" +
    "<td class='key'>left/right</td></tr>" +
    "<td class='key'>i</td><td class='exp'>Toggle info/abstract</td></tr>" +
    "<td class='key'>f</td><td class='exp'>Toggle figures</td></tr>" +
    "<td class='key'>t</td><td class='exp'>Add a tag</td></tr>" +
    "<td class='key'>T</td><td class='exp'>Edit tags</td></tr>" +
    "<td class='key'>Esc</td><td class='exp'>Close info/figure/tag window</td></tr>" +
    "<td class='key'>a</td><td class='exp'>Toggle tag bar</td></tr>" +
 //   "<td class='key'>q</td><td class='exp'>Toggle header bar</td></tr>" +
    "</table>";


 
