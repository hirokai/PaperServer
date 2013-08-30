// parse.js

var log = function(d) { console.log(d); };

function parseSupported(url) {
	var r = false;
	for(var i = 0; i < pubs.length; i++){
		r = pubs[i].supported(url);
		if(r) break;
	}
	return r;
}

function parseHtml(url,doc) {
	log("Parse: " + url);
	var r = undefined;
	for(var i = 0; i < pubs.length; i++){
		r = pubs[i].parse(url,doc);
		if(r) break;
	}
	var res = new Object();
	if(r){
		res.success = true;
		res.data = r
	}else{
		res.success = false;
	}
	return res;
}

function getMeta(doc,name){
	return $.map(doc.find('meta[name="'+name+'"]'),function(e){
		log(e);
		return $(e).attr('content');
	});
}

var rsc = {
	supported: function(url) {
		return (url.indexOf("/pubs.rsc.org/en/content/") != -1);
	},
	parse: function(url,html){
	if(! this.supported(url)){
		return undefined;
	}else{
		var doc = $('<iframe>').html(html);
		var abs = doc.find('p.abstract').html();
		var cit = new Object();
		cit.title = getMeta(doc,"DC.title")[0];
		cit.journal = getMeta(doc,"citation_journal_title")[0];
		cit.authors = getMeta(doc,"citation_author");
		cit.pageFrom = getMeta(doc,"citation_firstpage")[0];
		cit.pageTo = getMeta(doc,"citation_lastpage")[0];
		cit.doi = getMeta(doc,"citation_doi")[0];
		cit.url = url;
		cit.publisher = "";
		cit.type = "";
		cit.volume = getMeta(doc,"citation_volume")[0];
		cit.year = parseInt(getMeta(doc,"citation_date")[0].match(/\d{4}/)[0],10);
		return {availability: {citation: true,abstract: true,fulltext: false,references: false, figures: false},html:html, url: url, doi: cit.doi,
				citation: cit, abstract: abs, references: [], figures: [], resources: [], misc: "", tags: [], supportLevel: "abstract"};
	}
}};

var pubs = [rsc];
