// parse.js

var log = function(d) { console.log(d); };

// You need to fill all Citation fields to make JSON processing successful.

var emptyCitation = {authors: []
	, doi: ""
	, journal: ""
	, pageFrom: ""
	, pageTo: ""
	, publisher: ""
	, title: ""
	, type: ""
	, url: ""
	, volume: ""
	, year: 0};

var exampleCitation = {authors: []
	, doi: "hoge"
	, journal: "hoge"
	, pageFrom: "101"
	, pageTo: "102"
	, publisher: "hoge"
	, title: "hoge"
	, type: "hoge"
	, url: "hoge"
	, volume: "hoge"
	, year: 1999
};


function parseSupported(url) {
	var r = false;
	for(var i = 0; i < pubs.length; i++){
		r = pubs[i].supported(url);
		if(r) break;
	}
	return r;
}

function mkCitText(cit){
	return [cit.journal, ',', cit.volume, ',', cit.pageFrom,'-',cit.pageTo,'(',
			(cit.year>0 ? ('('+cit.year+')') : "")].join('');
}

function locator(txt){
	return "http://alocator.web.fc2.com/0.8/index.html?redirect=yes&q="+encodeURIComponent(txt);
}

function parseHtml(url,doc) {
	log("Parse: " + url);
	var r = undefined;
	for(var i = 0; i < pubs.length; i++){
//		console.log('Try parsing with: '+pubs[i].name);
		r = pubs[i].parse(url,doc);
		if(r) break;
	}
	var res = new Object();
	if(r){
		console.log(r);

		res.success = true;
		res.data = r;
	}else{
		res.success = false;
	}
	return res;
}

function getMeta(doc,name){
	return $.map(doc.find('meta[name="'+name+'"]'),function(e){
		return $(e).attr('content');
	});
}

var rsc = {
	name: "rsc",
	supported: function(url) {
		return (url.indexOf("/pubs.rsc.org/en/content/") != -1);
	},
	parse: function(url,html){
	if(! this.supported(url)){
		return undefined;
	}else{
		var doc = $('<iframe>').html(html);
		var abs = doc.find('p.abstract').html();
		var cit = commonReader(doc);
		cit.url = url;
		cit.type = "";
		cit.year = parseInt(getMeta(doc,"citation_date")[0].match(/\d{4}/)[0],10);
		return {availability: {citation: true,abstract: true,fulltext: false,references: false, figures: false}, url: url, doi: cit.doi,
				citation: cit, abstract: abs, mainhtml: "", references: [], figures: [], resources: [], misc: "", tags: [], supportLevel: "abstract"};
	}
}};

function commonReader(doc,c){
	var cit = c || emptyCitation;

	cit.doi = getMeta(doc,"citation_doi")[0] || "";

	cit.title = getMeta(doc,"citation_title")[0] || "";
	cit.authors = getMeta(doc,"citation_author") || [];

	cit.journal = getMeta(doc,"citation_journal_title")[0] || "";
	cit.volume = getMeta(doc,"citation_volume")[0] || "";
	cit.pageFrom = getMeta(doc,"citation_firstpage")[0] || "";
	cit.pageTo = getMeta(doc,"citation_lastpage")[0] || "";

	cit.publisher = getMeta(doc,"citation_publisher")[0] || "";

	return cit;
}

var plos = {
	name: "PLoS",
	supported: function(url){
		var m = url.match("^http://www.plos(.+?).org/article/");
		return (!!m);
	},
	parse: function(url,html) {
		if(! this.supported(url)){
			return undefined;
		}else{
			var doc = $('<iframe>').html(html);
			var cit = commonReader(doc);
			cit.url = url;

			var abshtml = this.abstract(doc);
			var mainhtml = this.mainHtml(doc);

			cit.type = $('#article-type-heading').text();
			cit.year = parseInt(getMeta(doc,"citation_date")[0].match(/\d{4}/)[0],10);
			var refs = _.map($('ol.references > li', doc),function(li){
				var m = $('span.label',$(li)).text().match(/(\d+)\./);
				var name = m ? m[1] : "";
				var id = $('a',$(li)).attr('name') || "";
				var t = li.innerText.replace(/\r?\n/g," ");
				var m2 = t.match(/(.+)doi:\s*(10\.\d{4}\/\S+)\.?\s*$/m);
				var txt = m2 ? m2[1] : "";
				var cref = emptyCitation;
				cref.doi = m2 ? m2[2] : "";
//				var urlel = _.find($('a',$(li)),function(el){return $(el).text().match(/10\..+/);});
				return {id: id,
					name: name,
					cit: cref,
					citText: txt,
					url: ""};
			});
			var toc = _.map($('ul.nav-page > li > a',doc),function(el){
				var e = $(el);
				return {title: e.html(), href: e.attr('href')};
			});
			return {availability: {citation: true,abstract: true,fulltext: true, references: true, figures: false}
					, url: url, doi: cit.doi,
					citation: cit, abstract: abshtml, mainhtml: mainhtml, references: refs, figures: [], resources: [],
					misc: "", tags: [], toc: toc, supportLevel: "abstract"};	
		}
	},
	abstract: function(doc){
		var abs = $('div.abstract',doc);
		$('h2',abs).remove();
		return abs.html();
	},
	mainHtml: function(doc){
		return (_.map($('div.article > div.section',doc),function(e){
			_.each($('a[href^="#"]',doc),function(e2){
				if(e2.innerHTML.match(/\[\d+\]/)){
					var elem = $(e2);
					elem.addClass('ref');
					elem.attr('id',elem.attr('href').slice(1));
					elem.removeAttr('href');
				}
			});
			return e.outerHTML;
		})).join();
	}
};

var elsevier = {
	name: "Elsevier",
	supported: function(url){
		var m = url.match("http://www.sciencedirect.com/science/article/");
		console.log(m);
		return (!!m);
	},
	parse: function(url,html){
		if(! this.supported(url)){
			return undefined;
		}else{
			var iframe = $('<iframe>');
			var doc = iframe.html(html);
			$('.head.headTemplate.page_fragment_ind',doc).after(htmls);
			var wholehtml = iframe.html();
			var cit = commonReader(doc);
			cit.url = url;

			var scr_el = this.getScriptEl(doc);
			cit.doi = this.doi(scr_el);
			var frag_keys = this.getFragKeys(scr_el);
			var htmls = this.getFrags(frag_keys).join('');
			var docmain = $('<iframe>').html(htmls);
			var abshtml = this.abstract(doc);
			var mainhtml = this.mainHtml(docmain);
			console.log("mainhtml Length: "+ mainhtml.length);
			var refs = _.map($('ol.references > li',docmain),function(el){
				var id = $(el).attr('id');
				var name = $('.label intra_ref',el).text();
				var txt = $('li.source',el).text();
				return {id: id, name: name, cit: emptyCitation, citText: txt, url: ""};
			});
			return {availability: {citation: true,abstract: true,fulltext: true, references: true, figures: false}
					, url: url, doi: cit.doi,
					citation: cit, abstract: abshtml, mainhtml: mainhtml, references: refs, figures: [], resources: [],
					misc: "", originalHtml: wholehtml, tags: [], supportLevel: "fulltext"};
		}
	},
	doi: function(scr_el){
		var scr = scr_el ? scr_el.innerHTML : "";
		var m = scr.match(/SDM\.doi\s*=\s*'(.+?)'/);
		return m ? m[1] : undefined;;
	},
	getFrags: function(frag_keys){
		var baseurl = 'http://www.sciencedirect.com/science/frag/'+frag_keys[0]+'/'+frag_keys[1]+'/frag_';
		var ret = [];
		for(var idx = 2;idx<30;idx++){
			$.ajax({
				method: "GET"
				, url: baseurl+idx
				, async: false
				, success: function(res){
					ret.push(res);
				}, error: function(){
					idx = 100;
				}
				});
		}
		return ret;
	},
	getScriptEl: function(doc){
		return _.find($('script',doc),function(scr){
			var txt = scr.innerHTML;
			return txt.indexOf("var SDM=new Object;") != -1;
		});
	},
	getFragKeys: function(scr_el){
		var scr = scr_el ? scr_el.innerHTML : "";
		var m = scr.match(/SDM\.pm\.pii\s*=\s*'(.+?)'/);
		var pii = m ? m[1] : undefined;
		var m2 = scr.match(/SDM\.pm\.fat\s*=\s*'(.+?)'/);
		var fat = m2 ? m2[1] : undefined;
		return [pii,fat];
	},
	abstract: function(doc){
		var abs = $('.abstract.svAbstract',doc);
		$('h2',abs).remove();
		return abs.html();
	},
	mainHtml: function(doc){
		return (_.map($('.page_fragment',doc),function(e){
			return e.innerHTML;
		})).join() || "";
	}
};

var oxford = {
	name: "Oxford Journals",
	supported: function(url){
		return url.indexOf("http://nar.oxfordjournals.org/content") == 0;
	},
	parse: function(url,html){
		if(! this.supported(url)){
			return undefined;
		}else{
			var iframe = $('<iframe>');
			var doc = iframe.html(html);

			var cit = commonReader(doc);

			var abshtml = this.abstract(doc);

			var refs = this.references(doc);

			return {availability: {citation: true,abstract: true,fulltext: false, references: true, figures: false}
					, url: url, doi: "Hige",
					citation: exampleCitation, abstract: "Hage", mainhtml: "Hoge", references: [], figures: [], resources: [],
					misc: "", tags: [], supportLevel: "abstract"};
		}		
	},
	abstract: function(doc){
		var abs = $('div.section.abstract',doc);
		$('h2',abs).remove();
		return abs.html();
	},
	references: function(doc){
		var lis = $('ol.cit-list > li',doc);
		return _.map(lis,function(el){
			var cit = emptyCitation;
			cit.title = $('span.cit-article-title',el).text() || "";
			cit.journal = $('abbr.cit-jnl-abbrev',el).text() || "";
			cit.year = $('span.cit-pub-date',el).text() || 0;
			cit.pageFrom = $('span.cit-fpage',el).text() || "";
			cit.pageTo = $('span.cit-lpage',el).text() || "";
			var id = $('a.rev-xref-ref',el).attr('id') || "";
			var name = id.slice(4);
			return {id: id, name: name, cit: cit, citText: mkCitText(cit), url: ""}
		});
	}
};

var pubs = [rsc,plos,elsevier,oxford];

///
var template_ = {
	name: "N/A",
	supported: function(url){
		return url.match(/hoge/);
	},
	parse: function(url,html){
		if(! this.supported(url)){
			return undefined;
		}else{
			var iframe = $('<iframe>');
			var doc = iframe.html(html);

			var cit = commonReader(doc);

			return {availability: {citation: true,abstract: false,fulltext: false, references: false, figures: false}
					, url: url, doi: cit.doi,
					citation: cit, abstract: abshtml, mainhtml: mainhtml, references: refs, figures: [], resources: [],
					misc: "", originalHtml: wholehtml, tags: [], supportLevel: "abstract"};
		}		
	}
};
///
