// elsevier.js

mydebug("Hi, elsevier.js is loaded!!");

var Elsevier = {
getPublisher: function(){return "Elsevier";},
isSupported: function(){
//		mydebug(location.href);
		var a = location.href.match(/www\.sciencedirect\.com/);
//		mydebug(!!a);
		return !!a;
},
getCitation: function(){
		var txt = $('div.cover > p').text();
		var m = txt.match(/Volume (\w+).+(\d{4}), Pages (\w+)–(\w+)/);
		var year = m ? m[2] : "";
		var vol = m ? m[1] : "";
		var pfrom = m ? m[3] : "";
		var pto = m ? m[4] : "";
  var ret = {
				title: $("h1.svTitle").html(),
			doi: Elsevier.getDoi(),
				journal: $('div.title > a > img').prop('alt'),
			type: $('div.publicationType').text(),
				volume: vol,
				year: year,
				pageFrom: pfrom,
				pageTo: pto,
        authors: [],
				authorStr: ""
			};
  ret.authorStr = ""; //ToDo
  return ret;
},

getAbstract: function(){
	return $("div.abstract").html();
},

getMainHtmlNotWorking: function(){
  var orig = $("<div/>");
  orig.html($('div#centerPane').html());
		$('hr',orig).first().prevAll().remove();
		$('hr',orig).first().remove();
		$('div#References',orig).remove();
		var ret = orig.html();
//		mydebug(ret);
		return ret;
},

getMainHtml: function(){
		var h = $('div#centerPane > .svArticle').map(function(){
				return this.outerHTML;
		}).get().join("");
		var ret = $('<div/>');
		ret.html(h);
		ret.find('div.figTblUpiOuter').remove();
		return ret.html();
},

getRefs: function(){
	var refg = $("ol.references > li");
		var ret = [];
		refg.each(function(){
			var h = $("div.boxLink",this).html();
			var m = h ? h.match(/dx.doi.org\/(10\.\d{4}\S+?)(&amp;|&)/) : null;
				var doi = m ? m[1].replace("%2F","/") : undefined;
			var urlAlt = doi ? undefined : ""; // Not working yet.
				var r2 = $(this).clone();
				var cit = $("li.source",this).html();
			ret.push([{id: $(this).prop('id'), url: ((doi && doi!="") ? urlFromDoi(doi) : urlAlt), cit: cit, doi:doi}]);	
		});
//	mydebug(ret);
	return ret;
},

	getAbsFig: function(){
  return ""; //Stub
},

getDoi: function(){
	var a = $("a#ddDoi").prop('href');
	var m = a.match(/\/(10\..+)/);
		return m ? m[1] : "";
},
getResourceList: function(){
		var ret = $('img').map(function(){
				return $(this).prop('src');
		}).get();
		ret = uniqueArray(ret);
//		mydebug(ret);
		return ret;
}

}

addPublisher(Elsevier);
