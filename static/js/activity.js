function log(arg){
	console.log(arg);
}

$(function(){

//
// Model definitions
//

var journalstat;

var app = app || {};
app.Paper = Backbone.Model.extend({
	defaults: function () {
		return { id: "", title: "", citation: "", tags:[], doi: "",checked: false };
	},
	toggle: function(){
		this.set('checked', !this.get('checked'));
	},
	parse: function (res) {
//		log(res.title);
		var res2 = res;
		res2.addedDate = new Date(res.addedDate);
		return res2;
	}
});


// This keeps all papers in the library, and provides filters for display.
// This design may be reconsidered for a large library.
var PaperList = Backbone.Collection.extend({
	model: app.Paper,
	url: "/activity.json",
	initialize: function(){
		this.$date = undefined;
		var test1 = new app.Paper({id:"123123",title:"Hoge",citation:"JACS,2000."});
//		log(test1);
//		this.add(test1);
		this.fetch();
	},
	selectDate: function(d){
		this.$date = d;
		this.trigger('changeDate');
	},
	getChecked: function(){
		return this.where({checked:true});
	},
	getByDate: function(date){
		log('getByDate: '+date);
//	return this.filter(function(){return true});
		return this.filter(function(p){
			var d = mkDateStr(p.get('addedDate')).slice(0,10);
//			log(d,date);
			return d == date;
		});
	},
	getPeriodFromType: function(s){
		var from;
		var to = new Date();
		if(s=='lastmonth'){
			from = to - (1000*60*60*24*30);
			return {from: from, to: to};
		}else if (s == 'lastweek'){
			from = to - (1000*60*60*24*7);
			return {from: from, to: to};
		}else if (s == 'today'){
			from = to - (1000*60*60*24);
			return {from: from, to: to};			
		}else if (s == 'lasthour'){
			from = to - (1000*60*60);
			return {from: from, to: to};			
		}else if (s == 'customtime'){  //FIXME. stub.
			from = to - (1000*60*60*24*30);
			return {from: from, to: to};			
		}else if (s == 'alltime'){
			from = 0;
			return {from: from, to: to};			
		}else{
			return undefined;
		}
	},
	//f: {period: {from: Date, to: Date}, fulltext: String, author: [String], authorAnd: Bool}
	getByFilters: function(f){
		return this.filter(function(p){
			return p.addedDate >= f.period.from && p.addedDate <= f.period.to
				&& true // stub
				&& true; //Stub
		});
	},
	parse: function(res){
		journalstat = getJournalStat(res,10);
		//console.log(res);

		//console.log(journalstat);
	//	mkTable(journalstat.time);
		mkCalendar(journalstat.calendar);
		mkPieChart(journalstat.chart);
		mk2DTime(res.papers);
		return res.papers;
	}
});

//Make an instance of Collection.
app.Papers = new PaperList();

//
// View definitions
//

app.CalendarItemView = Backbone.View.extend({
	tagName: "tr",
	template: _.template( $('#timeline-item-template').html() ),
	render: function(){
    	this.$el.html( this.template( this.model.toJSON() ) );
		return this;
	}});

app.CalendarView = Backbone.View.extend({
	el: $("#content-timeline"),
	events:{
		'.check': 'toggle'
	},
	initialize: function(){
		this.listenTo(app.Papers, 'changeDate', this.selectDate);
//		this.addAll();
	},
	toggle: function(){
		var view = new CalendarItemView({ model: app.Paper });
    	$('table#selected').append( view.render().el );
	},
	selectDate: function(){
		var ps = app.Papers.getByDate(app.Papers.$date);
		if(ps)
			this.addAll(ps);
	},
	//Make a view for one paper item.
	addOne: function( paper ) {
      var view = new app.CalendarItemView({ model: paper });
      $('#paper-list').append( view.render().el );
    },
    addAll: function(papers) {
    //	log(papers);
    	var c = this;
      this.$('#paper-list').html('');
      _.each(papers,function(p){c.addOne(p);});
    }
})

var Route = Backbone.Router.extend({
	routes: {
		"timeline": "timeline",
		"stat": "stat",
		"recent": "recent",
		"trend": "trend"
	},
	timeline: function(){
		$('.content').hide();
		$('#content-timeline').show();
	},
	stat: function(){
		$('.content').hide();
		$('#content-statistics').show();
		$('#journals').show();
	},
	recent: function(){
		$('.content').hide();
		$('#content-recent').show();
	},
	trend: function(){
		$('.content').hide();
		$('#content-trend').show();
	}
});

app.Papers.on("add",function(){
	log("added.")
});

var route = new Route();
var view = new app.CalendarView();
Backbone.history.start();

//ToDo: modify this to Backbone events.
$('#filter-time button').click(function(e){
	var t = e.target.id;
	if(t == 'customtime')
		return; // Stub
	else
		app.Papers.getByPeriodType(t);

});

function mkDateStr(date){
	var m = date.getMonth() + 1;
	var d = date.getDate();
	return "" + date.getFullYear() + "-" + (m >= 10 ? m : "0" + m) + "-" + (d >= 10 ? d : "0" + d);
}

function getJournalStat(dat,numHist) {
	var res = new Object();

	var js = _.map(dat.papers,function(j){
		return j.citation.journal;
	});
	var js2 = _.countBy(js,function(j){
		return (j && j != '' ? j : "unknown");
	});
	var js3 = _.map(js2,function(k,v){
		return [v,k];
	});
	res.chart = js3;

	res.time = _.take(_.sortBy(_.map(dat.papers,function(p){
		return [p.citation.title,new Date(p.addedDate),p.id];
	}), function(p) {
		return p[1];
	}).reverse(),numHist);

	var dates = _.groupBy(dat.papers,function(p){
		return mkDateStr(new Date(p.addedDate));
	});
	//console.log(dates);

	res.calendar = _.map(dates,function(papers,datestr){
		return {date: datestr, count: papers.length,papers: papers};
	});
	//console.log(res.calendar);

	return res;
}



function getTimeCourse() {
	var data = {
		labels : ["January","February","March","April","May","June","July"],
		datasets : [
		{
			fillColor : "rgba(220,220,220,0.5)",
			strokeColor : "rgba(220,220,220,1)",
			data : [65,59,90,81,56,55,40]
		},
		{
			fillColor : "rgba(151,187,205,0.5)",
			strokeColor : "rgba(151,187,205,1)",
			data : [28,48,40,19,96,27,100]
		}
		]
	}

	return data;
};

$('.choose-pane').click(function(e){
	var path = $(e.target).attr('data-pane');
	$(e.target).parent().parent().children().removeClass("active");
	$(e.target).parent().addClass("active");
	route.navigate(path,{trigger:true});
});

$('.choose-stat').click(function(e){
	var path = "stat/"+$(e.target).attr('data-stat-pane');
	route.navigate(path,{trigger:true});
})

function dateFormat(date) {
	var y = date.getFullYear();
	var m = date.getMonth() + 1;
	var d = date.getDate();
	var h = date.getHours();
	var mi = date.getMinutes();
	var w = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"][date.getDay()];
	return w + ", " + m + "/" + d + ", " + y + " " + h + ":" + mi;
}

function mkCitHtml(cit){
	var pt = cit.pageTo;
	if(cit.journal == null){
		return ""
	}else{
		return "<i>" + cit.journal + ",</i><b>"+cit.volume+",</b> "+cit.pageFrom+(pt ? "-"+pt : "")+" ("+cit.year+")";
	}
}


function getColorNum(count,max){
	var n = Math.round(count*10/max);
	return (n > 10 ? 10 : (n < 0 ? 0 : n));
}


function mkCalendar(json) {
	var data = d3.nest()
	.key(function(d) { return d.date; })
	.rollup(function(d) { return d[0].count; })
	.map(json);

	var max = _.max(data);

	rect.filter(function(d) { return d in data; })
	.attr("class", function(d) { return "day " + (data[d] ? "exist ": "") + "color" + getColorNum(data[d],max); })
	.attr("id", function(d){return d;})
	.on("click",function(d){
		app.Papers.selectDate(d);
	})
	.select("title")
	.text(function(d) { return d + ": " + data[d]; });
}

function mkPieChart(chart){
try{
		$('#journal-graph').highcharts({
			chart: {
				plotBackgroundColor: null,
				plotBorderWidth: null,
				plotShadow: false
			},
			title: {
				text: 'Journals in the library'
			},
			tooltip: {
				pointFormat: '<b>{point.y:.0f}</b>'
			},
			plotOptions: {
				pie: {
					allowPointSelect: true,
					cursor: 'pointer',
					dataLabels: {
						enabled: true,
						color: '#000000',
						connectorColor: '#000000',
						format: '<b>{point.name}</b>: {point.percentage:.1f} %'
					}
				}
			},
			series: [{
				type: 'pie',
				name: 'Journal statistics',
				data: chart
			}]
		});
	}catch(e){
		log(e);
	}
}


var width = 960,
height = 136,
    cellSize = 17; // cell size

    var day = d3.time.format("%w"),
    week = d3.time.format("%U"),
    percent = d3.format(".1%"),
    format = d3.time.format("%Y-%m-%d");

    var color = d3.scale.quantize()
    .domain([-.05, .05])
    .range(d3.range(11).map(function(d) { return "q" + d + "-11"; }));

    var year = (new Date()).getFullYear();

    var svg = d3.select("#calendar").selectAll("svg")
    .data(d3.range(year - 1, year + 1))
    .enter().append("svg")
    .attr("width", width)
    .attr("height", height)
    .attr("class", "RdYlGn")
    .append("g")
    .attr("transform", "translate(" + ((width - cellSize * 53) / 2) + "," + (height - cellSize * 7 - 1) + ")");

    svg.append("text")
    .attr("transform", "translate(-6," + cellSize * 3.5 + ")rotate(-90)")
    .style("text-anchor", "middle")
    .text(function(d) { return d; });

    var rect = svg.selectAll(".day")
    .data(function(d) { return d3.time.days(new Date(d, 0, 1), new Date(d + 1, 0, 1)); })
    .enter().append("rect")
    .attr("class", "day")
    .attr("width", cellSize)
    .attr("height", cellSize)
    .attr("x", function(d) { return week(d) * cellSize; })
    .attr("y", function(d) { return day(d) * cellSize; })
    .datum(format);

    rect.append("title")
    .text(function(d) { return d; });

    svg.selectAll(".month")
    .data(function(d) { return d3.time.months(new Date(d, 0, 1), new Date(d + 1, 0, 1)); })
    .enter().append("path")
    .attr("class", "month")
    .attr("d", monthPath)
    .attr("x", function(d) { return week(d) * cellSize; })
    .attr("y", function(d) { return day(d) * cellSize; })

    function monthPath(t0) {
    	var t1 = new Date(t0.getFullYear(), t0.getMonth() + 1, 0),
    	d0 = +day(t0), w0 = +week(t0),
    	d1 = +day(t1), w1 = +week(t1);
    	return "M" + (w0 + 1) * cellSize + "," + d0 * cellSize
    	+ "H" + w0 * cellSize + "V" + 7 * cellSize
    	+ "H" + w1 * cellSize + "V" + (d1 + 1) * cellSize
    	+ "H" + (w1 + 1) * cellSize + "V" + 0
    	+ "H" + (w0 + 1) * cellSize + "Z";
    }

    d3.select(self.frameElement).style("height", "800px");

function getTimeGrid(papers){
	log(papers);
    var d2 = _.map(_.range(0,8),function(y){
    	return _.map(_.range(0,25),function(x){
    		return {x:x,y:y,v:0};
    	});
    });
    _.each(papers,function(p){
    	var d = new Date(p.addedDate);
    	var day = d.getDay();
    	var time = d.getHours();
    //	log(day,time);
	    d2[day][time].v += 1;
    });
    return d2;
}

function mk2DTime(papers){
  	var data = getTimeGrid(papers);

    var width = 500;
    var height = 170;

  	if(!data)
  		return;
    var nx = data[0].length;
    var ny = data.length;

    var svg = d3.select("#time-graph").append("svg:svg")
    				.attr("width",width)
					.attr("height",height);
    var d1 = _.map(_.range(0,10),function(i){
    		return {x:i,y:i,v:i};
    	    });

    var v_max = _.max(_.flatten(data),function(d){return d.v;}).v;
//    v_max = 2;
    svg
    	.selectAll("rect").data(_.flatten(data)).enter().append("rect")
    	.attr("width",width/nx-1)
		.attr("height",height/ny-3)

		.attr("x",function(d){return (d.x-1)*(width/nx)})
		.attr("y",function(d){return (d.y-1)*(height/ny)})
		.attr("fill",function(d,i){
			var c = d3.hsl(240*(1-d.v/v_max),1,0.5);
			return c;
		});
}

});
//End of $() block
//

