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

var Filter = Backbone.Model.extend({
	defaults: function () {
		var current = new Date();
		return { from: new Date(current-1000*60*60*24*30), to: current};
	},
	setPeriod: function (s){
		this.set(this.calcPeriodFromType(s));
	},
	calcPeriodFromType: function(s){
		var from;
		var to = new Date();
		if(s=='lastmonth'){
			from = new Date(to - (1000*60*60*24*30));
			return {from: from, to: to};
		}else if (s == 'lastweek'){
			from = new Date(to - (1000*60*60*24*7));
			return {from: from, to: to};
		}else if (s == 'today'){
			from = new Date(to - (1000*60*60*24));
			return {from: from, to: to};			
		}else if (s == 'lasthour'){
			from = new Date(to - (1000*60*60));
			return {from: from, to: to};			
		}else if (s == 'customtime'){  //FIXME. stub.
			from = new Date(to - (1000*60*60*24*30));
			return {from: from, to: to};			
		}else if (s == 'alltime'){
			from = new Date("2013-01-01");  // Stub
			return {from: from, to: to};			
		}else{
			return undefined;
		}
	}
});

var commonFilter = new Filter();

var Timeline = Backbone.Model.extend({
	defaults: function () {
		var current = new Date();
		var from = new Date(current-1000*60*60*24*30);
		var to = current;
		return { from: from, to: to, loaded: false,
			fromTruncated: new Date(mkDateStr(from)),
			toTruncated: new Date(mkDateStr(to)),
			unit: "day"};
	},
	setData: function(papers){
		var current = new Date();
		var oldest = new Date("1980-01-01");
		var dates = _.map(papers,function(d){
			var date = new Date(d.addedDate);
			return (date >= oldest ? new Date(date) : current);
		});
		var vmin = _.min(dates);
		var vmax = _.max(dates);
		console.log(vmin,vmax);
		this.set({from: vmin,to:vmax,loaded: true});
	}
});

// This keeps all papers in the library, and provides filters for display.
// This design may be reconsidered for a large library.
var PaperList = Backbone.Collection.extend({
	model: app.Paper,
	url: "/list.json",
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

	//f: {period: {from: Date, to: Date}, search: String}
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
		mk2DTime(res);
		timeline.setData(res);
		return res;
	}
});

//Make an instance of Collection.
app.Papers = new PaperList();

var timeline = new Timeline();
//
// View definitions
//

app.CalendarItemView = Backbone.View.extend({
	tagName: "tr",
	template: _.template( $('#calendar-item-template').html() ),
	render: function(){
    	this.$el.html( this.template( this.model.toJSON() ) );
		return this;
	}});

app.TimelineView = Backbone.View.extend({
	el: $("#content-timeline"),
	initialize: function() {
		this.mkTimeline();
		this.listenTo(commonFilter, 'change', this.loadData);
	},
	tFrom: undefined,
	tTo: undefined,
	width: 800,
	height: 200,
	svg: undefined,
	bars: undefined,
	removeAll: function(){

	},
	mkTimeline: function() {
		this.svg = d3.select('#timeline-graph')
			.append('svg')
			.attr('width',this.width)
			.attr('height',this.height)
			.style('background','#eeeeee');

		var rect = this.svg.append('rect')
			.attr('x',10)
			.attr('y',10)
			.attr('width',this.width-20)
			.attr('height',this.height-20)
			.style('fill','#cccccc');

		this.tFrom = this.svg.append('text')
			.attr('x',20)
			.attr('y',this.height-20)
			.attr('text-anchor','start')
			.text("");

		this.tTo = this.svg.append('text')
			.attr('x',this.width-20)
			.attr('y',this.height-20)
			.attr('text-anchor','end')
			.text("");
	},
	loadData: function() {
		this.svg.selectAll('rect.bars').remove();
/*		if(this.bars){
			this.bars
			.transition()
			.duration(300)
			.style('fill-opacity',0)
			.remove();				
		}*/
		var from = commonFilter.get('from');
		var to = commonFilter.get('to');
		this.tFrom.text(formatDate(from));
		this.tTo.text(formatDate(to));

		var ybase = this.height - 50;
		this.svg.append('line')
			.attr('x1',20)
			.attr('y1',ybase)
			.attr('x2',this.width-20)
			.attr('y2',ybase)
			.attr('stroke','black')
			.attr('stroke-width',1);
		
		var vals = _.map(journalstat.calendar,function(d){return (new Date(d.date) >= from) ? d.count : 0;});
		var bar_height_factor = (this.height-80)/_.max(vals);
		var bar_width = (this.width-60)/((to-from)/((1000*60*60*24)));
		this.bars = this.svg
			.selectAll('rect.bars')
			.data(journalstat.calendar)
			.enter()
			.append('rect')
			.attr('class','bars')
			.attr('x',function(d,i){return ((new Date(d.date)-from)/(1000*60*60*24))*bar_width+30})
			.attr('y',function(d){return ybase-bar_height_factor*d.count;})
			.attr('width',bar_width*0.9)
			.attr('height',function(d){return bar_height_factor*d.count;})
			.attr('fill','#4444ff');
	},
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
});

$('#filter-time label').on('change',function(e){
	commonFilter.setPeriod($(e.target).attr('id'));
});


window.setTimeout(function(){
},3000);

function formatDate(d) {
	return ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"][d.getMonth()] + " " + d.getDate();
}

app.CalendarView = Backbone.View.extend({
	el: $("#content-calendar"),
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
	calendar: function(){
		$('.content').hide();
		$('#content-calendar').show();
	},
	trend: function(){
		$('.content').hide();
		$('#content-trend').show();
	}
});

app.Papers.on("add",function(){
//	log("added.")
});

var route = new Route();
var view = new app.CalendarView();
var viewtimeline = new app.TimelineView();
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

	var js = _.map(dat,function(j){
		return j.citation.journal;
	});
	var js2 = _.countBy(js,function(j){
		return (j && j != '' ? j : "unknown");
	});
	var js3 = _.map(js2,function(k,v){
		return [v,k];
	});
	res.chart = js3;

	res.time = _.take(_.sortBy(_.map(dat,function(p){
		return [p.citation.title,new Date(p.addedDate),p.id];
	}), function(p) {
		return p[1];
	}).reverse(),numHist);

	var dates = _.groupBy(dat,function(p){
		return mkDateStr(new Date(p.addedDate));
	});
	//console.log(dates);

	res.calendar = _.map(dates,function(papers,date){
		return {date: date, count: papers.length,papers: papers};
	});


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




d3.select(self.frameElement).style("height", "800px");

function getTimeGrid(papers){
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

