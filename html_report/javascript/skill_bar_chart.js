/**
 * Created by huisu on 2/8/16.
 */
$(document).ready(function(){
        drawchart3()
    });
function drawchart3(){
    var opts = {
            "dom": "chart3",
            "width":    800,
            "height":    400,
            "x": "skill",
            "y": "count",
            "group": "level",
            "type": "multiBarChart",
            "id": "chart3"
        },
        data = [
            {
                "count": 2,
                "skill": "R, graphic basics",
                "level": "Expert"
            },
            {
                "count": 3,
                "skill": "Github",
                "level": "Expert"
            },
            {
                "count": 2,
                "skill": "R, advanced ",
                "level": "Expert"
            },
            {
                "count": 6,
                "skill": "Reproducible documentation",
                "level": "Expert"
            },
            {
                "count": 1,
                "skill": "Matlab",
                "level": "Expert"
            },
            {
                "count": 34,
                "skill": "R, graphic basics",
                "level": "Confident"
            },
            {
                "count": 28,
                "skill": "Github",
                "level": "Confident"
            },
            {
                "count": 22,
                "skill": "R, advanced ",
                "level": "Confident"
            },
            {
                "count": 26,
                "skill": "Reproducible documentation",
                "level": "Confident"
            },
            {
                "count": 24,
                "skill": "Matlab",
                "level": "Confident"
            },
            {
                "count": 53,
                "skill": "R, graphic basics",
                "level": "A little"
            },
            {
                "count": 48,
                "skill": "Github",
                "level": "A little"
            },
            {
                "count": 57,
                "skill": "R, advanced ",
                "level": "A little"
            },
            {
                "count": 39,
                "skill": "Reproducible documentation",
                "level": "A little"
            },
            {
                "count": 44,
                "skill": "Matlab",
                "level": "A little"
            },
            {
                "count": 25,
                "skill": "R, graphic basics",
                "level": "None"
            },
            {
                "count": 35,
                "skill": "Github",
                "level": "None"
            },
            {
                "count": 33,
                "skill": "R, advanced ",
                "level": "None"
            },
            {
                "count": 43,
                "skill": "Reproducible documentation",
                "level": "None"
            },
            {
                "count": 45,
                "skill": "Matlab",
                "level": "None"
            }
        ]

    if(!(opts.type==="pieChart" || opts.type==="sparklinePlus" || opts.type==="bulletChart")) {
        var data = d3.nest()
            .key(function(d){
                //return opts.group === undefined ? 'main' : d[opts.group]
                //instead of main would think a better default is opts.x
                return opts.group === undefined ? opts.y : d[opts.group];
            })
            .entries(data);
    }

    if (opts.disabled != undefined){
        data.map(function(d, i){
            d.disabled = opts.disabled[i]
        })
    }

    nv.addGraph(function() {
        var chart = nv.models[opts.type]()
            .width(opts.width)
            .height(opts.height)

        if (opts.type != "bulletChart"){
            chart
                .x(function(d) { return d[opts.x] })
                .y(function(d) { return d[opts.y] })
        }

        d3.select("#" + opts.id)
            .append('svg')
            .datum(data)
            .transition().duration(500)
            .call(chart);

        nv.utils.windowResize(chart.update);
        return chart;
    });
};
