var redraw;

window.onload = function() {
    var width = $(document).width();
    var height = $(document).height() - 100;
    var url =  "https://api.github.com/repos/edofic/scrat-lang/contents/main/src/com/edofic/scrat/Parser.scala";
    $.getJSON(url + "?callback=?", null, function(response) {
        var content=response.data.content;
        var source = atob(content.split("\n").join(""))
        var blocks = source.split("private");

        //extract names
        var parsers = []
        for(var i=0; i<blocks.length; i++){
            var matched = blocks[i].match(/lazy val (\w+)/); 
            if(matched!=null && matched[1]!=undefined){
                parsers.push({
                    name:matched[1],
                    body:blocks[i].split("PackratParser")[1]
                });
            } 
        }

        //find dependencies
        data = []
        for(var i=0; i<parsers.length; i++){
            var body = parsers[i].body
            for(var j=0; j<parsers.length; j++){
                 var name = parsers[j].name
                 if(body.search(name) > 0){
                    data.push({a:parsers[i].name, b:name});
                 }
            }
        }
        
        var g = new Graph();
    
        for( var i=0, l=data.length; i<l; i++ ) {
            g.addEdge(data[i].a, data[i].b, {"directed":true});
        }
        
        var layouter = new Graph.Layout.Spring(g);
        var renderer = new Graph.Renderer.Raphael('canvas', g, width, height);

        redraw = function() {
            layouter.layout();
            renderer.draw();
        };
    });
};


    




