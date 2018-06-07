$(document).ready(function(){
     
  $("#valtoupdate").bind("DOMSubtreeModified",function(){
    
    var y = parseFloat($("#valtoupdate").text()); 
    
    console.log("changed: ", y);
    
    if(!isNaN(y)) {
      
      var x = (new Date()).getTime(); // current time
      
      console.log("updating: ", y, " on: ", x);
      
      $("#memoryc").highcharts().series[0].addPoint([x, y, true, true]);
      
    }
  
  });
  
});