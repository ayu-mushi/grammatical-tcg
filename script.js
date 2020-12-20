try {
  var ws = new WebSocket('ws://localhost:8000/');
  //var ws = new WebSocket('ws://153.127.56.158/');
} catch (err) {
  console.error(err);
}

$("form").submit(function(){
  ws.send($('input').val());
  $('input').val('');
  return false;
});

/* setInterval(function(){
  ws.send("dummy");
}, 25000); */

var who_i_am = true;


function initField(){
  function one(name){
    for(var i=0; i<40; i++){
      monster_zone = $("#"+name + "> .monster-zone");
      var zone_i = $("<div class='zone-"+i+" zone'>").appendTo(monster_zone);
    }
  }
  one("myself");
  one("opponent");
}
initField();

ws.onmessage = function (msg) {
  var msg_analyzed = JSON.parse(msg.data);
  if (msg_analyzed.tag == "Choice"){
    $("#select_reading").empty();
    ol = $("<div>").appendTo("#select_reading");

    for (var i = 0; i != msg_analyzed.contents.length;i++){
      li = $("<div class='syntax-tree'>").appendTo(ol);
      vis = $(Viz(msg_analyzed.contents[i])).appendTo(li);
      vis.height("200px").width("200px");
      vis.css("float","left");
      vis.click(function(){
        ws.send($(".syntax-tree").index(this.parents));
        $("#select_reading").empty();
      });
    }

    // $("#select_reading.ol").append("<li></li>").append(draw_syntax_tree(msg_analyzed.contents));
  } else if (msg_analyzed.tag == "YouAre"){
    var c = msg_analyzed.contents;
    who_i_am = c;
    $("#i-am").empty();
    if(who_i_am){
      $("#i-am").append("あなたは先手です");
    } else {
      $("#i-am").append("あなたは後手です");
    }
  }
  else if (msg_analyzed.tag == "Hand"){
    $("#hand").empty();
    var c = msg_analyzed.contents;
    c.forEach(function(x, i){
      hand_card = $("<div id='hand-card"+i+"' class='hand-card'>"+x+"</div>").appendTo("#hand");
    });
  } else if (msg_analyzed.tag=="RequireMonsterToSummon"){
    console.log("require");
    $("#button-zone").append("<button id='summon-button'>召喚</div>");
    $("#button-zone").append("<button id='clear-button'>召喚のやり直し</div>");

    $("#summon-button").on('click', () => {
      ws.send($("#monster-to-summon").html());
      $(".hand-card").removeClass("active");
      $("#monster-to-summon").empty();
      $("#summon-button").remove();
      $("#clear-button").remove();
    });
    $("#clear-button").on('click', () => {
      $("#monster-to-summon").empty();
    });

    $("#hand > .hand-card").addClass("active");
    $("#hand > .hand-card").on("click", function(){
      var i = $("#hand > .hand-card").index(this);
      if (!($("#monster-to-summon").html().split(" ").includes(i+""))){
        $("#monster-to-summon").append(" " + i);
      }
    });
  } else if (msg_analyzed.tag == "Refresh"){
    $(".zone").empty();
    var c = msg_analyzed.contents;
    if(who_i_am){
      myself = c._myself;
      opponent = c._opponent;
    } else {
      myself = c._opponent;
      opponent = c._myself;
    }
    var drawField = (function(player, name){
      myfield = ("#field > #"+name);
      hand = player._hands;
      deck = player._deck;
      field = player._field;
      //hand_div = $("#field > #"+name+" > .hand");
      monster_zone = $("#"+name + "> .monster-zone");
      $("zone").empty();

      field.forEach(function(cell, i){
        var zone_i = monster_zone.children(".zone-"+i);
        if(cell != null){
          $(Viz(cell[1])).appendTo(zone_i);
          $("svg").height("200px").width("200px");
        } else {
          var nomonster = $("<div class='no-monster'>N</div>").appendTo(zone_i);
        }
      });
    });
    drawField(myself,"myself");
    drawField(opponent, "opponent");

    $("#hand").empty();
    myself._hands.forEach(function(x, i){
      hand_card = $("<div id='hand-card"+i+"' class='hand-card'>"+x+"</div>").appendTo("#hand");
    });
    $("#hand-number").text("(手札: "+myself._hands.length+" 枚)");

    $("#opponent-hand").empty();
    opponent._hands.forEach(function(x, i){
      hand_card = $("<div id='hand-card"+i+"' class='hand-card'>■</div>").appendTo("#opponent-hand");
    });
    $("#opponent-hand-number").text("(手札: "+opponent._hands.length+"枚)");

  } else if(msg_analyzed.tag=="WhereToPlace"){
    $("#myself > .monster-zone > .zone").addClass("active");
    $("#myself > .monster-zone > .zone").on("click", function(){
      //if($(this).children("svg") != null) { return; }
      ws.send($("#myself > .monster-zone > .zone").index(this));
      $("#myself > .monster-zone > .zone").off();
      $("#myself > .monster-zone > .zone").removeClass("active");
    });
  }
  else if (msg_analyzed.tag == "Message"){
    $('#console').prepend(msg_analyzed.contents + '<br>');
  } else {
    console.log("unknown:" +msg_analyzed.tag);
  }
}

