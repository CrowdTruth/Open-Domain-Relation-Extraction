function capitalizeTerm(sentence, b, e) {
  var res = sentence.trim().split(" "); 
  var newSen = "";
  
  //alert(res);
  
  for (var i = 0; i < res.length; i++) {
    if (b <= i && i < e) {
      newSen += res[i].toUpperCase();
    }
    else {
      newSen += res[i];
    }
    
    if (i != res.length - 1) newSen += " ";
  }
  
  
  //return sentence.substring(0, b) + sentence.substring(b, e).toUpperCase() + sentence.substring(e, sentence.length - 1);
  return newSen;
}

function getSeedTermSpan(sentence, termsInFactor, noWords, b) {
  var index = new Array();
  index.push(b);
  
  if (noWords > 1) {
    for (i = 1; i < noWords; i ++) {
      index.push(parseInt(parseInt(index[i - 1])) + termsInFactor[i - 1].length + 1);
    }
  }
  
  return index;
}

$$('.cml_field.cboxparent input').sort(function(a,b) {
   return Math.round(Math.random()*25) - 1 
}).each(function(o) { 
   o.getParent('.cml_row').inject(o.getParent('.cml_field')) 
})


require(['jquery-noconflict'], function(jQuery) {
    
  //Ensure MooTools is where it must be
  Window.implement('$', function(el, nc){
    return document.id(el, nc, this.document);
  });
  var $ = window.jQuery;

selectedIds = new Array();
checkboxes = new Array();
inputText = document.getElementsByClassName('relations validates-required relations validates');
inputWordsText = document.getElementsByClassName('step_2a_copy__paste_only_the_words_from_the_sentence_that_express_the_relation_you_selected_in_step1 validates-required highlightWords validates');

inputOtherText = document.getElementsByClassName('step_2b_if_you_selected_none_in_step_1_explain_why validates-required others validates'); 
inputElements = document.getElementsByTagName('input');
sentence = document.getElementsByClassName("word_split");
defaultMsg = inputOtherText[0].value;
b1 = document.getElementsByClassName('b1val');
b2 = document.getElementsByClassName('b2val');
e1 = document.getElementsByClassName('e1val');
e2 = document.getElementsByClassName('e2val');
hiddenFieldFactor1 = document.getElementsByClassName('factor1');
hiddenFieldFactor2 = document.getElementsByClassName('factor2');
  
$(".word_split").text(capitalizeTerm($(".word_split").text(), parseInt(b1[0].value), parseInt(e1[0].value)));
$(".word_split").text(capitalizeTerm($(".word_split").text(), parseInt(b2[0].value), parseInt(e2[0].value)));

hiddenFieldFactor1[0].value = hiddenFieldFactor1[0].value.toUpperCase();
hiddenFieldFactor2[0].value = hiddenFieldFactor2[0].value.toUpperCase();
    
noWordsFactor1 = hiddenFieldFactor1[0].value.split(/-| /).length;
termsInFactor1 = hiddenFieldFactor1[0].value.split(/-| /);
noWordsFactor2 = hiddenFieldFactor2[0].value.split(/-| /).length;
termsInFactor2 = hiddenFieldFactor2[0].value.split(/-| /);

  
    (function($){function injector(t,splitter,klass,after){var a=t.text().split(splitter),inject='';if(a.length){$(a).each(function(i,item){inject+='<span class="'+klass+(i+1)+'">'+item+'</span>'+after});t.empty().append(inject)}}var methods={init:function(){return this.each(function(){injector($(this),'','char','')})},words:function(){return this.each(function(){injector($(this),' ','word',' ')})},lines:function(){return this.each(function(){var r="eefec303079ad17405c889e092e105b0";injector($(this).children("br").replaceWith(r).end(),r,'line','')})}};$.fn.lettering=function(method){if(method&&methods[method]){return methods[method].apply(this,[].slice.call(arguments,1))}else if(method==='letters'||!method){return methods.init.apply(this,[].slice.call(arguments,0))}$.error('Method '+method+' does not exist on jQuery.lettering');return this}})(jQuery);
   
Array.prototype.remove = function(x) {
    for(i in this){
        if(this[i].toString() == x.toString()){
            this.splice(i,1)
        }
    }
};
  
Array.prototype.clear = function() {
    this.splice(0, this.length);
};
  
String.prototype.trim = function () {
  return this.replace(/^\s*/, "").replace(/\s*$/, "");
}



  $(document).ready(function(){
    
    $(".word_split").lettering('words');
    
    inputText[0].readOnly = true;
    inputWordsText[0].readOnly = true;
    
    sentenceText = $(".word_split").text();
    index1 = getSeedTermSpan(sentenceText, termsInFactor1, noWordsFactor1, parseInt(b1[0].value));
    index2 = getSeedTermSpan(sentenceText, termsInFactor2, noWordsFactor2, parseInt(b2[0].value));
    
    var startOffset = 0;
    var spans = $(".word_split").find("span").each(function() {
      if ($(this).text() == "-" || $(this).text() == "/") {
        startOffset = startOffset - 1;
      }
      
      var idNumber = $(this).attr("class").slice(4);
      $(this).attr("id", idNumber);
      
      var i = sentenceText.indexOf($(this).text(), startOffset);
      
     /* for (j = 0; j < index1.length; j ++) {
        if (i == index1[j]) {
          $(this).css("color", "darkred");
        }
      }
      
      for (j = 0; j < index2.length; j ++) {
        if (i == index2[j]) {
          $(this).css("color", "darkred");
        }
      }*/
      
      if (parseInt(b1[0].value) < idNumber && idNumber <= parseInt(e1[0].value)) 
        $(this).css("color", "darkred");
      
      if (parseInt(b2[0].value) < idNumber && idNumber <= parseInt(e2[0].value)) 
        $(this).css("color", "darkred");
      
    });
    
    
    for (j = 0; j < inputElements.length; j++) {
      if (inputElements[j].type == 'checkbox') {
        checkboxes.push(parseInt(j));
      }
    }
    
    
    indexOfNONE = -1
    for(i=0; i<checkboxes.length; i++) { 
    	if (inputElements[checkboxes[i]].value == "NONE") {
    			indexOfNONE = i;
    	}
    }
   // alert(indexOfNONE);
    
    
    for(i=0; i<checkboxes.length; ++i) {
      index = checkboxes[i];
      inputElements[index].onclick=function() {
        inputOtherText[0].value = "N/A";
        
        if(this.checked) {
          if (this.value != "NONE") {
          	inputText[0].value += this.value + " ";
          	inputOtherText[0].value = "N/A";
          }
          
          if (this.value == "NONE") {
            inputOtherText[0].value = "";
            
            for(j = 0; j < checkboxes.length; ++j) {
            	if (j != indexOfNONE) inputElements[checkboxes[j]].checked = 0;
            }
            inputText[0].value = "NONE ";
            
            inputWordsText[0].readOnly = true;
            inputWordsText[0].value = "N/A";
            $('span').each(function(){
              if($(this ).css( "background-color","yellow" )){
                $(this ).css( "background-color","" );
                selectedIds.clear();
              }
            });
          }
          else {
            inputElements[checkboxes[indexOfNONE]].checked = 0;
            
            newValue = inputText[0].value.replace("NONE ", "");
            inputText[0].value = newValue;
            newValue = "";
            inputWordsText[0].value = "";
            
            if (inputText[0].value.indexOf("NONE") != -1) {
              inputWordsText[0].readOnly = true;
            }
            else {
              inputWordsText[0].readOnly = true; 
            }
          }
        }
        else {
          newValue = inputText[0].value.replace(this.value + " ", "");
          inputText[0].value = newValue;
          newValue = "";
          if (this.value == "NONE") {
            
            inputWordsText[0].readOnly = true;
            inputWordsText[0].value = "";
            
          }
          if (inputText[0].value == "") {
            
            inputWordsText[0].value = "";
            $('span').each(function(){
              if($(this ).css( "background-color","yellow" )){
                $(this ).css( "background-color","" );
                selectedIds.clear();
              }
            });
          }
        }
      };
    }
  });

  inputWordsText[0].onchange = function() {
    if (inputWordsText[0].value.trim().length == 0) {
      inputWordsText[0].value = "";
    }
  };
  
  inputOtherText[0].onchange = function() {
    if (inputOtherText[0].value.trim().length == 0) {
      inputOtherText[0].value = "";
    }
    if (inputOtherText[0].value == defaultMsg) {
      inputOtherText[0].value = "";
    }
  };
  
  sentence[0].onclick = function( event ) {
    index = event.target.parentNode.id;
    if (inputText[0].value.indexOf("NONE") == -1 && inputText[0].value.trim().length != 0) {
      if(event.target.nodeName == "SPAN") {
        if(inputWordsText[0].readOnly == true) {
          if (event.target.style.backgroundColor == "yellow") {
            selectedIds.remove(event.target.id);
            selection = updateHighlightedWords(selectedIds);
            inputWordsText[0].value = selection;
            event.target.style.backgroundColor = "white";
          }
          else {
            selectedIds.push(parseInt(event.target.id));
            event.target.style.backgroundColor = "yellow";
            selection = updateHighlightedWords(selectedIds);
            inputWordsText[0].value = selection;
          }
        }
      }
    }
  };
    
    function updateHighlightedWords(arrayId) {
      arrayId.sort(function(a, b) {
        if (isNaN(a) || isNaN(b)) {
          if (a > b) return 1;
          else return -1;
        }
        return a - b;
      });
      
      var selection2 = "";
      for (var i = 0; i < arrayId.length; i ++) {
        var num = parseInt(arrayId[i]);
        var n = num.toString();
        selection2 += document.getElementById(n).innerHTML + " ";
      }
      
      return selection2;
    }
  
});
