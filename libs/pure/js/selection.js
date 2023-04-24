function getSelection(input) {
  if ("selectionStart" in input && document.activeElement == input) {
      return {
          start: input.selectionStart,
          end: input.selectionEnd
      };
  }
  else if (input.createTextRange) {
      var sel = document.selection.createRange();
      if (sel.parentElement() === input) {
          var rng = input.createTextRange();
          rng.moveToBookmark(sel.getBookmark());
          for (var len = 0;
                   rng.compareEndPoints("EndToStart", rng) > 0;
                   rng.moveEnd("character", -1)) {
              len++;
          }
          rng.setEndPoint("StartToStart", input.createTextRange());
          for (var pos = { start: 0, end: len };
                   rng.compareEndPoints("EndToStart", rng) > 0;
                   rng.moveEnd("character", -1)) {
              pos.start++;
              pos.end++;
          }
          return pos;
      }
  }
  return {start: 0, end: 0};
}

function replaceSelection(replacementText) {
  var sel, range;
  if (window.getSelection) {
      sel = window.getSelection();
      if (sel.rangeCount) {
          rng = sel.getRangeAt(0);
          rng.deleteContents();
          rng.insertNode(document.createTextNode(replacementText));
      }
  } else if (document.selection && document.selection.createRange) {
      rng = document.selection.createRange();
      rng.text = replacementText;
  }
}

function replaceTextInRange(input,start,end,replacementText) {
  setSelection(input,start,end);
  replaceSelection(replacementText);
}

function setSelection(input, start, end) {
  if (arguments.length < 3) end = start;
  if ("selectionStart" in input) {
      setTimeout(function() {
          input.selectionStart = start;
          input.selectionEnd = end;
      }, 1);
  }
  else if (input.createTextRange) {
      var rng = input.createTextRange();
      rng.moveStart("character", start);
      rng.collapse();
      rng.moveEnd("character", end - start);
      rng.select();
  }
}
