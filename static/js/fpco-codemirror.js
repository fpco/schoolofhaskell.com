window.fpco = window.fpco || {};

// TODO: Create a proper codemirror module
fpco.createCodeMirror = function (options) {
    var settings = { };
    if (window.fpco.keyMap) {
        settings.keyMap = window.fpco.keyMap;
    }
    settings.readOnly = options.cmsReadOnly;
    settings.viewportMargin = options.cmsInfiniteViewport ? 1 / 0 : 10;
    settings.lineWrapping = options.cmsWrapping;
    // These are all configured by the clientside user configuration.
    settings.indentUnit = options.cmsIndentUnit;
    if (options.cmsMatchBrackets == true) {
      settings.matchBrackets = "()[]{}";
    }
    /* FIXME: allow customization of bracket matching
    if (options.cmsMatchBrackets.instance == "Just") {
      settings.matchBrackets = options.cmsMatchBrackets.slot1;
    }
    */
    if (options.cmsCloseBrackets.instance == "Just") {
      settings.autoCloseBrackets = options.cmsCloseBrackets.slot1;
    } else {
      settings.autoCloseBrackets = "";
    }
    settings.lineNumbers = options.cmsLineNumbers;
    settings.showTrailingSpace = options.cmsShowTrailingSpace;
    settings.styleActiveLine = options.cmsHighlightLine;
    settings.showAlignments = options.cmsShowAlignments;

    // Only show cursor when selecting if codemirror's vim keymap is used.
    if (settings.keyMap == "vim") {
      settings.showCursorWhenSelecting = true;
    }

    // Add support for drag dropping images
    // Prevent code mirror from handling the drop default event
    var $doc = $(options.cmsElement);
    $doc.bind('drop', function(e) {e.preventDefault(); return false;});
    // Handle the event ourselves
    if(options.cmsDragDrop && !options.cmsReadOnly) {
      settings.dragDrop = true;
      settings.onDragEvent = function(cm,e){
        e.preventDefault();
        if(e.dataTransfer.files.length){
          var file = e.dataTransfer.files[0];
          fpco.uploadImageFile(cm,file,options.cmsSilent);
        }
        return true;
      };
    }

    // Create the editor
    var cm = CodeMirror(options.cmsElement, settings);

    cm.setOption("mode", { name: options.cmsMode, fencedCodeBlocks: options.cmsFencedCodeBlocks });
    cm.setOption("tabSize", options.cmsIndentUnit);
    if (options.cmsMatchSelection) {
      cm.setOption("highlightSelectionMatches", {
        minChars: 1,
        tokens:
          [{ regex: /^\w+$/, boundary: /\W/ }]
        });
    }

    // Handle image paste events
    if(options.cmsDropDrop) {
      $(cm.getWrapperElement()).bind('paste', function(e) {
        if(e.type.indexOf('paste') === 0) {
          var files = e.originalEvent.clipboardData.items;
          if(files.length > 0) return !(fpco.uploadImageFile(cm, files[0].getAsFile()));
        }
        return false;
      });
    }

    if (settings.keyMap == "vim") {
      var vimMode = null;
      cm.on('vim-mode-change', function(e) {
        vimMode = e.mode;
      });
      // FIXME: this ought to not be necessary.
      cm.on('cursorActivity', function(e) {
        if (vimMode != "insert" && vimMode != "visual") {
          var from = e.getCursor("start");
          var to = e.getCursor("end");
          if (from.line == to.line && from.ch == to.ch) {
            var line = cm.getLine(to.line);
            if (line && line.length == to.ch) {
              cm.setCursor({ line: to.line, ch: Math.max(0, to.ch) - 1 });
            }
          }
        }
      });
    }

    // Add a class to the element when the cursor is a selection.
    cm.on('cursorActivity', function(e) {
      var from = e.getCursor("start");
      var to = e.getCursor("end");
      var elem = cm.getWrapperElement()
      if (from.line == to.line && from.ch == to.ch) {
        $(elem).removeClass("CodeMirror-selecting");
      } else {
        $(elem).addClass("CodeMirror-selecting");
      }
    });

    // Add the codemirror instance to the dom for later access
    if(!$doc.data('hascodemirror')) {
      // This needs to be an attr to be accesible from css selectors later
      $doc.attr('data-hascodemirror', 'true');
      $doc.data('codemirrorinstance', []);
    }
    $doc.data('codemirrorinstance').push(cm);
    return cm;
};

// Function to upload a file to the server and show it in markdown
fpco.uploadImageFile = function(cm, file, silent) {
  if(!file) return false;
  var content = cm.getValue();
  var selection = cm.getCursor();
  cm.replaceRange('![Uploading ' + file.name + '. . .]()', selection);

  function resetEditor(s) {
    cm.setValue(content);
    cm.setCursor(selection);
    $.growlUI(s);
  }

  // upload the file
  var fileReader = new FileReader();
  fileReader.onerror = function(e) {
    switch(e.target.error.code) {
      case e.target.error.NOT_FOUND_ERR:
        resetEditor("Image file not found");
        return false;
      case e.target.error.NOT_READABLE_ERR:
        resetEditor("Image file not readable");
        return false;
      case e.target.error.ABORT_ERR:
        resetEditor("Image file upload aborted");
        return false;
      default:
        resetEditor("Unknown image upload error occured");
        return false;
    };
  };
  fileReader.onload = function() {
    // Construct the form data
    var boundary = '--------FormDataD' + Math.random().toString(36).substr(7);
    var data = '--' + boundary + "\r\n" +
      'Content-Disposition: form-data; name="file"; filename="' + file.name + "\"\r\n" +
      "Content-Type: " + file.type + "\r\n" +
      "\r\n" +
      this.result + "\r\n" +
      '--' + boundary + '--';
    // Make the call
    $.ajax({
      url: '/media?encode=base64',
      data: data,
      cache: false,
      contentType:'multipart/form-data; boundary=' + boundary,
      processData: false,
      type: 'POST',
      success: function(data) {
        if (data.result === "success") {
          // Done
          cm.setValue(content);
          cm.replaceRange('![' + file.name + ']('+data.url+')', selection);
          cm.setCursor(selection);
          if(!silent) $.growlUI("Uploaded image");
        }
        else {
          resetEditor("Image upload failed: " + data.message);
        }
      },
      error: function(xhr, s) {
        if (xhr.status === 413) {
            resetEditor("The image you tried to upload was too large.");
        }
        else {
            resetEditor("Error: Image upload failed. The server said: " + s);
            console.log(JSON.stringify(xhr));
        }
      }
    });
  };
  fileReader.readAsDataURL(file);
  return true;
};
