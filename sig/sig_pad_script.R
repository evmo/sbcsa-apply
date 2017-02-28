sig_pad <- "shinyjs.init = function() {

var signaturePad = new SignaturePad(document.getElementById('signature-pad'), {
  backgroundColor: 'rgba(255, 255, 255, 0)',
  penColor: 'rgb(0, 0, 0)'
});

var saveButton = document.getElementById('save');
var cancelButton = document.getElementById('clear');

saveButton.addEventListener('click', function (event) {
  var sig_data = signaturePad.toDataURL('image/png');
  document.getElementById('sig_img').src = sig_data;
});

cancelButton.addEventListener('click', function (event) {
  signaturePad.clear();
});

}"

# signature-drawing widget
hidden(div(id = "draw_sig",
HTML("<canvas id='signature-pad' class='signature-pad' width=600 height=400></canvas>"),
HTML("<div>
     <button id='save'>Save</button>
     <button id='clear'>Clear</button>
     </div>")
))

shinyjs::extendShinyjs(text = sig_pad),