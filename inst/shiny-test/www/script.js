// Elements for taking the snapshot
var canvas = document.getElementById('canvas');
var context = canvas.getContext('2d');
var canvasGray = document.getElementById('canvasGray');
var contextGray = canvasGray.getContext('2d');
var video = document.getElementById('video');

// Get access to the camera!
if(navigator.mediaDevices && navigator.mediaDevices.getUserMedia) {
    // Not adding `{ audio: true }` since we only want video now
    navigator.mediaDevices.getUserMedia({ video: true }).then(function(stream) {
        video.src = window.URL.createObjectURL(stream);
        video.play();
    });
}

setInterval(function(){
  context.drawImage(video, -52, -26, 200, 150);

  var imageData = context.getImageData(0, 0, 96, 96);
  var data = imageData.data;

  // convert to grayscale
  for(var i = 0; i < data.length; i += 4) {
    var brightness = 0.34 * data[i] + 0.5 * data[i + 1] + 0.16 * data[i + 2];
    // red
    data[i] = brightness;
    // green
    data[i + 1] = brightness;
    // blue
    data[i + 2] = brightness;
  }


  // overwrite original image
  contextGray.putImageData(imageData, 0, 0);

  // trigger
  Shiny.onInputChange("img", contextGray.getImageData(0, 0, 96, 96).data.join());
}, 300);
