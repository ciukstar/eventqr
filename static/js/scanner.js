

import QrScanner from 'https://cdn.jsdelivr.net/npm/qr-scanner@1.4.2/qr-scanner.min.js';

const scanner = new QrScanner(
  document.getElementById('scannervideo'), 
  function (result) {

    if (!isNaN(result.data)) {
      scanner.stop();
      document.body.dispatchEvent(
        new CustomEvent('scanqrcode', {
          bubbles: false,
          detail: Number(result.data)
        })
      );
    }
    
  }, {
    highlightScanRegion: true,
    highlightCodeOutline: true
  }
);

scanner.start();
