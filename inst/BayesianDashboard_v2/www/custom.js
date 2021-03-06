var checkCookie = setInterval(
    function() {
        if (Shiny && document.cookie) {
          Shiny.onInputChange('cookie', document.cookie);
          //clearInterval(checkCookie);
        }
    },
    1000)

$(document).ready( function() {

  $('[data-toggle=collapse]').click( function() {

    var caret = $(this).find('i');
    caret.toggleClass('fa-caret-right');
    caret.toggleClass('fa-caret-down');

    Shiny.bindAll();
  });

  // If collapsible UI elements are initialized with class 'collapse', Shiny doesn't see
  // the inputs and won't bind to them. So we initialize them with class 'collapse-later',
  // let Shiny do its work, then add class 'collapse' and remove class 'collapse-later'.
  // Ugly, but this is web dev.
  setTimeout(
    function() {
        $('.collapse-later').addClass('collapse in').removeClass('collapse-later');
    },
    1000)

  Shiny.addCustomMessageHandler('confirm_upload', function(message) {
    $('#browse-button').text('');
    $('#file-box').attr('placeholder', message);
  });

  Shiny.addCustomMessageHandler('log', function(message) {
    console.log(message);
  })
  
  Shiny.addCustomMessageHandler('update-progress-bar', function(value) {
    $('#progress-bar')
    .css('width', value + '%')
    .attr('aria-valuenow', value)
    .text(value + '%');
  })
  
  $('[data-toggle="tooltip"]').tooltip();

}); //<-end document.ready

