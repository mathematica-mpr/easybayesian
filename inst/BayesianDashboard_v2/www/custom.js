$(document).ready( function() {

  $('[data-toggle=collapse]').click( function() {

    var caret = $(this).find('i');
    caret.toggleClass('fa-caret-right');
    caret.toggleClass('fa-caret-down');
  });

  // If collapsible UI elements are initialized with class 'collapse', Shiny doesn't see
  // the inputs and won't bind to them. So we initialize them with class 'collapse-later',
  // let Shiny do its work, then add class 'collapse' and remove class 'collapse-later'.
  // Ugly, but this is web dev.
  setTimeout(
    function() {
        $('.collapse-later').addClass('collapse').removeClass('collapse-later');
    },
    1000)

}); //<-end document.ready

