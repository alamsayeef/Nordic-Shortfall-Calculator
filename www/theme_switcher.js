Shiny.addCustomMessageHandler('toggleTheme', function(message) {
  var currentTheme = $('body').attr('class');
  if (currentTheme.includes('dark')) {
    $('body').removeClass('dark');
    $('nav').removeClass('navbar-dark bg-dark').addClass('navbar-light bg-light');
  } else {
    $('body').addClass('dark');
    $('nav').removeClass('navbar-light bg-light').addClass('navbar-dark bg-dark');
  }
});
