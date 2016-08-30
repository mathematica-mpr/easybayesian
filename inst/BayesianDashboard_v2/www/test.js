function test() {
  console.log('document.ready');
  Shiny.onInputChange('test', 'this is a test');
}

setTimeout(test, 10000)
