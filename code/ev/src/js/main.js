const page   = document.querySelector('.l-page');
const pageId = page.dataset.id;

// running each init functions.
if (pageId == 'index') {
  // require('./index/init.js').default();
} else {
  const canvas = document.getElementById('canvas-webgl');
  switch (pageId) {
    case 'flame':
      require('./sketch/flame/init.js').default();
    default:
      break;
  }
}
