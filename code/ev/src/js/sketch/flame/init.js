const THREE = require('three');
const { debounce } = require('@ykob/js-util');
const { MathEx } = require('@ykob/js-util');
const { OrbitControls } = require('three/examples/jsm/controls/OrbitControls');
const FlameCore = require('./FlameCore').default;
const FlameCylinder = require('./FlameCylinder').default;

export default function() {
  // ==========
  // Define common variables
  //
  const resolution = new THREE.Vector2();
  const mousemove = new THREE.Vector2();
  const canvas = document.getElementById('canvas-webgl');
  const renderer = new THREE.WebGL1Renderer({
    alpha: true,
    antialias: true,
    canvas: canvas,
  });

  var flameAdded = false;

  const scene = new THREE.Scene();
  const clock = new THREE.Clock();
  const camera = new THREE.PerspectiveCamera
    ( 50
    , document.body.clientWidth / document.body.innerHeight
    , 0.1
    , 50000
    );
  const controls = new OrbitControls(camera, renderer.domElement);

  // ==========
  // Define unique variables
  //

  const flameCore = new FlameCore();
  const flameCylinder = new FlameCylinder();

  // ==========
  // Define functions
  //
  const render = () => {
    const time = clock.getDelta();
    flameCore.render(time);

    const elapsed = clock.getElapsedTime();

    if( !flameAdded && elapsed > 0 ) {
      scene.add(flameCylinder.obj);
      flameAdded = true;
    }

    if (flameAdded) {
      flameCylinder.render(time);
    }

    controls.update();
    renderer.render(scene, camera);
  };
  const renderLoop = () => {
    render();
    requestAnimationFrame(renderLoop);
  };
  const resizeCamera = () => {
    camera.aspect = resolution.x / resolution.y;
    camera.updateProjectionMatrix();
  };
  const resizeWindow = () => {
    resolution.set(document.body.clientWidth, window.innerHeight);
    canvas.width = resolution.x;
    canvas.height = resolution.y;
    resizeCamera();
    renderer.setSize(resolution.x, resolution.y);
  };
  const on = () => {
    window.addEventListener('resize', debounce(resizeWindow, 1000));
  };

  // ==========
  // Initialize
  //
  const init = () => {
    controls.enableDamping = true;
    controls.minDistance = 1;
    controls.maxDistance = 3000;
    controls.minZoom = 30;

    camera.position.z = 2000;

    flameCore.createObj();
    flameCylinder.createObj();

    scene.add(flameCore.obj);

    renderer.setClearColor(0xffffff, 0);
    clock.start();

    on();
    resizeWindow();
    renderLoop();
  }
  init();
}
