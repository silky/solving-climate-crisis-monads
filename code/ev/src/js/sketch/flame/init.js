const THREE = require('three');
const { debounce } = require('@ykob/js-util');
const { OrbitControls } = require('three/examples/jsm/controls/OrbitControls');
const FlameCore = require('./FlameCore').default;
const FlameCylinder = require('./FlameCylinder').default;
const BackgroundSphere = require('./BackgroundSphere').default;
const paho = require("paho-mqtt");

export default function() {
  const client = new paho.Client("localhost", 1884, "", "js");
  client.onMessageArrived = (msg) => {
    console.log("msg: ", msg.payloadString);
  };
  client.connect({
    onSuccess: () => {
      console.log("Connected.");
      client.subscribe("earth");
    }
  });

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
  const background = new BackgroundSphere();

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
    background.createObj();

    scene.add(flameCore.obj);
    scene.add(background.obj);

    renderer.setClearColor(0xffffff, 0);
    clock.start();

    on();
    resizeWindow();
    renderLoop();
  }
  init();
}
