const THREE = require('three');
const { debounce } = require('@ykob/js-util');
const { OrbitControls } = require('three/examples/jsm/controls/OrbitControls');
const FlameCore = require('./FlameCore').default;
const FlameCylinder = require('./FlameCylinder').default;
const BackgroundSphere = require('./BackgroundSphere').default;
const paho = require("paho-mqtt");

export default function() {
  var step = 0;

  const resolution = new THREE.Vector2();
  const mousemove = new THREE.Vector2();
  const canvas = document.getElementById('canvas-webgl');
  const renderer = new THREE.WebGL1Renderer({
    alpha: true,
    antialias: true,
    canvas: canvas,
  });

  var flameAdded = false;

  const props = {}
  const scene = new THREE.Scene();
  const camera = new THREE.PerspectiveCamera
    ( 50
    , document.body.clientWidth / document.body.innerHeight
    , 0.1
    , 50000
    );
  const controls = new OrbitControls(camera, renderer.domElement);
  const flameCore = new FlameCore();
  const flameCylinder = new FlameCylinder();
  const background = new BackgroundSphere();

  const render = () => {
    flameCore.render(props);

    if( !flameAdded && props["step"] > 0.3 ) {
      scene.add(flameCylinder.obj);
      flameAdded = true;
    }

    if( flameAdded && props["step"] <= 0.3 ) {
      scene.remove(flameCylinder.obj);
      flameAdded = false;
    }

    if (flameAdded) {
      flameCylinder.render(props);
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

  const init = () => {
    const client = new paho.Client("localhost", 1884, "", "js");

    client.onMessageArrived = (msg) => {
      let str = msg.payloadString;
      let v   = parseFloat(str);
      props["step"] = v ? v : 0.0;
      console.log("props", props);
    };

    client.connect({
      onSuccess: () => {
        console.log("Connected.");
        client.subscribe("earth");
      }
    });


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

    on();
    resizeWindow();
    renderLoop();
  }
  init();
}
