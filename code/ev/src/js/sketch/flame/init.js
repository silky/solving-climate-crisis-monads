const THREE = require('three');
const { debounce } = require('@ykob/js-util');
const { OrbitControls } = require('three/examples/jsm/controls/OrbitControls');
const FlameCore = require('./FlameCore').default;
const FlameCylinder = require('./FlameCylinder').default;
const BackgroundSphere = require('./BackgroundSphere').default;
const paho = require("paho-mqtt");
const { GUI } = require('lil-gui');

export default function() {
  var step = 0;

  const props =
    { "Factor": 0.0
    , "MQTT": false
    }
  const propHandles = {};

  const gui = new GUI( { width: 350 });

  const admin = gui.addFolder("~ Admin ~");
  const factorThing = admin.add( props, "Factor", 0, 1, 0.01 );
  const mqttThing   = admin.add( props, "MQTT" );
  mqttThing.disable(true);
  admin.close();

  const earth  = gui.addFolder("~ Earth ~");

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
    // TODO: Rename `step` to `factor`;
    const step = props["Factor"];
    flameCore.render(step);

    if( !flameAdded && step > 0.3 ) {
      scene.add(flameCylinder.obj);
      flameAdded = true;
    }

    if( flameAdded && step <= 0.3 ) {
      scene.remove(flameCylinder.obj);
      flameAdded = false;
    }

    if (flameAdded) {
      flameCylinder.render(step);
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
    const client = new paho.Client("localhost", 1884, "", "js" + Math.random());

    client.onMessageArrived = (msg) => {
      let str = msg.payloadString;
      let obj = JSON.parse(str);

      let v = obj["factor"];

      var keys = Object.keys( obj["blob"] ).sort();

      keys.forEach( key => {
        let value = obj["blob"][key];

        props[key] = value;

        if( !(key in propHandles) ) {
          // TODO: User-definable max.
          propHandles[key] = earth.add(props, key, 0, 100, 1 );
        }

        propHandles[key].setValue(value);
      });

      let step = obj["step"];

      props["Factor"] = v ? v : 0.0;
      factorThing.setValue(v);
    };

    client.connect({
      onSuccess: () => {
        mqttThing.setValue(true);
        client.subscribe("earth");
      }
    });


    controls.enableDamping = true;
    controls.minDistance = 700;
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
