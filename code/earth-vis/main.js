import * as THREE from "https://cdn.skypack.dev/three@0.129.0";
import { OrbitControls } from "https://cdn.skypack.dev/three@0.129.0/examples/jsm/controls/OrbitControls.js";

let scene, camera, renderer, controls, skybox;
let earth, datas;
let material_shh;

function createPathStrings(filename) {

  const basePath = "assets/skybox/";
  const baseFilename = basePath + filename;
  const fileType = ".png";
  const sides = ["ft", "bk", "up", "dn", "rt", "lf"];
  const pathStrings = sides.map((side) => {
    return baseFilename + "_" + side + fileType;
  });
  return pathStrings;
}

function createMaterialArray (filename) {
  const skyboxImagepaths = createPathStrings(filename);
  const materialArray = skyboxImagepaths.map((image) => {
    let texture = new THREE.TextureLoader().load(image);
    return new THREE.MeshBasicMaterial({ map: texture, side: THREE.BackSide });
  });
  return materialArray;
}

function setSkyBox () {
  const material = createMaterialArray("space");
  const geometry = new THREE.BoxGeometry(200, 200, 200);

  skybox = new THREE.Mesh(geometry, material);
  scene.add(skybox);
}

function setEarthBox () {

  var vertShader = `
varying vec2 vUv;

void main()
{
    vUv = uv;
    vec4 mvPosition = modelViewMatrix * vec4( position, 1.0 );
    gl_Position = projectionMatrix * mvPosition;
}
  `;
  var fragShader = `
#ifdef GL_ES
precision highp float;
#endif

uniform sampler2D tOne;
uniform sampler2D tSec;
uniform float t;

varying vec2 vUv;

void main(void)
{
    vec3 c;
    vec4 Ca = texture2D(tOne, vUv);
    vec4 Cb = texture2D(tSec, vUv);

    // We want everything to become red. How red? Well, it's defined by
    // Ca.rgb.

    // float x1 = ((1.0 - Ca.b) + (1.0 - Ca.g)) / 2.0;
    // float x2 = Cb.r;
    // float x = t * x1   + (1.0 - t) * x2;
    // float y = t * Ca.g + (1.0 - t) * Cb.g;
    // float z = t * Ca.b + (1.0 - t) * Cb.b;

    float x1 = ((1.0 - Ca.r) + (1.0 - Ca.g) + (1.0 - Ca.b));
    float y1 = 0.0;
    float z1 = 0.0;

    float x2 = Cb.r;
    float y2 = Cb.g;
    float z2 = Cb.b;

    float x = t * x1 + (1.0 - t) * x2;
    float y = t * y1 + (1.0 - t) * y2;
    float z = t * z1 + (1.0 - t) * z2;

    // float x = 1.0 - Ca.r;
    // float y = 1.0 - Ca.g;
    // float z = 1.0 - Ca.b;

    // float x = Cb.r;
    // float y = Cb.g;
    // float z = Cb.b;

    c = vec3( x, y, z );
    gl_FragColor= vec4(c, 1.0);
}
  `;

  var uniforms = {    // custom uniforms (your textures)
    tOne: { type: "t", value: new THREE.TextureLoader().load( "assets/earth-warming-2.png" ) },
    tSec: { type: "t", value: new THREE.TextureLoader().load( "assets/earth-hd.jpg" ) }
  };

  uniforms["time"] = { value: 0 };

  var bumpMap  = new THREE.TextureLoader().load("assets/earth-bump.jpeg");
  var specMap  = new THREE.TextureLoader().load("assets/earth-spec.jpeg");

  material_shh = new THREE.ShaderMaterial({
    uniforms: uniforms,
    vertexShader: vertShader,
    fragmentShader: fragShader
  });

  var geometry = new THREE.SphereGeometry(5, 64, 64);
  // var texture  = new THREE.TextureLoader().load("assets/earth-hd.png");
  // var texture  = new THREE.TextureLoader().load("assets/earth-warming-2.png");

  // var material = new THREE.MeshBasicMaterial({
  //   shininess: 40,
  //   bumpScale: 1,
  //   map: texture,
  //   bumpMap: bumpMap,
  //   specularMap: specMap
  // });

  earth = new THREE.Mesh(  geometry, material_shh );

  // earth = new THREE.Mesh(geometry, material);
  scene.add(earth);
}

function init () {
  scene = new THREE.Scene();
  camera = new THREE.PerspectiveCamera
    ( 50
    , window.innerWidth / window.innerHeight
    , 0.1
    , 1000
    );

  setSkyBox();
  setEarthBox();

  renderer = new THREE.WebGLRenderer({ antialias: true, alpha: true });
  renderer.setClearColor(0xffffff, 0);
  renderer.setSize(window.innerWidth - 100, window.innerHeight - 100);
  var container = document.getElementById("container");
  container.appendChild(renderer.domElement);
  renderer.domElement.id = "c";

  controls = new OrbitControls(camera, renderer.domElement);
  controls.enableDamping = true;
  controls.minDistance = 12;
  controls.maxDistance = 30;
  controls.minZoom = 30

  camera.position.z = 20;
}

function animate () {
  requestAnimationFrame(animate);

  let now = Date.now();
  var seconds = (now - start) / 1000;
  var m = Math.min(1, seconds / 10);

	material_shh.uniforms["t"] = { value: m };

  earth.rotation.y += 0.002;
  controls.update();
  renderer.render(scene, camera);
}

function onWindowResize () {
  camera.aspect = window.innerWidth / window.innerHeight;
  camera.updateProjectionMatrix();
  renderer.setSize(window.innerWidth - 100, window.innerHeight - 100);
}

window.addEventListener("resize", onWindowResize, false);


let start;

start = Date.now();

init();
animate();
