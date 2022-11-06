const THREE = require('three');
const { MathEx } = require('@ykob/js-util');

export default class FlameCylinder {
  constructor() {
    this.uniforms = {
      opacityFactor: {
        type: 'f',
        value: 0.0
      },
      time: {
        type: 'f',
        value: 0
      },
    };
    this.clock = new THREE.Clock();
    this.clock.start();
  }
  createObj() {
    // Define Geometry
    const geometry = new THREE.PlaneGeometry(1, 4000, 256, 256);

    // Define Material
    const material = new THREE.RawShaderMaterial({
      uniforms: this.uniforms,
      vertexShader: require('./glsl/flameCylinder.vs').default,
      fragmentShader: require('./glsl/flameCylinder.fs').default,
      transparent: true,
      depthWrite: false,
      side: THREE.DoubleSide,
      blending: THREE.AdditiveBlending,
    });

    // Create Object3D
    this.obj = new THREE.Mesh(geometry, material);
  }
  render(props) {
    const step = props["step"];

    // step = 0 - good
    // step = >0.3 start burning
    // step = 1 - very bad


    // var elapsed = this.clock.getElapsedTime();
    // let t = Math.min(1, (elapsed - 3) / 10);
    let t = step - 0.3;

    // var opacityFactor = step > 3 ? 0.5 * t : 0.0;
    // var opacityFactor = time > 0.3 ? 0.5 * t : 0.0;
    var opacityFactor = 1 * ( t / 0.7 ); // time > 0.3 ? 0.5 * t : 0.0;

    this.uniforms.opacityFactor.value = opacityFactor;
    this.uniforms.time.value += this.clock.getDelta();
  }
}
