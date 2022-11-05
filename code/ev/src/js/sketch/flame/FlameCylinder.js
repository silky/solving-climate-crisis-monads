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
  render(time) {
    var elapsed = this.clock.getElapsedTime();
    let t = Math.min(1, (elapsed - 3) / 10);

    var opacityFactor = elapsed > 3 ? 0.5 * t : 0.0;

    this.uniforms.opacityFactor.value = opacityFactor;
    this.uniforms.time.value += time;
  }
}
