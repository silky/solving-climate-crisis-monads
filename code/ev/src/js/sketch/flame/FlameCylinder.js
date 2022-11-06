const THREE = require('three');

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
    const geometry = new THREE.PlaneGeometry(1, 4000, 256, 256);

    const material = new THREE.RawShaderMaterial({
      uniforms: this.uniforms,
      vertexShader: require('./glsl/flameCylinder.vs').default,
      fragmentShader: require('./glsl/flameCylinder.fs').default,
      transparent: true,
      depthWrite: false,
      side: THREE.DoubleSide,
      blending: THREE.AdditiveBlending,
    });

    this.obj = new THREE.Mesh(geometry, material);
  }
  render(step) {

    // step = 0 - good
    // step = >0.3 start burning
    // step = 1 - very bad

    let t = step - 0.3;

    var opacityFactor = 1 * ( t / 0.7 );

    this.uniforms.opacityFactor.value = opacityFactor;
    this.uniforms.time.value += this.clock.getDelta();
  }
}
