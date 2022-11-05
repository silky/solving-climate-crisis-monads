const THREE = require('three');
const { MathEx } = require('@ykob/js-util');

export default class FlameCore {
  constructor() {
    this.uniforms = {
      tOne: {
        type: "t",
        value: new THREE.TextureLoader().load("../img/common/earth-warming-2.png")
      },
      tSec: {
        type: "t",
        value: new THREE.TextureLoader().load("../img/common/earth-hd.jpg")
      },
      t: {
        type: "f",
        value: 0
      }
    };
    this.clock = new THREE.Clock();
  }

  createObj() {
    const geometry = new THREE.SphereGeometry(450, 128, 128);

    const material = new THREE.RawShaderMaterial({
      uniforms: this.uniforms,
      vertexShader: require("./glsl/flameCore.vs").default,
      fragmentShader: require("./glsl/flameCore.fs").default,
    });

    this.obj = new THREE.Mesh(geometry, material);
    this.clock.start();
  }

  render(time) {
    const t = Math.min(1, this.clock.getElapsedTime() / 10);
    this.uniforms.t.value = t;
    this.obj.rotation.y += 0.002;
  }
}
