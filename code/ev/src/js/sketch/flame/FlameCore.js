const THREE = require('three');

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
  }

  createObj() {
    const geometry = new THREE.SphereGeometry(450, 128, 128);

    const material = new THREE.RawShaderMaterial({
      uniforms: this.uniforms,
      vertexShader: require("./glsl/flameCore.vs").default,
      fragmentShader: require("./glsl/flameCore.fs").default,
    });

    this.obj = new THREE.Mesh(geometry, material);
  }

  render(step) {
    this.uniforms.t.value = step;
    this.obj.rotation.y += 0.002;
  }
}
