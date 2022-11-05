const THREE = require('three');
const { MathEx } = require('@ykob/js-util');

export default class backgroundSphere {
  constructor() {
  }

  createPathStrings (filename) {
    const basePath = "../img/common/skybox/";
    const baseFilename = basePath + filename;
    const fileType = ".png";
    const sides = ["ft", "bk", "up", "dn", "rt", "lf"];
    const pathStrings = sides.map((side) => {
      return baseFilename + "_" + side + fileType;
    });
    return pathStrings;
  }

  createMaterialArray (filename) {
    const skyboxImagepaths = this.createPathStrings(filename);
    const materialArray = skyboxImagepaths.map((image) => {
      let texture = new THREE.TextureLoader().load(image);
      return new THREE.MeshBasicMaterial({ map: texture, side: THREE.BackSide });
    });
    return materialArray;
  }

  createObj() {
    const material = this.createMaterialArray("space");
    const geometry = new THREE.BoxGeometry(5000, 5000, 5000);

    this.obj = new THREE.Mesh(geometry, material);
  }

  render(time) {
  }
}
