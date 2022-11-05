/* #extension GL_OES_standard_derivatives : enable */
/* precision highp float; */

/* varying vec3 vPosition; */
/* varying vec2 vUv; */

/* const vec3 light = vec3(0.0, 1.0, 0.0); */

/* #pragma glslify: convertHsvToRgb = require(@ykob/glsl-util/src/convertHsvToRgb); */

/* void main() { */
/*   // Flat Shading */
/*   vec3 normal = normalize(cross(dFdx(vPosition), dFdy(vPosition))); */
/*   float diff = (dot(normal, light) + 1.0) / 2.0; */

/*   vec3 hsv = vec3(0.88 + diff * 0.2, 1.0 - diff * 0.4, pow(diff, 3.0) * 0.88 + 0.12); */
/*   vec3 rgb = convertHsvToRgb(hsv); */

/*   gl_FragColor = vec4(rgb, 1.0); */
/* } */




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

    float x1 = ((1.0 - Ca.r) + (1.0 - Ca.g) + (1.0 - Ca.b));
    float y1 = 0.0;
    float z1 = 0.0;

    float x2 = Cb.r;
    float y2 = Cb.g;
    float z2 = Cb.b;

    float x = t * x1 + (1.0 - t) * x2;
    float y = t * y1 + (1.0 - t) * y2;
    float z = t * z1 + (1.0 - t) * z2;

    c = vec3( x, y, z );
    gl_FragColor= vec4(c, 1.0);
}
