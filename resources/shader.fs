precision mediump float;
uniform sampler2D uFont, uGlyphs, uFg, uBg;
uniform vec2 fontSize, termDimensions, fontTextureDimensions, glyphTextureDimensions;

varying vec2 vTextureCoord;

void main(void) {
  // width and height of the screen in pixels
  float screenWidth = fontSize.x * termDimensions.x;
  float screenHeight = fontSize.y * termDimensions.y;
  // termDimensions in cols and rows
  // (termCol, termRow) = (col, row) the fragment belongs to in the terminal
  float termCol = floor(vTextureCoord.x * termDimensions.x);
  float termRow = floor(vTextureCoord.y * termDimensions.y);
  // row,column -> glyph texture uvs
  float termu = (termCol / glyphTextureDimensions.x);
  float termv = (termRow / glyphTextureDimensions.y);
  // look up in uGlphs the index in the font texture to use for the fragment
  // fontIndex.x = font index 
  vec2 fontIndex = texture2D(uGlyphs, vec2(termu, termv)).xy;
  // That green and red channel mingling? Yeah that's because some
  // mobile devices have less than 8 bits per channel
  //float gp = floor(glyph.r * 256.0 + floor((glyph.g * 16.0) * 256.0)) / 16.0;
  //float gy = 0.9375 - (floor(gp) / 16.0);
  //float gx = gp - floor(gp);
  // calc the position of the fragment relative to the terminal cell
  float charu = fract(vTextureCoord.x * termDimensions.x);
  float charv = fract(vTextureCoord.y * termDimensions.y);
  float fontu = ((fontIndex.x * fontTextureDimensions.x) + charu) * fontSize.x / fontTextureDimensions.x;
  float fontv = ((fontIndex.y * fontTextureDimensions.y - charv + 1) * fontSize.y)/ fontTextureDimensions.y;
  vec4 fnt = texture2D(uFont, vec2(fontu, fontv));

  vec4 fg  = texture2D(uFg, vec2(termu, termv));
  vec4 bg  = texture2D(uBg, vec2(termu, termv));
  //vec4 fg  = texture2D(uFg, vec2(termu, termv));
  //vec4 bg  = texture2D(uBg, vec2(termu, termv));
  //gl_FragColor = vec4(vTextureCoord.s, vTextureCoord.t, 1.0, 1.0);
  //gl_FragColor = vec4(termCol/100, termRow/100, 0, 0);
  //gl_FragColor = vec4(termu, termv, 0, 0);

  //gl_FragColor = vec4(fontIndex.x, fontIndex.y, 0, 1.0) * 10;
  //gl_FragColor = vec4(charu, charv, 0, 0);
  //gl_FragColor = vec4(fontu, fontv, 0, 0) * 100;
  //gl_FragColor = fnt;
  //gl_FragColor = fg;

  //gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);
  // Final color
  gl_FragColor = mix(bg, fg, fnt.r);

}