#version 330

uniform sampler2D uFont, uFg, uBg;
uniform usampler2D uGlyphs;
uniform vec2 fontSize, termDimensions, fontTextureDimensions, glyphTextureDimensions;

in vec2 vTextureCoord;
out vec4 outcolor;

void main(void) {
  // width and height of the screen in pixels
  float screenWidth = fontSize.x * termDimensions.x;
  float screenHeight = fontSize.y * termDimensions.y;
  // termDimensions in cols and rows
  // (termCol, termRow) = (col, row) the fragment belongs to in the terminal
  ivec2 termXY = ivec2(floor(vTextureCoord.x * termDimensions.x),
                       floor(vTextureCoord.y * termDimensions.y));
  ivec2 charSize = ivec2(fontSize.x, fontSize.y);
  // row,column -> glyph texture uvs
  //float termu = (termCol / glyphTextureDimensions.x);
  //float termv = (termRow / glyphTextureDimensions.y);
  // look up in uGlphs the index in the font texture to use for the fragment
  // fontIndex.x = font index 
  uvec2 fontIndex = texelFetch(uGlyphs, termXY, 0).xy;
  ivec2 fontXY = ivec2(int(fontIndex.x) * charSize.x, int(fontIndex.y) * charSize.y);
  // That green and red channel mingling? Yeah that's because some
  // mobile devices have less than 8 bits per channel
  //float gp = floor(glyph.r * 256.0 + floor((glyph.g * 16.0) * 256.0)) / 16.0;
  //float gy = 0.9375 - (floor(gp) / 16.0);
  //float gx = gp - floor(gp);
  // calc the position of the fragment relative to the terminal cell
  ivec2 charXY = ivec2(fract(vTextureCoord.x * termDimensions.x) * charSize.x,
                       (-fract(vTextureCoord.y * termDimensions.y) + 1) * charSize.y);
  //float fontu = ((fontIndex.x * fontTextureDimensions.x) + charu) * fontSize.x / fontTextureDimensions.x;
  //float fontv = ((fontIndex.y * fontTextureDimensions.y - charv + 1) * fontSize.y)/ fontTextureDimensions.y;
  vec4 fnt = texelFetch(uFont, fontXY + charXY, 0);

  vec4 fg  = texelFetch(uFg, termXY, 0);
  vec4 bg  = texelFetch(uBg, termXY, 0);

  outcolor = mix(bg, fg, fnt.r);

  //vec4 fg  = texture2D(uFg, vec2(termu, termv));
  //vec4 bg  = texture2D(uBg, vec2(termu, termv));
  //outcolor = vec4(charXY.x, charXY.y, 0, 0);

  //outcolor = vec4(fontIndex.x, fontIndex.y, 0, 0) / 5120000000.0;
  //outcolor = (vec4(fontIndex.x, fontIndex.y, 0, 0) + vec4(charXY.x, charXY.y, 0, 0)) / 512.0;
  //outcolor = fg;
  //outcolor = bg;

  //gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);
  // Final color

}