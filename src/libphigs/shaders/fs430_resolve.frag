#version 430 compatibility
/*
 * Order independent rendering, pass 2 of 2: resolve.
 *
 * One invocation per pixel, run after all geometry has been rasterised by
 * pass 1. It walks the linked list of transparent fragments belonging to the
 * pixel, sorts them back to front and composites them into a single colour.
 *
 * Only transparent fragments are in the list. Opaque geometry was written
 * straight to the framebuffer by pass 1, so the result of this pass is
 * blended over it, which is what lets an opaque track stay visible behind a
 * transparent surface.
 *
 * The bindings have to match the ones wsgl_oir_reset() sets up, and the ones
 * fs430.frag appends through.
 *
 * The head pointer is a shader storage buffer of one uint per pixel, indexed
 * as y * oirWidth + x, rather than a uimage2D -- see the matching comment in
 * fs430.frag for why.
 */
layout (std430, binding = 0)   readonly buffer HeadPointers { uint head_pointers[]; };
layout (binding = 1, rgba32ui) coherent uniform uimageBuffer list_buffer;
uniform uint list_capacity;
uniform uint oirWidth;

#define MAX_FRAGMENTS 16
#define LIST_END 0xFFFFFFFFu
/*
  Hard cap on how many links of a pixel's chain we are willing to follow.
  The chain is built by concurrent appends, so a torn entry can leave a bogus
  next index and, in the worst case, a cycle. The walk below no longer stops
  at MAX_FRAGMENTS, so without this bound a corrupt list would hang the
  shader, and with it the display.
*/
#define MAX_WALK 256
#define DEPTH_EPS 1e-6

/* Define the mode in which the final color is calculated */
uniform int oirMode;

uvec4 fragments[MAX_FRAGMENTS];

/* returns true if a is farther (less important to keep) than b under the
   same (depth, draw-order) ordering used by sortFragments() */
bool isFarther(uvec4 a, uvec4 b){
  float da = uintBitsToFloat(a.z);
  float db = uintBitsToFloat(b.z);
  if (abs(da - db) < DEPTH_EPS)
    return a.w < b.w;         /* tied: earlier draw is "farther" (less kept) */
  return da > db;             /* larger depth = farther */
}

/*
 * createFragmentList: collect the fragments of this pixel, head first.
 *
 * The whole chain is walked, but only MAX_FRAGMENTS entries are kept, and the
 * ones kept are the NEAREST rather than the most recently appended. Keeping
 * the newest would make the choice depend on draw order, so a thin primitive
 * such as a track could be dropped purely because many layers happened to be
 * appended after it. Sorting by depth instead means the fragments discarded
 * are the far ones, whose contribution is the most attenuated anyway.
 */
int createFragmentList(){
  int n = 0;
  int steps = 0;
  uint headIndex = uint(gl_FragCoord.y) * oirWidth + uint(gl_FragCoord.x);
  uint current = head_pointers[headIndex];
  while (current != LIST_END && steps < MAX_WALK){
    steps++;
    if (current >= list_capacity) break;
    uvec4 item = imageLoad(list_buffer, int(current));
    current = item.x;
    if (n < MAX_FRAGMENTS){
      fragments[n] = item;
      n++;
    } else {
      /* full: let this fragment displace the farthest one held, if nearer */
      int far = 0;
      int i;
      for (i = 1; i < MAX_FRAGMENTS; i++){
        if (isFarther(fragments[i], fragments[far])) far = i;
      }
      if (isFarther(fragments[far], item)) fragments[far] = item;
    }
  }
  return(n);
}

/*
 * Sort the fragments by Z value but respecting the order in which they
 * have been added to the list, in case the Z value is the same. For this,
 * we use the index which has been stored in the 4th component.
 */

void sortFragments(int n){
  int i, j;
  for (i=0; i<n-1; i++){
    for (j=0; j<n-1-i; j++){
      float depth_j  = uintBitsToFloat(fragments[j].z);
      float depth_j1 = uintBitsToFloat(fragments[j+1].z);
      bool tied = abs(depth_j - depth_j1) < DEPTH_EPS;
      bool shouldSwap = tied
        ? (fragments[j].w > fragments[j+1].w)   /* equal depth: later draw goes on top */
        : (depth_j < depth_j1);                 /* different depth: farther goes first */
      if (shouldSwap){
        uvec4 tmp = fragments[j];
        fragments[j] = fragments[j+1];
        fragments[j+1] = tmp;
      }
    }
  }
}

/*
 * finalColor: composite the sorted fragments with the over operator.
 *
 * The accumulation is premultiplied, but the result is handed back with the
 * colour divided out again, so that the ordinary
 * GL_SRC_ALPHA / GL_ONE_MINUS_SRC_ALPHA blend puts it over the opaque image
 * correctly. This is the default mode.
 */
vec4 finalColor1(int nfrag){
  vec3 acc = vec3(0.0, 0.0, 0.0);
  float alpha = 0.0;
  int i;
  for (i=0; i<nfrag; i++){
    vec4 inCol = unpackUnorm4x8(fragments[i].y);
    acc   = acc   * (1.0 - inCol.a) + inCol.rgb * inCol.a;
    alpha = alpha * (1.0 - inCol.a) + inCol.a;
  }
  if (alpha <= 0.0) return vec4(0.0, 0.0, 0.0, 0.0);
  return vec4(acc / alpha, alpha);
}

/*
 * Alternative approach: walk the fragments nearest-first, scaling each by a
 * factor (e.g. 0.6) so ones further away contribute less and appear darker.
 *
 * Starts from the same fully transparent seed as finalColor1(), i.e. the
 * background, rather than the nearest fragment's own raw colour: seeding
 * with the nearest fragment made it count twice (once unweighted as the
 * seed, once more through the loop below) and skipped the 0.6 attenuation
 * every other layer gets, which is not "starting from the background", it is
 * starting from the frontmost layer with no background at all.
 */
vec4 finalColor2(int nfrag){
  vec3 acc = vec3(0.0, 0.0, 0.0);
  float alpha = 0.0;
  int i;
  for (i=nfrag-1; i>=0; i--){
    vec4 inCol = unpackUnorm4x8(fragments[i].y);
    acc = acc * (1.0 - inCol.a) + inCol.rgb*inCol.a * 0.6;
    alpha = alpha * (1.0 - inCol.a) + inCol.a;
  }
  if (alpha <= 0.0) return vec4(0.0, 0.0, 0.0, 0.0);
  return vec4(acc / alpha, alpha);
}

/*
 * nearestDepth: depth of the transparent fragment closest to the viewer
 */
float nearestDepth(int nfrag){
  float d = 1.0;
  int i;
  for (i=0; i<nfrag; i++){
    d = min(d, uintBitsToFloat(fragments[i].z));
  }
  return d;
}

void main()
{
  int nFragments = createFragmentList();
  /* nothing transparent here, leave the opaque image alone */
  if (nFragments == 0) discard;
  sortFragments(nFragments);
  /*
    Hand the depth of the nearest transparent fragment to the depth test, so
    that the opaque image still occludes this pixel's transparent surfaces
    when it is in front of them. Without this the resolve would paint over
    opaque geometry that was drawn after the transparent surfaces had already
    been appended, for instance a banner drawn on top of the scene.
  */
  gl_FragDepth = nearestDepth(nFragments);
  switch (oirMode){
  case 1:
    gl_FragColor = finalColor1(nFragments);
    break;    
  case 2:
    gl_FragColor = finalColor2(nFragments);
    break;    
  default:
    gl_FragColor = finalColor1(nFragments);
    break;
  }
}
