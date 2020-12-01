# mandelbulb

a haskell application which generates [raymarched](http://jamie-wong.com/2016/07/15/ray-marching-signed-distance-functions/) frames of an animated [mandelbulb](https://en.wikipedia.org/wiki/Mandelbulb)

## notes

- the rendered animation is available here: https://youtu.be/r8h9KLWCueg
- it took over 70 hours to render the 1800 frames used in the animation
- for this reason some sample frames are provided
- as you can tell, it isn't particularly fast, owing to the fact it's a software renderer
- to recoup some of that loss, we compile it with "-O2 -threaded" to enable optimizations and multithreading and run it with "+RTS -Nx", with 'x' being the number of threads on the system

## mechanism

- i could explain how this works, but i couldn't do a better job than the sources listed in the inspiration section, so check those out

## prerequisites

- to build requires the [directory](https://hackage.haskell.org/package/directory), [gloss-export](https://hackage.haskell.org/package/gloss-export), [gloss-raster](https://hackage.haskell.org/package/gloss-raster), [vect](https://hackage.haskell.org/package/vect) packages
- directory: file-handling
- gloss-export: exporting gloss images
- gloss-raster: writing pixels to a gloss image
- vect: vector calculations

## video from frames

- to compile rendered frames into a video, you can use makeVideo.sh, which when placed in a directory of frames and ran, will export a video of the frames
- makeVideo.sh requires [ffmpeg](https://ffmpeg.org/) and [imagemagick](https://imagemagick.org/index.php)
- ffmpeg: compiling frames into a video
- imagemagick: resizing of frames

## inspirations

- i learned about the powers of raymarching from [this video](https://www.youtube.com/watch?v=svLzmFuSBhk)
- [this article](http://jamie-wong.com/2016/07/15/ray-marching-signed-distance-functions/) and [this website](https://www.iquilezles.org/) were instrumental in figuring out how to apply the principles of raymarching, and how to generate a mandelbulb in the latter case
- i learned about the [equidistribution theorem]() which i used to shuffle the frames that were rendered from [this post](https://gamedev.stackexchange.com/a/46469) i read

## future

- i'd like to keep working on this however i recognize it's likely better suited for a GPU shader
- there are definitely ways of squeezing more performance out of this code however
