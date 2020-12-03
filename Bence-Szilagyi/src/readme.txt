-----------
-- Setup --
-----------

The setup should be as simple as:
cabal update
cabal build fp-competition
cabal new-run fp-competition -- par 16000 +RTS -N3

However, if you are on windows like me, you will want to install freeglut.dll first, or you will not be able to compile the program.

1600x800 rendering is recommended for enough detail, recommended value for 'aAMult' is 100 (See description for details)
This can take up to 10 minutes or even more depending on computational power and the settings of the algorithms

------------------------------
-- Description and Settings --
------------------------------

    The original idea behind my submission was to take a few winning entries from the previous years, and combine their aspects to create
an aesthetically pleasing 3D abstract scene, however this has evolved into something more specific during development, and after going deep down
the rabbithole of fractals. To accomplish my goal, I first started with building a simple raytracer, which currently only supports rendering
spheres, with two materials, "diffuse" and "metal". I have tried to design it so that I will be able to expand on it more easily later on, however this
project was more so an exercise in development within a short timeframe, rather than an exercise in best practices and maintainable/elegant code.
    The next step was expanding the scene to somehow incorporate fractals. The natural approach was to render the fractals on top of the
spheres so that they create a whole together. And then the task was finding fractals that would best facilitate this, while also looking good.
In my research, I stumbled upon "spherical algorithmic worlds", which are basically Julia sets, with the added step of applying a Moebius
transform to the function that creates their corresponding dynamic system. The reason these sets are so perfect for this application is that
they map naturally onto the sphere using stereographic projection.
    Or, rather perhaps, they would if I had the time to properly do the maths for the projection. As for now there are parts of the spheres
that do not have fractals rendered upon them. Though this is arguably an improvement to the look of the scene, as the reflective material from
the raytracer can shine - quite literally - off the homogeneous surfaces created by this effect. At least this is a good enough reason for me
to call it a feature. If one so desires, the area of these homogeneously coloured surfaces on the spheres can be affected by changing the
'escapeRadius' variable in Fractals.hs. However, I would warn against doing this, as it has other effects, including a significant increase in
rendering time, which is not insignificant to begin with.
    There are other variables in that file however, which I do recommend playing around with, namely setA and similar. These control some
parameters of the fractals projected onto various spheres. Even the shape of the fractal can be adjusted by playing around with the helper
expression 'func' for the 'inJulia' function in Fractals.hs. However do be aware that this changes the shape of the fractals on all spheres in
the current version. The fractal parameters for each of the spheres can be adjusted individually as well, by going into the 'SphereHitCheck.hs'
file, and adjusting their 'fractalInit' parameters. Also in that file is the variable that control the position of the camera, however do be
warned that moving the camera currently does not automatically move the image plane with it. This variable can be a useful tool however, as it
allows us to affect the FOV given the current position of the image plane by changing the camera's z offset. In this file we can also add and
remove spheres to and from the scene as desired, by following the pattern shown in the elements of 'sphereList', and adding our own, or removing
the ones we do not like.
    Moving on to Tracer.hs, 'xW' and 'yW' control the x and y dimensions of the window, respectively. The variable 'aAMult' controls how many 
rays are shot at one pixel in the image, which is necessary for the diffuse materials to look right, although there is some bug in my
implementation, probably in the random number generation part, where I am needing to re-seed my generators all the time, instead of using a
single one, thereby breaking their cycles, and probably creating bad quality randomness, which I believe could be the culprits. The bug I'm
talking about here is the one where given a low number of rays per pixel, the images and shadows of diffuse spheres are dotted with random,
chaotic dots. (Perhaps it is not about the quality of the random numbers after all?) As previously, I would not reccomend changing the rest of
the variables here either.
    Finally, in Main.hs, we can swap around the two definitions given for 'ourPicture' using comments as shown. The default version renders
our raytraced scene in all its glory, whilst the other option makes the program render an 800x600 rendition of the fractal presented in the
background of the main scene. As a heads up for rendering, currently I have the window setup to be 1600x800, which can take over 10 minutes to
render a frame, even with 3 core parallelization. However, this produces the best looking images and I believe it is worth the wait. Maybe you
can even have a coffee break while your computer does all the work for you for once.

--------------
-- Settings --
--------------

As explained in the description

Main.hs
    ourPicture    -- Render modes ('3D vs 2D' / Scene vs Bg. fractal)
SphereHitCheck.hs
    originPoint (z coord is relatively safe, the rest less so)
    sphereList    -- Individual access to sphere properties
Tracer.hs
    xW, yW        -- Probably the most important settings because of their effect on rendering times.
    aAMult        -- Higher means less spottiness for diffuse materials, also has a significant effect on render times.
Fractals.hs
    escapeRadius  -- Higher means smaller homogeneous area on spheres, but more "zoomed out" fractals on them too.
    func          -- Input custom polynomial for differently shaped fractals.
    set...        -- 'Remote' access to the fractal parameters in sphereList of SphereHitCheck.hs

I highly recommend not touching the other modules, as the code is the opposite of clean for the ones I wrote latest, as well as having the
most complicated Maths. You can look through the code of course, but for example I also do not fully understand Mandelbrot.hs and
MandelbrotColors.hs. I'll even tell you the secret that the latter has a pretty major bug, but it just so happens that the result still looks
good, with the added benefit of it being unique to my program.

----------------
-- Known Bugs --
----------------

When two spheres are really close to each other, or intersecting, the shadows right on the boundary between them become light grey.
The coloring of the Mandelbrot fractal in the background is not exactly as intended.
The big homogeneous circles on the front of the spheres.

-------------------------
-- Credits and sources --
-------------------------

Peter Shirley for his awesome books that let even a complete beginner at 3D rendering like me, create this in 5 days or so
Dan for helping me debug my renderer and shader code
http://www.juliasets.dk/Pictures_of_Julia_and_Mandelbrot_sets.htm for being an amazing resource on Mandelbrot sets and Julia sets
And their wikibook 'Pictures of Julia and Mandelbrot Sets', which provided the algorithms for most of Mandelbrot.hs and MandelbrotColors.hs
Syntopia's Fragmentarium code for its implementation of stereographic projection, which I ported to Haskell basically without changes (I think)
The people of the first years' CompSci Discord server for keeping me from going insane while writing this
www.sekinoworld.com and www.algorithmic-worlds.net for inspiration
The awesome person from the CompSci Discord who helped me port the algorithm used in Mandelbrot.hs to Haskell and let me use some of his code
Paula for coming up with the colors I've used for the last few spheres I've pre-rendered (see gallery)
The FP course staff for teaching us Haskell
All other major sources listed in sources.txt
