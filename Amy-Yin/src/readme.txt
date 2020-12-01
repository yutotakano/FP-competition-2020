
--Creepyfrac--

This project was created by Amy Yin (s2023721) for entry to the FP Programming Competition on 29/11/2020.


--some information--

Creepyfrac is a new fractal I accidentally came upon when trying to render the Mandelbox fractal. Therefore, the use of Box Fold and Ball Fold functions in my project were heavily inspired by this website's introduction of the Mandelbox: 
https://digitalfreepen.com/mandelbox370/

This project renders Creepyfrac onto a 2D plane either as a still image, a gif zooming into its centre, or a gif zooming into its centre but in funky colours. The technique used is the standard escape time rendering frequently used for visualising the Mandelbrot set fractal, although I made slight changes to it so that the resulting image is solid colour.


--dependencies--

Please install Codec.Picture like this:

stack install JuicyPixels


--running it--

Please run it in GHC using the following optimisations because or else it will take very, very long:

stack ghc -- -O2 -optc-ffast-math -fexcess-precision creepyfrac.hs


--code--

There are four main functions in the code. In order to run one of them, comment out the other three, then run Creepyfrac.hs in GHC as specified above.

The first one makes a gif of zooming into the fractal, where the fractal is coloured in a nasty blue-green colour. The zooming effect is achieved by rendering a smaller section of the fractal in higher definition every frame. The zooming drastically accelerates at the end because the size of the area rendered decreases linearly. This was a deliberate choice because it's cool and feels like a movie scene where someone suddenly falls down a tunnel.

The second one makes a very high-definition png of the fractal. You can also do this by running the makeOnePic function in GHCi. This version is recommended for examining the details of the fractal (they start looking very weird if you stare at them for long enough!). Please modify the height and width data at the beginning of the code to 1800 or some other reasonably large size if you plan to do this.

The third one makes a gif identical to the first except it's now in vapourwave colours. This was done mainly to lessen the discomfort caused by viewing the fractal. It is achieved by making pixels within the set of points to colour, a colour based on a specific blue-pink gradient. The pixels outside of that set are just pink. Because keeping the gradients exactly the same would be boring, the function foregroundGradient actually makes a neverending gradient that the fractals "scrolls through", which creates the colour-changing effect in outvwSample.gif. This would not have been possible without lazy evaluation. Thank you, lazy evaluation!

The fourth one makes a large still png of the vapourwave version. I find it to be a bit easier on the eye than the other one.


--sample images--

The pictures attached are samples of what the results of running each of the main functions listed above should look like.


--that's it, thank you for reading!--