 I used the Haskell package "Gloss" to create it, so to run the program importing "Graphics.Gloss" is required.

Instructions:
To run the graphic, just type "main" into the terminal after compiling the code. This will run the main graphic with all the components on at a fast speed which creates the illusion of different shapes from spinning circles. However, I found that just the underlying fractal on its own looked cool as well, so I created a secondary function " main' " (that's main prime), which shows just the underlying graphic by itself.

Alternatively, it is also possible to run the graphic with the expanding circles by running "fast True" or "slow True" or without the expanding circles by running "fast False" or "slow False", where "fast _" runs it at a high rotation speed and "slow _" runs it at a low rotation speed. These functions exist to give some level of customisation, as I found the graphic looks cooler to some with different settings than to others.
