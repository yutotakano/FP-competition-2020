# copy this to an output directory and run it to make a video
mkdir temp && # create temporary directory
magick.exe mogrify -resize 1280x720 -quality 100 -path ./temp *.png && # downsize all frames
ffmpeg -framerate 60 -pix_fmt yuv420p -pattern_type glob -i "./temp/*.png" temp.mp4 && # compiles frames into video
ffmpeg -i temp.mp4 -filter_complex "[0:v]reverse,fifo[r];[0:v][r] concat=n=2:v=1 [v]" -map "[v]" output.mp4 && # ping-pong loop video
rm -r temp && # remove temporary directory
rm temp.mp4 # remove temporary video