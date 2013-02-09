cl-dmd-control
==============

Use TI Lightcrafter projector, to display a video with 1440 fps.

![photograph](http://www.ti.com/graphics/tool/LCR-with-Pencil3-12.5-lrg.jpg)

This TI Lightcrafter is a small projector, that contains a DMD chip with 608x648 mirrors.
Normally you would just connect a HDMI cable and it would display the RGB data with 60Hz.

However, it contains an ARM chip that runs Linux. I connect to this using network and more
or less directly control the DLPC3000 controller that prepares data for display on the DMD.

My initial goal was to get direct access to the DMD pixels and make it display lines as
fast as possible. I managed to do this after 2 days. The fastest framerate that I can achieve
in this way is 24*60Hz=1440Hz.

I could also store up to 96 bitplanes directly in the device and run them at maximal 4000Hz.
But I rather want to generate the data on a GPU using OpenGL and perhaps OpenCL.
