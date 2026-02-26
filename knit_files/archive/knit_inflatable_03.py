##### inflatable knit #####
# from knit_inflatable_02
# create float stitches in background (non-channel fabric)
# transfer stitches in center of fabric are different than left/right transfers

# Author: Mich Lin (kerberos: shuyulin)
# Date created: 16FEB2026
# Date last modified: FEB172026

##### housekeeping #####
from PIL import Image
import numpy as np
import math
import warnings
import sys

##### set sizing parameters #####
width_cm = 60; height_cm=80; # width/height [cm]
num_channels = 6; # sets of channels
edge_gap_cm = 3; # extra edge gap [cm]
edge_cm = 4; # length of each edge [cm], changing to 1in diameter (approx 8 cm circumference)
channel_max_cm = 6; # maximum channel width [cm]
channel_min_cm = 3; # minimum channel width [cm]
bulb_size_cm = 10; # approx height of one bulb [cm]

# warning check
if (channel_max_cm*num_channels + edge_cm*2) >= width_cm:
    warnings.warn("Knit width is insufficient to accommodate parameters; please recheck.", UserWarning)
    sys.exit(1)

# conver to st (* in/2.5cm * 10 st./in (gauge))
width = int(width_cm/2.5*10); # width [st]
height = int(height_cm/2.5*10); # length [st]
edge = int(edge_cm/2.5*10); # edge width [st]
edge_gap = int(edge_gap_cm/2.5*10); # extra edge gap [st]
opening = int(2); # make opening 2 st wide
channel_max = int(channel_max_cm/2.5*10); # maximum channel width [st]
channel_min = int(channel_min_cm/2.5*10); # minimum channel width [st]
channel_gap = (width - edge*2 - num_channels*channel_max)//(num_channels); # channel gap
bulb_size = int(bulb_size_cm/2.5*10); # bulb size [st]

# get indices of median lines of channels
# get evenly spaced points within the fabric space (+/- gap from edge)
channel_med = np.linspace(edge+channel_gap+edge_gap, width-edge-channel_gap-edge_gap, num = num_channels, dtype=int)

##### create new image #####
# color code
white = (255,255,255) # unknitted needle (floats)
dark_blue = (0,0,255) # channel
light_blue = (175,200,225) # channel (opposite)
neon_green = (0,225,0) # knit back only 
gray_green = (0, 200, 0) # transfer to back needles
dark_green = (0, 100, 0) # tuck on back needles
red = (255,0,0) # knit
yellow = (255,255,200) # loop / tuck 
dark_orange = (255, 140, 0) # transfer left
purple = (128, 0, 128) # transfer right
img = Image.new( 'RGB', (width,height), (175,200,225)) 
pixels = np.array(img) # Create the pixel map
# make background checkered, just the edge closing
# col, row
pixels[::2, :5:2] = (0,0,255) 
pixels[1::2, 1:5:2] = (0,0,255)
pixels[::2, -5::2] = (0,0,255) 
pixels[1::2, -4::2] = (0,0,255)
# make background checkered with drop stitches
pixels[::2, 5:-5:4] = (0,0,255) 
pixels[1::2, 7:-6:4] = (0,0,255)
# every other column is blacked out
pixels[:, 6:-6:2] = (255,255,255)

##### set knit shape #####

### create channels
# make linspace vector (rn set to height, make larger) and parameterize to pi
# scale cosine by half of width of channel difference
# add offset by (min/2 + (max-min)/4)
# round to an integer
channel_half_width = np.round((channel_min/2 + (channel_max-channel_min)/4) + np.cos(np.linspace(0,height*2,height*2+1) * math.pi/bulb_size)*((channel_max-channel_min)/4)).astype(int); # from edge of channel to midpoint

for i in range(np.size(pixels, 0)):
    for j in range(math.ceil(num_channels/2)):
        channel_start = channel_med[2*j] - channel_half_width[i]  # starting point (median - fabric width)
        channel_end = channel_med[2*j] + channel_half_width[i]  # ending point (edge + fabric width + channel width)

        pixels[i, channel_start:channel_end] = dark_blue; # make that section blue

# shift with pi offset, recreate second set of channels 
channel_half_width = np.round((channel_min/2 + (channel_max-channel_min)/4) + np.cos(np.linspace(0,height*2,height*2+1) * math.pi/bulb_size - math.pi)*((channel_max-channel_min)/4)).astype(int);

for i in range(np.size(pixels, 0)):
    for j in range(math.floor(num_channels/2)):
        channel_start = channel_med[2*j+1] - channel_half_width[i]  # starting point (median - fabric width)
        channel_end = channel_med[2*j+1] + channel_half_width[i]  # ending point (edge + fabric width + channel width)

        pixels[i, channel_start:channel_end] = dark_blue; # make that section blue

# add transfer st to the left of channels
for k in channel_med:
    for i in range(np.size(pixels, 0)):
        for j in range(int(k-channel_max/2), int(k)):
            if (all(pixels[i,j] == dark_blue) and all(pixels[i-1,j] == white)):
                pixels[i,j] = dark_orange
        for j in range(int(k), int(k+channel_max/2)):
            if (all(pixels[i,j] == dark_blue) and all(pixels[i-1,j] == white)):
                pixels[i,j] = purple

##### create knitting structures #####
# add ending structure
knit_end = np.array(Image.new( 'RGB', (np.size(pixels, 1),30), (0, 255, 0))) # wide as num of columns of pixel, 30 st long
# checkerboard every other row to transfer to back needles
knit_end[-1,::2] = (0, 200, 0)
knit_end[-2,1::2] = (0, 200, 0)

# create edges
pixels[:,5:edge+5] = (0,0,255); # left edge
pixels[:,width-edge-5:-5] = (0,0,255); #right edge

# edit last lines to make ending structure
# christmas line! 
pixels[-1,:] = (255,0,0); 
# create openings for edges
left_opening_i = int(5+edge/2);
right_opening_i = int(width-5-edge/2)

pixels[-1, (left_opening_i-3):(left_opening_i+3)] = (0,100,0);
pixels[-1,(right_opening_i-3):(right_opening_i+3)] = (0,100,0);
for i in range(num_channels):
    pixels[-1, (channel_med[i]-opening):(channel_med[i]+opening)] = (0,100,0);

# make transfers for starting line to avoid retained stitches
for j in range(np.size(pixels,1)):
    if all(pixels[-2,j]==white):
        pixels[-1,j] = dark_orange

# drop needles in ending structure
for j in range(np.size(pixels,1)):
    if all(pixels[0,j]==white):
        # print(pixels[0,j])
        knit_end[:,j] = white

# make checkerboard for knit openings
# create openings for edges
# left edge
# starting at 5 st + 2 st for buffer
pixels[-2, (left_opening_i-3):(left_opening_i+3):2] = (255,255,200);
pixels[-3, (left_opening_i-3+1):(left_opening_i+3):2] = (255,255,200);
# right edge
pixels[-2, (right_opening_i-3):(right_opening_i+3):2] = (255,255,200);
pixels[-3, (right_opening_i-3+1):(right_opening_i+3):2] = (255,255,200);
for i in range(num_channels):
    pixels[-2, (channel_med[i]-opening):(channel_med[i]+opening):2] = (255,255,200);
    pixels[-3, (1+channel_med[i]-opening):(channel_med[i]+opening):2] = (255,255,200);

# show final image
final = Image.fromarray(np.append(knit_end, pixels, axis = 0))
final.show()

final.save('knit_inflatable_03.bmp')
final.save('knit_inflatable_03.png')