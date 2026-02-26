##### inflatable knit #####
# horizontal knit
# with hex pattern / dropped needles
# knitting with elastic yarn (yg3)

# Author: Mich Lin (kerberos: shuyulin)
# Date created: 26EB2026
# Date last modified: 26FEB2026

##### housekeeping #####
from PIL import Image
import numpy as np
import math
import warnings
import sys

##### set sizing parameters #####
width_cm = 80; height_cm=60; # width/height [cm]
num_channels = 4; # sets of channels
edge_cm = 4; # length of each edge [cm], changing to 1in diameter (approx 8 cm circumference)
channel_gap_cm = 8; # manually set, channel gap is the gap from the sides [cm]
channel_max_cm = 8; # maximum channel width [cm]
channel_min_cm = 1; # minimum channel width [cm]
bulb_size_cm = 20; # approx height of one bulb [cm]

# warning check
if (channel_max_cm*num_channels + edge_cm*2) >= width_cm:
    warnings.warn("Knit width is insufficient to accommodate parameters; please recheck.", UserWarning)
    sys.exit(1)

# conver to st (* in/2.5cm * 10 st./in (gauge))
width = int(width_cm/2.5*10); # width [st]
height = int(height_cm/2.5*10); # length [st]
edge = int(edge_cm/2.5*10); # edge width [st]
opening = int(1); # make opening 2 st
channel_max = int(channel_max_cm/2.5*10); # maximum channel width [st]
channel_min = int(channel_min_cm/2.5*10); # minimum channel width [st]
channel_gap = int(channel_gap_cm/2.5*10); # gap between channels [st]
# channel_gap = (height - edge*2 - num_channels*(channel_max-channel_min)//2)//(num_channels); # channel gap
# channel_gap = (height - edge*2 - num_channels*channel_max)//(num_channels); # channel gap
bulb_size = int(bulb_size_cm/2.5*10); # bulb size [st]

# get indices of median lines of channels
# get evenly spaced points within the fabric space (+/- gap from edge)
channel_med = np.linspace(edge+channel_gap, height-edge-channel_gap, num = num_channels, dtype=int)

##### create new image #####
# color code
white = (255,255,255) # unknitted needle (floats)
dark_blue = (0,0,255) # channel (with yg1/2 both on black yarn)
dark_purple = (48, 25, 52) # channel with half elastic (yg3)
light_blue = (175,200,225) # interlock st (opp. channel)
light_purple = (218, 177, 218) # interlock st with half elastic on yg3 (opp of dark_purple)
neon_green = (0,225,0) # knit back only 
gray_green = (0, 175, 0) # transfer to back needles
dark_green = (0, 100, 0) # tuck on back needles
red = (255,0,0) # knit front
yellow = (255,255,200) # loop / tuck 
orange = (255,175,0) # transfer
dark_orange = (255, 140, 0) # transfer left
purple = (128, 0, 128) # transfer right
pink = (255,105,180) # drop front/back

# Create the pixel map
img = Image.new( 'RGB', (width,height), dark_blue) 
pixels = np.array(img) 
# make background checkered
# right/left edges
pixels[::2, :2:2] = light_blue
pixels[1::2, 1:2:2] = light_blue
pixels[::2, -2::2] = light_blue
pixels[1::2, -1::2] = light_blue
# middle
pixels[::2, 2:-2:4] = light_blue
pixels[1::2, 4:-2:4] = light_blue

##### set knit shape #####
### create channels
# make linspace vector (rn set to height, make larger) and parameterize to pi
# scale cosine by half of width of channel difference
# add offset by (min/2 + (max-min)/4)
# round to an integer

# recreate grid near channels here to comply with yarn guide switching
for k in range(num_channels):
    channel_grid_start = int(channel_med[k]-channel_max/2)
    channel_grid_end = int(channel_med[k]+channel_max/2)

    for i, row in enumerate(pixels[channel_grid_start:channel_grid_end,3:-3]):
        for j, pixel in enumerate(row):
            if all(pixel == dark_blue):
                pixels[i+channel_grid_start,j+3] = dark_purple
            elif all(pixel == light_blue):
                pixels[i+channel_grid_start,j+3] = light_purple

channel_half_width = np.round((channel_min/2 + (channel_max-channel_min)/4) + np.cos(np.linspace(0,width*2,width*2+1) * math.pi/bulb_size)*((channel_max-channel_min)/4)).astype(int); # from edge of channel to midpoint

for j in range(np.size(pixels, 1)):
    for k in range(math.ceil(num_channels/2)):
        channel_start = channel_med[2*k] - channel_half_width[j]  # starting point (median - fabric width)
        channel_end = channel_med[2*k] + channel_half_width[j]  # ending point (edge + fabric width + channel width)

        pixels[channel_start:channel_end, j] = dark_purple; # make that section blue

# shift with pi offset, recreate second set of channels 
channel_half_width = np.round((channel_min/2 + (channel_max-channel_min)/4) + np.cos(np.linspace(0,width*2,width*2+1) * math.pi/bulb_size - math.pi)*((channel_max-channel_min)/4)).astype(int);

for j in range(np.size(pixels, 1)):
    for k in range(math.floor(num_channels/2)):
        channel_start = channel_med[2*k+1] - channel_half_width[j]  # starting point (median - fabric width)
        channel_end = channel_med[2*k+1] + channel_half_width[j]  # ending point (edge + fabric width + channel width)

        pixels[channel_start:channel_end, j] = dark_purple; # make that section blue

# create edges
pixels[5:edge+5, :] = dark_blue; # top edge
pixels[height-edge-5:-5,:] = dark_blue; # bottom edge

# float every kbf st except first/last two rows
for i, row in enumerate(pixels[5:-5,4:-3]):
    for j, pixel in enumerate(row):
        if all(pixel==light_blue):
            pixels[i+5,j+4] = orange

# add floats
pixels[:, 3:-2:2] = white

# take everything besides the grid and drop/color accordingly 
# every column dropped should be dark blue otherwise
# for j in range(np.size(pixels, 1)):
#     if all(pixels[0,j] == pink):
#         pixels[:,j] = dark_blue 
#         pixels[::25,j] = pink # recolor

# switch transfer st at drop rows back to knit
# for i, row in enumerate(pixels[::25,3:-2]):
#     for j, pixel in enumerate(row):
#         if all(pixel==orange):
#             pixels[i*25,j+3] = light_blue

##### create knitting structures #####
# add starting structure
knit_end = np.array(Image.new( 'RGB', (np.size(pixels, 1),30), neon_green)) # wide as num of columns of pixel, 30 st long
# checkerboard every other row to transfer to back needles
knit_end[-1,::2] = gray_green
knit_end[-2,1::2] = gray_green
# no transfers for dropped needles
for j, pixel in enumerate(pixels[0,:]):
    if all(pixel == white):
        knit_end[:,j] = neon_green

# add ending structure 
knit_start = np.array(Image.new( 'RGB', (np.size(pixels, 1),1), red)) # wide as num of columns of pixel, 30 st long
# add transfers for dropped stitches - only if we don't want start to also unravel
knit_start[:,3:-2:2] = gray_green 
pixels[-1,3:-2:2] = dark_orange # transfer sideways in first line

# show final image
final = Image.fromarray(np.append(knit_end, np.append(pixels, knit_start, axis = 0), axis = 0))
final.show()

final.save('knit_inflatable_08a.bmp')
final.save('knit_inflatable_08a.png')