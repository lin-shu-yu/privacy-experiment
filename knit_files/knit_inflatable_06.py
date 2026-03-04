##### inflatable knit #####
# create dropped stitches with hex pattern / no drop stitches around channels
# [x] remove drop stitches around channel
# [x] recreate grid
# [x] smaller channels
# [x] add drops in channel

# Author: Mich Lin (kerberos: shuyulin)
# Date created: 20EB2026
# Date last modified: 23FEB2026

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
opening = int(2); # make opening 2 st both sides
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
gray_green = (0, 175, 0) # transfer to back needles
dark_green = (0, 100, 0) # tuck on back needles
red = (255,0,0) # knit
yellow = (255,255,200) # loop / tuck 
orange = (255,175,0) # transfer
dark_orange = (255, 140, 0) # transfer left
purple = (128, 0, 128) # transfer right
pink = (255,105,180)

# Create the pixel map
img = Image.new( 'RGB', (width,height), (175,200,225)) 
pixels = np.array(img) 
# make background checkered
# edge
pixels[::2, :5:2] = dark_blue
pixels[1::2, 1:5:2] = dark_blue
pixels[::2, -5::2] = dark_blue
pixels[1::2, -4::2] = dark_blue

pixels[::2, 5:-5:4] = dark_blue
pixels[1::2, 7:-5:4] = dark_blue

##### set knit shape #####
### create channels
# make linspace vector (rn set to height, make larger) and parameterize to pi
# scale cosine by half of width of channel difference
# add offset by (min/2 + (max-min)/4)
# round to an integer
pixels[::25,edge+4:width-edge-5:2] = pink # drop stitches every 25 rows

# recreate grid near channels here (with 2 px widening)
for k in range(num_channels):
    channel_grid_start = int(channel_med[k]-channel_max/2) -2
    channel_grid_end = int(channel_med[k]+channel_max/2) +2

    pixels[:,channel_grid_start:channel_grid_end] = light_blue
    pixels[::2,channel_grid_start:channel_grid_end:2] = dark_blue
    pixels[1::2,1+channel_grid_start:channel_grid_end:2] = dark_blue

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

# manually add drop st
pixels[:,122] = dark_blue
pixels[::25,122] = pink
pixels[:,182] = dark_blue
pixels[::25,182] = pink

# add some drop st in the channel
for k in range(num_channels):
    pixels[::25,channel_med[k]-4:channel_med[k]+4:2] = pink

# create edges
pixels[:,5:edge+5] = dark_blue; # left edge
pixels[:,width-edge-5:-5] = dark_blue; #right edge

# take everything besides the grid and drop/color accordingly 
for k in range(num_channels+1):
    if k==0:
        grid_start = 5+edge
        grid_end = int(channel_med[k]-channel_max/2) -2
    elif k==num_channels:
        grid_start = int(channel_med[k-1]+channel_max/2) +2
        grid_end = width-edge-5
    else:
        grid_start = int(channel_med[k-1]+channel_max/2) +2
        grid_end = int(channel_med[k]-channel_max/2) -2

    # every column dropped should be dark blue otherwise
    for j in range(grid_start,grid_end):
        if all(pixels[0,j] == pink):
            pixels[:,j] = dark_blue 
            pixels[::25,j] = pink # recolor
    
    # float every kbf st except first/last two rows
    for i, row in enumerate(pixels[2:-2,grid_start:grid_end]):
        for j, pixel in enumerate(row):
            if all(pixel==light_blue):
                pixels[i+2,j+grid_start] = orange

# switch transfer st at drop rows back to knit
for i, row in enumerate(pixels[::25,5:-5]):
    for j, pixel in enumerate(row):
        if all(pixel==orange):
            pixels[i*25,j+5] = light_blue

##### create knitting structures #####
# add starting structure
knit_end = np.array(Image.new( 'RGB', (np.size(pixels, 1),30), neon_green)) # wide as num of columns of pixel, 30 st long
# checkerboard every other row to transfer to back needles
knit_end[-1,1::2] = gray_green
knit_end[-2,::2] = gray_green
# no transfers for dropped needles
for j, pixel in enumerate(pixels[0,:]):
    if all(pixel == pink):
        knit_end[:,j] = neon_green

# add ending structure - christmas line! 
knit_start = np.array(Image.new( 'RGB', (np.size(pixels, 1),1), red)) # wide as num of columns of pixel, 30 st long
# create openings for edges
left_opening_i = int(5+edge/2);
right_opening_i = int(width-5-edge/2)

knit_start[:, (left_opening_i-3):(left_opening_i+3)] = dark_green
knit_start[:,(right_opening_i-3):(right_opening_i+3)] = dark_green
for i in range(num_channels):
    knit_start[:, (channel_med[i]-opening):(channel_med[i]+opening)] = dark_green

# make checkerboard for channel openings
# create openings for edges
# left edge
# starting at 5 st + 2 st for buffer
pixels[-1, (left_opening_i-3):(left_opening_i+3):2] = yellow;
pixels[-2, (left_opening_i-3+1):(left_opening_i+3):2] = yellow;
# right edge
pixels[-1, (right_opening_i-3):(right_opening_i+3):2] = yellow;
pixels[-2, (right_opening_i-3+1):(right_opening_i+3):2] = yellow;
for i in range(num_channels):
    pixels[-1, (channel_med[i]-opening):(channel_med[i]+opening):2] = yellow;
    pixels[-2, (1+channel_med[i]-opening):(channel_med[i]+opening):2] = yellow;

# add transfers for dropped stitches - only if we don't want start to also unravel
# for j in range(np.size(pixels,1)):
#     if all(pixels[0,j]==pink):
#         knit_start[:,j] = gray_green 
#         pixels[-1,j] = dark_orange # transfer sideways in first line

# show final image
final = Image.fromarray(np.append(knit_end, np.append(pixels, knit_start, axis = 0), axis = 0))
final.show()

final.save('knit_inflatable_06.bmp')
final.save('knit_inflatable_06.png')