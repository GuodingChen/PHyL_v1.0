

# used to make the video

import cv2
import numpy as np
import glob
import os

img_array = []


path = './R/'
file_list = [f for f in os.listdir(path) if f.endswith('.jpg')]
file_list = sorted(file_list, key=lambda x: int("".join([i for i in x if i.isdigit()])))


for filename in file_list:
    
    
    print(filename)
    img = cv2.imread(path + filename)
    height, width, layers = img.shape
    size = (width,height)
    img_array.append(img)


out = cv2.VideoWriter('R.avi',cv2.VideoWriter_fourcc(*'DIVX'), 5, size)
 
for i in range(len(img_array)):
    out.write(img_array[i])
out.release()

