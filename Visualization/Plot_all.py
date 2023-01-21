#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Nov  8 15:08:15 2022

@author: Guoding_Chen
this program is used to plot all the matrix 


"""


import cv2
import numpy as np
import re
import h5py
import matplotlib
import matplotlib as mpl
import shapefile as shp
from matplotlib import colors
matplotlib.use('Qt5Agg')
import matplotlib.pyplot as plt
from matplotlib.colors import ListedColormap, LinearSegmentedColormap
import os
import glob
font1={'family':'Arial',
     'style':'normal',
    'weight':'normal',
     'size':16}
font_xylable={'family':'Arial',
     'style':'normal',
    'weight':'normal',
     'size':40}
font_legend={'family':'Arial',       
     'style':'normal',
    'weight':'normal',
     'size':12}


# -------------------------------------empty directory--------------------------


for filename in os.listdir('./W/'):
    
    os.remove('./W/' + filename)
    
for filename in os.listdir('./SM/'):
    
    os.remove('./SM/' + filename)

for filename in os.listdir('./R/'):
    
    os.remove('./R/' + filename)
    
for filename in os.listdir('./FS/'):
    
    os.remove('./FS/' + filename)

for filename in os.listdir('./PF/'):
    
    os.remove('./PF/' + filename)    
    
for filename in os.listdir('./Volume/'):
    
    os.remove('./Volume/' + filename)
    

    

# ----------------------------------start to plot figure----------------------
variable_list = ["R", "W","SM", "FS3D", "PF", "FVolume"]

# read the necessary information from the control file
Control_file = open('../Control.Project','r')

ControlFile_contents = Control_file.readlines()

# identify the boundary information for hydrology
XLLCorner_line_hydro = re.sub(r'\s+', '', [s for s in ControlFile_contents if "XLLCorner_Hydro" in s][0])
XLLCorner_hydro = XLLCorner_line_hydro.split("#",1)[0]
XLLCorner_hydro = float(XLLCorner_hydro.split("=",1)[1])

YLLCorner_line_hydro = re.sub(r'\s+', '', [s for s in ControlFile_contents if "YLLCorner_Hydro" in s][0])
YLLCorner_hydro = YLLCorner_line_hydro.split("#",1)[0]
YLLCorner_hydro = float(YLLCorner_hydro.split("=",1)[1])

nCols_line_hydro = re.sub(r'\s+', '', [s for s in ControlFile_contents if "NCols_Hydro" in s][0])
nCols_hydro = nCols_line_hydro.split("#",1)[0]
nCols_hydro = float(nCols_hydro.split("=",1)[1])

nRows_line_hydro = re.sub(r'\s+', '', [s for s in ControlFile_contents if "NRows_Hydro" in s][0])
nRows_hydro = nRows_line_hydro.split("#",1)[0]
nRows_hydro = float(nRows_hydro.split("=",1)[1])

cellSize_line_hydro = re.sub(r'\s+', '', [s for s in ControlFile_contents if "CellSize_Hydro" in s][0])
cellSize_hydro = cellSize_line_hydro.split("#",1)[0]
cellSize_hydro = float(cellSize_hydro.split("=",1)[1])


# identify the boundary information for landslide
XLLCorner_line_land = re.sub(r'\s+', '', [s for s in ControlFile_contents if "XLLCorner_Land" in s][0])
XLLCorner_land = XLLCorner_line_land.split("#",1)[0]
XLLCorner_land = float(XLLCorner_land.split("=",1)[1])

YLLCorner_line_land = re.sub(r'\s+', '', [s for s in ControlFile_contents if "YLLCorner_Land" in s][0])
YLLCorner_land = YLLCorner_line_land.split("#",1)[0]
YLLCorner_land = float(YLLCorner_land.split("=",1)[1])

nCols_line_land = re.sub(r'\s+', '', [s for s in ControlFile_contents if "NCols_Land" in s][0])
nCols_land = nCols_line_land.split("#",1)[0]
nCols_land = float(nCols_land.split("=",1)[1])

nRows_line_land = re.sub(r'\s+', '', [s for s in ControlFile_contents if "NRows_Land" in s][0])
nRows_land = nRows_line_land.split("#",1)[0]
nRows_land = float(nRows_land.split("=",1)[1])

cellSize_line_land = re.sub(r'\s+', '', [s for s in ControlFile_contents if "CellSize_Land" in s][0])
cellSize_land = cellSize_line_land.split("#",1)[0]
cellSize_land = float(cellSize_land.split("=",1)[1])



# creat the extent for hydrological variables
extent_hydro = [XLLCorner_hydro, XLLCorner_hydro + nCols_hydro * cellSize_hydro,
                YLLCorner_hydro, YLLCorner_hydro + nRows_hydro * cellSize_hydro]

# creat the extent for landslide variables
extent_land = [XLLCorner_land, XLLCorner_land + nCols_land * cellSize_land,
                YLLCorner_land, YLLCorner_land + nRows_land * cellSize_land]



# read the boundary shp file
Boundary_shpfile = "../HydroBasics/Basin_boundary.shp"
sf = shp.Reader(Boundary_shpfile)
for shape in sf.shapeRecords():
    x = np.array([i[0] for i in shape.shape.points[:]])
    y = np.array([i[1] for i in shape.shape.points[:]])



NODATA_value = -9999

# prepare the colorbar
colorbar_R = 'YlGnBu'
colorbar_W = plt.get_cmap('GnBu', 8) 
colorbar_SM = plt.get_cmap('YlGnBu', 8) 


# prepare the colorbar for FS map
style_color_FS = np.array([(151, 0, 29),
                        (219, 116, 93), 
                        (233, 245, 186),
                        (136, 189, 103),
                        (100, 169, 77),
                        (22, 120, 55)])


bounds_FS = np.array([0, 0.8, 1, 1.2, 1.4, 2, 3])
norm_FS = colors.BoundaryNorm(boundaries = bounds_FS, ncolors = 7)


colorbar_FS = LinearSegmentedColormap.from_list('my_palette', style_color_FS / 255, N = 7)

style_color_PF = np.array([(243, 243, 243),
                        (96, 187, 249),
                        (97, 132, 249), 
                        (82, 84, 251),
                        (102, 42, 249),
                        (151, 41, 249),
                        (196, 37, 247),
                        (12, 12, 12)])


bounds_PF = np.array([0, 0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6])
norm_PF = colors.BoundaryNorm(boundaries = bounds_PF, ncolors = 8)


colorbar_PF = LinearSegmentedColormap.from_list('my_palette', style_color_PF / 255, N = 8)


style_color_Vl = np.array([(221, 75, 15),
                        (237, 129, 45),
                        (250, 180, 101), 
                        (247, 224, 193),
                        (220, 218, 231),
                        (174, 165, 206),
                        (123, 101, 171),
                        (75, 39, 135)])


bounds_Vl = np.array([0, 0.5e5, 1e5, 1.5e5, 2e5, 2.5e5, 3e5, 3.5e5, 4e5])
norm_Vl = colors.BoundaryNorm(boundaries = bounds_Vl, ncolors = 8)


colorbar_Vl = LinearSegmentedColormap.from_list('my_palette', style_color_Vl / 255, N = 8)


# read the HDF5 file name
filename = '../Results/Result_all.h5'
data = h5py.File(filename, 'r')
for group in data.keys() :
    
    for dset in data[group].keys():      
        ds_data = data[group][dset] # returns HDF5 dataset object
        
        variable_tag = dset.split("_",2)[1]
        Time_moment = dset.split("_",2)[2]
        OutFigure_name = dset.split("_",1)[1] + ".jpg"
        
        if variable_tag in variable_list :
            variable_matrix = data[group][dset][:] # adding [:] returns a numpy array
            variable_matrix = variable_matrix.astype(np.float32)
            variable_matrix[variable_matrix == NODATA_value] = np.nan
            # Convert scaled integer back to floating point
            variable_matrix = variable_matrix / 100
            # start to plot the figure
            
            if variable_tag == "R":
                variable_matrix[variable_matrix == 0] = 0.000001
                fig, ax = plt.subplots(figsize=(6, 6))

                plt.subplots_adjust(bottom=0.0, top=0.98,left = 0.00, right=0.9 )

                im = ax.imshow(variable_matrix, extent = extent_hydro,
                            cmap = colorbar_R, norm = colors.LogNorm(vmin=10**-1, vmax=10**3))

                ax.plot(x, y, 'k', linewidth = 1.5)

                ax.set_xlim([XLLCorner_hydro, XLLCorner_hydro + nCols_hydro * cellSize_hydro])
                ax.set_ylim([YLLCorner_hydro, YLLCorner_hydro + nRows_hydro * cellSize_hydro])

                plt.suptitle(Time_moment, fontsize=16)
                ax.axis('off')

                cbar_ax = fig.add_axes([0.82, 0.35, 0.035, 0.3])
                cbar_ax.tick_params(labelsize=15)
                cb = plt.colorbar(im, cax=cbar_ax)
                cb.set_label('R ($\mathrm{m^3/s}$)', fontdict = font1, rotation=0, labelpad=-40, y=1.25)
                plt.show()
                plt.savefig("./R/" + OutFigure_name, dpi = 300)
                plt.close()  
                
                print ('successfully plot ' + OutFigure_name )
                
            if variable_tag == "W":
                
                fig, ax = plt.subplots(figsize=(6, 6))

                plt.subplots_adjust(bottom=0.0, top=0.98,left = 0.00, right=0.9 )

                im = ax.imshow(variable_matrix, extent = extent_hydro,
                            cmap = colorbar_W, vmin = 0, vmax = 150)

                ax.plot(x, y, 'k', linewidth = 1.5)

                ax.set_xlim([XLLCorner_hydro, XLLCorner_hydro + nCols_hydro * cellSize_hydro])
                ax.set_ylim([YLLCorner_hydro, YLLCorner_hydro + nRows_hydro * cellSize_hydro])

                plt.suptitle(Time_moment, fontsize=16)
                ax.axis('off')

                cbar_ax = fig.add_axes([0.82, 0.35, 0.035, 0.3])
                cbar_ax.tick_params(labelsize=15)
                cb = plt.colorbar(im, cax=cbar_ax)
                cb.set_label('W ($\mathrm{mm}$)', fontdict = font1, rotation=0, labelpad=-30, y=1.25)
                cb.set_ticks([0,25,50,75,100,125,150])
                plt.show()
                plt.savefig("./W/" + OutFigure_name, dpi = 300)
                plt.close()  
                print ('successfully plot ' + OutFigure_name )
            if variable_tag == "SM":
                fig, ax = plt.subplots(figsize=(6, 6))

                plt.subplots_adjust(bottom=0.0, top=0.98,left = 0.00, right=0.9 )

                


                im = ax.imshow(variable_matrix, extent = extent_hydro,
                            cmap=colorbar_SM, vmin = 0, vmax = 100)

                ax.plot(x, y, 'k', linewidth = 1.5)

                ax.set_xlim([XLLCorner_hydro, XLLCorner_hydro + nCols_hydro * cellSize_hydro])
                ax.set_ylim([YLLCorner_hydro, YLLCorner_hydro + nRows_hydro * cellSize_hydro])

                plt.suptitle(Time_moment, fontsize=16)
                ax.axis('off')

                cbar_ax = fig.add_axes([0.82, 0.35, 0.035, 0.3])
                cbar_ax.tick_params(labelsize=15)
                cb = plt.colorbar(im, cax=cbar_ax)
                cb.set_label('SM ($\%$)', fontdict = font1, rotation=0, labelpad=-30, y=1.25)
                plt.show()
                plt.savefig("./SM/" + OutFigure_name, dpi = 300)
                plt.close()  
                print ('successfully plot ' + OutFigure_name )
                
            if variable_tag == "FS3D":
                fig, ax = plt.subplots(figsize=(6, 6))

                plt.subplots_adjust(bottom=0.0, top=0.92,left = 0.00, right=0.9 )

                

                im = ax.imshow(variable_matrix, extent = extent_land,
                            cmap = colorbar_FS, norm = norm_FS)

                ax.plot(x, y+0.004, 'k', linewidth = 1.5)

                ax.set_xlim([XLLCorner_land, XLLCorner_land + nCols_land * cellSize_land])
                ax.set_ylim([YLLCorner_land, YLLCorner_land + nRows_land * cellSize_land])

                plt.suptitle(Time_moment, fontsize=16)
                ax.axis('off')

                cbar_ax = fig.add_axes([0.86, 0.30, 0.035, 0.4])
                cbar_ax.tick_params(labelsize=15)
                cb = plt.colorbar(im, cax=cbar_ax, extend="max")
                cb.set_label('$F_{s}$ (-)', fontdict = font1, rotation=0, labelpad=-20, y=1.18)
                plt.show()
                plt.savefig("./FS/" + OutFigure_name, dpi = 300)
                plt.close()  
                print ('successfully plot ' + OutFigure_name )
            if variable_tag == "PF":
                fig, ax = plt.subplots(figsize=(6, 6))

                plt.subplots_adjust(bottom=0.0, top=0.92,left = 0.00, right=0.9 )


                im = ax.imshow(variable_matrix, extent=extent_land,
                            cmap = colorbar_PF, norm = norm_PF)

                ax.plot(x, y+0.004, 'k', linewidth = 1.5)

                ax.set_xlim([XLLCorner_land, XLLCorner_land + nCols_land * cellSize_land])
                ax.set_ylim([YLLCorner_land, YLLCorner_land + nRows_land * cellSize_land])

                plt.suptitle(Time_moment, fontsize=16)
                ax.axis('off')

                cbar_ax = fig.add_axes([0.86, 0.25, 0.035, 0.5])
                cbar_ax.tick_params(labelsize=15)
                cb = plt.colorbar(im, cax=cbar_ax)
                cb.set_label('$P_{f}$ (%)', fontdict = font1, rotation=0, labelpad=-30, y=1.15)

                cb.set_ticks([0, 0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6])

                cb.set_ticklabels(["=0", "=0", "5", "10", "20", "30", "40", "50", "60"])
                plt.show()
                plt.savefig("./PF/" + OutFigure_name, dpi = 300)
                plt.close()  
                print ('successfully plot ' + OutFigure_name )
            if variable_tag == "FVolume":
                
                fig, ax = plt.subplots(figsize=(6, 6))

                plt.subplots_adjust(bottom=0.0, top=0.92,left = 0.00, right=0.9 )

                


                im = ax.imshow(variable_matrix, extent=extent_land,
                            cmap=colorbar_Vl, norm = norm_Vl)

                ax.plot(x, y+0.004, 'k', linewidth = 1.5)

                ax.set_xlim([XLLCorner_land, XLLCorner_land + nCols_land * cellSize_land])
                ax.set_ylim([YLLCorner_land, YLLCorner_land + nRows_land * cellSize_land])

                plt.suptitle(Time_moment, fontsize=16)
                ax.axis('off')

                cbar_ax = fig.add_axes([0.86, 0.25, 0.035, 0.5])
                cbar_ax.tick_params(labelsize=15)
                cb = plt.colorbar(im, cax=cbar_ax, extend="max")
                cb.set_label('$V_{L}$ ($\mathrm{10^5~m^{3}}$)', fontdict = font1, rotation=0, labelpad=-30, y=1.15)
                cb.set_ticks( [0, 0.5e5, 1e5, 1.5e5, 2e5, 2.5e5, 3e5, 3.5e5, 4e5] )
                cb.set_ticklabels(["0", "0.5", "1", "1.5", "2", "2.5", "3", "3.5", "4"])
                
                
                plt.show()
                plt.savefig("./Volume/" + OutFigure_name, dpi = 300)
                plt.close()  
                print ('successfully plot ' + OutFigure_name )
        else:
            break   
            
           
            

        # print (ds_data.shape, ds_data.dtype)
        
        
        # print (arr.shape, arr.dtype)
        # print (arr)
        # variable = np.array(data.get('DeltaValue/deltaFlood_veg'))
        # print (dset)
data.close()
    

#--------------------------start making the video--------------------------- 
print ('start making the video' )
# check if there is any file 
def dir_empty(dir_path):
    return not any((True for _ in os.scandir(dir_path)))  


# make the video from the figures

folder_list = ["R", "W","SM", "FS", "PF", "Volume"]

for FolderName in folder_list: 
    
    if not dir_empty('./' + FolderName + '/'):
        
        img_array = []
        
        
        
        file_list = [f for f in os.listdir('./' + FolderName + '/') if f.endswith('.jpg')]
        file_list = sorted(file_list, key=lambda x: int("".join([i for i in x if i.isdigit()])))
        
        
        for filename in file_list:
            
            
            
            img = cv2.imread('./' + FolderName + '/' + filename)
            height, width, layers = img.shape
            size = (width,height)
            img_array.append(img)
        
        
        out = cv2.VideoWriter(FolderName + '.avi',cv2.VideoWriter_fourcc(*'DIVX'), 5, size)
         
        for i in range(len(img_array)):
            out.write(img_array[i])
        out.release()




 
    
    
    
    
    
    
    
    
    
    

    
    
    
