# PHyL v1.0
#### What is PHyL v1.0?

PHyL is an open-source, **p**arallelized, and modular modeling software for regional **hy**drologic processes and **l**andslides simulation and prediction. The PHyL mainly includes the following modules: (i) a distributed hydrological model (CREST); (ii) a regional 3D slope stability model; and (iii) a soil moisture downscaling (SMD) method. Parallel computational technique is applied for both hydrological and slope-stability modeling modules, which is further seamlessly coupled by the SMD method. An advanced storage method, visualization, and validation processes are designed to improve the post-processing of the simulation results. PHyL is therefore an advanced and user-friendly tool for regional flood-landslide disaster forecasting.

#### Main characteristics

- Cross-platform application
- Parallel computation in both hydrological module and slope-stability module
- HDF5  Compression to save I/O time and storage
- Post-processing visualization for figures and videos

#### Basic information

- Software: PHyL v1.0

- Languages: Fortran, Python, and CMake

- Operating systems supported: Linux, macOS, and Windows

- Availability: https://github.com/GuodingChen/PHyL_v1.0.git

#### Software (or environment) requirements

- CMake$\geq$3.23
- Fortran compiler: GNU$\geq$11.2, Intel oneAPI
- OpenMP$\geq$4.5
- HDF5$\geq$1.10.*
- Python$\geq$3.8

Note: the PHyL is also expected to be compiled and performed earlier versions of the above environments. The versions are recommended to avoid unforeseen errors.

#### Hardware requirements

- RAM$\geq$3 G
- Number of CPU cores$\geq$2

PHyL can run on most current computer equipment, from personal computers to enterprise-class high-performance computers (HPC).

#### Overview of the file structure

The following file tree includes a detailed description:

```
📦PHyL_v1.0
 ┣ 📂Build (Folder used to store CMake configuring files)
 ┣ 📂DownscalingBasicData (Basic inputs for the soil downsacling module)
 ┃ ┣ 📜TWI_coarse.asc (Topographic wetness index with coarse resolution)
 ┃ ┣ 📜TWI_fine.asc (Topographic wetness index with fine resolution)
 ┃ ┣ 📜aspect_coarse.asc (Geographical aspect angle (in degree) with coarse resolution)
 ┃ ┣ 📜curvature_coarse.asc (Curvature with coarse resolution)
 ┃ ┗ 📜curvature_fine.asc (Curvature with fine resolution)
 ┣ 📂HydroBasics (Basic inputs for the hydrological module)
 ┃ ┣ 📜DEM.asc (Digital elevation model)
 ┃ ┣ 📜FAC.asc (Flow accumulation)
 ┃ ┣ 📜FDR.asc (Flow direction)
 ┃ ┣ 📜Mask.asc (Computational pixels within the basin)
 ┃ ┗ 📜Stream.asc (River channel pixels)
 ┣ 📂ICS (Initial conditions setup for PHyL)
 ┃ ┗ 📋InitialConditions.txt
 ┣ 📂LandslideBasics (Basic inputs for the landslide module)
 ┃ ┣ 📜DEM_fine.asc (digital elevation model with fine resolution)
 ┃ ┣ 📜Soil.asc (Soil texture map)
 ┃ ┣ 📜aspect_fine.asc (Geographical aspect angle (in degree) with fine resolution)
 ┃ ┣ 📜mask_fine.asc (Mask map with fine resolution)
 ┃ ┗ 📜slope_fine.asc (Geographical slope angle (in degree) with fine resolution)
 ┣ 📂OBS (Field observation used for calibration)
 ┃ ┗ 📋Yuehe_Obs.csv (Benchmark river discharge observation of the Yuehe River basin)
 ┣ 📂PETs (Evapotranspiration forcing: here hourly data is taken as an example)
 ┃ ┣ 📜pet2012062700.asc
 ┃ ┣ 📜pet2012062701.asc
 ┃ ┣ 📜pet2012062702.asc
 ┃ ┣ 📜pet2012062703.asc
 ┃ ┗ 📜pet2012062704.asc
 ┣ 📂Params (Modeling paramters in PHyL)
 ┃ ┣ 📜IM.asc (Percentage impervious area)
 ┃ ┣ 📜Ksat.asc (Soil saturate hydraulic conductivity)
 ┃ ┣ 📋Parameters_hydro.txt (Parameters setups for hydrological module)
 ┃ ┣ 📋Parameters_land.txt (Parameters setups for landslide module)
 ┃ ┣ 📋Parameters_parallel.txt (Parallel computational setups)
 ┃ ┣ 📜WM.asc (Soil water storage capacity)
 ┣ 📂Rains (Precipitation forcing: here hourly data is taken as an example)
 ┃ ┣ 📜rain2012062700.asc
 ┃ ┣ 📜rain2012062701.asc
 ┃ ┣ 📜rain2012062702.asc
 ┃ ┣ 📜rain2012062703.asc
 ┃ ┗ 📜rain2012062704.asc
 ┣ 📂Results (Store all the simulation results)
 ┣ 📂States (Intermediate variables for model warming up)
 ┃ ┣ 📜State_2012070200_SI0.asc
 ┃ ┣ 📜State_2012070200_SS0.asc
 ┃ ┗ 📜State_2012070200_W0.asc
 ┣ 📂Visualization (Model visualization data and code)
 ┃ ┣ 📂FS (Store the plots of the factor of safety)
 ┃ ┣ 📂PF (Store the plots of the failure probability)
 ┃ ┣ 📂R (Store the plots of the runoff)
 ┃ ┣ 📂SM (Store the plots of the soil moisture)
 ┃ ┣ 📂Volume (Store the plots of landslide volume)
 ┃ ┣ 📂W (Store the plots of soil water amount)
 ┃ ┣ 📑Plot_all.py (Python code used to plot figures)
 ┃ ┗ 📑VideoMaker.py (Python code used to make videos)
 ┣ 📂include (Compiled files with .mod format)
 ┣ 📂logs (Simulation logs)
 ┣ 📂src (Fortran source code)
 ┃ ┣ 📑BasicModule.f90
 ┃ ┣ 📑CREST_Main_Pre.f90
 ┃ ┣ 📑CREST_Simu.f90
 ┃ ┣ 📑Landslide_module.f90
 ┃ ┣ 📑Parallel_pre.f90
 ┃ ┣ 📑ReadAndWriteMatrix.f90
 ┃ ┣ 📑Runoff_Routing.f90
 ┃ ┣ 📑SoilDownscale_pre.f90
 ┃ ┣ 📑Stability3D.f90
 ┃ ┣ 📑hdf5_utils.f90
 ┃ ┗ 📑main.f90
 ┣ 📋CMakeLists.txt (CMake file)
 ┣ 📋Control.Project (Basic information for simulation)
```

#### Parameters in PHyL

| Parameters    | Description                                                  | Unit       | Ranges      |
| :------------ | :----------------------------------------------------------- | ---------- | ----------- |
| $K_{sat}$     | Soil  saturated hydraulic conductivity                       | $mm/h$     | /           |
| $WM$          | Soil water  storage capacity                                 | $mm$       | /           |
| $B$           | The exponent  of the variable infiltration curve             | -          | [0.05, 1.5] |
| $IM$          | Impervious  area ratio                                       | -          | /           |
| $coeM$        | The overland  runoff velocity coefficient                    | -          | [1, 150]    |
| $expM$        | The overland  flow speed exponent                            | -          | [0.1, 0.55] |
| $coeR$        | The flow  speed ratio of channel to overland                 | -          | [1, 3]      |
| $coeS$        | The flow  speed ratio of interflow to overland               | -          | [0.01, 1]   |
| $KS$          | Overland  reservoir discharge parameter                      | -          | [0.001, 1]  |
| $KI$          | Interflow  reservoir discharge parameter                     | -          | [0.001, 1]  |
| $c_s$         | Soil  cohesion                                               | $kPa$      | /           |
| $\gamma_{s}$  | The unit  weight of dry soil                                 | $kN/m^{3}$ | /           |
| $\varphi$     | The angle of  internal friction (in dgree)                   | °          | /           |
| $\theta_{s}$  | Soil saturated  moisture content                             | $m^3/m^3$  | /           |
| $\theta_{r}$  | Soil residual  moisture content                              | $m^3/m^3$  | /           |
| $\alpha$      | Inverse of  air entry pressure for water saturated soil      | $kPa^{-1}$ | /           |
| $n$           | Pore size  distribution parameter                            | -          | /           |
| $D_{e}$       | Density  sampling for random ellipsoids (potential landslides) | -          | [10, 1000]  |
| $a_{e,min}$   | Minimum of the  major semiaxis of a random ellipse           | m          | /           |
| $a_{e,max}$   | Maximum of  the major semiaxis of a random ellipse           | m          | /           |
| $b_{e,min}$   | Minimum of  the minor semiaxis of a random ellipse           | m          | /           |
| $b_{e,max}$   | Maximum of  the minor semiaxis of a random ellipse           | m          | /           |
| $c_{e,min}$   | Minimum of  the landslide depth                              | m          | /           |
| $c_{e,max}$   | Maximum of  the landslide depth                              | m          | /           |
| $N_{sub}$     | Number of  the sub-basin in a drainage basin                 | -          | /           |
| $N_{tile}$    | Number of  tiles decomposing the whole area                  | -          | /           |
| $N_{Hthread}$ | Number of parallel  processes for the hydrological module    | -          | /           |
| $N_{Lthread}$ | Number of parallel  processes for the slope stability module | -          | /           |

Note: the parameters without ranges are determined by inputs (e.g., soil texture or field data) or user-defined.

#### Compile the PHyL

Enter the folder and open the terminal:

```
git clone https://github.com/GuodingChen/PHyL_v1.0.git
```

Then you'll notice that there are still several missing input folders because the benchmark input was compressed via the link:

```
https://drive.google.com/file/d/1bi-HnDpz9tcfG5OZkgvkMY6JZornETEM/view?usp=sharing
```

You'll get the full file directory when you unzip the file (like the file tree structure above). If you want to test the full series of the forcing data (precipitation and evapotranspiration), please link to:

```
https://drive.google.com/file/d/1QZyMOmp6yVYDrA0xGOr2tlQss74yPhPO/view?usp=sharing
```

Now you are ready to work with the PHyL. Enter the "Build" directory:

```
cd Build
```

Configure the PHyL:

```
cmake ..
```

Build to the target PHyL:

```
make
```

If all goes well, you will easily get the executable: PHyL_v1

#### Run the PHyL for a real project

Users are recommended to prepare all the file as in our benchmark case. In order to facilitate our explanation of the use of the PHyL, we start from our own river basin, Yuehe project.

Modify the "📋Control.Project" to finish the most of the setup: 

1. Set the basic information of the hydrological and slope-stability module input

   ```python
   # Hydrological map  (basic information of the hydrological module input)
   NCols_Hydro		=	598	# Number of columns
   NRows_Hydro		=	650	# Number of rows
   XLLCorner_Hydro		=	108.335815
   YLLCorner_Hydro		=	32.654663
   CellSize_Hydro		=	0.000833	
   # Landslide map	(basic information of the slope-stability module input)
   NCols_Land		=	3481	# Number of columns
   NRows_Land		=	3891	# Number of rows
   XLLCorner_Land		=	108.354919
   YLLCorner_Land		=	32.674500
   CellSize_Land		=	0.000125	
   # define the invalid value
   NoData_value	=	-9999
   ```

2. Confirm computational coordinate system

   ```python
   # Model map Coordinate System  
   # Geographic Coordinate System: GCS
   # Projected Coordinate System: PCS
   CoordinateSystem = GCS 
   ```

3. Set the simulation time information

   ```python
   # MODEL Run Time Information
   # y(year);m(month);d(day);h(hour);u(minute);s(second)
   ##########################################################################
   TimeMark		=	h
   TimeStep		=	1
   StartDate		= 	2012062700   
   LoadState		=	no  # Switch to "yes" for warm-up simulation fashion
   WarmupDate		=	2012070201  # Start date of the warm-up modeling
   EndDate			= 	2012062704  
   SaveState		=	no  # Store intermediate variables for the next warm start
   ```

4. Set the model run style

   ```python
   # MODEL Run Style ("simu" is valid only in this version; "cali_SCEUA" is on the way) 
   ##########################################################################
   RunStyle = simu	# simu, cali_SCEUA
   ModelCore = HydroSlide3D    # Hydro, HydroSlide3D (switch to "Hydro" for pure hydrological modeling)
   ##########################################################################
   # Routing scheme (Note: CLR is more advanced but time-consuming)
   RoutingType =CLR		# JLR (default), CLR
   ##########################################################################
   ```

5. Set input file format and path

   ```python
   # Note: .asc format is default for basic input; both .asc and .hdf5 are optional for all outputs
   HydroBasicFormat	=	asc 
   HydroBasicPath	=	"./HydroBasics/"
   ##########################################################################
   SoilDownscaleFormat	=	asc 
   SoilDownscalePath	=	"./DownscalingBasicData/"
   ##########################################################################
   LandslideFormat	=	asc 
   LandslidePath	=	"./LandslideBasics/"
   ##########################################################################
   ParamFormat	=	asc
   ParamPath	=	"./Params/"
   ##########################################################################
   StateFormat	=	asc
   StatePath	=	"./States/"
   ##########################################################################
   ICSFormat	=	asc
   ICSPath		=	"./ICS/"
   ##########################################################################
   RainFormat	=	asc
   RainPath	=	"./Rains/rain"
   ##########################################################################
   PETFormat	=	asc
   PETPath		=	"./PETs/pet"
   ##########################################################################
   ResultFormat	=	asc  # asc, hdf5  # be aware that .hdf5 is available 
   ResultPath	=	"./Results/"
   ##########################################################################
   CalibFormat	=	asc
   CalibPath	=	"./Calibs/"
   ##########################################################################
   OBSFormat	=	asc
   OBSPath		=	"./OBS/"
   ```

6. Custom output location

   ```python
   #OutPix Information
   # Defaults to 0, but can be any non-zero integer (>0) to output the region of interest
   NOutPixs		=	 0
   OutPixColRow 	=	no
   OutPixName1 	=	Yuehe
   OutPixLong1	=	108.7737
   OutPixLati1		=	32.7327747
   OutPixCol1		=	 0
   OutPixRow1 	= 	120
   ```

7. Set the outlet information

   ```python
   #Outlet Information
   # "yes" for HasOutlet is necessary for this version
   HasOutlet		=	yes
   OutletColRow	=	no
   OutletName		=	Yuehe
   OutletLong		=	108.773696
   OutletLati		=	32.732192
   OutletCol		=	0
   OutletRow		=	0
   ```

8. Decide whether to output 

   ```python
   #Grid Outputs
   # Switch to "yes" to indicate willingness to output
   GOVar_Rain		=	no
   GOVar_PET			=	no
   GOVar_EPot		=	no
   GOVar_EAct		=	no
   GOVar_W				= no
   GOVar_SM			=	no
   GOVar_R				=	no
   GOVar_ExcS		=	no
   GOVar_ExcI		=	no
   GOVar_RS			=	no
   GOVar_RI			=	no
   GOVar_FS3D		=	no
   GOVar_PF			=	no
   GOVar_FVolume	=	no
   GOVar_FArea		=	no
   ```

9. Custom output moments

   ```python
   # Defaults to 0, but can be any non-zero integer (>0) to output any moments of interest
   # For example, if this is set to 6, only the following six moments will be output
   NumOfOutputDates	=	0      # 6
   OutputDate_1 = 2012070400
   OutputDate_2 = 2012070403
   OutputDate_3 = 2012070406
   OutputDate_4 = 2012070412
   OutputDate_5 = 2012070500
   OutputDate_6 = 2012070600
   ```

Now, basic setup has been done. Let's move to parameter setting. 







#### Example of compiling and running the model in DelftBlue: The TU Delft supercomputer

- ##### Hardware

  CPU: 2x Intel XEON E5-6248R 24C 3.0GHz

  Cores: 48

  Memory: 192 GB

  SSD: 480 GB

- ##### Load the necessary modules

  DelftBlue requires loading modules hierarchically, i.e., modules need to be loaded with the right module paths. Other HPC systems are expected to behave in a similar manner. This system is based on [lmod](https://lmod.readthedocs.io/en/latest/). The module organization is hierarchical. This means, that the modules you see depend on the ones you loaded: for example, if you don't load `openmpi` you don't see `hdf5`, etc. The following are the necessary load packages:

  ```bash
  module load 2022r2
  module load openmpi
  module load gcc/11.2.0
  module load hdf5
  module load cmake
  ```

- **Compile the model and run**

  Create a new directory to store the compiled files (e.g., build)：

  ```
  mkdir build
  ```

  Enter the new directory:

  ```
  cd build
  ```

  Compile the project:

  ```
  cmake ..
  ```

  Link and built the target:

  ```
  make
  ```

  Then you will find the executable file along with the `CMakeLists.txt`. You can simply run the model by:

  ```
  ./PHyL
  ```

  Or you can submit your job using slurm workload manager with a `sbatch` command:

  ```
  sbatch name-of-your-submission-script.sh
  ```

  Finally, check the simulation results in the results directory. In addition, you can visualize them using our python script.
