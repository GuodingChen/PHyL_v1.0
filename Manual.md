# PHyL v1.0
A massively parallel framework for regional floods and landslides modeling

#### Languages: Fortran, Python, and CMake

#### Operating systems supported: Linux, macOS, and Windows



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
