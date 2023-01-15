# PHyL v1.0
A massively parallel framework for regional floods and landslides modeling

#### Languages: Fortran, Python, and CMake

#### Operating systems supported: Linux, macOS, and Windows



```python
📦PHyL_v1.0
 ┣ 📂Build
 ┣ 📂DownscalingBasicData
 ┃ ┣ 📜TWI_coarse.asc
 ┃ ┣ 📜TWI_fine.asc
 ┃ ┣ 📜aspect_coarse.asc
 ┃ ┣ 📜curvature_coarse.asc
 ┃ ┗ 📜curvature_fine.asc
 ┣ 📂HydroBasics
 ┃ ┣ 📜DEM.asc
 ┃ ┣ 📜FAC.asc
 ┃ ┣ 📜FDR.asc
 ┃ ┣ 📜Mask.asc
 ┃ ┗ 📜Stream.asc
 ┣ 📂ICS
 ┃ ┗ 📋InitialConditions.txt
 ┣ 📂LandslideBasics
 ┃ ┣ 📜DEM_fine.asc
 ┃ ┣ 📜Soil.asc
 ┃ ┣ 📜Soil_ori.asc
 ┃ ┣ 📜aspect_fine.asc
 ┃ ┣ 📜mask_fine.asc
 ┃ ┗ 📜slope_fine.asc
 ┣ 📂OBS
 ┃ ┗ 📋Yuehe_Obs.csv
 ┣ 📂PETs (only )
 ┃ ┣ 📜pet2012062700.asc
 ┃ ┣ 📜pet2012062701.asc
 ┃ ┣ 📜pet2012062702.asc
 ┃ ┣ 📜pet2012062703.asc
 ┃ ┗ 📜pet2012062704.asc
 ┣ 📂Params
 ┃ ┣ 📜IM.asc
 ┃ ┣ 📜Ksat.asc
 ┃ ┣ 📋Parameters_hydro.txt
 ┃ ┣ 📋Parameters_land.txt
 ┃ ┣ 📋Parameters_parallel.txt
 ┃ ┣ 📜WM.asc
 ┃ ┗ 📜coeM.asc
 ┣ 📂Rains
 ┃ ┣ 📜rain2012062700.asc
 ┃ ┣ 📜rain2012062701.asc
 ┃ ┣ 📜rain2012062702.asc
 ┃ ┣ 📜rain2012062703.asc
 ┃ ┗ 📜rain2012062704.asc
 ┣ 📂Results
 ┣ 📂States
 ┃ ┣ 📜State_2012070200_SI0.asc
 ┃ ┣ 📜State_2012070200_SS0.asc
 ┃ ┗ 📜State_2012070200_W0.asc
 ┣ 📂Visualization
 ┃ ┣ 📂FS
 ┃ ┣ 📂PF
 ┃ ┣ 📂R
 ┃ ┣ 📂SM
 ┃ ┣ 📂Volume
 ┃ ┣ 📂W
 ┃ ┣ 📑Plot_all.py
 ┃ ┗ 📑VideoMaker.py
 ┣ 📂include
 ┣ 📂logs
 ┣ 📂src
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
 ┣ 📋CMakeLists.txt
 ┣ 📋Control.Project
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

  Finally, harvest your simulation results in the results directory. In addition, you can visualize them using our python script.
