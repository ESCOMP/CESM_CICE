.. _troubleshooting:

Troubleshooting
===============

Code does not Compile or Run 
----------------------------

Check the **ice.log.\*** or **ice.bldlog.\*** files in the executable
directory, or the standard output and error files for information. Also,
try the following:

-  Delete the executable directory and rebuild the model.

-  Make sure that there is a **Macros.\< OS \>** file
   for your platform. Modify the directory paths for the libraries.

-  Make sure all paths and file names are set correctly in the scripts.

-  If changes were made to the **ice\_domain\_size.F90** file in the
   source code directory, they will be overwritten by the file in
   **input\_templates**.

Departure points out of bounds
------------------------------

This error is written from **ice\_transport\_remap.F90** when the ice
speed is causing parcels of ice to go beyond a grid cell. This is akin
to a CFL violation. Generally changing the timestep in **env_run.xml**
with ``ATM_NCPL`` will allow the model to proceed. Note this can only
be done for hybrid or startup runs. One can try just adjusting the dynamic
timestep as described in the next section.

Negative Ice Area in Horizontal Remapping
-----------------------------------------

This error is written from **ice\_transport\_remap.F90** when the ice
model is checking for negative ice areas. If it happens well into a
model integration, it can be indicative of a CFL violation. The output
looks like:

::

      60: New area < 0, istep = 119588
      60: (my_task,i,j,n) = 4 21 380 1
      60: Old area = 0.960675000975677174E-05
      60: New area = -0.161808948357841311E-06
      60: Net flux = -0.976855895811461324E-05
      60:(shr_sys_abort) ERROR: remap transport: negative area
      60:(shr_sys_abort) WARNING: calling shr_mpi_abort() and stopping
      60:(shr_mpi_abort):remap transport: negative area 0

The dynamics timestep should be reduced to integrate past this problem.
In **user\_nl\_cice** set

::

     ndtd = 2

and restart the model. When the job completes set the
value back to 1.

Picard convergence error
------------------------

This is an error from the mushy layer thermodynamics ``ktherm = 2``. One
can try changing ``nit_max`` in the **ice_therm_mushy.F90** code, but
this does not often help. Most likely this is an indication of a problem
in the forcing. Sometimes reducing the overall timestep may help.

Tsn init problems
-----------------

Sometimes the surface temperature or snow temperature at the beginning
of the thermodynamic iteration may become unrealistic. The lower bound
on this error is -100C. This either indicates a problem with the CICE
initial file or the forcing. Changing the timestep will not help.

Thermodynamic Iteration Error
-----------------------------

This error is written from **ice\_therm\_vertical.F90** when the ice
model temperature iteration is not converging in the thermodynamics.
This is usually a problem with the forcing from the atmosphere or ocean, 
but sometimes can be indicative of a timestep problem in the ice.
Check the forcing files at point i,j first.

::

      Thermo iteration does not converge
      istep1, my_task, i, j:

Conservation Error
------------------

This error is written from **ice\_itd.F** when the ice model is checking
that initial and final values of a conserved field are equal to within a
small value. The output looks like:

::

      Conservation error: vice, add_new_ice
      11  :  14  185
      Initial value =  1362442.600400560
      Final value =  1362442.600400561
      Difference =  2.328306436538696D-10
      (shr_sys_abort) ERROR: ice: Conservation error
      (shr_sys_abort) WARNING: calling shr_mpi_abort() and stopping
      (shr_mpi_abort):ice: Conservation error  0

Non-conservation can occur if the ice model is receiving very bad
forcing, and is not able to deal with it. This has occurred after a CFL
violation in the ocean. The timestep in the ocean may be decreased to
get around the problem.

NX does not divide evenly into grid
-----------------------------------

If you modify the number of tasks used by the ice model, the model may
stop with this error written to the log file:

::

    'ERROR: NX must divide evenly into grid,100,8'

The number of MPI processors used by the ice model must divide evenly
into the grid dimensions. For example, running the ice model with 8
tasks on the gx3v7 grid will result in an error, since 8 does not divide
evenly into the 100 longitude points. To fix this error, change the
value of $NTASKS for the uncoupled ice model in the main script. In this
case, a value of 4 would work, and the task geometry would also have to
be changed.

Enabling the Debugger
---------------------

This section explains how to set some compiler options for debugging.
For the coupled model, set DEBUG to TRUE in the **env\_build.xml** script.
Before running the model, be sure to delete the object files or a clean build
so that the source code will be recompiled. If a core file is created, it will be in
the executable directory. Use some debugging tools for your platform to look 
at the core file. Useful information may also appear in the standard error and output files.
