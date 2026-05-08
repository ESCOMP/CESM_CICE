.. _standard-output:

Stdout Output
=============

Diagnostics from the ice model are written to an ASCII file that contains information from the compilation, a record of the input parameters, and how hemispherically averaged, maximum and minimum values are evolving with the integration. 
Certain error conditions detected within the ice setup script or the ice model will also appear in this file. 
Upon the completion of the simulation, some timing information will appear at the bottom of the file. The file name is of the form **ice.log.$LID**, where ``$LID`` is a timestamp for the file ID. 
It resides in the executable directory. 
The frequency of the diagnostics is determined by the namelist parameter ``diagfreq``. 
Other diagnostic messages appear in the **cesm.log.$LID** or **cpl.log.$LID** files in the executable directory. 
See the CIME scripts documentation.

