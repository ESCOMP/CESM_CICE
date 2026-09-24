.. _overview:

Introduction
============

What is CICE6?
--------------

This CICE User’s Guide accompanies the CESM3 User’s Guide, and is
intended for those who would like to run CICE coupled, on a supported
platform, and “out of the box”. It includes a quick start guide for
downloading the CESM3 source code and input datasets, and information
on how to configure, build and run the model. The supported
configurations and scripts for building the fully coupled model are
also described in the CESM3 User’s Guide:

http://www.cesm.ucar.edu/models/cesm3

The CICE User’s Guide is intended for users interested in making
modifications to the ice model scripts or namelists within the CESM.
Users interested in modifying the source code or using the standalone
version should see the CICE Code Reference/Developer’s Guide
:cite:`cice15`.

CICE6.6.3 is the current version of the CICE Consortium sea ice model used in CESM3.
It is the result of a community effort to develop a portable, efficient sea
ice model that can be run coupled in a global climate model or
uncoupled as a standalone ice model. CICE6 has been released as the sea
ice component of the Community Earth System Model (CESM), a
fully-coupled global climate model that provides simulations of
Earth's past, present, and future climate states. CICE6 in CESM3 is
supported on different resolution Greenland Pole and tripole grids,
which are identical to those used by the Parallel Ocean Program version 2 (POP2)
and Modular Ocean Model version 6 (MOM6) ocean models. The standard resolution
version is best suited for simulating present-day and future climate scenarios
while the lower resolution option is used for paleoclimate simulations and debugging.

An uncoupled version of CICE6.6.3 is available separately:

https://github.com/CICE-Consortium/CICE

This standalone CICE configuration provides a means of running the sea ice model
independent of the other CESM components. It can read in atmospheric
and ocean forcing, which eliminates the need for the flux coupler, and
the atmosphere, land and ocean data models. It can be run on a reduced
number of processors, or without MPI (Message Passing Interface) for
researchers without access to these computer resources.

CICE is a dynamic-thermodynamic model that includes a subgrid-scale ice
thickness distribution :cite:`cice15`. It uses the energy conserving
thermodynamics of :cite:`turner15` or :cite:`bitz01`, has multiple
layers in each thickness category, and accounts for the influences of
brine pockets within the ice cover. The ice dynamics utilizes the
elastic-viscous-plastic (EVP) rheology of :cite:`hunk97`. Sea ice
ridging has the options of :cite:`roth75b` and :cite:`thor75` or the
newer ridging scheme of :cite:`lipscomb07`.  A slab ocean
mixed layer model is included.  A Scientific Reference Guide
:cite:`cice15` is available that contains more detailed information on
the model physics. The physics available in the uncoupled ice model 
are identical to those in the ice model used in the fully coupled system.  

This document uses the following text conventions:
Variable names used in the code are ``typewritten``. 
Subroutine are given in *italic*.
File and directory names are in **boldface**.

What’s new in CICE6?
--------------------

CICE6 is structurally different from previous versions of CICE and has a separate vertical
column package known as Icepack. Icepack can be run using its standalone driver and can be
found here:

https://github.com/CICE-Consortium/Icepack

The major changes are:

- The sea-level melt pond scheme (``tr_pond_sealvl = .true.``) is the default CESM3 option for melt ponds, representing pond drainage and area-depth evolution using a hypsometric formulation that is more consistent with Stage III pond conditions.

- The one-step congelation growth of Plante et al. is the default.

- The prognostic floe-size distribution (``tr_fsd = .true.``) is available for representing lateral melt, lateral growth, wave fracture, and floe welding, with the number of size classes controlled by ``nfsd``.

Sea-level ponds and floe-size distribution
------------------------------------------

The CICE6 sea-level pond parameterization is intended to capture the
observed behavior of melt ponds that sit near sea level and drain through
ice permeability and macroscopic flaws. In CESM3 this option is enabled by
setting ``tr_pond_sealvl = .true.``. It parameterizes pond depth and pond
area using a hypsometric curve for each thickness category and is designed
for use with the mushy thermodynamics option (``ktherm = 2``) and the
Delta-Eddington shortwave scheme (``shortwave = 'dEdd'``). This is work
led by David Clemens-Sewall (unpublished).

The floe-size distribution (FSD) is an optional prognostic tracer that
tracks the distribution of floe sizes within each thickness category. When
``tr_fsd = .true.``, CICE can represent lateral melt, lateral growth,
wave-induced breakup, and floe welding in addition to the usual thickness
and area evolution. The number of floe-size bins is controlled by
``nfsd`` and may be set to 1, 12, 16, or 24 depending on the desired
resolution. When wave forcing is provided, the FSD can interact with
surface wave spectra through the ``wave_spec_type`` namelist option.
When CICE with the FSD is active and WW3 is active, this sets a mediator
flag ``wav_coupling_to_cice``. This means that the ice concentration, ice
thickness, and floe diameter are sent to the mediator and WW3 sends the
elevation spectrum. (:cite:'Roach19`, Tremblay and Roach, 2026, in review).

The CICE source code used in the CESM is based on the CICE Consortium
Sea Ice Model version 6. The main source code is very similar
in both versions, but the drivers are significantly different. If there 
are topics that are not covered in this CICE documentation, users are
encouraged to look at the CICE science, user, and developer guide documentation available at:

https://cice-consortium-cice.readthedocs.io/en/main/
