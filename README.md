# OSIRIS Simulation Framework with Checkpoint Support and steering Features

[![License](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![OSIRIS](https://img.shields.io/badge/OSIRIS-4.0+-orange.svg)](https://github.com/osiris-code/osiris)
[![Experimental](https://img.shields.io/badge/Under_development-yellow)](https://opensource.org/licenses/MIT)


>Enhanced version of the OSIRIS particle-in-cell (PIC) code framework with checkpoint/restart capabilities for HPC environments. 

For detailed information on usage and features, consult the **[user/dev guide](https://github.com/Luis-Nobrega/OSIRIS_with_Checkpoints/blob/main/Steering_user_dev_guide.md)**. If you're interested in the project methodology, consider reading the **[academic report](https://github.com/Luis-Nobrega/OSIRIS_with_Checkpoints/blob/main/ReportPIC-1106716.pdf)**.

## Summary of Key Features

- **Visualization tools**: Treat and visualize data without dowloading it from HPC clusters
- **Checkpoint/Restart System**: Save simulation state and resume from interruption points
- **HPC Optimization**: Improved resource management for cluster environments
- **Enhanced Diagnostics**: Plasma physics diagnostics editing and addition

## Installation

### Prerequisites
This version uses the same prerequisites as the open source version. For detailed info, consult [OSIRIS QUICKSTART](https://osiris-code.github.io/quickstart/#0-prerequisites).

An overview is as follows:
- Suitable Compilers: `gfortran` ; `gcc` ; `make` 
- Valid MPI library: `OpenMPI`
- External dependencies such as `HDF5`

### Installation Steps
```bash
git clone https://github.com/Luis-Nobrega/OSIRIS_with_Checkpoints.git
cd OSIRIS_with_Checkpoints
```
Then follow the regular installation steps described **[here](https://osiris-code.github.io/quickstart/#2-configure-and-compile-the-codes)**.

If you find yourself having configuration problems, reach out to thales.silva@tecnico.ulisboa.pt.

## Contributing

This version is temporary and intended to integrate the official OSIRIS code after review by senior developers. If you have suggestions or detect bugs, please email support.

## OSIRIS visualization tools 

This project also features two visualization tools developed on the side to further ease researchers lives. 

To **be granted access** to **restricted** features, please email both provided contacts.

### Jupyter notebook

A set of scripts was developed so as to allow forwarding of a jupyter server from a reserved node to a local browser. This was designed for servers that do not use [OpenOnDemand](https://openondemand.org/), as this platform eases the use of jupyter notebooks.

This tool is intended for HPC environments that allow for exclusive node allocation, such as [Deucalion](https://www.fccn.pt/areas-tecnologicas/inovacao/projeto-deucalion/). All testing was done on this HPC environment and this software is **fully public**.

For detailed info, consult the **[user/dev guide](https://github.com/Luis-Nobrega/OSIRIS_with_Checkpoints/blob/main/Jupyter_user_guide.md).**


### VisXD container version 

A contaiener was also developed so as to allow for the use of VisXD, a [GOLP](https://golp.tecnico.ulisboa.pt/) native visualization software for plotting and manipulating multiple sets of HDF5 data. Further scripts aid the initialization of this container and forward porting to a graphical interface. 

The use of this software is **RESTRICTED** as of right now.

For detailed info, consult the **[user/dev guide](https://github.com/Luis-Nobrega/OSIRIS_with_Checkpoints/blob/main/VisXD_user_guide.md).**

## License

Distributed under the MIT License. See LICENSE for more information.

## Developer Contacts

📧 Luís Nóbrega (Undergrad) - luis.nobrega@tecnico.ulisboa.pt

📧 Thales Silva (PhD) - thales.silva@tecnico.ulisboa.pt

Group of Lasers and Plasmas - https://golp.tecnico.ulisboa.pt/

Project Link: https://github.com/Luis-Nobrega/OSIRIS_with_Checkpoints

