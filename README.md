# OSIRIS Simulation Framework with Checkpoint Support

[![License](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![Python](https://img.shields.io/badge/Python-3.6+-blue.svg)](https://www.python.org/)
[![OSIRIS](https://img.shields.io/badge/OSIRIS-4.0+-orange.svg)](https://github.com/osiris-code/osiris)

Enhanced version of the OSIRIS particle-in-cell (PIC) code framework with checkpoint/restart capabilities for HPC environments.

## Key Features

- **Visualization tools**: Treat and visualize data without dowloading it from HPC clusters
- **Checkpoint/Restart System**: Save simulation state and resume from interruption points
- **HPC Optimization**: Improved resource management for cluster environments
- **Enhanced Diagnostics**: Plasma physics diagnostics editing and addition

## Installation

### Prerequisites
- OSIRIS 4.0+ installed
- Python 3.6+
- MPI environment (OpenMPI/MPICH)
- HDF5 libraries

### Installation Steps
```bash
git clone https://github.com/Luis-Nobrega/OSIRIS_with_Checkpoints.git
cd OSIRIS_with_Checkpoints
pip install -r requirements.txt
```

## Contributing

1. Fork the repository

2. Create your feature branch (git checkout -b feature/AmazingFeature)

3. Commit your changes (git commit -m 'Add some AmazingFeature')

4. Push to the branch (git push origin feature/AmazingFeature)

5. Open a Pull Request

## License

Distributed under the MIT License. See LICENSE for more information.

## Contact

Luís Nóbrega - luis.nobrega@tecnico.ulisboa.pt

Thales Silva - thales.silva@tecnico.ulisboa.pt

Project Link: https://github.com/Luis-Nobrega/OSIRIS_with_Checkpoints
