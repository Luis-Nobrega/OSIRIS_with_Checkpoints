# OSIRIS Steering Module User and Developer Guide  
**Authors**: Luís Nóbrega, Thales Silva  
📧 `luis.nobrega@tecnico.ulisboa.pt` | `thales.silva@tecnico.ulisboa.pt`  

---

## Table of Contents
- [Introduction](#introduction)
- [1. Usage](#1-usage)
  - [1.1 Disabling the Module](#11-disabling-the-module)
  - [1.2 Setting Steering Frequency](#12-setting-steering-frequency)
  - [1.3 Steering File Format](#13-steering-file-format)
- [2. Functionalities](#2-functionalities)
  - [2.1 Basic Commands](#21-basic-commands)
  - [2.2 Diagnostic Commands](#22-diagnostic-commands)
- [3. Moving Window](#3-moving-window)
- [4. Automatic Restart](#4-automatic-restart)
- [5. Developer Guide](#5-developer-guide)
  - [5.1 Core Modules](#51-core-modules)
  - [5.2 Key Functions](#52-key-functions)
  - [5.3 Data Structures](#53-data-structures)
  - [5.4 Limitations](#54-limitations)
  - [5.5 Future Development](#55-future-development)
- [Appendix: Fortran Modules](#appendix-fortran-modules)
  - [os-workflow.f03](#os-workflowf03)
  - [os-workflow_reader.f03](#os-workflow_readerf03)
- [Support & Contact](#support--contact)

---

## Introduction
The [OSIRIS](https://osiris-code.github.io/) open-source project aims to provide a Particle-in-cell (PIC) code for laser-plasma physics. Given the rigidity of the code, a first attempt at allowing steering during simulations was made.

The newly developed steering module enables runtime modification of OSIRIS simulation parameters without restarting. 

Key features include:
- Dynamic parameter adjustments
- On-demand checkpoint creation  
- Diagnostic frequency control  

⚠️ **Experimental Status**: This code was developed in a short period by an undergrad student. It mainly serves as a proof of concept for a definitive future expansion of OSIRIS. Not all exceptions are handled. Please report issues to the authors.

---

## 1. Usage
This module was built on top of the opersource version of OSIRIS and can be downloaded [here](https://github.com/Luis-Nobrega/OSIRIS_with_Checkpoints). 
### 1.1 Disabling the Module
Add to config file:  
```fortran
DISABLE_STEERING=1
```
Reconfigure (i.e):
```fortran
./configure -d 2 -s linux.gnu
```

### 1.2 Setting Steering Frequency

Mandatory in `time_step` namelist:

```fortran
time_step {
  dt = 0.07,
  ndump = 5,
  steering_step = 50   ! Check every 50 steps
}
```

Deactivate with `steering_step=0`. Can be deactivated while the simulation is running. If not set, will default to 0.

### 1.3 Steering File Format

- **Filename:** `steering_input_deck`

- **Format:** `key = value`

- **Processing:** File renamed to `steering_input_deck.used` after reading

**Example:**

```fortran
! Example steering commands
checkpoint = 1
tmax = 3.5
diag_emf_1 = [b1; ndump_fac; 5]
```
**Rules:**

- Comments must be on separate lines (`! comment`)

- Last command wins (overwrites previous)

- Blank lines ignored

## 2. Functionalities

### 2.1 Basic commands 

| **Key**           | **Description**                              | **Value Type**                   |
|-------------------|----------------------------------------------|----------------------------------|
| `checkpoint`      | Create restart files without stopping        | Integer (0/1)                    |
| `stop`            | Create restart files and stop simulation     | Integer (0/1)                    |
| `abort`           | Stop without restart files                   | Integer (0/1)                    |
| `tmax`            | New maximum simulation time                  | Real > current time              |
| `steering_step`   | Change check frequency                       | Integer > 0                      |
| `omega_p0`        | Plasma frequency                             | Real > 0                         |

## 2.2 Diagnostic Commands

**Structure:**  

```fortran
diag_type_N = [identifier; command; value1; value2; ...]
```

- type: `emf`, `current`, `species`, `neutral`

- N: Unique command ID

- identifier: Report name (e.g., `b1`, `e1`)

- command: Parameter to modify (e.g., `ndump_fac`, `n_tavg`). See [OSIRIS Reference Guide](https://osiris-code.github.io/osiris/reference/) for more details.

**Examples:**

```fortran
! Change EMF dump frequency
diag_emf_1 = [e1; ndump_fac; 10]

! Add time averaging
diag_emf_2 = [e1; n_tavg; 50]
```

⚠️ Critical Notes:

- Species diagnostics require `+` for averaged reports:

```fortran
diag_species_1 = [electrons+; q1; ndump_fac; 2]
```

- New spatial/time averages require dependent parameters

- Changes don't persist through restarts (resubmit commands)

# 3. Moving Window

Modify species background distributions dynamically:

```fortran
math_func_expr = [species_name; "custom_function"]
```

**Requirements:**

- Initial distribution must use custom math function
- Predefined profiles not supported

# 4. Automatic Restart

❌ Not Functional (June 2025)
Experimental script `Preemptible_HPC_OSIRIS.sh` detects cancellations and restarts from last checkpoint.

Contact support if you need this feature.

# 5. Developer Guide

## 5.1 Core Modules

| **File**                  | **Purpose**                                     |
|---------------------------|-------------------------------------------------|
| `m_workflow.f03`          | Steering logic and command execution            |
| `m_workflow_reader.f03`   | Steering file parsing and MPI comms             |
| `main.f03`                | Integration with main simulation loop           |
| `m_restart.f03`           | Checkpoint handling                             |

## 5.2 Key Functions

- `check_workflow_step()`: Main steering trigger

- `parse_workflow_diagnostic()`: Decodes diagnostic commands

- `steering_<TYPE>_diag()`: Modifies diagnostics (EMF/current/species)

- `add_<TYPE>_report()`: Creates new diagnostics dynamically

## 5.3 Data Structures

```fortran
! Key-value storage
type term_value_pair
  character(:), allocatable :: term, value
end type

! MPI-aware command collection
type term_value_collection
  type(term_value_pair), allocatable :: pairs(:)
  integer :: count = 0
end type
```

##  5.4 Limitations

**Critical Issues:**

1. Neutral/species diagnostics may dereference null pointers

2. Temporal averaging crashes if tavg_data unallocated

3. No type checking for command values

4. Species distribution type changes unreliable

5. Charge conservation diagnostics disabled

**Functional Gaps:**

- Particle diagnostic addition has problems and is incomplete

- Phasespace diagnostics not yet implemented

- No state rollback on failure

## 5.5 Future Development

**Priority Fixes:**

- Safe species initialization

- Error propagation via MPI

- Memory allocation checks

**Enhancements:**

- Laser turn on/off

- Failsafe mechanism

- HPC integration improvements

- AI tool implementation

- Further paramether handling

# Appendix: Fortran Modules
## os-workflow.f03

**Core Subroutines:**

## Control subrouines (Steering)

| **Subroutine**             | **Arguments**                                               | **Description**                          |
|---------------------------|-------------------------------------------------------------|------------------------------------------|
| `set_workflow_step`       | `(step, sim)`                                               | Sets steering frequency                  |
| `check_workflow_step`     | `(file_ok, sim, no_co, steering_exit)`                      | Main steering trigger                    |
| `check_and_execute`       | `(sim, steering_exit)`                                      | Executes steering commands               |
| `steering_<TYPE>_diag`    | `(sim, report_spec, command, values, ierr)`                 | Modifies diagnostics                     |
| `add_<TYPE>_report`       | `(sim, input_string)`                                       | Creates new diagnostics                  |
| `write_restart`           | `(sim)`                                                     | Checkpoint writer                        |


## os-workflow_reader.f03

**Core Subroutines:**

## Parsing and utility subroutines

| **Subroutine**                    | **Arguments**                                              | **Description**                         |
|----------------------------------|------------------------------------------------------------|-----------------------------------------|
| `read_steering_file`             | `(no_co, file_content, iostat)`                           | Parses steering file                    |
| `parse_workflow_diagnostic`      | `(string, name, id, cmd, data, add, ierr)`                | Decodes diagnostic commands             |
| `str_array_to_<TYPE>`            | `(str_array, out_array, ierr)`                            | Type conversion utilities               |
| `get_value`                      | `(key)`                                                   | Retrieves command value                 |
| `get_keys`                       | `()`                                                      | Returns all command keys                |


# Support & Contact

For issues/questions:

📧 Luís Nóbrega: luis.nobrega@tecnico.ulisboa.pt

or 

📧 Thales Silva: thales.silva@tecnico.ulisboa.pt

GitHub: [OSIRIS_with_Checkpoints](https://github.com/Luis-Nobrega/OSIRIS_with_Checkpoints/tree/main)