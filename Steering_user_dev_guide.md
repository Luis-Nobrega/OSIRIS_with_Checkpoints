# OSIRIS Steering Module User/Developer Guide  
**Authors**: Luís Nóbrega, Thales Silva  
📧 `luis.nobrega@tecnico.ulisboa.pt` | `thales.silva@tecnico.ulisboa.pt`  

**Technical Advisory**: Ricardo Fonseca 
📧 `ricardo.fonseca@iscte-iul.pt`

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
  - [2.3 Valid diagnostic commands](#23-valid-diagnostic-commands)
    - [2.3.1 Current and EMF](#231-current-and-emf)
    - [2.3.2 Neutrals](#232-neutrals)
    - [2.3.3 Species](#233-species)
    
- [3. Moving Window](#3-moving-window)
- [4. Automatic Restart](#4-automatic-restart)
- [5. Developer Guide](#5-developer-guide)
  - [5.1 Reading file](#51-reading-file)
  - [5.2 Executing file](#52-executing-file)
  - [5.3 Limitations](#53-limitations)
  - [5.4 Future Development](#54-future-development)
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
This module was built on top of the opersource version of OSIRIS. The donwload as well as installing instructions cane be found  **[here](https://github.com/Luis-Nobrega/OSIRIS_with_Checkpoints)**. 

### 1.1 Disabling the Module
By default, the steering module will be compiled alongside OSIRIS. If you want to disable it, add the following flag to the config file:

```fortran
DISABLE_STEERING=1
```
Only then configure it as you desire (e.g):
```fortran
./configure -d 2 -s linux.gnu
```

### 1.2 Setting Steering Frequency

To check the existance of a steering file every **N** steps (absolute number of steps), add the `time_step` block in your namelist as follows:

```fortran
time_step {
  dt = 0.07,
  ndump = 5,
  steering_step = 50   ! <- Check steering file every 50 steps
}
```

This command can be deactivated by setting `steering_step=0`. If not defined, **defaults to 0**. 

️️️️*Note that this frequency can be changed at any point in the  simulation!* 
️

### 1.3 Steering File Format

To pass commands to OSIRIS during the simulation, create a file named `steering_input_deck`. 

- The steering module will verify it's existance duiring runtime and read its contents. 
- Only commands on the format `key = value` will be parsed and everything after a `!` will be ignored.  
- Both `key` and `value` should be strings. Blank spaces will be parsed!
- The format of `value` varies on the type of instruction and is described below. 
- If the command format, `key` or `value` are invalid, it will be ignored. 
- If there is the same command, the later one is executed. 
- Blank lines are ignored 
- After reading the file and executing commands, the steering module renames it to `steering_input_deck.used`
- **FIRST** command to be read is the first to be processed. Leave critical commands to the EOF and don't forget to ask for a checkpoint.

**Example:**

```fortran
! this line is ignored

checkpoint = 1  ! Comment 
tmax = 3.5 
diag_emf_1 = [b1; ndump_fac; 5] 
```

## 2. Functionalities

A list of valid formats for the `value` are described below:

### 2.1 Basic commands 

Commands that involve changing variables, such as the steering step or to ask for checkpoint files are considered basic. This means the info passed in `value` is only a number representing a numeric or boolean value.


| **Key**           | **Description**                              | **Value Type**                   |
|-------------------|----------------------------------------------|----------------------------------|
| `checkpoint`      | Create restart files without stopping        | Integer (0/1)                    |
| `stop`            | Create restart files and stop simulation     | Integer (0/1)                    |
| `abort`           | Stop without restart files                   | Integer (0/1)                    |
| `tmax`            | New maximum simulation time                  | Real > current time              |
| `steering_step`   | Change check frequency                       | Integer > 0                      |
| `omega_p0`        | Plasma frequency  (for radiation reaction)                           | Real > 0                         |

**Example:**

```fortran
checkpoint = 1  ! Performs checkpoint
tmax = 3.5 ! Alters maximum simulation time
steering_step = 0 ! Stops steering verifications completely
```

To add new features, please contact support!

## 2.2 Diagnostic Commands

**Structure:**  

Diagnostics are data produced by the simulation used to analyse its evolution. Therefore, a special format of `key = value` was defined.

This format is as follows:

```fortran
diag_type_N = [particle_name; identifier; command; value1; value2; ...]
```

**`key` Command options:** 

- **type**: `emf`, `current`, `species` or  `neutral`

- **N**: Unique command ID defined by the user. **DO NOT** repeat N in commands with the same type.

**`value` Command options:** 

- **particle_name**: Only useful for `species` or  `neutral` as there can be multiple objects of these types. For the remaining types use a random value for this option. The code is designed to parse an optional `+` at the end of the particle name. 

- **identifier**: Report name (e.g., `b1`, `e1`). See [diagnostics](https://osiris-code.github.io/osiris/reference/) for specific report names.

- **command**: Parameter to modify (e.g., `ndump_fac`, `n_tavg`). See [OSIRIS Reference Guide](https://osiris-code.github.io/osiris/reference/) for more details.

- **value1-3**: Some parameters are defined by more than 1 value. 

**Examples:**

```fortran
! Change EMF dump frequency
diag_emf_1 = [e1; ndump_fac; 10]

! Add time averaging
diag_emf_2 = [e1; n_tavg; 50]

! Change spacial averaging for current density
diag_current_3 = [test+; j2; n_ave; 2; 2]

! note that diag_species_1 would also be a valid name
diag_species_4 = [electrons+; q2; ndump_fac; 3]
```

⚠️ Critical Notes:

- Species diagnostics require `+` for cell averaged reports:

```fortran
diag_species_1 = [electrons+; q1; ndump_fac; 2]
```

- The usage of `+` for other type of diagnostics is ignored.

- Adding new spatial/time averages requires adding and initializing the reports, first. See next section for more info.

- Changes don't persist through restarts. If you restart from a checkpoint, resubmit the same namelist.

## 2.3 Valid diagnostic commands

There is currently **NO FAILSAFE** when it comes to shielding the code from initalization errors caused by changing/adding diagnostics through steering. 

Given this, **ALWAYS** ask for checkpoint data if you are unsure, and put this as the **FIRST** command in the namelist.

Before using this feature, first determine: 

>- If the report was already initialized
>- What quantities of the report were initialized (tavg, savg, tavg-savg)

This is important as:

>- A report, such as `b1`, can contain `tavg`, `savg`, `tavg-savg` or regular data, and only some of them might be ininitialized. These will pose as secundary reports in your directoryeg: `b1-tavg`
>- If a report is pre-initialized, adding/changing other quantities such as `tavg` is straight forward
>- If a report isn't initialized at all, to add secondary quantities, such as `b1-tavg` you **MUST** initialize `b1` first. 

Case by case examples are shown below:

### 2.3.1 Current and EMF

!!! UNDER DEVELOPMENT !!!

### 2.3.2 Neutrals

!!! UNDER DEVELOPMENT !!!

### 2.3.3 Species

!!! UNDER DEVELOPMENT !!!

# 3. Moving Window

A special feature that allows for the dynamic modification of the backgorund density of species. This is a physically interesting feature.

The `key = value` format is as follows:


```fortran
math_func_expr = [species_name; "custom_function"]
```

Check [OSIRIS Reference Guide](https://osiris-code.github.io/osiris/reference/) for available math functions.

**WARNING:**

- Initial distribution **MUST** use custom math function
- Predefined profiles not yet supported
- Neutral profile modification not yet supported but all species dependent on neutrals are supported

# 4. Automatic Restart

❌ Not Functional (June 2025)

Experimental script `Preemptible_HPC_OSIRIS.sh` is meant to detect if there was a hault in the simulation and rerun it. This feature is designed for Preemptive clusters.

Contact support if you need this feature.

# 5. Developer Guide

This part of the guide is intended to give insightful information to whoever wants to add/change code features.

There are two main files in the steering module. 

## 5.1 Reading file 

This file is responsible for all parsing functions as well as reading the input deck. 

**Core Procedures:**

```fortran
 subroutine read_steering_file(no_co, file_content, iostat)
```
  Reads and parses the steering file content into internal storage.
  - `no_co` → `type(t_node_conf), intent(in), target` (node configuration object)
  - `file_content` → `character(len=*), intent(in)` (raw text of steering file)
  - `iostat` → `integer, intent(out), optional` (I/O status code)

---

```fortran
 subroutine parse_steering_data(file_content, iostat)
```
  Parses the raw steering file content line by line.
  - `file_content` → `character(len=*), intent(in)`
  - `iostat` → `integer, intent(out)`

---

```fortran
 subroutine read_key_value_lines(file_unit, processor, iostat)
```
  Reads key-value pairs from a file unit and processes them.
  - `file_unit` → `integer, intent(in)` (open Fortran file unit)
  - `processor` → `procedure` (callback to handle parsed key/value)
  - `iostat` → `integer, intent(out)`

---

```fortran
 subroutine processor(key, value, success)
```
  Generic processor for key-value pairs.
  - `key` → `character(len=*), intent(in)`
  - `value` → `character(len=*), intent(in)`
  - `success` → `logical, intent(out)` (true if parsing was successful)

---

```fortran
 subroutine process_pair(key, value, success)
```
  Processes and validates a parsed key-value pair.
  - `key` → `character(len=*), intent(in)`
  - `value` → `character(len=*), intent(in)`
  - `success` → `logical, intent(out)`

---

```fortran
 subroutine add_or_update_term(this, key, value)
```
  Adds a new key-value pair to collection or updates if key already exists.
  - `this` → `class(term_value_collection), intent(inout)`
  - `key` → `character(len=*), intent(in)`
  - `value` → `character(len=*), intent(in)`

---

```fortran
 subroutine parse_workflow_diagnostic(string, name, identifier, command, data, add, ierr)
```
  Parses a diagnostic command into components.
  - `string` → `character(len=*), intent(in)` (raw command)
  - `name, identifier, command, data` → `character(len=:), allocatable, intent(out)`
  - `add` → `logical, intent(out)` (flag to add diagnostic)
  - `ierr` → `integer, intent(out)` (error code)

---

```fortran
 subroutine trim_diagnostic(string, trimmed_name, ierr)
```
  Cleans and extracts the diagnostic name.
  - `string` → `character(len=*), intent(in)`
  - `trimmed_name` → `character(len=:), allocatable, intent(out)`
  - `ierr` → `integer, intent(out)`

---

```fortran
 subroutine parse_bracketed_pair(string, data1, data2, ierr)
```
  Parses a string of the form [a; b] into two parts.
  - `string` → `character(len=*), intent(in)`
  - `data1, data2` → `character(len=:), allocatable, intent(out)`
  - `ierr` → `integer, intent(out)`

---

```fortran
 subroutine str_array_to_int(str_array, int_array, ierr)
```
  Converts an array of strings to integers.
  - `str_array` → `character(len=:), allocatable, intent(in)`
  - `int_array` → `integer, allocatable, intent(out)`
  - `ierr` → `integer, intent(out)`

---

```fortran
 subroutine str_array_to_real(str_array, real_array, ierr)
```
  Converts an array of strings to reals.
  - `str_array` → `character(len=:), allocatable, intent(in)`
  - `real_array` → `real(p_double), allocatable, intent(out)`
  - `ierr` → `integer, intent(out)`

---

```fortran
 subroutine str_array_to_logical(str_array, logical_array, ierr)
```
  Converts an array of strings to logicals.
  - `str_array` → `character(len=:), allocatable, intent(in)`
  - `logical_array` → `logical, allocatable, intent(out)`
  - `ierr` → `integer, intent(out)`

---

```fortran
 subroutine determine_array_type(str_array, int_array, real_array, logical_array, ierr)
```
  Determines the correct type of an array of strings and converts accordingly.
  - `str_array` → `character(len=:), allocatable, intent(in)`
  - `int_array` → `integer, allocatable, intent(out)`
  - `real_array` → `real(p_double), allocatable, intent(out)`
  - `logical_array` → `logical, allocatable, intent(out)`
  - `ierr` → `integer, intent(out)`

---

```fortran
 function get_value(key)
```
  Returns stored value for a given key.
  - `key` → `character(len=*), intent(in)`

---

```fortran
 function get_size()
```
  Returns total number of stored key-value pairs.

---

```fortran
 function get_term_value(this, key)
```
  Retrieves the value for a given key from collection.
  - `this` → `class(term_value_collection), intent(in)`
  - `key` → `character(len=*), intent(in)`

---

```fortran
 function collection_size(this)
```
  Returns number of elements in a collection.
  - `this` → `class(term_value_collection), intent(in)`

---

```fortran
 function get_keys()
```
  Returns all stored keys in the collection.

---


## 5.2 Executing file 

This file contains the logic for applying steering commands to the simulation. It interprets parsed key-value pairs and modifies simulation state accordingly.

**Core Procedures:**

```fortran
 subroutine set_workflow_step(step, sim)
```
  Set workflow step procedure.

  - `step` → `integer, intent(in)`
  - `sim` → `class(t_simulation), intent(inout)`

---

```fortran
 subroutine check_file_exists(filename, file_exists, file_size, file_content)
```
  Check file exists procedure.

  - `filename` → `character(len=*), intent(in)`
  - `file_exists` → `logical, intent(out)`
  - `file_size` → `integer, intent(out)`
  - `file_content` → `character(:), allocatable, intent(out)`

---

```fortran
 subroutine rename_file(old_name, new_name, success)
```
  Rename file procedure.

  - `old_name` → `character(len=*), intent(in)`
  - `new_name` → `character(len=*), intent(in)`
  - `success` → `logical, intent(out)`

---

```fortran
 subroutine check_workflow_step(file_ok, sim, no_co, steering_exit)
```
  Check workflow step procedure.

  - `file_ok` → `logical, intent(out)`
  - `sim` → `type(t_simulation), intent(inout)`
  - `no_co` → `type(t_node_conf), intent(in)`
  - `steering_exit` → `logical, intent(out)`

---

```fortran
 subroutine check_and_execute(sim, steering_exit)
```
  Check and execute procedure.

  - `sim` → `class(t_simulation), intent(inout)`
  - `steering_exit` → `logical, intent(out)`

---

```fortran
 subroutine steering_emf_diag(sim, report_spec, command_name, new_value, ierr)
```
  Steering emf diag procedure.

  - `sim` → `class(t_simulation), intent(inout)`
  - `report_spec` → `character(*), intent(in)`
  - `command_name` → `character(*), intent(in)`
  - `new_value` → `integer, dimension(:), intent(in)`
  - `ierr` → `integer, intent(out)`

---

```fortran
 subroutine add_emf_report(sim, input_string)
```
  Add emf report procedure.

  - `sim` → `class(t_simulation), intent(inout)`
  - `input_string` → `character(len=*), intent(in)`

---

```fortran
 subroutine steering_current_diag(sim, report_spec, command_name, new_value, ierr)
```
  Steering current diag procedure.

  - `sim` → `class(t_simulation), intent(inout)`
  - `report_spec` → `character(*), intent(in)`
  - `command_name` → `character(*), intent(in)`
  - `new_value` → `integer, dimension(:), intent(in)`
  - `ierr` → `integer, intent(out)`

---

```fortran
 subroutine add_current_report(sim, input_string)
```
  Add current report procedure.

  - `sim` → `class(t_simulation), intent(inout)`
  - `input_string` → `character(len=*), intent(in)`

---

```fortran
 subroutine steering_neutral_diag(sim, report_spec, particle_name, command_name, new_value, ierr)
```
  Steering neutral diag procedure.

  - `sim` → `class(t_simulation), intent(inout)`
  - `report_spec` → `character(*), intent(in)`
  - `particle_name` → `character(*), intent(in)`
  - `command_name` → `character(*), intent(in)`
  - `new_value` → `integer, dimension(:), intent(in)`
  - `ierr` → `integer, intent(out)`

---

```fortran
 subroutine add_neutral_report(sim, input_string, particle_name)
```
  Add neutral report procedure.

  - `sim` → `class(t_simulation), intent(inout)`
  - `input_string` → `character(len=*), intent(in)`
  - `particle_name` → `character(*), intent(in)`

---

```fortran
 subroutine steering_species_diag(sim, report_spec, particle_name, command_name, new_value, averaged_quant, ierr)
```
  Steering species diag procedure.

  - `sim` → `class(t_simulation), intent(inout)`
  - `report_spec` → `character(*), intent(in)`
  - `particle_name` → `character(*), intent(in)`
  - `command_name` → `character(*), intent(in)`
  - `new_value` → `integer, dimension(:), intent(in)`
  - `averaged_quant` → `logical, intent(in)`
  - `ierr` → `integer, intent(out)`

---

```fortran
 subroutine add_species_report(sim, input_string, name, averaged_quant)
```
  Add species report procedure.

  - `sim` → `class(t_simulation), intent(inout)`
  - `input_string` → `character(len=*), intent(in)`
  - `name` → `character(len=*), intent(in)`
  - `averaged_quant` → `logical, intent(in)`

---

```fortran
 subroutine steering_particles_diag(sim, report_spec, command_name, new_value, ierr)
```
  Steering particles diag procedure.

  - `sim` → `class(t_simulation), intent(inout)`
  - `report_spec` → `character(*), intent(in)`
  - `command_name` → `character(*), intent(in)`
  - `new_value` → `integer, dimension(:), intent(in)`
  - `ierr` → `integer, intent(out)`

---

```fortran
 subroutine add_particles_report()
```
  Add particles report procedure.


---

```fortran
 subroutine parse_report_spec(report_spec, quant, details, item_type, direction, gipos, is_tavg)
```
  Parse report spec procedure.

  - `report_spec` → `character(*), intent(in)`
  - `quant` → `character(:), allocatable, intent(out)`
  - `details` → `character(:), allocatable, intent(out)`
  - `item_type` → `integer, intent(out)`
  - `direction` → `integer, intent(out)`
  - `gipos` → `integer, dimension(2), intent(out)`
  - `is_tavg` → `logical, intent(out)`

---

```fortran
 subroutine valid_report_add(sim, identifier, diag_command, report, ierr)
```
  Valid report add procedure.

  - `sim` → `class(t_simulation), intent(inout)`
  - `identifier` → `character(len=*), intent(in)`
  - `diag_command` → `character(len=*), intent(in)`
  - `report` → `type(t_vdf_report), pointer, intent(inout)`
  - `ierr` → `integer, intent(out)`

---

```fortran
 subroutine write_restart(sim)
```
  Write restart procedure.

  - `sim` → `class ( t_simulation ), intent(inout)`

---

```fortran
 subroutine set_max_time(sim, val, ierr)
```
  Set max time procedure.

  - `sim` → `class(t_simulation), intent(inout)`
  - `val` → `character(len=*), intent(in)`
  - `ierr` → `integer, intent(out)`

---

```fortran
 subroutine set_omega_p0(sim, val, ierr)
```
  Set omega p0 procedure.

  - `sim` → `class(t_simulation), intent(inout)`
  - `val` → `character(len=*), intent(in)`
  - `ierr` → `integer, intent(out)`

---

```fortran
 subroutine set_species_math_func(sim, name, math_function, ierr)
```
  Set species math func procedure.

  - `sim` → `class(t_simulation), intent(inout)`
  - `name` → `character(len=*), intent(in)`
  - `math_function` → `character(len=*), intent(in)`
  - `ierr` → `integer, intent(out)`

---



##  5.3 Limitations

As this code is stil a proof of concept, many features are incomplete due to time limitations, these include:

<span style="color:red">**Critical Issues:**</span>

>- The isn't a failsafe mechanism when it comes to verifying if a user dictated diagnostic change will crash the simulation
>- Adding a subcategory of report such as `b1-tavg` without initializing the base category `b1` crashes the code. This is true for all diagnostics and is caused by null pointers. 
>- The order of commands for adding new diagnostics has to be exactly what is described in section [2.3 Valid diagnostic commands](#23-valid-diagnostic-commands)
>- Only basic diagnostics were developed. This means that **phasespaces** (for species) and some raw diagnostics aren't available. This won't crash the simulation, but will leave an error message.
>- Typechecking for inscrutions can have minor flaws. If your command crashes a simulation, please send the error logs and the list to support.
>- Moving window charge function is not yet implemented for neutrals 
>- Moving window charge function for species is not yet implemented for pre fabricated distributions such as **piecewise**, etc


<span style="color:orange">**Functional gaps:**</span>

>- Some unnecessary **DEBUG** messages were left in the code to aid with bug catching. Comment them before compiling the code if you find it necessary.
>- Particle diagnostic addition has problems and is incomplete. (To be fixed before September 2025)
>- No state rollback on failure
>- Error propagation via MPI is not well ensured
>- Memory allocation checks is not well ensured. Altough no strange behaviour has been seen, for bigger simulations that might be the case.
>- Definition of values for space and time averages still need to comply to base of OSIRIS rules. This means crashes might occur for invalid values
>- Some features such as laser turn-on/off were abandoned due to time constraints 
>- The main file is not well organized and should probably be split into two, one for the main routines and the other for the auxiliary initialization diagnostic routines. Some comments are still in Portuguese, so mind my laziness.

<span style="color:green">**Modifying the code:**</span>

If you want to modify the code, please contact support first to save yourself some time. It is also a good idea to contact the technical advisory email (ricardo.fonseca@iscte-iul.pt) for expertise on the internal OSIRIS structure. 

Take into account:

>- All changes to OSIRIS source code are marked by the `!*!` symbol. To find them, navigate the source directory and use `grep -rl '!\*!' . `
>- As of right now (Sept 2025), changing report relies on reusing initialization functions pre defined by OSIRIS. This might be a dangerous practice but it worked at the time. IF you fear there might be **memory leaks**, please contact tecnical support.
>- Before adding a feature that changes a base value of the code, first contact technical support. Some quantities crash teh simulation.
>- Try to change the OSIRIS source code as little as possible, as that may turn it incompatible with other modules. 
>- The main archive could have a better layout, my bad.
>- For code specificity, contact luis.nobrega@tecnico.ulisboa.pt, for questions about what is physically acceptable, thales.silva@tecnico.ulisboa.pt and for OSIRIS insight, ricardo.fonseca@iscte-iul.pt
>- Good luck, you will need it.

## 5.4 Future Development

**Enhancements:**

>- Laser turn on/off
>- Failsafe mechanism
>- HPC integration improvements
>- Code tidying and unnecessary **DEBUG** comment removal 
>- AI tool implementation (?)
>- Further paramether handling

<span style="color:magenta">**A note from a previous developer:**</span>

Congratulations! You've just inherited a piece of code developed with equal parts passion, physics, and panic. 

Think of this less as "not so professional" and more as "artisanal, hand-crafted code with character." It was built by a physics undergrad (me!) running on caffeine and cosmic radiation during finals season. It works… mostly. It's like a quantum state: both functioning and full of surprises until observed.

Yes, I left a few doors open for bugs. Consider them… features waiting to be discovered. 

My time at the helm is over, but I leave you with this wisdom:

- The comments are your friends (especially the ones that look like !*! or !^! – they're my cryptic breadcrumbs).

- When in doubt, try turning it off and on again. (Just kidding. Mostly.)

- And remember: my brilliant supervisors are just an email away. If you find something particularly eldritch, please do cc them—they're the real wizards here.

This code may have been rushed, but it was made with love (and maybe a few tears). I’m excited to see where you take it!

Happy coding—and may the runtime be ever in your favor.

Cheers,
The sleep-deprived physics undergrad who probably should’ve commented more 😄.

# Support & Contact

For issues/questions:

📧 Luís Nóbrega: luis.nobrega@tecnico.ulisboa.pt

or 

📧 Thales Silva: thales.silva@tecnico.ulisboa.pt

GitHub: [OSIRIS_with_Checkpoints](https://github.com/Luis-Nobrega/OSIRIS_with_Checkpoints/tree/main)