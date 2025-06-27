# USER GUIDE

# Topic 1

# Prerequisites

- Have a working graphical interface in the local pc
- Have singularity installed on the HPC environment

# Quick guide

1. Enter the HPC environment with the -X option ex: `ssh -X deucalion`
2. Transfer [`graphics.sh`](http://graphics.sh) and `minha_imagem.sif` to the same folder
3. Give permissions to the executable with`chmod +x graphics.sh`
4. Run `./graphics.sh <file_name>`, without the `.sif` extension ex: `./graphics.sh minha_imagem`
5. Inside the container, do `nano ~/.bashrc` , then add the contents listed below and finally `source ~/.bashrc`
6. You’ll see a small propt with `>singularity` , just write `idl vis2d`, for example and wait
7. If you’re gonna use it for a while, allocate an interactive mode such as described here: https://docs.macc.fccn.pt/jobs/#interactive-slurm-jobs

### bashrc file contents to add:

```bash
# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
        . /etc/bashrc
fi

# User specific environment
if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]
then
    PATH="$HOME/.local/bin:$HOME/bin:$PATH"
fi
export PATH

# User specific aliases and functions
export IDL_PATH="<IDL_DEFAULT>:+/home/swe/visxd"

```

# Detailed guide

## Singularity image

### Initial Docker file

The Singularity image was built using a previous Docker image as a base. To build your own singularity, base yourself off this one. IDL and VisXD were then manually installed on the container as that required root priviledges. To create a new image, use the base docker image, modify it and then create a singularity container with `singularity build singularity_image.sif docker-archive://docker_image.tar`, changing the image names as necessary.

```bash
 # Ubuntu 22.04 LTS
 # Set up base OS environment

FROM ubuntu:22.04 
LABEL Luis Nobrega <luis.nobrega@tecnico.ulisboa.pt>

# Install the required packages

# Update package lists and install all necessary packages
RUN apt-get update && \
    apt-get install -y \
    hostname m4 make perl tar bash tcsh time git \
    openssh-client openssh-server net-tools curl wget \
    libxext6 libxrender1 libxpm4 imagemagick \
    sudo file libpng-dev nano \
    python3 python3-pip python3-dev \
    cmake pkg-config libcurl4-openssl-dev \
    libhdf5-dev hdf5-tools \
    rsync unzip jq && \
    apt-get clean

# Clean up APT caches
RUN apt-get clean

# Create user
RUN useradd -m swe
RUN echo "swe ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/swe
USER swe
WORKDIR /home/swe

#
# setup environment
#
ENV HOME /home/swe
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8
RUN echo "alias cp='cp -iv'" >> ~/.bashrc && \
    echo "alias mv='mv -iv'" >> ~/.bashrc && \
    echo "alias mkdir='mkdir -pv'" >> ~/.bashrc && \
    echo "alias ll='ls -FGlAhp'" >> ~/.bashrc

# Transfer IDL to inner environment
#COPY idl8.2.zip /home/swe/
#RUN unzip /home/swe/idl8.2.zip -d /home/swe/ && rm /home/swe/idl8.2.zip
```

### Internal scructure

By default, a SIngularity image mounts the root and home directories. To access all new modifications such as `IDL` and `VisXD`, use `cd /home/swe` . To change the base container, first change the Docker container and then create a new Singularity image (`.sif` ) as described above.

If you execute the `graphics` shell script, the directory from where it is executed will be mounted into the image and the files will be directly accessible. 

**NOTE: The script and the image have to be in the same folder** 

### Sending information in and out

After you process data and create frames, for example, all the data will be accessible in your local mounted directory (the directory you executed the script from). To send new data in, just exit the container, move the info to the directory (or ideally a sub directory) you are working on and execute the script again to gain access to the graphic cpabilities of the container.

## Shell script “`graphics.sh`”

### Pre-requisites

This script requires the forwarding of the local user graphical port throught the use of the `-X` flag while loggin in. For example `ssh -X deucalion` if you have your local `.shh` file configured with your ssh key.

### Setup and usage

After [`graphics.sh`](http://graphics.sh) is copied, give it executable permissions `chmod +x graphics.sh` .

The use is as follows `./graphics.sh <file_name>` with `<file_name>` being the name of the `.sif` file, WITHOUT the `.sif` extension. For example `./graphics my_image`.

### Running idl and visxd

This script mounts the local directory onto the above mentioned singularity container so that the files in the local directory are accessible. After executing the file, use `idl` and then treat your images with a visxd module such as `vis2d`.

### File structure

The file structure is as follows:

```bash
#!/bin/bash

# Check if an argument is provided
if [ -z "$1" ]; then
  echo "Usage: $0 <file>"
  echo 'The "file" should be a sif filename without the extension .sif'
  exit 1
fi

# Create a variable for path.sif
IMAGE_PATH="$1"
SIF_IMAGE="${IMAGE_PATH}.sif"
LOCAL_DIR="$PWD"

# Check if the .sif file exists
if [ ! -f "$SIF_IMAGE" ]; then
  echo "Error: SIF image '$SIF_IMAGE' not found!"
  exit 1
fi

# Check if graphical interface is available
if [ -z "$DISPLAY" ]; then 
    echo "ERROR: DISPLAY variable unset!"
    echo "Check if local machine has a graphical interface."
    echo "Else, check if login used: ssh -X deucalion."
    exit 1
else
    # Start Singularity shell with DISPLAY forwarding, enter the correct directory and start idl interactively
    singularity shell --env DISPLAY="$DISPLAY" --workdir "$LOCAL_DIR" --bind "$LOCAL_DIR" "$SIF_IMAGE"
    exit 0
fi

```