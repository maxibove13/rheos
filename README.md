# rheos

Ancient Greek: <em>$\rho \epsilon \omega $ (rhéō, "flow") </em>

<br>

> <em>Everything flows and nothing stand still.</em>
>
> > Heraclitus

<br>

Set of scripts to build one or several domains to simulate at CHAMAN LES flow solver, and visualize the results.

<br>

> <em>Python where we can, C where we must</em>
>
> > Sergey, Larry, Craig

<br>

This repository contains the following directories:

## samples

All the simulation cases should have a folder inside this directory.

### duct11

Test case. Currently it only contains block grid generating files `*.gin`

## src/pre

Build your own simulation domain with the desired geometry and export it through some files in order to run it in CHAMAN.

### Modules/Files

- checkgrd

  - Contains functions to read `*.grd` files containing grid information in binary.

- makegrid

  - Contains a function to execute grid3d.lnx program.

- checkgrid_matlab

  - Contains old MATLAB functions to read `*.grd` files. (Deprecated)

- grid3d.lnx

  - Program to compute and generate a structure grid block from `*.gin` ASCII files.

- grid3d.MB.f

  - Source code of grid3d.lnx

## src/post

Takes the outputs of CHAMAN as input and generates the desired visualization of the results.

<br>

### Modules

- mayavi_demo

  - Contains a function to test the `mayavi` library visualizing a fancy 3D function.

- plotgrids

  - Contains functions to plot grids.

<br>

The current main entry of this package is the script: `src/main_pre.py`

<br>

## Installation

1. Create a virtual environment (See `venv` section below) in `rheos` project root folder.

   i. Create venv

   ```bash
   python3 -m venv .venv
   ```

2. Install necessary dependencies via `requirements.txt`

   ```bash
   pip install -r requirements.txt
   ```

3. Add your virtual environment to Jupyter (the interactive IDE)

   ```bash
   python -m ipykernel install --user --name=.venv
   ```

4. (Optionally) Install python matlab.engine in order to run matlab functions from python

   i. Open a matlab session and obtain the root path of matlab

   ```matlab
   matlabroot
   ```

   ii. Go to `matlabroot/extern/engines/python`

   ```bash
   cd matlabroot/extern/engines/python
   ```

   Note that `matlabroot` means the path to matlab obtained in step i.

   iii. Install matlab.engine

   ```bash
   python setup.py install
   ```

## Execution

1.  Activate `venv`

    ```bash
    source .venv/bin/activate
    ```

<br>

Currently it is only implemented the task of processing already generated `*.gin` files and visualizing them in a 3D plot using `mayavi` library.

2. Run `main_pre.py` from an IDE or terminal

   ```bash
   ./src/main_pre.py
   ```

   Expect a modal window to popup and visualize your grid.

## General Python recomendations

## venv

venv is a python utlity to create virtual environment where all the python files and packages are stored isolated from the rest of your computer.
Different projects needs different packages and maybe different versions of those packages and even of python, so it is a good practice to isolate the python environment of your particular project or application.

1. Install venv in Debian/Ubuntu

   ```bash
   sudo apt-get install -y python3-venv
   ```

2. Create virtual environment

   Usually, the virtual environment goes inside your project/application directory, and inside a folder called `.venv`

   ```bash
   python3 -m venv <project_dir>/.venv
   ```

3. Activate virtual environment

   When you activate the virtual environment all the python related calls will use the python version and packages installed in your `.venv` directory.

   ```bash
   source <project_dir>/.venv/bin/activate
   ```

4. Deactivate virtual environment

   ```bash
   deactivate
   ```

## Dependencies

### Display installed dependencies

Activate your virtual environment and type:

```
pip freeze
```

That shows a list of your installed dependencies.

### Export dependencies to `requirements.txt`

```bash
pip freeze > requirements.txt
```

### Install dependencies given a `requirements.txt`

A collection of needed dependencies are usually stored in a file called `requirements.txt`
If such a file is present in your project, you can install all the dependencies needed for that project typing:

```bash
pip install -r requirements.txt
```

## Headers

As a convention, python scripts should roughly and generally contain the following (ultimately is up to the programmer):

1. The interpreter path in order to run the script like an executable with the used encoding below:

   ```python
   #!/usr/bin/env python3
   # -*- coding: UTF-8 -*-
   ```

2. A Docstring with a brief description of the script/module/function/class:

   ```python
   """This is a description of this module"""
   ```

3. Some dunder names, that is, especial variables that contains info about the module.

   ```python
   __author__ = "Maximiliano Bove"
   __email__ = "mbove@fing.edu.uy"
   __status__ = "Prototype" # Could also be "Development" and "Production"
   __date__ = "06/21"
   ```

4. All your imports separated by three categories:

   ```python
   # Built-in
   import os
   import datetime

   # Third-party
   import numpy

   # Owned
   from myScript import myFunction

   ```

## Python in VS Code

1.  Install the [Python extension](https://marketplace.visualstudio.com/items?itemName=ms-python.python)

2.  Inside your root project directory create a dir called `.vscode`.
3.  There, create a file called `settings.json`

4.  Specify in that file your default python interpreter for your current project (In this case I will use the interpreter of the virtual environment):

    ```json
    {
      // Sets this project .venv Python interpreter
      "python.pythonPath": "/<your_path_to_venv>/.venv/bin/python3"
    }
    ```

5.  Specify formatter

    i. Go to the `settings.json` of your workspace and make sure you have no default formatter:

    ```json
    {
      "python.formatting.provider": null
    }
    ```

    ii. Configure your format document shortcut going to `Files` > `Preferences` > `Keyboard Shortcuts`, and search for 'format'

    iii. Try to format your document using the configured shortcut.

    iv. When asked whether to install a formatter choose `pep8` (you can choose whatever you like actually)

## Comments, Tips & Reminders

- The python interpreter has several implementations, the most common one is `CPython`.

- Use `SymPy` to integrate symbolic mathematical expressions.

- Use `ipywidgets`. You can create interactive plots and modify any variable

- You can prevent warnings!

- `MATLAB` employs a copy-on-write memory management system, where an array may only be copied to a new memory location when it is modified. In `NumPy`, slices of arrays are views to the original array. They are like instances, or shortcuts to the original variable, like pointers to the same memory address. So, if you modify that view, the original arrays is modified also. It's the same variable with different name, exactly the same variable. See https://realpython.com/matlab-vs-python/#an-overview-of-basic-array-operations
