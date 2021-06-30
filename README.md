# rheo

> Everything flows and nothing stand still.
>
> > Heraclitus

Set of scripts aimed to build one or several simulation domains to simulate at chaman flow solver, and visualize the results.

This repository contains two main directories: pre and post

## pre

Build your own simulation domain with the desired geometry and export it through some files in order to run it in chaman.

## post

Takes the _rgc_ outputs of chaman flow solver as input and generates the desired visualization of the results.

> Python where we can, C where we must
>
> > Sergey, Larry, Craig

## Program instructions

1. Create and activate virtual environment (See `venv` section below) in `rheos` project root folder.

   i. Create venv

   ```bash
   python3 -m venv .venv
   ```

   ii. Activate venv

   ```bash
   source .venv/bin/activate
   ```

2. Install necessary dependencies via `requirements.txt`

   ```bash
   pip install -r requirements.txt
   ```

### venv

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
   # Generic
   import os
   import datetime

   # Well-known
   import numpy

   # Specifics
   from ugtm import eGTM

   # Owned
   from myScript import myFunction

   ```

## Run in Visual Studio Code

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

    iv. When asked if install a formatter choose `black` (you can choose whatever you like actually)

## Comments

The python interpreter has several implementations, the most common one by far is CPython.

Use SymPy to integrate symbolic mathematical expressions.
