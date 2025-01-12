# Flappy Bird

## Overview

Welcome to the **Flappy Bird** game implemented in Assembly 8088! This game was developed from scratch by me in collaboration with my partner **@Waleed**. We used **DOSBox AFD** as a debugger and **NASM** as the compiler to bring this classic game to life on a vintage 8088 machine. The game runs in **graphics mode (0xA000)**, giving a unique retro feel.

## Features

- **Dynamic Game Settings**: Global variables allow for changing game settings on the fly.
- **Main Menu**: A fully animated main menu, setting the stage for the gameplay.
- **Pillar Movement**: Two pairs of pillars are generated at a time, moving across the screen.
- **Random Height Difference**: Pillars are placed at random vertical heights to create dynamic gameplay.
- **Gravity**: The bird continuously moves downward, simulating gravity.
- **Jumping Functionality**: The bird can jump to avoid hitting the pillars.
- **Collision Detection**: 
  - Detects when the bird goes out of bounds.
  - Detects collisions with the pillars.
- **Graphics Optimization**: Smoothed out flickering by minimizing pixel rendering, ensuring a more fluid animation.
- **Bitmap for the Ground**: Added a bitmap for the ground to improve the visual experience.
- **Python Bitmap Manipulation**: I wrote Python scripts to manipulate bitmap data, allowing more control over graphics rendering.
- **Multitasking for Music**: Implemented multitasking in Assembly to add music in a previous version, though this feature was omitted in the current version to prevent slowing down the game.

## Installation

To play the game, you need **NASM** and **DOSBox**. Follow these steps:

1. **Install NASM**: You'll need NASM to compile the Assembly code.
   - Download and install NASM from [here](https://www.nasm.us).

2. **Install DOSBox**: You'll need DOSBox to emulate the 8088 architecture.
   - Download and install DOSBox from [here](https://www.dosbox.com).

3. **Clone the Repository**:
   - You need to clone the repository:
     
   ```bash
   git clone https://github.com/AbdulMuhaiminIsmail/FlapFlap.git
   cd FlapFlap/Code
   ```

4. **Mount a Drive**:
  - You need to mount the directory as a drive in DOSBox. Use the following command inside DOSBox:

  ```bash
  mount c: /path/to/FlapFlap/Code
  c:
  ```

5. **Compile the Game**:
  - Inside DOSBox, use NASM to compile the Assembly code into a .com file:

  ```bash
  nasm -f bin -o flappy_bird.com Main.asm
  ```

6. **Run the Game**:
  - After compiling, run the game in DOSBox by executing:

  ```bash
  flappy_bird.com
  ```

   
