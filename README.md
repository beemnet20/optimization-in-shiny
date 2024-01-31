# Optimization in Shiny

## Overview

This Shiny app demonstrates the use of the hill climbing algorithm to optimally place hospitals on a grid with houses. The goal is to minimize the total distance from each house to its nearest hospital. This README provides an overview of the app, how to run it, and a brief explanation of the hill climbing algorithm.

## Running the App

To run this app:

1. Ensure that you have R and Shiny installed.
2. Clone or download this repository to your local machine.
3. Open R and set your working directory to the folder containing this app.
4. Run the app with the command `shiny::runApp()`.

## Features

- **Interactive Grid**: A grid where hospitals and houses are placed. Users can drag and drop hospitals to an optimal location.
- **Cost Display**: Shows the current cost and the optimal cost. 
- **Algorithm Explanation**: A step-by-step explanation of how the hill climbing algorithm works.

![screencapture.gif](screencapture.gif)

## Hill Climbing Algorithm

The hill climbing algorithm is an optimization technique used in the app as follows:

1. **Initial Placement**: Hospitals are randomly placed on the grid alongside fixed house locations.
2. **Cost Evaluation**: The algorithm calculates the total distance from each house to its nearest hospital.
3. **Seeking Improvements**: The app iteratively moves each hospital to test if a new position reduces the overall distance.
4. **Iterative Optimization**: This process is repeated, continually seeking positions that lower the total distance.
5. **Reaching Optimum**: The algorithm stops when it can no longer find a position that reduces the total distance.
6. **Final Output**: The app displays the optimized arrangement of hospitals.


