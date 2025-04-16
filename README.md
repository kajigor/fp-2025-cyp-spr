# Projects 

## Submission deadline 23:59 15.05.2025
## Final deadline 23:59 18.05.2025

A project amounts to 50% of your final grade. 
A project is to be done by a single student, no teamwork allowed. 
The maximum number of points for a project is 100, with concrete subtasks having different values.
Each project should be followed by a written report in which the following topics are discussed:

* What was the task.
* The architecture of your solution.
* Why certain architecture decisions were done. 
* Why certain libraries were chosen.
* Investigation of the performance.

Choose your project [here](https://docs.google.com/spreadsheets/d/1sbGvfjiFUUyU4O3KafuZkJbIqg1IJ_gzB83Q_uSTguk/edit?usp=sharing), 1 person per project. 

## 1. Markdown to HTML Converter

* (10 points) Console interface to specify input/output files.
* (20 points) Parser for a Markdown subset. You choose the subset, but please include at least headings, lists, links, emphasis, code blocks.
* (20 points) HTML renderer for the parsed AST.
* (15 points) Support for nested structures and inline formatting.
* (20 points) Performance analysis for large input files.
* (15 points) Unit and property-based tests.

## 2. Music Playlist Manager

* (10 points) Console interface for playlist commands (e.g., add, remove, play, search).
* (20 points) Functional data model for songs, playlists, and playback state.
* (20 points) Persistence layer: load/save playlists from JSON or another format.
* (15 points) Command parsing and error handling for invalid user input.
* (20 points) Implement additional features like shuffling, queueing, or filtering by tags.
* (15 points) Unit and property-based tests.

## 3. Pathfinding Visualizer

* (10 points) Interface for defining a 2D grid with obstacles, start, and goal.
* (20 points) Implement common pathfinding algorithms (A*, Dijkstra, BFS).
* (20 points) Allow step-by-step or full-run visualizations (textual or graphical).
* (15 points) Allow different heuristics or grid topologies (e.g., diagonal movement).
* (20 points) Compare performance and accuracy of different algorithms.
* (15 points) Unit and property-based tests.

## 4. Turtle Graphics Interpreter

* (10 points) REPL-style interface or script input mode.
* (20 points) Implement a small DSL for commands like forward, left, right, penup, pendown.
* (20 points) Interpreter that converts the DSL into a 2D drawing (ASCII or graphical).
* (15 points) Add support for variables and loops (e.g., `repeat 4 [forward 10 right 90]`).
* (20 points) Optional graphics backend using [gloss](https://hackage.haskell.org/package/gloss) or [diagrams](https://hackage.haskell.org/package/diagrams).
* (15 points) Unit and property-based tests.

## 5. Tokenizer for Natural Language Preprocessing

* (10 points) Console interface to specify input files and see tokenized output.
* (10 points) Preprocessing pipeline (e.g., Unicode normalization, lowercasing, whitespace splitting).
* (30 points) Implement the core BPE algorithm:
    * Build vocabulary of characters or initial tokens.
    * Count frequency of token pairs.
    * Merge most frequent pairs iteratively.
* (15 points) Allow saving and loading of the trained tokenizer model (merge rules or vocabulary).
* (20 points) Tokenize new text using the trained tokenizer, producing a sequence of tokens.
* (15 points) Unit and property-based tests.
  


