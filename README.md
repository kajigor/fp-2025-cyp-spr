# Md2HtmlParser

## Task

### Markdown to HTML Converter
 - (10 points) Console interface to specify input/output files.
 - (20 points) Parser for a Markdown subset. You choose the subset, but please include at least:
   - headings
   - lists
   - links
   - ___emphasis___
   - code blocks
 - (20 points) HTML renderer for the parsed AST.
 - (15 points) Support for nested structures and inline formatting.
 - (20 points) Performance analysis for large input files.
 - (15 points) Unit and property-based tests.

# Markdown to HTML Parser

A Haskell library and command-line tool for parsing Markdown and converting it to HTML.

## Features

- Parse a Markdown subset into an AST
- Convert Markdown to HTML
- Command-line interface to specify input/output files
- Support for:
  - Headers (# Header)
  - Lists (bullet and numbered)
  - Links and images
  - Emphasis and strong formatting
  - Code blocks
  - Block quotes
  - Horizontal rules
- Nested structures and inline formatting

## Usage

```bash
stack run Md2HtmlParser-exe -- -h
```

### Running the Application

You can run the parser directly with Cabal:

**Bodl1 *Italic **Inner Bold** Italic2* Bold2**