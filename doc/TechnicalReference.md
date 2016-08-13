# VisibleSim Technical Reference

This technical reference sheet should be used as a starting point for anyone who is interested in contributing to VisibleSim. It details how the repository is organised, attempts to clearly outline the software's architecture, and provides new contributors with everything they need to know about the coding style to adopt.

## Coding Style
The following rules should be taken into account when contributing new source code to VisibleSim, as they are the coding practises used on existing code, and homogeneous coding style makes it a lot easier to understand the project's sources.

### Whitespace and Indentation
Indentation size is at 4 spaces per logic level. No tabs. No trailing whitespace at the end of a line. Newline characters should be UNIX style (use `\n`, not `\r\n`).

__Warning__: There is no additional indentation inside a _Namespace_ logical unit.

### Lines Length
Lines of codes (LoC) should be kept as short as possible to avoid formatting issues while diff-ing, using split-views, or printing. Ideally, lines of codes should not be longer than __80__ characters, nonetheless, you may sometimes see LoC of up to __100__ characters in VisibleSim. It is highly discouraged to exceed this limit, since it makes code a lot harder to read.
Hence, please break long conditions after && and || logical operators, and fold long function parameter lists on several lines.

### Bracing Style 
Use [K&R bracing style](https://en.wikipedia.org/wiki/Indent_style#K.26R_style): opening brace at end of first line, cuddle else on both sides.

### General C++ Style Guide
Insightful guidelines for contributing to C++ source code can be found [here](https://google.github.io/styleguide/cppguide.html).
