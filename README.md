# Frame Analysis

This code is for analyzing frame using stiffness matrix. This code was developed based on a course project for structural analysis course.

## Getting Started

Run in Frame-Analysis/src/__init__.py with python while you keep the input file (FRAME.txt) in the same path as your code. The FRAME_RESULT.TXT will be generated after you run the code.


### Detail

It is possible to understand both input and output files by looking in the FRAME_RESULT.TXT file. 
For instance, these lines are related to defining different materials with different properties and the elements of a frame.

```
MATERIAL PROPERTIES
1   200000.0   0.00129
2   200000.0   0.00258
ELEMENT   NODES   MAT.
1   1   6   2
2   2   6   2
3   2   7   2
4   3   7   2
5   3   8   2
```

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details



